type option_params =
    { s0 : float  (* inital price *)
    ; k  : float  (* strike *)
    ; r  : float  (* risk-free rate *) 
    ; sigma : float  (* volatility *)
    ; t : float (* time to maturity *)
    }

let payoff_call ~k s_t =
    Float.max 0.0 (s_t -. k)

let simulate_terminal_price 
    ~(rng : Random.State.t)
    ~(params : option_params) =

    let z = Random.State.float rng 1.0 
    (* Box-Muller transform *)
    |> fun u -> let v = Random.State.float rng 1.0 in
                Float.sqrt (-2.0 *. Float.log u) *. Float.cos (2.0 *. Float.pi *. v)      
    in
    let drift = 
        (params.r -. 0.5 *. params.sigma *. params.sigma) *. params.t
    in
    let diffusion =
        params.sigma *. Float.sqrt params.t *. z
    in
    params.s0 *. Float.exp (drift +. diffusion)

let simulate_pair 
    ~(rng : Random.State.t)
    ~(params : option_params) =

    let u = Random.State.float rng 1.0 in
    let v = Random.State.float rng 1.0 in
    let z = Float.sqrt(-2.0 *. Float.log u) *. Float.cos (2.0 *. Float.pi *. v) in
    let drift =
        (params.r -. 0.5 *. params.sigma *. params.sigma) *. params.t
    in
    let vol = params.sigma *. Float.sqrt params.t in
    let s_plus = params.s0 *. Float.exp (drift +. vol *. z) in
    let s_minus = params.s0 *. Float.exp (drift -. vol *. z) in
    (s_plus, s_minus)

let simulate_payoff_and_spot
    ~(rng: Random.State.t)
    ~(params : option_params) =

    let s_t = simulate_terminal_price ~rng ~params in
    let payoff = payoff_call ~k:params.k s_t in
    (payoff, s_t)

let monte_carlo_price 
    ~(n_steps : int) 
    ~(seed : int) 
    ~(params : option_params) =

    let rng = Random.State.make [| seed |] in
    let sum =
        List.init n_steps (fun _ ->
            let s_t = simulate_terminal_price ~rng ~params in
            payoff_call ~k:params.k s_t)
        |> List.fold_left (+.) 0.0
    in
    Float.exp (-. params.r *. params.t) *. (sum /. Float.of_int n_steps)

let monte_carlo_antithetic
    ~(n_steps : int)
    ~(seed : int)
    ~(params : option_params) =

    let rng = Random.State.make [| seed |] in
    let sum =
        List.init n_steps (fun _ ->
            let (s_plus, s_minus) = simulate_pair ~rng ~params in
            0.5 *. (
                payoff_call ~k:params.k s_plus +.
                payoff_call ~k:params.k s_minus
                ))
        |> List.fold_left (+.) 0.0
    in
    Float.exp (-. params.r *. params.t) *. (sum /. Float.of_int n_steps)
        
let monte_carlo_control_variate
    ~(n_steps : int)
    ~(seed : int)
    ~(params : option_params) =

    let rng = Random.State.make [| seed |] 
    in
    let samples = List.init n_steps (fun _ ->
            simulate_payoff_and_spot ~rng ~params)
    in

    let mean_x =
        List.fold_left (+.) 0.0 (List.map fst samples)
        /. Float.of_int n_steps
    in

    let mean_y =
        List.fold_left (+.) 0.0 (List.map snd samples)
        /. Float.of_int n_steps
    in

    let cov_xy, var_y =
        samples
        |> List.fold_left (fun (cov, var) (x, y) ->
            ( cov +. (x -. mean_x) *. (y -. mean_y),
             var +. (y -. mean_y) ** 2.0)) (0.0, 0.0)
    in

    let beta = cov_xy /. var_y in
    let expected_y = params.s0 *. Float.exp (params.r *. params.t) in

    let adjusted_mean =
        mean_x -. beta *. (mean_y -. expected_y)
    in
    Float.exp (-. params.r *. params.t) *. adjusted_mean

let normal_cdf x =
    0.5 *. (1.0 +. Float.erf (x /. Float.sqrt(2.0)))

let black_scholes_price 
    ~(params : option_params) =

    let { s0; k; r; sigma; t } = params in
    let d1 = 
        (Float.log (s0 /. k) +. (r +. 0.5 *. sigma *. sigma) *. t) /. 
        (sigma *. Float.sqrt t)
    in
    let d2 = d1 -. (sigma *. Float.sqrt t)
    in
    s0 *. normal_cdf d1 -. k *. Float.exp (-. r *. t) *. normal_cdf d2

type pricer =
    n:int -> seed:int -> params:option_params -> float

let run_experiment ( price : pricer ) =
    let params =
        { s0 = 100.0
        ; k = 100.0
        ; r = 0.05
        ; sigma = 0.2
        ; t = 1.0
        }
    in
    let bs_price = black_scholes_price ~params 
    in
    List.iter (fun n -> 
            let mc_price = price ~n ~seed:33 ~params in
            Printf.printf "paths=%d BS price=%.4f MC price=%.4f err=%.4f\n"
            n bs_price mc_price (mc_price -. bs_price))
        [1_000; 5_000; 10_000; 50_000; 100_000]

let mc_pricer ~n ~seed ~params =
    monte_carlo_price ~n_steps:n ~seed ~params

let mc_antithetic_pricer ~n ~seed ~params =
    monte_carlo_antithetic ~n_steps:n ~seed ~params

let mc_control_variate ~n ~seed ~params =
    monte_carlo_control_variate ~n_steps:n ~seed ~params

let () =
    Printf.printf "Monte Carlo Pricer:\n";
    run_experiment mc_pricer;
    Printf.printf "\nMonte Carlo Antithetic Pricer:\n";
    run_experiment mc_antithetic_pricer;
    Printf.printf "\nMonte Carlo Control Variate Pricer:\n";
    run_experiment mc_control_variate