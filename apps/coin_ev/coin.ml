open Core

let sim_one ~n_flips =
    let rec loop i acc =
        if i = n_flips then acc
        else 
            let r = Random.int 2 in
            loop (i + 1) (acc + if r = 0 then 1 else -1)
    in
    loop 0 0

let monte_carlo ~n_trials ~n_flips =
    let sum = ref 0.0 in
    for _ = 1 to n_trials do
        sum := !sum +. Float.of_int (sim_one ~n_flips)
    done;
    !sum /. Float.of_int n_trials

let () =
    Random.self_init ();
    let n_trials = 100_000 in
    let n_flips = 100 in
    let ev = monte_carlo ~n_trials ~n_flips in
    printf "EV over %d trials of %d flips = %.6f\n" n_trials n_flips ev