type side =
  Buy | Sell

type order_status =
    | Pending
    | Active
    | Partially_filled
    | Filled
    | Cancelled
    | Rejected

type order = {
  id : int;
  side : side;
  qty : int;
  price : int;
}

type order_state = {
  order : order;
  mutable remaining : int;
  mutable status : order_status;
}

type trade = {
  buy_id : int;
  sell_id : int;
  qty : int;
  price : int;
}

type oms = {
    orders : (int, order_state) Hashtbl.t;
    active_buys : (int, int list) Hashtbl.t;
    active_sells : (int, int list) Hashtbl.t
}

let create () = {
    orders = Hashtbl.create 128;
    active_buys = Hashtbl.create 32;
    active_sells = Hashtbl.create 32
}

let next_id =
    let counter = ref 0 in
    fun () ->
    incr counter;
    !counter

let create_order ~side ~price ~qty =
    { id = next_id (); side; price; qty }

let submit_order oms (order : order) = 
    if order.qty <= 0 then
        None
    else
        let state = {
            order;
            remaining = order.qty;
            status = Active
        }
        in
        Hashtbl.add oms.orders order.id state;

        let book =
            match order.side with
            | Buy -> oms.active_buys
            | Sell -> oms.active_sells
        in
        
        let existing =
            Hashtbl.find_opt book order.price |> Option.value ~default:[]
        in

        Hashtbl.replace book order.price (order.id :: existing);

        Some state

let remove_from_active oms state =
    let book =
        match state.order.side with
        | Buy -> oms.active_buys
        | Sell -> oms.active_sells
    in
    match Hashtbl.find_opt book state.order.price with
    | None -> ()
    | Some ids ->
        let filtered = List.filter (fun x -> x <> state.order.id) ids in
        Hashtbl.replace book state.order.price filtered

let update_order oms state qty =
    let new_remaining = state.remaining - qty in
    if new_remaining < 0 then
        failwith "Overfill detected in OMS"
    else begin
        state.remaining <- new_remaining;
        if new_remaining = 0 then begin
            state.status <- Filled;
            remove_from_active oms state
        end else
            state.status <- Partially_filled
    end

let apply_trade oms trade =
    let update id qty =
        match Hashtbl.find_opt oms.orders id with
        | None -> ()
        | Some state ->
            update_order oms state qty
    in
    
    update trade.buy_id trade.qty;
    update trade.sell_id trade.qty

let cancel_order oms id =
    match Hashtbl.find_opt oms.orders id with
    | None -> ()
    | Some state ->
        match state.status with
        | Filled | Cancelled -> ()
        | _ ->
            state.status <- Cancelled;

            let book =
                match state.order.side with
                | Buy -> oms.active_buys
                | Sell -> oms.active_sells
            in

            match Hashtbl.find_opt book state.order.price with
            | None -> ()
            | Some ids ->
                let filtered =
                    List.filter (fun x -> x <> id) ids
                in
                Hashtbl.replace book state.order.price filtered

let print_orders oms =
    Hashtbl.iter
        (fun id state ->
            Printf.printf
            "Order %d side=%s price=%d remaining=%d status=%s\n"
            id
            (match state.order.side with Buy -> "Buy" | Sell -> "Sell")
            state.order.price
            state.remaining
            (match state.status with
            | Pending -> "Pending"
            | Active -> "Active"
            | Partially_filled -> "Partial"
            | Filled -> "Filled"
            | Cancelled -> "Cancelled"
            | Rejected -> "Rejected"))
    oms.orders

let () =
    let oms_state = create () in

    let o1 = create_order ~side: Buy ~price: 100 ~qty: 5 in
    let o2 = create_order ~side: Sell ~price: 100 ~qty: 5 in
    let o3 = create_order ~side: Sell ~price: 105 ~qty: 15 in

    ignore (submit_order oms_state o1);
    ignore (submit_order oms_state o2);
    ignore (submit_order oms_state o3);

    let t1 = {
        buy_id = 1; sell_id = 2; price = 100; qty = 5
    }
    in

    apply_trade oms_state t1;

    cancel_order oms_state 3;

    print_orders oms_state;