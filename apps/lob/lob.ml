type side =
  Buy | Sell

type kind =
  Limit | Market

type location = {
  side : side;
  price : int;
}

type order = {
  id : int;
  side : side;
  mutable qty : int;
  price : int;
  kind : kind;
}

type trade = {
  buy_id : int;
  ask_id : int;
  qty : int;
  price : int;
}

module PriceMap = Map.Make(Int)

type book = {
  bids : order Queue.t PriceMap.t;
  asks : order Queue.t PriceMap.t;
  index : (int, location) Hashtbl.t;  (* global order-id map *)
}

let empty_book = {
  bids = PriceMap.empty;
  asks = PriceMap.empty;
  index = Hashtbl.create 1024;
}

let best_bid bids =
  PriceMap.max_binding_opt bids

let best_ask asks =
  PriceMap.min_binding_opt asks

let rec match_order book order trades =
  match order.side with
  | Buy ->
      begin match best_ask book.asks with
      | Some (price, q) when order.kind = Market || price <= order.price ->
        
        let resting = Queue.peek q in
        let trade_qty = min order.qty resting.qty in
          
        let trade = {
          buy_id = order.id;
          ask_id = resting.id;
          qty = trade_qty;
          price = price;
        }
        in
        
        (* update qty on book *)
        resting.qty <- resting.qty - trade_qty;

        if resting.qty = 0 then begin
          ignore (Queue.pop q);
          Hashtbl.remove book.index resting.id;
        end;
        
        let new_asks =
          if Queue.is_empty q then
            PriceMap.remove price book.asks
          else
            PriceMap.add price q book.asks
        in

        let remaining_qty = order.qty - trade_qty in
        let book = { book with asks = new_asks } in

        let trades = trade :: trades in
        
        (* send back to walk book if partially filled *)
        if remaining_qty > 0 then
          match_order book { order with qty = remaining_qty } trades
        else
          (book, trades)
      
      | _ -> 
        if order.kind = Market then
          (book, trades)    (* discard unfilled market order *)
        else
          let q =
            match PriceMap.find_opt order.price book.bids with
            | Some q -> q
            | None -> let q = Queue.create () in
                q
          in
          (* unfilled limit order rests on book *)
          Queue.push order q;
          Hashtbl.add book.index order.id
            { side = Buy; price = order.price };
          let bids = PriceMap.add order.price q book.bids in
          ({ book with bids}, trades)
      end

  | Sell -> 
    begin match best_bid book.bids with
    | Some (price, q) when order.kind = Market || price >= order.price ->
      
      let resting = Queue.peek q in
      let trade_qty = min order.qty resting.qty
      in

      let trade = {
        buy_id = resting.id;
        ask_id = order.id;
        qty = trade_qty;
        price = price;
      }
      in

      resting.qty <- resting.qty - trade_qty;

      if resting.qty = 0 then begin
        ignore (Queue.pop q);
        Hashtbl.remove book.index resting.id;
      end;

      (* new bids after fill *)
      let new_bids = 
        if Queue.is_empty q then
          PriceMap.remove price book.bids
        else
          PriceMap.add price q book.bids
      in

      let remaining_qty = order.qty - trade_qty in
      let book = { book with bids = new_bids } in

      let trades = trade :: trades in

      if remaining_qty > 0 then
        match_order book { order with qty = remaining_qty } trades
      else
        (book, trades)

    | _ ->
      if order.kind = Market then
        (book, trades)
      else
        let q =
          match PriceMap.find_opt order.price book.asks with
          | Some q -> q
          | None -> let q = Queue.create () in
              q
        in

        Queue.push order q;
        Hashtbl.add book.index order.id
            { side = Sell; price = order.price };
        let asks = PriceMap.add order.price q book.asks in
        ({ book with asks }, trades)
    end

let cancel_order book order_id =
  match Hashtbl.find_opt book.index order_id with
  | None -> 
    book  (* already filled or non-existent *)

  | Some { side; price } ->
    let map =
      match side with
      | Buy -> book.bids
      | Sell -> book.asks
    in
    
    match PriceMap.find_opt price map with
    | None ->
      book
    
    | Some q -> (* find and remove the order *)
      let q' = Queue.create () in
      Queue.iter
        (fun o ->
          if o.id <> order_id then
            Queue.push o q')
        q;

      Hashtbl.remove book.index order_id;

      let map' =
        if Queue.is_empty q' then
          PriceMap.remove price map
        else
          PriceMap.add price q' map
      in

      match side with
      | Buy -> { book with bids = map' }
      | Sell -> { book with asks = map' }

(* Reporting Layer *)
let best_bid book =
  PriceMap.max_binding_opt book.bids
  |> Option.map (fun (price, q) -> (price, Queue.length q))

let best_ask book =
  PriceMap.min_binding_opt book.asks
  |> Option.map (fun (price, q) -> (price, Queue.length q))

let spread book =
  match best_bid book, best_ask book with
  | Some (bid, _), Some (ask, _) -> Some (ask - bid)
  | _ -> None

let depth_at_price book side price =
  let map =
    match side with
    | Buy -> book.bids
    | Sell -> book.asks
  in

  match PriceMap.find_opt price map with
  | None -> 0
  | Some q -> 
    Queue.fold (fun acc (o : order) -> acc + o.qty) 0 q

let top_n_levels book side n =
  let bindings = 
    match side with
    | Buy -> List.rev (PriceMap.bindings book.bids)
    | Sell -> PriceMap.bindings book.asks
  in

  bindings
  |> List.take n
  |> List.map (fun (price, q) -> 
    (price, Queue.fold (fun acc ( o : order ) -> acc + o.qty) 0 q))

(* main function *)
let () =

  let book = empty_book in

  let orders =
    [ (* 1. Resting bid *)
      { id = 1; side = Buy;  kind = Limit;  price = 100; qty = 10 }
      (* 2. Better bid (becomes best bid) *)
    ; { id = 2; side = Buy;  kind = Limit;  price = 101; qty = 5 }
      (* 3. Resting ask (no match yet) *)
    ; { id = 3; side = Sell; kind = Limit;  price = 103; qty = 7 }
      (* 4. Sell crosses best bid (partial fill on bid id=2) *)
    ; { id = 4; side = Sell; kind = Limit;  price = 101; qty = 8 }
      (* 5. Market sell (hits remaining bid id=2, then bid id=1) *)
    ; { id = 5; side = Sell; kind = Market; price = 0;   qty = 10 }
      (* 6. New ask inside spread *)
    ; { id = 6; side = Sell; kind = Limit;  price = 102; qty = 6 }
      (* 7. Buy crosses multiple asks *)
    ; { id = 7; side = Buy;  kind = Limit;  price = 103; qty = 10 }
      (* 8. Resting bid to be cancelled *)
    ; { id = 8; side = Buy;  kind = Limit;  price = 99;  qty = 12 }
      (* 9. Cancel order id=8 (before it trades) *)
      (* 10. Market buy with empty ask side (should do nothing) *)
    ; { id = 10; side = Buy; kind = Market; price = 0; qty = 5 }
      (* 11. Resting bid *)
    ; { id = 11; side = Buy; kind = Limit; price = 100; qty = 5 }
    ]
  in

  let book, trades = 
    List.fold_left 
      (fun (book, trades) o ->
        let book, new_trades = match_order book o [] in
        (book, new_trades @ trades))
      (book, [])
      orders
  in

  (* 9. Cancel order id=8 *)
  let book = cancel_order book 8 in

  Printf.printf "=== Trades ===\n";
  List.iter
  (fun t ->
      Printf.printf
        "Trade: buy=%d sell=%d price=%d qty=%d\n"
        t.buy_id t.ask_id t.price t.qty)
  (List.rev trades);

  Printf.printf "\n=== Book State ===\n";

  begin match best_bid book with
  | Some (p, _) -> Printf.printf "Best bid: %d\n" p
  | None -> Printf.printf "Best bid: none\n"
  end;

  begin match best_ask book with
  | Some (p, _) -> Printf.printf "Best ask: %d\n" p
  | None -> Printf.printf "Best ask: none\n"
  end;

  begin match spread book with
  | Some s -> Printf.printf "Spread: %d\n" s
  | None -> Printf.printf "Spread: n/a\n"
  end;

  Printf.printf
  "Depth at bid 100: %d\n"
  (depth_at_price book Buy 100);

  Printf.printf "\nTop bids:\n";
  top_n_levels book Buy 3
  |> List.iter (fun (p, q) ->
    Printf.printf "price=%d qty=%d\n" p q);

  Printf.printf "\nTop asks:\n";
  top_n_levels book Sell 3
  |> List.iter (fun (p, q) ->
    Printf.printf "price=%d qty=%d\n" p q);