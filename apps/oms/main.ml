let () =
  let oms_state = Oms.create () in
  let book = Lob.empty_book in

  (* simulated market prices *)
  let prices = [100; 101; 99; 98; 102; 103; 97] in

  let reference_price = 100 in

  let handle_order side price qty book =
    let order =
      Oms.create_order ~side ~price ~qty
    in
    match Oms.submit_order oms_state order with
    | None -> book
    | Some _ ->
      let lob_order = Exchange_adapter.oms_to_lob order in
      let book, trades =
        Lob.match_order book lob_order []
      in
      List.iter
        (fun t ->
          Oms.apply_trade oms_state
            (Exchange_adapter.lob_trade_to_oms t))
        trades;
      book
  in

  let _ =
    List.fold_left
      (fun book price ->
        let decision =
          Strategy.decide { price; reference = reference_price }
        in

        match decision.signal with
        | Strategy.Hold ->
            Printf.printf "Price %d → Hold\n" price;
            book
            
        | Strategy.Buy ->
            Printf.printf "Price %d → Buy signal\n" price;
            handle_order Oms.Buy decision.price decision.qty book
            
        | Strategy.Sell ->
            Printf.printf "Price %d → Sell signal\n" price;
            handle_order Oms.Sell decision.price decision.qty book
      )
      book
      prices
  in

  Printf.printf "\nFinal OMS State\n";
  Oms.print_orders oms_state