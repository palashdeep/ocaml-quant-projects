let oms_to_lob (o : Oms.order) : Lob.order =
  {
    id = o.id;
    side =
      (match o.side with
      | Oms.Buy -> Lob.Buy
      | Oms.Sell -> Lob.Sell);
    price = o.price;
    qty = o.qty;
    kind = Lob.Limit;
  }

let lob_trade_to_oms (t : Lob.trade) : Oms.trade =
  {
    buy_id = t.buy_id;
    sell_id = t.sell_id;
    price = t.price;
    qty = t.qty;
  }