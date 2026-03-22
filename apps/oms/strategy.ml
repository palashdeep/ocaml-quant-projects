type signal =
  | Buy
  | Sell
  | Hold

type decision = {
  signal : signal;
  price : int;
  qty : int
}

type market = {
  price : int;
  reference : int;
}

let default_qty = 5

(* simple mean-reversion strategy rule *)
let decide { price; reference } =
  if price < reference - 2 then
    { signal = Buy; price; qty = 5 }
  else if price > reference + 2 then
    { signal = Sell; price; qty = 5 }
  else
    { signal = Hold; price; qty = 0 }