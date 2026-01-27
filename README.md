# OCaml Quantitative Projects

This repository contains a collection of small, focused OCaml projects exploring numerical simulation and market microstructure through correctness-first, well scoped design.

The projects emphasize:
- clear data modeling using strong static types
- deterministic and reproducible behavior
- explicit handling of state and invariants
- implementations that are easy to reason about end-to-end

Each is intentionally self-contained and avoids unnecessary abstraction or premature optimization

## Repository Structure

```graphql
apps/
  coin_ev/          # Monte Carlo coin-flip simulation
  option_pricer/    # Monte Carlo option pricing
  lob/              # Limit Order Book matching engine
```

Each project builds independently using `dune`.

## 1. Monte Carlo Coin Flip EV Simulator

**Location:** `apps/coin_ev`

