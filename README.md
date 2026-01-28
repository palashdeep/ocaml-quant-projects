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

### Overview
A simple Monte Carlo simulation estimating the expected value of repeated fair coin flips. This project serves as a compact numerical example with an exphasis on clarity, deterministic randomness, and functional structure.

### Key Characteristics
- Explicit random number generation
- Deterministic results via fixed seds
- Clear separation between simulation logic and aggregation
- Minimal, readable implementation

### Concepts
- Monte Carlo estimation
- Functional control flow
- Basic performance considerations

### Run
```bash
dune exec ./apps/coin_ev/coin.exe
```

## 2. Monte Carlo Option Pricer

**Location:** `apps/option_pricer`

### Overview
A European call option pricer implemented via Monte Carlo simulation of geometric Brownian motion. The implementation is validated against the Black-Schole closed form solution and incrementally extended with standard variance-reduction techniques.

The focus is on correctness, numerical reasoning, and experimental discipline.

### Features
- Risk-neutral GBM simulation
- Black-Scholes analytical benchmark
- Antithetic variates
- Control variates using terminal stock price
- Reproducible convergence experiments

### Design Notes
- Numerical logic is kept explicit and transparent
- Functional structure is preferred
- Estimators are implemented independently to allow direct comparison

### Run
```bash
dune exec ./apps/option_pricer/option_pricer.exe
```

## 3. Limit Order Book Matching Engine

**Location:** `apps/lob`

### Overview
A deterministic single-instrument limit order book implementing price-time priority matching. The engine models the core mechanics of an exchange matching system while remaining compact and readable.

The design prioritizes correctness, explicit invariants, and predictable behavior over feature bandwidth.

### Features
- Price-time priority matching
- FIFO queues at each price level
- Partial and full fills
- Limit and Market orders
- Order cancellation
- Read-only book inspection utilities

### Design Highlights
- Ordered maps for price levels
- FIFO queues to preserve time priority
- Localized mutation for order quantities
- Explicit handling of edge cases such as partial fills and cancellation

### Inspection Utilities
- Best bid / ask
- Spread
- Depth at a given price level
- Top-N price levels

### Run
```bash
dune exec ./apps/lob/lob.exe
```

## Build Instructions

From the repository root:
```bash
opam switch create . ocaml-base-compiler.5.1.0 --deps-only
eval $(opam env)
opam install dune
dune build
```

## Notes on Scope

These projects are intentionally limited in scope. The goal is to present complete, correct implementations that can be understood end-to-end, rather than large systems with incidental complexity.

Additional layers (e.g. agent simulation, latency modeling, or strategy logic) were deliberaterly excluded to keep core logic clear and maintainable.