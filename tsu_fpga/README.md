# TSU-FPGA — Thermodynamic Sampling Unit for Tang Nano 9K

A digital emulator of a Thermodynamic Sampling Unit (TSU) in synthesisable Verilog,
targeting the **Gowin GW1NR-9** FPGA on the Tang Nano 9K board.

---

## Architecture

```
┌──────────────────────────────────────────────────────────────────┐
│  tsu_top.v                                                        │
│                                                                   │
│  ┌──────────────┐    ┌────────────────────────────────────────┐  │
│  │ gibbs_       │    │  8 × pbit_cell  (Gibbs update)         │  │
│  │ scheduler    │───▶│  8 × lfsr_prng  (unique seeds)         │  │
│  │ (2-colour)   │    │  1D ring Ising, frustrated bond @3-4   │  │
│  └──────────────┘    └────────────────┬───────────────────────┘  │
│                                       │ spin_vec[7:0]             │
│  ┌──────────────┐    ┌───────────────▼───────────────────────┐  │
│  │ uart_tx_     │◀───│  energy_calc  (H = -ΣJ s_i s_j)       │  │
│  │ framer       │    └───────────────────────────────────────┘  │
│  │ (115200 8N1) │                                                │
│  └──────┬───────┘    RGB LED: blue=ordered / red=disordered      │
│         │ UART TX                                                 │
└─────────┼────────────────────────────────────────────────────────┘
          ▼
    USB-UART → host_monitor.py
```

## What each file does

| File | Role |
|------|------|
| `rtl/pbit_cell.v` | Core p-bit: computes local field, sigmoid LUT, Bernoulli sample |
| `rtl/lfsr_prng.v` | 32-bit Galois LFSR — one per cell, unique seeds |
| `rtl/gibbs_scheduler.v` | 2-colour block Gibbs: even cells → odd cells → repeat |
| `rtl/ising_weight_rom.v` | J/h weight storage (LUT-RAM, upgradeable to BSRAM) |
| `rtl/energy_calc.v` | Ising Hamiltonian H = −ΣJ s_i s_j |
| `rtl/uart_tx_framer.v` | 8N1 UART TX, 5-byte packet per sweep |
| `rtl/tsu_top.v` | Top-level, wires everything, LED driver |
| `tb/tsu_tb.v` | Icarus Verilog testbench |
| `syn/tangnano9k.cst` | Physical pin constraints |
| `syn/synth.tcl` | Gowin IDE synthesis script |
| `host_monitor.py` | Python host: reads UART, prints spins + energy |

---

## The p-bit update rule

Each cell implements:

```
I_i  = Σ_j  J_ij · s_j  +  h_i        (local field, integer arithmetic)
σ_i  = sigmoid(I_i / T)                 (5-bit clamped, 31-entry LUT)
s_i  ~ Bernoulli(σ_i)                   (compare rnd[7:0] < σ_i)
```

This is exactly the Gibbs conditional of a Boltzmann machine / Ising model.

---

## Physics being emulated

**Ising model** — 8 spins on a ring:

```
H = -Σ J_ij s_i s_j

Bonds:  J = +20 (ferromagnetic) for all pairs
        J = -20 (antiferromagnetic) for bond 3-4  ← frustrated
```

Frustrated bond creates a classic **frustrated Ising chain**.
Expected ground states (E = -120):  `11110000` and `00001111`
(domain wall sits at the frustrated bond).

The simulation output confirms this:
```
[SWEEP 700] spins=11111111  E=-120   ← all up  (ground state)
[SWEEP 1200] spins=11110000  E=-120  ← domain wall at frustrated bond
```

---

## Build & simulate

### Icarus Verilog (quick check)
```bash
iverilog -g2012 -o tsu_sim \
    tb/tsu_tb.v \
    rtl/tsu_top.v rtl/pbit_cell.v rtl/lfsr_prng.v \
    rtl/gibbs_scheduler.v rtl/energy_calc.v rtl/uart_tx_framer.v

vvp tsu_sim          # prints sweep data
gtkwave tsu_sim.vcd  # optional waveform view
```

### Gowin EDA (synthesise for Tang Nano 9K)
1. Open Gowin IDE → New Project → Device: GW1NR-LV9QN88PC6/I5
2. Add all files from `rtl/` and `syn/tangnano9k.cst`
3. Top module: `tsu_top`
4. Run: **Synthesis → Place & Route → Generate Bitstream**
5. Flash with openFPGALoader:
```bash
openFPGALoader -b tangnano9k tsu_top.fs
```

### Host monitor
```bash
pip install pyserial
python3 host_monitor.py /dev/ttyUSB0
```

---

## Resource estimate (Tang Nano 9K)

| Resource | Used (est.) | Available |
|----------|-------------|-----------|
| LUT4     | ~180        | 8448      |
| FF       | ~60         | 8448      |
| BSRAM    | 0           | 26 × 18Kb |
| DSP      | 0           | 20        |

Leaves ample room to scale to **1024+ p-bits** using BSRAM for weights.

---

## Scaling up

| Scale | Change needed |
|-------|---------------|
| 16 p-bits | Widen `N`, add more prng instances |
| 1024 p-bits | Move weights to BSRAM, use `gowin_sp` primitive |
| Boltzmann machine | Change topology; use dense J matrix in BSRAM |
| Hopfield network | Load stored patterns as biases |
| Anneal | Drive `temperature` down from 15→1 via counter |
| ASIC path | Replace `lfsr_prng` with true analog noise circuit |

---

## What this is NOT

This is a **digital emulator** — not a true thermodynamic computer.
Real TSUs (e.g. Extropic's chip) use **physical transistor noise** as
the stochastic source, which is what gives them energy efficiency gains.
This design uses an LFSR PRNG — same math, deterministic silicon.

The FPGA is the **prototype stage** in the path:
```
Algorithm → Python/JAX → FPGA emulator → RTL verification → ASIC
```
