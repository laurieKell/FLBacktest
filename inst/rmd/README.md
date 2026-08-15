# Shared materials for application papers

FLBacktest provides four layers that Short-Cut, neaMac / ERP, blueMarine, and
related papers should share:

| Layer | Location | How to use |
|-------|----------|------------|
| **Code** | Exported S4 API (`hcrICES`, `backtest`, `shortcut`, …) | `library(FLBacktest)` — do not copy local `hcrICES*.R` |
| **Methods text** | `vignettes/methods-backtest.Rmd`, `inst/methods/methods-backtest.tex` | Cite vignette; `\input` the `.tex` fragment into the manuscript |
| **Supplementary Rmd** | `inst/rmd/01`–`04_*.Rmd` | Child-document, knit as HTML/Word supplement, or copy prose |
| **Worked pattern** | `inst/examples/erp_clean-style.R` | Commented end-to-end call sequence |

## Supplementary Rmd set

| File | Topic |
|------|--------|
| `01_conditioning.Rmd` | OM conditioning helpers and application checklist |
| `02_observation_error.Rmd` | Perfect / short-cut / full OEM |
| `03_management_procedure.Rmd` | Hockey-stick HCR closed loop |
| `04_performance_statistics.Rmd` | Trajectories + shared metric families |

Locate installed paths from R:

```r
FLBacktest::backtestMaterials()
FLBacktest::backtestMaterials("rmd")
FLBacktest::backtestMaterials("methods")
```

## Application vs package

**Package:** generic engine, Methods prose, supplementary Rmd fragments.  
**Application:** stock data, cleaning, SAG/refs, scenario labels, figures, paper-specific metrics (ABI, forage, ERPs), manuscript case-study subsections.

## Per-paper wiring

1. Depend on / install FLBacktest (`devtools::install("C:/active/flr/backtest")` or GitHub).
2. Replace local HCR / short-cut / `samIndex` copies with package calls.
3. Methods: keep a short case-study Methods note that points at the package vignette / `.tex`, then fill stock-specific bullets.
4. Supplement: knit or child-document the four `inst/rmd` files; append case-study checklists.

See also `docs/app_vs_package.md` in blueMarine for a worked application split.
