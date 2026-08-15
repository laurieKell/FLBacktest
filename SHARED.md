# Shared backtest materials for application papers

Three (and future) papers should consume the **same** FLBacktest stack:

1. **Code** — package API (`hcrICES`, `backtest`, `shortcut`, `retroBias`,
   `samIndex*`, `hcrParams`, `recDevs`, `omIters`, `backtestResults`, …).
2. **Methods text** — `vignettes/methods-backtest.Rmd` and
   `inst/methods/methods-backtest.tex`.
3. **Supplementary Rmd** — `inst/rmd/`:
   - `01_conditioning.Rmd`
   - `02_observation_error.Rmd`
   - `03_management_procedure.Rmd`
   - `04_performance_statistics.Rmd`

Details and consume pattern: [`inst/rmd/README.md`](inst/rmd/README.md).

```r
devtools::load_all()          # or install the package
backtestMaterials()           # all paths
backtestMaterials("methods")  # LaTeX Methods fragment
backtestMaterials("rmd")      # supplementary directory
```

| Paper | Typical path | Status vs FLBacktest |
|-------|--------------|----------------------|
| blueMarine | `C:/active/blueMarine` | Uses package; Methods note points at vignette |
| Short-Cut / ERP | `C:/active/shortcut-full` | Still has local `hcrICES*` — migrate to package |
| neaMac resubmission | `C:/active/flrpapers/neaMac` | Still has local `source/` — migrate to package |

Application repos keep: stock cleaning, SAG/refs, scenario labels, figures,
paper-specific metrics, and a short case-study Methods subsection.
