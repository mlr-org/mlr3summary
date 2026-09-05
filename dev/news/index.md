# Changelog

## mlr3summary (development version)

- [`summary()`](https://rdrr.io/r/base/summary.html) with
  `importance_measures = "shap"` now requires `fastshap` to be installed
  from r-universe (`https://bgreenwell.r-universe.dev`), since the
  package was archived on CRAN.

## mlr3summary 0.1.2

CRAN release: 2026-02-18

- resolve issue if `task$properties` is a vector
- compatibility: mlr3 1.4.0

## mlr3summary 0.1.0

CRAN release: 2024-04-24

- Initial release

## mlr3summary 0.1.1

CRAN release: 2026-01-23

- Add missing suggests
- Resolve error with integer transformation
- Add CITATION
