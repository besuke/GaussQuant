# GaussQuant

High-level quantitative finance workflows built on QuantLibGauss.

## Package layers

- `QuantLibGauss`: QuantLib infrastructure and low-level wrappers
- `GaussQuant`: reusable models and analytical workflows
- `LagrangeFinance`: examples, tables, plots, and explanations

GaussQuant functions use the `gq_` prefix.

## QuantLib build materials

The manual and SWIG-generated package sources for rebuilding the QuantLib R
package from C++ are kept in [`tools/quantlib_swig/`](tools/quantlib_swig/README.md)
(moved from QuantLib_tidy). They are excluded from the package build.
The SWIG interface definitions (`*.i` files) have not been obtained, so a
rebuild from these materials alone has not been verified.
GaussQuant itself uses the installed `QuantLib` package as a regular dependency.
