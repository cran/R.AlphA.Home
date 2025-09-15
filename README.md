# R.AlphA.Home <img src="man/figures/logo.png" align="right" style="height: 138px;" alt="Logo" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/R.AlphA.Home)](https://CRAN.R-project.org/package=R.AlphA.Home)
[![R-CMD-check](https://github.com/R-alpha-act/R.AlphA.Home/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R-alpha-act/R.AlphA.Home/actions/workflows/R-CMD-check.yaml)<!-- badges: end -->

> **Feel at Home using R, Thanks to Shortcuts Functions Making it Simple**

## Overview

R.AlphA.Home is a collection of personal functions designed to simplify and streamline common R programming tasks. This package provides reusable tools and shortcuts for frequently used calculations and workflows, making R programming more accessible and efficient.

## Installation

You can install the released version of R.AlphA.Home from [CRAN](https://CRAN.R-project.org) with:

```r
install.packages("R.AlphA.Home")
```

Or install the development version from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("R-alpha-act/R.AlphA.Home")
```

## Main Functions

### 📊 Data Manipulation and Processing
- **`cols_pad()`** - Add variables to ease data usage in pivot tables
- **`compareVars()`** - Compare table variables
- **`countSwitches()`** - Create incremented counters based on start/stop markers
- **`importAll()`** - Import and concatenate multiple data files efficiently *(enhanced v1.1.0)*
- **`left_join_checks()`** - Left join operations with built-in validation checks

### 📅 Date and Time Utilities
- **`rdate()`** - Generate random dates with similar usage as r* functions
- **`tmr()`** - Allow organized tracking of R code execution time *(new in v1.1.0)*

### 💾 Data Output and Formatting
- **`quickSave()`** - Save files with automatic date prefixing in designated directory
- **`sepThsd()`** - Quick number formatting with customizable defaults
- **`printif()`** - Conditionally print objects based *(new in v1.1.0)*

### 🎨 Graphics and Visual Utilities
- **`lum_0_100()`** - Adjust graphics window brightness for comfortable ggplot2 viewing
- **`ret_lum()`** - Adjust brightness of hex colors
- **`shiny_lum_0_100()`** - Set Shiny background and sidebar colors to chosen grey shades

### 🔧 System and Environment
- **`root()`** - Get root directory of current source file
- **`setOption()`** - Set global options from named list elements *(new in v1.1.0)*
- **`ralpha_fold()`** / **`ralpha_unfold()`** - Enhanced code folding functionality *(new in v1.1.0)*
- **RStudio Addins** - Convenient shortcuts for folding functions *(new in v1.1.0)*

### ⚠️ Deprecated Functions
- `timer()` → Use `tmr()` instead
- `foldAllBr()` → Use `ralpha_fold()` and `ralpha_unfold()` instead

## Dependencies

The package imports several essential R packages:
- **Data manipulation**: `data.table`, `dplyr`, `tibble`, `tidyr`
- **Date/time**: `lubridate`
- **String operations**: `stringr`, `stringi`
- **Graphics**: `ggplot2`, `grDevices`
- **File operations**: `openxlsx`, `R.utils`
- **Shiny**: `shiny`, `shinyWidgets`
- **Utilities**: `rstudioapi`, `magrittr`

## Contributing

We welcome contributions! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

## Support

-  **Documentation**: Access help with `?function_name` or `help(package = "R.AlphA.Home")`
-  **Bug Reports and Feature Requests**: [GitHub Issues](https://github.com/R-alpha-act/R.AlphA.Home/issues)

## License

This package is licensed under GPL-3.

## Authors

- **Raphaël Flambard** - *Author and Maintainer* - [raphael@ralpha.fr](mailto:raphael@ralpha.fr)
- **Adrien Cocuaud** - *Contributor* - [adrien@ralpha.fr](mailto:adrien@ralpha.fr)

---

*Making R programming feel like home, one function at a time.* 🏠
