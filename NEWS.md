# R.AlphA.Home 1.1.0

## New Features
* `setOption()` - Sets a global option from a named list element
* `printif()` - Conditionally print an object
* `loadCheck()` - Load and install package if necessary
* `show_diff()` - Compare two texts or files with diffr
* `Rtimer` - New R6 class replacing the `timer()` function : provides functionality
to add timing checkpoints, calculate time differences, and generate summary data table.
* `ralpha_fold()` - Easily fold all code brackets
* `ralpha_unfold()` - Efficient way to unfold code brackets
* Added RStudio addins for folding functions (recommended shortcuts: Ctrl+Up/Down)

## Major Changes
**Newly deprecated**
* `timer()` - It is recommended to use the new class `RTimer`
* `foldAllBr()` - It is recommended to use `ralpha_fold()` and `ralpha_unfold()`

## Minor Improvements
* `importAll()` - Improved column type harmonization across imported files.
* `importAll()` - Added `renameTable` parameter for systematic column renaming across imported files.
* `importAll()` - Added `excludePattern` parameter to exclude files from import based on pattern matching.

## Bug Fixes
* `importAll()` - Fixed handling of all different possibilities between path
and fileList (relative/absolute/omitted path, combined with fileList as 
absolute/relative paths, or a pattern)

---

# R.AlphA.Home 1.0.0

## Initial CRAN Release

### Package Overview
R.AlphA.Home provides a collection of personal functions designed to simplify
and streamline common R programming tasks. This package offers reusable tools
and shortcuts for frequently used calculations and workflows, making R 
programming more accessible and efficient.

### Core Functions

**Data Manipulation and Processing:**
* `cols_pad()` - Add Variables to ease data usage in a Pivot Table
* `compareVars()` - Compare Table Variables
* `countSwitches()` - Create an incremented Counter, based on Start/Stop Markers
* `importAll()` - Function to Import and Concatenate Multiple data files
* `left_join_checks()` - Left Join with Validation Checks

**Date and Time Utilities:**
* `rdate()` - Generate Random Dates, with a similar usage as the r* functions
* `timer()` - Allow organized tracking of R code execution time

**Data Output and Formatting:**
* `quickSave()` - Save File in a Directory storing saves, prefixing it with current date
* `sepThsd()` - Quick number formatting with customizable defaults
* `printif()` - Conditionally Print an Object

**Graphics and Visual Utilities:**
* `lum_0_100()` - Adjust the Brightness of the Graphics Window for confortable viewing when using ggplot2
* `ret_lum()` - Adjust the Brightness of a Hex Color
* `shiny_lum_0_100()` - Set Shiny Background and Sidebar Colors to a Chosen Shade of Grey

**System and Environment:**
* `root()` - Get Root Directory of Current Source File
* `setOption()` - Sets a global option from a named list element
* `foldAllBr()` - Easily Fold Code Parts
