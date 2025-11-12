# snippets 0.0.22 (Development)

## Bug Fixes

* Fixed critical bug in `open_rstudio_snippets_file()` that was checking the wrong variable (`type` instead of `file`), causing incorrect file existence detection

## Performance Improvements

* Improved performance of module discovery and listing functions by replacing inefficient `rbind()` in loops with list accumulation followed by `data.table::rbindlist()`
  - Affects: `discover_local_files()`, `list_snippet_modules()`, `show_active_modules()`
  - Expected performance improvement for repositories with many snippet modules

## Code Quality

* Added input validation to `install_snippet_modules()` and `remove_snippet_modules()`
  - Validates parameter types (character, logical)
  - Validates parameter lengths
  - Provides clear error messages for invalid inputs

* Removed obsolete `stringsAsFactors = FALSE` arguments throughout the codebase (no longer needed in R >= 4.0.0)

* Added test suite for input validation (`tests/testthat/test-input-validation.R`)

* Added comprehensive code review findings document (`REVIEW_FINDINGS.md`)

## Dependencies

* Added `data.table` to Imports for improved data frame operations

## Internal Changes

* Replaced `dplyr::bind_rows()` with `data.table::rbindlist()` for better performance
* Improved code maintainability by consolidating data frame operations

---

# snippets 0.0.21

## Features

* Modular snippet management system with intelligent auto-discovery
* Support for installing snippets from local directories and URLs
* Registry-based tracking of installed modules
* Automatic backup creation before modifying snippet files
* Fallback mechanism from specific modules to generic type files

## Documentation

* Added comprehensive module installation vignette
* Improved README with quickstart examples
* Added pkgdown documentation website

---

# snippets 0.0.20 and Earlier

See commit history for changes in earlier versions.
