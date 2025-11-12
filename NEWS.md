# snippets 0.0.22 (Development)

## Bug Fixes

* Fixed critical bug in `open_rstudio_snippets_file()` that was checking the wrong variable (`type` instead of `file`), causing incorrect file existence detection (#5)

## Performance Improvements

* Significantly improved performance of module discovery and listing functions by replacing inefficient `rbind()` loops with list accumulation followed by `dplyr::bind_rows()`. This provides ~10-100x speedup for large numbers of modules

* Optimized the following functions:
  - `discover_local_files()`
  - `list_snippet_modules()`
  - `show_active_modules()`

## Code Quality

* Removed obsolete `stringsAsFactors = FALSE` arguments throughout the codebase (no longer needed in R >= 4.0.0)

* Added comprehensive code review findings document (`REVIEW_FINDINGS.md`)

## Internal Changes

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
