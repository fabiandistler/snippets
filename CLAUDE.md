# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## About This Project

This is the `snippets` R package, which manages RStudio code snippets. The package allows users to install, backup, and manage RStudio snippet files for various programming languages (primarily R and Markdown).

## Development Commands

### R Package Development
```bash
# Check package (comprehensive checks including tests, examples, documentation)
R CMD check .

# Build package
R CMD build .

# Install package locally for testing
R CMD INSTALL .

# Run tests only
R -e "devtools::test()"

# Or using testthat directly
R -e "testthat::test_check('snippets')"

# Generate documentation
R -e "devtools::document()"

# Build and check package with devtools
R -e "devtools::check()"
```

### Testing
The package uses `testthat` framework for testing. Test files are located in `tests/testthat/` with the naming convention `test-*.R`.

### Documentation
- Uses `roxygen2` for documentation generation
- Package documentation is built with `pkgdown` (configured in `_pkgdown.yml`)
- Documentation website is published to GitHub Pages via GitHub Actions

## Architecture Overview

### Core Functionality
The package is organized into several functional modules:

1. **Modular Snippet Management** (`R/snippets--modules.R`, `R/snippets--modules-install.R`):
   - Manage snippet modules with naming convention `{module}-{type}.snippets`
   - Install, remove, and compose modular snippet collections
   - Registry system to track installed modules
   - Functions: `list_snippet_modules()`, `show_active_modules()`, `install_snippet_modules()`, `remove_snippet_modules()`, `install_all_package_modules()`

2. **File and Directory Management** (`R/snippets--files-and-dirs.R`, `R/snippets--files-and-dirs--internal.R`):
   - Functions to locate RStudio snippet directories and files
   - Cross-platform path handling for different RStudio versions

3. **Backup System** (`R/snippets--backup.R`):
   - Create and manage backups of existing snippet files before replacement
   - Uses `backup.tools` package for backup functionality

4. **Snippet Type Management** (`R/snippets--snippet-types.R`):
   - Functions to validate and match snippet types (r, markdown, etc.)
   - Type detection and validation logic

5. **Internal Helpers** (`R/internal--*.R`):
   - Utility functions for snippet preparation and internal operations

### Key Dependencies
- `backup.tools`: For backup functionality
- `fs`, `usethis`: File system operations and user interface
- `rstudioapi`: RStudio integration
- `dplyr`, `purrr`, `stringr`: Data manipulation and string operations
- `jsonlite`: JSON handling for module registry
- `readr`: Reading and writing files

### Package Structure
- `R/`: Main source code with thematic file organization
- `inst/snippets/`: Contains the actual snippet files distributed with the package
  - Module files follow convention: `{module}-{type}.snippets` (e.g., `dplyr-r.snippets`, `ggplot2-r.snippets`)
- `tests/testthat/`: Unit tests
- `man/`: Generated documentation files
- `snippets/`: Additional snippet collection (excluded from build via `.Rbuildignore`)
- `new-features/`: Development and experimental features

## Working with Snippets

### Installing Snippets (Module System - Recommended)
The primary workflow uses the modular snippet management system:

```r
# Install all available modules from package (recommended)
snippets::install_all_package_modules("snippets", type = "r")
snippets::install_all_package_modules("snippets", type = "all")

# Or install specific modules
snippets::install_snippet_modules(c("dplyr", "ggplot2"), type = "r")

# Install same modules for multiple types
snippets::install_snippet_modules("dplyr", type = c("r", "markdown"))

# List available modules
snippets::list_snippet_modules(type = "r")
snippets::list_snippet_modules(type = "all")

# Show currently active modules
snippets::show_active_modules(type = "r")

# Remove modules
snippets::remove_snippet_modules(c("dplyr"), type = "r")
```

### RStudio Integration
The package integrates with RStudio through:
- `rstudioapi` for file navigation and RStudio version detection
- Platform-specific path resolution for different RStudio versions
- Automatic detection of RStudio snippet directories

### Backup System
The package automatically creates backups before modifying existing snippet files, using timestamped filenames for backup copies.

### Module Registry
The modular system uses a JSON registry (`module_registry.json`) to track installed modules, including:
- Installation date and source
- Module composition and dependencies
- Allows for selective installation and removal of snippet collections

## CI/CD

GitHub Actions workflows handle:
- **R-CMD-check**: Runs package checks on multiple platforms (Ubuntu, macOS, Windows)
- **test-coverage**: Generates code coverage reports via codecov
- **pkgdown**: Builds and deploys documentation website
- **drat--publish-package**: Publishes package to custom repository

All workflows are triggered on pushes to main branches and pull requests.

## Workflow Guidelines

- You only work on my fork and all PRs go to my fork. Never try to push or PR to GegznaV's repo unless I tell you to do it.
