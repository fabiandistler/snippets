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

1. **Snippet Installation** (`R/snippets--install.R`): 
   - Functions to install snippets from packages or directories
   - `install_snippets_from_package()` and `install_snippets_from_dir()` are main entry points

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

### Package Structure
- `R/`: Main source code with thematic file organization
- `inst/snippets/`: Contains the actual snippet files distributed with the package
- `tests/testthat/`: Unit tests
- `man/`: Generated documentation files
- `snippets/`: Additional snippet collection (excluded from build via `.Rbuildignore`)

## Working with Snippets

### Installing Snippets
The main workflow involves installing snippet files from the package to the user's RStudio configuration:

```r
# Install all available snippets
snippets::install_snippets_from_package("snippets")

# Install specific types
snippets::install_snippets_from_package("snippets", type = c("r", "markdown"))
```

### RStudio Integration
The package integrates with RStudio through:
- `rstudioapi` for file navigation and RStudio version detection
- Platform-specific path resolution for different RStudio versions
- Automatic detection of RStudio snippet directories

### Backup System
The package automatically creates backups before modifying existing snippet files, using timestamped filenames for backup copies.

## CI/CD

GitHub Actions workflows handle:
- **R-CMD-check**: Runs package checks on multiple platforms (Ubuntu, macOS, Windows)
- **test-coverage**: Generates code coverage reports via codecov
- **pkgdown**: Builds and deploys documentation website
- **drat--publish-package**: Publishes package to custom repository

All workflows are triggered on pushes to main branches and pull requests.