
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/snippets)](https://CRAN.R-project.org/package=snippets)
[![GitHub
version](https://img.shields.io/badge/GitHub-v0.0.21-brightgreen.svg)](https://github.com/GegznaV/snippets)
[![R build
status](https://github.com/GegznaV/snippets/workflows/R-CMD-check/badge.svg)](https://github.com/GegznaV/snippets/actions)
[![Codecov test
coverage](https://codecov.io/gh/GegznaV/snippets/branch/master/graph/badge.svg)](https://codecov.io/gh/GegznaV/snippets?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2025--06--29-yellowgreen.svg)]()
<!-- badges: end -->

# Package `snippets`

<!-- (0.0.21) -->

## Installation

<!-- You can install the released version of snippets from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("snippets") -->

<!-- ``` -->

To install package from CRAN-like repository:

``` r
repos <- c("https://mokymai.github.io/download/", getOption("repos"))
install.packages("snippets", repos = repos)
```

To install from [GitHub](https://github.com/):

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("GegznaV/snippets")
```

## Quickstart

### Ultra-Simple Module Installation

The new API is incredibly simple with smart defaults:

``` r
# Install everything found locally (all types, all modules)
snippets::install_snippet_modules()

# Install everything from a specific path or URL
snippets::install_snippet_modules(from = "path/to/snippets/")
snippets::install_snippet_modules(from = "https://raw.githubusercontent.com/user/repo/main/snippets/")

# Install all modules of a specific type
snippets::install_snippet_modules(type = "r")
snippets::install_snippet_modules(type = "markdown")

# Install specific modules (auto-detects type if not specified)
snippets::install_snippet_modules("dplyr")
snippets::install_snippet_modules(c("dplyr", "ggplot2"))

# Install specific modules of specific type
snippets::install_snippet_modules("dplyr", type = "r")

# List available modules
snippets::list_snippet_modules(type = "r")

# Show active modules
snippets::show_active_modules(type = "r")

# Remove modules
snippets::remove_snippet_modules("dplyr", type = "r")
```

See `vignette("module-installation")` for detailed documentation.

### More Examples

``` r
library(snippets)
```

``` r
# Get the name of the directory where RStudio snippets are stored
get_path_rstudio_snippets_dir()
```

``` r
# Open the directory with RStudio snippets
open_rstudio_snippets_dir()
```

``` r
# Install all snippets found locally
install_snippet_modules()
list_snippet_file_backups()
```

``` r
# View and edit file with snippets of certain type: r
open_rstudio_snippets_file(type = "r")
```

``` r
# View and edit file with snippets of certain type: markdown
open_rstudio_snippets_file(type = "markdown")
```

### Create and Clean-up Back-ups

``` r
# Create several back up files
backup_rstudio_snippets(type = "r")
Sys.sleep(1)
backup_rstudio_snippets(type = "r")
Sys.sleep(1)
backup_rstudio_snippets(type = "r")
```

``` r
list_snippet_file_backups(type = "r")
```

``` r
# Remove duplicated back-up files
remove_snippet_backup_duplicates()
```

<!-- 
&#10;### Revert to a Certain Version of Back-up 
&#10;1. List all back-up files and select the one of interest.
    &#10;    ``` r
    list_snippet_file_backups(type = "r")
    ```
    ```r
    #> c:/.R/snippets/r.snippets
    #> c:/.R/snippets/r.snippets--backup-2019-11-12-033948
    #> c:/.R/snippets/r.snippets--backup-2019-10-31-015042
    ```
2. Restore the back-up of interest, e.g.:
    &#10;    ``` r
    restore_snippets_from_backup("r.snippets--backup-2019-10-31-015042")
    ```
    ```r
    #> v Back-up file was found: 'r.snippets--backup-2019-10-31-015042'
    #> i Snippets' type: r
    #> v Current 'r.snippets' file was backed up:
    #>   'r.snippets' -> 'r.snippets--backup-2020-01-05-012602'
    #> v Snippets were restored from the back-up file:
    #>   'r.snippets--backup-2019-10-31-015042' -> 'r.snippets'.
    ```
-->

# Additional resources

## More on using and writing RStudio snippets

1.  [Code
    Snippets](https://support.rstudio.com/hc/en-us/articles/204463668-Code-Snippets)
    (on RStudio website). All information is relevant to RStudio users.
2.  [R tip: Save time with RStudio code
    snippets](https://www.youtube.com/watch?v=h_i__VTSurU) (Case study
    on YouTube).
3.  [Snippets](https://cloud9-sdk.readme.io/docs/snippets) (on Cloud9
    SDK website). Advanced topics on writing snippets. Most sections
    (but not all) are relevant to RStudio users.

## Similar projects

1.  Package [**snippr**](https://github.com/dgrtwo/snippr).

## More snippets

1.  [Snippets](https://github.com/dgrtwo/snippets) by @dgrtwo.
2.  [Snippets](https://github.com/gadenbuie/snippets) by @gadenbuie.
    <!-- 2. [R snippets](https://github.com/Hellerz/hellerz/blob/6180ad13d73c7ff826c9cf514840bbbd899d5873/scripts/ace/snippets/r.snippets) by @Hellerz -->
    <!-- 3. [markdown snippets](https://github.com/Hellerz/hellerz/blob/6180ad13d73c7ff826c9cf514840bbbd899d5873/scripts/ace/snippets/markdown.snippets) by @Hellerz -->
    <!-- 4. [R snippets](https://github.com/dick7/vimrc/blob/81a78437c9d6f45c3372e6998f4a8a832890c243/sources_non_forked/vim-snippets/snippets/r.snippets) by @dick7 -->
    <!-- 5. [markdown snippets](https://github.com/dick7/vimrc/blob/81a78437c9d6f45c3372e6998f4a8a832890c243/sources_non_forked/vim-snippets/snippets/markdown.snippets) by @dick7 -->

<!-- <div style="text-align:right;">2025-06-29</div> -->
