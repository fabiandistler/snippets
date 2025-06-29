## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
# library(snippets)

## -----------------------------------------------------------------------------
# # Install a single module
# install_snippet_modules("dplyr", type = "r")
# 
# # Install multiple modules at once
# install_snippet_modules(c("dplyr", "ggplot2", "tidyr"), type = "r")
# 
# # Install markdown modules
# install_snippet_modules("markdown", type = "markdown")

## -----------------------------------------------------------------------------
# # List all available R modules
# list_snippet_modules(type = "r")
# 
# # List all available markdown modules
# list_snippet_modules(type = "markdown")

## -----------------------------------------------------------------------------
# # Show active modules for R snippets
# show_active_modules(type = "r")
# 
# # Show active modules for markdown snippets
# show_active_modules(type = "markdown")

## -----------------------------------------------------------------------------
# # Remove a single module
# remove_snippet_modules("tidyr", type = "r")
# 
# # Remove multiple modules
# remove_snippet_modules(c("dplyr", "ggplot2"), type = "r")

## -----------------------------------------------------------------------------
# install_snippet_modules(
#   modules = c("dplyr", "ggplot2"), # Modules to install
#   type = "r", # Snippet type ("r", "markdown", etc.)
#   source = "package", # Source ("package", "local", "github", "url")
#   backup = TRUE, # Create backup before installation
#   force_update = FALSE # Force reinstall existing modules
# )

## -----------------------------------------------------------------------------
# # Force update an existing module
# install_snippet_modules("dplyr", type = "r", force_update = TRUE)

## -----------------------------------------------------------------------------
# install_snippet_modules("ggplot2", backup = FALSE)

## -----------------------------------------------------------------------------
# # All available modules
# list_snippet_modules()

## -----------------------------------------------------------------------------
# # Good - creates backup
# install_snippet_modules("dplyr", backup = TRUE)

## -----------------------------------------------------------------------------
# # Install tidyverse-related modules together
# install_snippet_modules(c("dplyr", "ggplot2", "tidyr", "purrr"), type = "r")

## -----------------------------------------------------------------------------
# # Check what's installed
# show_active_modules(type = "r")
# 
# # Remove unused modules
# remove_snippet_modules("old_module", type = "r")

## -----------------------------------------------------------------------------
# # Check available modules
# list_snippet_modules(type = "r")
# 
# # Make sure you're using the correct module name

## -----------------------------------------------------------------------------
# # Check if file exists
# file.exists(path_rstudio_snippets_file(type = "r"))

## -----------------------------------------------------------------------------
# # After installing modules, you can:
# 
# # Open the snippet directory
# open_rstudio_snippets_dir()
# 
# # Edit the main snippet file
# open_rstudio_snippets_file(type = "r")
# 
# # Manage backups
# list_snippet_file_backups(type = "r")

## -----------------------------------------------------------------------------
# # 1. Check what's currently installed
# show_active_modules(type = "r")
# 
# # 2. See what modules are available
# available <- list_snippet_modules(type = "r")
# print(available$module)
# 
# # 3. Install specific modules
# install_snippet_modules(c("dplyr", "ggplot2"))
# 
# # 4. Verify installation
# show_active_modules(type = "r")
# 
# # 5. Later, add more modules
# install_snippet_modules("plotly")
# 
# # 6. Remove a module you don't need
# remove_snippet_modules("plotly", type = "r")
# 
# # 7. Check final state
# show_active_modules(type = "r")

## -----------------------------------------------------------------------------
# # Install essential R modules
# install_snippet_modules(
#   modules = c("dplyr", "ggplot2", "data-types"),
#   type = "r"
# )
# 
# # Install markdown modules
# install_snippet_modules(
#   modules = c("markdown", "rmd"),
#   type = "markdown"
# )
# 
# # Verify installation
# show_active_modules(type = "r")
# show_active_modules(type = "markdown")

