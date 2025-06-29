# ======================================================================== ~~~~
# Snippet Module Installation -------------------------------------------- ====
# ======================================================================== ~~~~


#' Install snippet modules
#'
#' Install one or more snippet modules and compose them into the active
#' snippet file that RStudio reads.
#'
#' @param modules Character vector of module names (optional)
#' @param type Snippet type filter (optional - if NULL, installs all types found)
#' @param from Path or URL to install from (optional - defaults to local modules directory)
#' @param backup Create backup before modifying existing snippets
#' @param force_update Update modules even if already installed
#'
#' @details
#' ## Ultra-Simple API
#' 
#' All parameters are optional! The function uses intelligent auto-discovery:
#' 
#' **Install everything found (simplest usage):**
#' ```r
#' install_snippet_modules()                        # Install all from local dir
#' install_snippet_modules(from = "path/")          # Install all from path
#' install_snippet_modules(from = "https://...")    # Install all from URL
#' ```
#' 
#' **Install specific type:**
#' ```r
#' install_snippet_modules(type = "r")              # All R snippets from local
#' install_snippet_modules(type = "r", from = "path/")  # All R snippets from path
#' ```
#' 
#' **Install specific modules (traditional):**
#' ```r
#' install_snippet_modules("dplyr", type = "r")     # Specific module, local
#' install_snippet_modules("dplyr", from = "path/") # Specific module, path (type = "r" default)
#' ```
#' 
#' ## Intelligent Features
#' 
#' - **Auto-detection**: URLs vs local paths detected automatically
#' - **Smart fallback**: Tries `[module]-[type].snippets`, then `[type].snippets`
#' - **Auto-discovery**: Scans location and installs all found snippet files
#' - **Flexible**: Works with both specific modules and generic type files
#'
#' @return Invisibly returns list of installation results
#' @export
#' @concept snippet modules
#'
#' @examples
#' \dontrun{\donttest{
#' # Ultra-simple: install everything found
#' install_snippet_modules()  # Install all from local modules directory
#' install_snippet_modules(from = "/path/to/snippets/")  # Install all from path
#' install_snippet_modules(from = "https://github.com/user/snippets/")  # Install all from URL
#' 
#' # Install all of specific type
#' install_snippet_modules(type = "r")  # All R snippets from local
#' install_snippet_modules(type = "r", from = "https://...")  # All R snippets from URL
#' 
#' # Traditional: install specific modules
#' install_snippet_modules("dplyr")  # type defaults to "r"
#' install_snippet_modules("dplyr", type = "r")  # explicit type
#' install_snippet_modules(c("dplyr", "ggplot2"), type = "r")  # multiple modules
#' 
#' # Advanced options
#' install_snippet_modules(backup = FALSE)  # No backup
#' install_snippet_modules(force_update = TRUE)  # Force update all
#' install_snippet_modules("dplyr", from = "path/", force_update = TRUE)  # Force specific
#' }}
install_snippet_modules <- function(modules = NULL, 
                                   type = NULL, 
                                   from = NULL,
                                   backup = TRUE,
                                   force_update = FALSE) {
  
  # Auto-discovery: if no specific parameters, install everything found
  if (is.null(modules) && is.null(type)) {
    return(install_everything_found(from, backup, force_update))
  }
  
  # Auto-discovery: if only type specified, install all modules of that type
  if (is.null(modules) && !is.null(type)) {
    return(install_all_of_type(type, from, backup, force_update))
  }
  
  # Traditional mode: specific modules specified
  if (!is.null(modules)) {
    # If type not specified, try to infer or default to "r"
    if (is.null(type)) {
      type <- "r"  # Default assumption
      usethis::ui_info("No type specified, defaulting to 'r' snippets")
    }
    
    return(install_specific_modules(modules, type, from, backup, force_update))
  }
  
  usethis::ui_stop("Invalid parameter combination")
}

#' Install everything found at location
#'
#' @param from Path or URL to scan
#' @param backup Create backup
#' @param force_update Force update existing
#' @return Installation results
#' @noRd
install_everything_found <- function(from = NULL, backup = TRUE, force_update = FALSE) {
  discovered <- discover_snippet_files(from)
  
  if (nrow(discovered$modules) == 0 && nrow(discovered$generics) == 0) {
    usethis::ui_info("No snippet files found at {from %||% 'default location'}")
    return(invisible(list()))
  }
  
  usethis::ui_info("Auto-discovery found {nrow(discovered$modules)} modules and {nrow(discovered$generics)} generic files")
  
  results <- list()
  
  # Install discovered modules
  for (i in seq_len(nrow(discovered$modules))) {
    row <- discovered$modules[i, ]
    result <- install_specific_modules(row$module, row$type, from, backup, force_update)
    results[[paste(row$module, row$type, sep = "-")]] <- result
  }
  
  # Install discovered generics
  for (i in seq_len(nrow(discovered$generics))) {
    row <- discovered$generics[i, ]
    result <- install_specific_modules(row$type, row$type, from, backup, force_update)
    results[[row$type]] <- result
  }
  
  usethis::ui_done("Auto-discovery installation completed")
  invisible(results)
}

#' Install all modules of specific type
#'
#' @param type Snippet type
#' @param from Path or URL
#' @param backup Create backup
#' @param force_update Force update existing
#' @return Installation results
#' @noRd
install_all_of_type <- function(type, from = NULL, backup = TRUE, force_update = FALSE) {
  discovered <- discover_snippet_files(from, type)
  
  if (nrow(discovered$modules) == 0 && nrow(discovered$generics) == 0) {
    usethis::ui_info("No {type} snippet files found at {from %||% 'default location'}")
    return(invisible(list()))
  }
  
  usethis::ui_info("Installing all {type} snippets found...")
  
  results <- list()
  
  # Install discovered modules of this type
  for (i in seq_len(nrow(discovered$modules))) {
    row <- discovered$modules[i, ]
    result <- install_specific_modules(row$module, row$type, from, backup, force_update)
    results[[row$module]] <- result
  }
  
  # Install generic file if found
  for (i in seq_len(nrow(discovered$generics))) {
    row <- discovered$generics[i, ]
    result <- install_specific_modules(row$type, row$type, from, backup, force_update)
    results[[paste0(row$type, "_generic")]] <- result
  }
  
  invisible(results)
}

#' Install specific modules (traditional mode)
#'
#' @param modules Module names
#' @param type Snippet type
#' @param from Path or URL
#' @param backup Create backup
#' @param force_update Force update existing
#' @return Installation results
#' @noRd
install_specific_modules <- function(modules, type, from = NULL, backup = TRUE, force_update = FALSE) {
  # Detect source type and set proper source path
  source_type <- auto_detect_source(from)
  source <- if (source_type == "default") get_snippet_modules_dir() else from
  
  # Validate type
  type <- match_snippet_type(type, several.ok = FALSE)
  
  if (length(modules) == 0) {
    usethis::ui_stop("No modules specified for installation")
  }
  
  # No pre-validation needed - intelligent fallback will handle missing modules
  
  # Read current registry
  registry <- read_module_registry()
  
  # Check if modules are already installed
  already_installed <- character(0)
  for (module in modules) {
    module_key <- paste(module, type, sep = "-")
    if (module_key %in% names(registry$modules) && !force_update) {
      already_installed <- c(already_installed, module)
    }
  }
  
  if (length(already_installed) > 0) {
    usethis::ui_info(
      "Module(s) already installed: {paste(already_installed, collapse = ', ')}"
    )
    if (!force_update) {
      usethis::ui_info("Use force_update = TRUE to reinstall")
      modules <- setdiff(modules, already_installed)
    }
  }
  
  if (length(modules) == 0) {
    usethis::ui_info("No new modules to install")
    return(invisible(list()))
  }
  
  # Create backup if requested
  if (backup) {
    tryCatch({
      backup_rstudio_snippets(type = type)
    }, error = function(e) {
      usethis::ui_warn("Could not create backup (outside RStudio): {e$message}")
    })
  }
  
  # Copy modules to local directory
  modules_dir <- get_snippet_modules_dir()
  installation_results <- list()
  
  for (module in modules) {
    result <- install_single_module(module, type, source, modules_dir)
    installation_results[[module]] <- result
    
    if (result$success) {
      # Update registry with fallback information
      module_key <- paste(module, type, sep = "-")
      registry$modules[[module_key]] <- list(
        module = module,
        type = type,
        source = source,
        installed_date = as.character(Sys.time()),
        file_path = result$local_path,
        fallback_used = result$fallback_used,
        actual_filename = result$actual_filename,
        source_url = if (auto_detect_source(source) == "url") result$source_url else result$source_path
      )
    }
  }
  
  # Write updated registry
  write_module_registry(registry)
  
  # Recompose the main snippet file
  installed_modules <- get_installed_modules_for_type(type)
  recompose_snippet_file(installed_modules, type)
  
  # Report results
  successful <- sum(sapply(installation_results, function(x) x$success))
  usethis::ui_done(
    "Successfully installed {successful} module(s) for {type} snippets"
  )
  
  if (successful > 0) {
    usethis::ui_info(paste0(
      "You will be able to use the snippets after ",
      '{usethis::ui_field("RStudio")} is ',
      '{crayon::underline("closed and reopened")}.'
    ))
  }
  
  invisible(installation_results)
}

#' Install a single module
#'
#' @param module Module name
#' @param type Snippet type
#' @param source Module source
#' @param modules_dir Local modules directory
#' @return List with installation result
#' @noRd
install_single_module <- function(module, type, source, modules_dir) {
  # Use original module name for local filename (always [module]-[type].snippets)
  local_filename <- paste0(module, "-", type, ".snippets")
  local_path <- fs::path(modules_dir, local_filename)
  
  # Try installation with intelligent fallback
  source_type <- auto_detect_source(source)
  
  if (source_type == "url") {
    result <- try_url_with_fallback(module, type, source, local_path)
  } else {
    result <- try_local_with_fallback(module, type, source, local_path)
  }
  
  # Add local_path to result
  result$local_path <- if (result$success) local_path else NA
  
  # User feedback
  if (result$success) {
    fallback_msg <- if (result$fallback_used) paste0(" (using generic ", result$actual_filename, ")") else ""
    source_type <- if (source_type == "url") "URL" else source
    usethis::ui_done("Installed module: {crayon::green(module)} from {source_type}{fallback_msg}")
  } else {
    usethis::ui_oops("Failed to install module {module}: {result$error}")
  }
  
  result
}

#' Remove snippet modules
#'
#' Remove one or more snippet modules and recompose the active snippet file.
#'
#' @param modules Character vector of module names to remove
#' @param type Snippet type (e.g., "r", "markdown")
#' @param backup Create backup before modifying existing snippets
#'
#' @return Invisibly returns list of removal results
#' @export
#' @concept snippet modules
#'
#' @examples
#' \dontrun{\donttest{
#' # Remove specific modules
#' remove_snippet_modules(c("dplyr", "tidyr"), type = "r")
#' 
#' # Remove without backup
#' remove_snippet_modules("ggplot2", type = "r", backup = FALSE)
#' }}
remove_snippet_modules <- function(modules, type = "r", backup = TRUE) {
  type <- match_snippet_type(type, several.ok = FALSE)
  
  if (length(modules) == 0) {
    usethis::ui_stop("No modules specified for removal")
  }
  
  # Read current registry
  registry <- read_module_registry()
  
  # Check which modules are actually installed
  not_installed <- character(0)
  for (module in modules) {
    module_key <- paste(module, type, sep = "-")
    if (!module_key %in% names(registry$modules)) {
      not_installed <- c(not_installed, module)
    }
  }
  
  if (length(not_installed) > 0) {
    usethis::ui_warn(
      "Module(s) not installed: {paste(not_installed, collapse = ', ')}"
    )
    modules <- setdiff(modules, not_installed)
  }
  
  if (length(modules) == 0) {
    usethis::ui_info("No modules to remove")
    return(invisible(list()))
  }
  
  # Create backup if requested
  if (backup) {
    tryCatch({
      backup_rstudio_snippets(type = type)
    }, error = function(e) {
      usethis::ui_warn("Could not create backup (outside RStudio): {e$message}")
    })
  }
  
  # Remove modules from registry
  removal_results <- list()
  for (module in modules) {
    module_key <- paste(module, type, sep = "-")
    
    if (module_key %in% names(registry$modules)) {
      registry$modules[[module_key]] <- NULL
      removal_results[[module]] <- list(success = TRUE)
      usethis::ui_done("Removed module: {crayon::red(module)}")
    } else {
      removal_results[[module]] <- list(success = FALSE, error = "Not installed")
    }
  }
  
  # Write updated registry
  write_module_registry(registry)
  
  # Recompose the main snippet file
  installed_modules <- get_installed_modules_for_type(type)
  recompose_snippet_file(installed_modules, type)
  
  successful <- sum(sapply(removal_results, function(x) x$success))
  usethis::ui_done("Successfully removed {successful} module(s)")
  
  if (successful > 0) {
    usethis::ui_info(paste0(
      "Changes will take effect after ",
      '{usethis::ui_field("RStudio")} is ',
      '{crayon::underline("closed and reopened")}.'
    ))
  }
  
  invisible(removal_results)
}

#' Get installed modules for a specific type
#'
#' @param type Snippet type
#' @return Character vector of installed module names
#' @noRd
get_installed_modules_for_type <- function(type) {
  registry <- read_module_registry()
  
  installed <- character(0)
  for (module_key in names(registry$modules)) {
    module_data <- registry$modules[[module_key]]
    if (module_data$type == type) {
      installed <- c(installed, module_data$module)
    }
  }
  
  installed
}

#' Recompose main snippet file from installed modules
#'
#' @param modules Character vector of module names
#' @param type Snippet type
#' @noRd
recompose_snippet_file <- function(modules, type) {
  # Use safe version that works outside RStudio
  output_path <- tryCatch({
    path_rstudio_snippets_file(type = type)
  }, error = function(e) {
    # Fallback for testing outside RStudio
    base_dir <- fs::path_expand_r("~/.R/snippets/")
    fs::dir_create(base_dir, recurse = TRUE)
    fs::path(base_dir, paste0(type, ".snippets"))
  })
  
  if (length(modules) == 0) {
    # No modules installed, create empty file or remove existing
    readr::write_lines("# No modules installed", output_path)
    usethis::ui_info("Created empty {type}.snippets file")
    return()
  }
  
  success <- compose_snippet_modules(modules, type, output_path)
  
  if (success) {
    usethis::ui_done(
      "Recomposed {type}.snippets with {length(modules)} module(s): {paste(modules, collapse = ', ')}"
    )
  } else {
    usethis::ui_oops("Failed to recompose {type}.snippets")
  }
}