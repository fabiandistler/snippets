# ======================================================================== ~~~~
# Snippet Module Installation -------------------------------------------- ====
# ======================================================================== ~~~~

#' Install all available modules from a package
#'
#' Convenience function to install all available snippet modules from a package
#' without having to specify individual module names.
#'
#' @param package Package name to install modules from
#' @param type Snippet type (e.g., "r", "markdown") or "all" for all types
#' @param backup Create backup before modifying existing snippets
#'
#' @return Invisibly returns list of installation results
#' @export
#' @concept snippet modules
#'
#' @examples
#' \dontrun{\donttest{
#' # Install all R modules from package
#' install_all_package_modules("snippets", type = "r")
#' 
#' # Install all modules of all types
#' install_all_package_modules("snippets", type = "all")
#' }}
install_all_package_modules <- function(package = "snippets", 
                                       type = "r", 
                                       backup = TRUE) {
  
  if (type == "all") {
    available_types <- get_default_snippet_types()
  } else {
    available_types <- match_snippet_type(type, several.ok = TRUE)
  }
  
  results <- list()
  
  for (current_type in available_types) {
    available_modules <- list_snippet_modules(
      type = current_type, 
      source = "package"
    )
    
    if (nrow(available_modules) > 0) {
      usethis::ui_info("Installing all {current_type} modules from package '{package}'")
      
      results[[current_type]] <- install_snippet_modules(
        modules = available_modules$module,
        type = current_type,
        source = "package",
        backup = backup
      )
    } else {
      usethis::ui_info("No {current_type} modules found in package '{package}'")
    }
  }
  
  invisible(results)
}

#' Install snippet modules
#'
#' Install one or more snippet modules and compose them into the active
#' snippet file that RStudio reads.
#'
#' @param modules Character vector of module names to install
#' @param type Snippet type (e.g., "r", "markdown") or vector of types
#' @param source Source of modules ("package", "local") or direct URL
#' @param backup Create backup before modifying existing snippets
#' @param force_update Update modules even if already installed
#'
#' @details
#' ## Source Types
#' 
#' - **"package"**: Install from R package (default)
#' - **"local"**: Install from local filesystem
#' - **URL**: Direct URL to snippet file or base directory
#' 
#' ## URL Usage
#' 
#' URLs are automatically detected and can be used in several ways:
#' 
#' **Direct file URL:**
#' ```r
#' install_snippet_modules("dplyr", type = "r",
#'   source = "https://example.com/dplyr-r.snippets")
#' ```
#' 
#' **Base directory URL (automatically appends module filename):**
#' ```r
#' install_snippet_modules("dplyr", type = "r", 
#'   source = "https://raw.githubusercontent.com/user/repo/main/inst/snippets/")
#' ```
#' 
#' **GitHub raw URLs:**
#' ```r
#' install_snippet_modules("dplyr", type = "r",
#'   source = "https://raw.githubusercontent.com/user/repo/main/inst/snippets/dplyr-r.snippets")
#' ```
#'
#' @return Invisibly returns list of installation results
#' @export
#' @concept snippet modules
#'
#' @examples
#' \dontrun{\donttest{
#' # Install modules from package (default)
#' install_snippet_modules(c("dplyr", "ggplot2"), type = "r")
#' 
#' # Install same modules for multiple types
#' install_snippet_modules("dplyr", type = c("r", "markdown"))
#' 
#' # Install from local files
#' install_snippet_modules("custom_module", type = "r", source = "local")
#' 
#' # Install from direct URL
#' install_snippet_modules("dplyr", type = "r", 
#'   source = "https://example.com/dplyr-r.snippets")
#' 
#' # Install from GitHub base URL (automatically appends filename)
#' install_snippet_modules("dplyr", type = "r",
#'   source = "https://raw.githubusercontent.com/user/repo/main/inst/snippets/")
#' 
#' # Install multiple modules from same base URL
#' install_snippet_modules(c("dplyr", "ggplot2"), type = "r",
#'   source = "https://raw.githubusercontent.com/user/repo/main/inst/snippets/")
#' 
#' # Install with backup disabled
#' install_snippet_modules("tidyr", type = "r", backup = FALSE)
#' 
#' # Force update existing modules
#' install_snippet_modules("dplyr", type = "r", force_update = TRUE)
#' }}
install_snippet_modules <- function(modules, 
                                   type = "r", 
                                   source = "package",
                                   backup = TRUE,
                                   force_update = FALSE) {
  
  types <- match_snippet_type(type, several.ok = TRUE)
  
  # Handle multiple types by calling recursively
  if (length(types) > 1) {
    results <- list()
    for (single_type in types) {
      results[[single_type]] <- install_snippet_modules(
        modules = modules,
        type = single_type,
        source = source,
        backup = backup,
        force_update = force_update
      )
    }
    return(invisible(results))
  }
  
  # Single type processing (original logic)
  type <- types[1]
  
  if (length(modules) == 0) {
    usethis::ui_stop("No modules specified for installation")
  }
  
  # Validate modules exist (skip for URLs)
  available_modules <- data.frame()
  if (!stringr::str_detect(source, "^https?://")) {
    available_modules <- list_snippet_modules(type = type, source = source)
    missing_modules <- setdiff(modules, available_modules$module)
    
    if (length(missing_modules) > 0) {
      usethis::ui_stop(
        "Module(s) not found: {paste(missing_modules, collapse = ', ')}"
      )
    }
  }
  
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
    result <- install_single_module(module, type, source, modules_dir, available_modules)
    installation_results[[module]] <- result
    
    if (result$success) {
      # Update registry
      module_key <- paste(module, type, sep = "-")
      registry$modules[[module_key]] <- list(
        module = module,
        type = type,
        source = source,
        installed_date = as.character(Sys.time()),
        file_path = result$local_path
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
#' @param available_modules Data frame of available modules
#' @return List with installation result
#' @noRd
install_single_module <- function(module, type, source, modules_dir, available_modules) {
  module_filename <- make_module_filename(module, type)
  local_path <- fs::path(modules_dir, module_filename)
  
  # Check if source is a URL
  if (stringr::str_detect(source, "^https?://")) {
    # Direct URL source
    if (stringr::str_detect(source, "/$")) {
      # Base URL - construct full URL
      source_url <- paste0(source, module_filename)
    } else {
      # Direct file URL
      source_url <- source
    }
    
    tryCatch({
      if (!download_from_url(source_url, local_path)) {
        stop("Failed to download from URL")
      }
      
      usethis::ui_done("Installed module: {crayon::green(module)} from URL")
      
      list(
        success = TRUE,
        local_path = local_path,
        source_path = source_url
      )
    }, error = function(e) {
      usethis::ui_oops("Failed to install module {module}: {e$message}")
      list(
        success = FALSE,
        error = e$message,
        local_path = NA
      )
    })
  } else {
    # Traditional source (package, local)
    module_row <- available_modules[
      available_modules$module == module & 
      available_modules$type == type & 
      available_modules$source == source, 
    ]
    
    if (nrow(module_row) == 0) {
      return(list(
        success = FALSE,
        error = paste("Module", module, "not found in", source),
        local_path = NA
      ))
    }
    
    source_path <- module_row$path[1]
    
    tryCatch({
      # Copy file from local filesystem
      fs::file_copy(source_path, local_path, overwrite = TRUE)
      
      usethis::ui_done("Installed module: {crayon::green(module)} from {source}")
      
      list(
        success = TRUE,
        local_path = local_path,
        source_path = source_path
      )
    }, error = function(e) {
      usethis::ui_oops("Failed to install module {module}: {e$message}")
      list(
        success = FALSE,
        error = e$message,
        local_path = NA
      )
    })
  }
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