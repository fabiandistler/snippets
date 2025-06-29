# ======================================================================== ~~~~
# Snippet Modules -------------------------------------------------------- ====
# ======================================================================== ~~~~

#' @name snippet-modules
#' @title Manage Snippet Modules
#' @description
#' Install, remove and manage modular snippet collections.
#' Modules follow the naming convention `{module}-{type}.snippets`
#' (e.g., `dplyr-r.snippets`, `ggplot2-r.snippets`).
#'
#' @concept snippet modules

# Helper Functions --------------------------------------------------------

#' Check if source is a URL
#'
#' @param source Source string to check
#' @return Logical indicating if source is URL
#' @noRd
is_url <- function(source) {
  stringr::str_detect(source, "^https?://")
}

#' Construct URL from base source and filename
#'
#' @param base_source Base URL (with or without trailing slash)
#' @param filename Filename to append
#' @return Complete URL
#' @noRd
construct_url <- function(base_source, filename) {
  if (stringr::str_detect(base_source, "/$")) {
    # Base URL with trailing slash: just append filename
    paste0(base_source, filename)
  } else if (stringr::str_detect(base_source, "/[^/]+\\.[^/]+$")) {
    # Direct file URL: replace filename
    base_dir <- dirname(base_source)
    paste(base_dir, filename, sep = "/")
  } else {
    # Assume it's a base URL without trailing slash
    paste(base_source, filename, sep = "/")
  }
}

#' Try to install from URL with fallback
#'
#' @param module Module name
#' @param type Snippet type  
#' @param source URL source
#' @param local_path Local destination path
#' @return List with success status and metadata
#' @noRd
try_url_with_fallback <- function(module, type, source, local_path) {
  # Step 1: Try specific module format [module]-[type].snippets
  module_filename <- paste0(module, "-", type, ".snippets")
  module_url <- construct_url(source, module_filename)
  
  if (download_from_url(module_url, local_path)) {
    return(list(
      success = TRUE,
      fallback_used = FALSE,
      actual_filename = module_filename,
      source_url = module_url
    ))
  }
  
  # Step 2: Fallback to generic format [type].snippets
  generic_filename <- paste0(type, ".snippets")
  generic_url <- construct_url(source, generic_filename)
  
  if (download_from_url(generic_url, local_path)) {
    return(list(
      success = TRUE,
      fallback_used = TRUE,
      actual_filename = generic_filename,
      source_url = generic_url
    ))
  }
  
  return(list(
    success = FALSE,
    fallback_used = FALSE,
    actual_filename = NA,
    source_url = NA,
    error = paste("Neither", module_filename, "nor", generic_filename, "found at", source)
  ))
}

#' Try to install from local directory with fallback
#'
#' @param module Module name
#' @param type Snippet type
#' @param source Local source directory
#' @param local_path Local destination path
#' @return List with success status and metadata
#' @noRd
try_local_with_fallback <- function(module, type, source, local_path) {
  modules_dir <- if (source == "local") get_snippet_modules_dir() else source
  
  # Step 1: Try specific module format [module]-[type].snippets
  module_filename <- paste0(module, "-", type, ".snippets")
  module_file <- fs::path(modules_dir, module_filename)
  
  if (fs::file_exists(module_file)) {
    if (fs::file_copy(module_file, local_path, overwrite = TRUE)) {
      return(list(
        success = TRUE,
        fallback_used = FALSE,
        actual_filename = module_filename,
        source_path = module_file
      ))
    }
  }
  
  # Step 2: Fallback to generic format [type].snippets
  generic_filename <- paste0(type, ".snippets")
  generic_file <- fs::path(modules_dir, generic_filename)
  
  if (fs::file_exists(generic_file)) {
    if (fs::file_copy(generic_file, local_path, overwrite = TRUE)) {
      return(list(
        success = TRUE,
        fallback_used = TRUE,
        actual_filename = generic_filename,
        source_path = generic_file
      ))
    }
  }
  
  return(list(
    success = FALSE,
    fallback_used = FALSE,
    actual_filename = NA,
    source_path = NA,
    error = paste("Neither", module_filename, "nor", generic_filename, "found in", modules_dir)
  ))
}

#' Download file from URL
#'
#' @param url URL to download from
#' @param dest_path Local destination path
#' @return Logical indicating success
#' @noRd
download_from_url <- function(url, dest_path) {
  tryCatch({
    utils::download.file(url, dest_path, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) {
    usethis::ui_warn("Failed to download from URL {url}: {e$message}")
    FALSE
  })
}


#' Get path to snippet modules directory
#'
#' @return Path to the snippet modules directory
#' @noRd
get_snippet_modules_dir <- function() {
  # Use safe version that works outside RStudio
  base_dir <- tryCatch({
    get_path_rstudio_snippets_dir()
  }, error = function(e) {
    # Fallback for testing outside RStudio
    fs::path_expand_r("~/.R/snippets/")
  })
  
  modules_dir <- fs::path(base_dir, "snippet-modules")
  if (!fs::dir_exists(modules_dir)) {
    fs::dir_create(modules_dir, recurse = TRUE)
  }
  modules_dir
}

#' Get path to module registry file
#'
#' @return Path to the module registry JSON file
#' @noRd
get_module_registry_path <- function() {
  # Use safe version that works outside RStudio
  base_dir <- tryCatch({
    get_path_rstudio_snippets_dir()
  }, error = function(e) {
    # Fallback for testing outside RStudio
    fs::path_expand_r("~/.R/snippets/")
  })
  
  fs::path(base_dir, "module_registry.json")
}

#' Create module filename from module name and type
#'
#' @param module Module name (e.g., "dplyr")
#' @param type Snippet type (e.g., "r")
#' @return Filename following convention `{module}-{type}.snippets`
#' @noRd
make_module_filename <- function(module, type) {
  stringr::str_glue("{module}-{type}.snippets")
}

#' Parse module filename to extract module name and type
#'
#' @param filename Module filename
#' @return Named list with 'module' and 'type' components
#' @noRd
parse_module_filename <- function(filename) {
  # Remove .snippets extension
  basename <- stringr::str_remove(filename, "\\.snippets$")
  
  # Split on last dash to get module and type
  parts <- stringr::str_split(basename, "-", n = Inf)[[1]]
  
  if (length(parts) < 2) {
    usethis::ui_stop("Invalid module filename format: {filename}")
  }
  
  # Last part is type, everything before is module name
  type <- parts[length(parts)]
  module <- paste(parts[-length(parts)], collapse = "-")
  
  list(module = module, type = type)
}

#' Read module registry
#'
#' @return List containing module registry data
#' @noRd
read_module_registry <- function() {
  registry_path <- get_module_registry_path()
  
  if (!fs::file_exists(registry_path)) {
    return(list(modules = list(), last_updated = Sys.time()))
  }
  
  tryCatch({
    jsonlite::fromJSON(registry_path, simplifyVector = FALSE)
  }, error = function(e) {
    usethis::ui_warn("Module registry corrupted, creating new one")
    list(modules = list(), last_updated = Sys.time())
  })
}

#' Write module registry
#'
#' @param registry Registry data to write
#' @noRd
write_module_registry <- function(registry) {
  registry$last_updated <- Sys.time()
  registry_path <- get_module_registry_path()
  
  # Ensure directory exists
  fs::dir_create(fs::path_dir(registry_path), recurse = TRUE)
  
  jsonlite::write_json(registry, registry_path, pretty = TRUE, auto_unbox = TRUE)
}

#' Compose snippets from multiple modules into a single file
#'
#' @param modules Character vector of module names
#' @param type Snippet type (e.g., "r")
#' @param output_path Path where to write the composed file
#' @return Logical indicating success
#' @noRd
compose_snippet_modules <- function(modules, type, output_path) {
  modules_dir <- get_snippet_modules_dir()
  composed_content <- character(0)
  
  # Header
  composed_content <- c(
    composed_content,
    "# This file was automatically generated by the snippets package",
    paste("#", Sys.time()),
    paste("# Modules:", paste(modules, collapse = ", ")),
    ""
  )
  
  for (module in modules) {
    module_filename <- make_module_filename(module, type)
    module_path <- fs::path(modules_dir, module_filename)
    
    if (!fs::file_exists(module_path)) {
      usethis::ui_warn("Module file not found: {module_filename}")
      next
    }
    
    # Read module content
    module_content <- readr::read_lines(module_path)
    
    # Add module header
    composed_content <- c(
      composed_content,
      paste("# === MODULE:", paste(module, type, sep = "-"), "==="),
      module_content,
      paste("# === END MODULE:", paste(module, type, sep = "-"), "==="),
      ""
    )
  }
  
  # Write composed file
  readr::write_lines(composed_content, output_path)
  
  TRUE
}

# Main Functions ----------------------------------------------------------

#' List available snippet modules
#'
#' @param type Snippet type to filter by (e.g., "r", "markdown"). 
#'   Use "all" to show all types.
#' @param source Source to filter by ("local", "all") or URL
#' @param installed_only Show only installed modules
#'
#' @return Data frame with module information
#' @export
#' @concept snippet modules
#'
#' @examples
#' \dontrun{\donttest{
#' # List all available R modules from all sources
#' list_snippet_modules(type = "r")
#' 
#' # List all installed modules
#' list_snippet_modules(installed_only = TRUE)
#' 
#' # List modules from all sources
#' list_snippet_modules(source = "all")
#' 
#' # List modules from local source only
#' list_snippet_modules(source = "local", type = "r")
#' }}
list_snippet_modules <- function(type = "all", source = "all", installed_only = FALSE) {
  modules_info <- data.frame(
    module = character(0),
    type = character(0),
    source = character(0),
    installed = logical(0),
    path = character(0),
    stringsAsFactors = FALSE
  )
  
  # Get local modules
  if (source %in% c("all", "local")) {
    modules_dir <- get_snippet_modules_dir()
    if (fs::dir_exists(modules_dir)) {
      # Find all .snippets files (both [module]-[type].snippets and [type].snippets)
      all_snippet_files <- fs::dir_ls(modules_dir, regexp = ".*\\.snippets$")
      
      for (file in all_snippet_files) {
        filename <- fs::path_file(file)
        
        # Try to parse as [module]-[type].snippets first
        if (stringr::str_detect(filename, "^.+-.+\\.snippets$")) {
          tryCatch({
            parsed <- parse_module_filename(filename)
            if (type == "all" || parsed$type == type) {
              modules_info <- rbind(modules_info, data.frame(
                module = parsed$module,
                type = parsed$type,
                source = "local",
                installed = FALSE,
                path = file,
                stringsAsFactors = FALSE
              ))
            }
          }, error = function(e) {
            # Skip files that can't be parsed as modules
          })
        } 
        # Try to parse as [type].snippets (generic format)
        else if (stringr::str_detect(filename, "^[^-]+\\.snippets$")) {
          file_type <- stringr::str_remove(filename, "\\.snippets$")
          if (type == "all" || file_type == type) {
            modules_info <- rbind(modules_info, data.frame(
              module = file_type,  # Use type as module name for generic files
              type = file_type,
              source = "local",
              installed = FALSE,
              path = file,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  # Check installation status
  registry <- read_module_registry()
  for (i in seq_len(nrow(modules_info))) {
    module_key <- paste(modules_info$module[i], modules_info$type[i], sep = "-")
    modules_info$installed[i] <- module_key %in% names(registry$modules)
  }
  
  if (installed_only) {
    modules_info <- modules_info[modules_info$installed, ]
  }
  
  modules_info
}

#' Show currently active (installed) modules
#'
#' @param type Snippet type to show (e.g., "r", "markdown"). 
#'   Use "all" to show all types.
#'
#' @return Data frame with active module information
#' @export
#' @concept snippet modules
#'
#' @examples
#' \dontrun{\donttest{
#' # Show active R modules
#' show_active_modules(type = "r")
#' 
#' # Show all active modules
#' show_active_modules(type = "all")
#' }}
show_active_modules <- function(type = "all") {
  registry <- read_module_registry()
  
  if (length(registry$modules) == 0) {
    usethis::ui_info("No modules are currently installed.")
    return(invisible(data.frame()))
  }
  
  active_info <- data.frame(
    module = character(0),
    type = character(0),
    source = character(0),
    installed_date = character(0),
    stringsAsFactors = FALSE
  )
  
  for (module_key in names(registry$modules)) {
    module_data <- registry$modules[[module_key]]
    parsed <- parse_module_filename(paste0(module_key, ".snippets"))
    
    if (type == "all" || parsed$type == type) {
      active_info <- rbind(active_info, data.frame(
        module = parsed$module,
        type = parsed$type,
        source = if(is.null(module_data$source)) "unknown" else module_data$source,
        installed_date = if(is.null(module_data$installed_date)) "unknown" else module_data$installed_date,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if (nrow(active_info) > 0) {
    usethis::ui_info("Active modules:")
    print(active_info)
  } else {
    usethis::ui_info("No modules of type '{type}' are installed.")
  }
  
  invisible(active_info)
}