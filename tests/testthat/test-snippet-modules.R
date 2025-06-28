test_that("module filename functions work correctly", {
  # Test make_module_filename
  expect_equal(
    snippets:::make_module_filename("dplyr", "r"),
    "dplyr-r.snippets"
  )
  
  expect_equal(
    snippets:::make_module_filename("my-custom-module", "markdown"),
    "my-custom-module-markdown.snippets"
  )
  
  # Test parse_module_filename
  result <- snippets:::parse_module_filename("dplyr-r.snippets")
  expect_equal(result$module, "dplyr")
  expect_equal(result$type, "r")
  
  result <- snippets:::parse_module_filename("my-custom-module-markdown.snippets")
  expect_equal(result$module, "my-custom-module")
  expect_equal(result$type, "markdown")
  
  # Test error handling
  expect_error(
    snippets:::parse_module_filename("invalid.snippets"),
    "Invalid module filename format"
  )
})

test_that("list_snippet_modules returns correct structure", {
  modules <- list_snippet_modules(type = "r", source = "package")
  
  expect_s3_class(modules, "data.frame")
  expect_true("module" %in% names(modules))
  expect_true("type" %in% names(modules))
  expect_true("source" %in% names(modules))
  expect_true("installed" %in% names(modules))
  expect_true("path" %in% names(modules))
})

test_that("module registry functions work", {
  # Create temporary directory for testing
  temp_dir <- tempdir()
  temp_registry <- file.path(temp_dir, "test_registry.json")
  
  # Mock get_module_registry_path to use temp file
  with_mocked_bindings(
    get_module_registry_path = function() temp_registry,
    {
      # Test reading empty registry
      registry <- snippets:::read_module_registry()
      expect_type(registry, "list")
      expect_true("modules" %in% names(registry))
      expect_true("last_updated" %in% names(registry))
      
      # Test writing registry
      registry$modules[["test-module-r"]] <- list(
        module = "test-module",
        type = "r",
        source = "package"
      )
      
      snippets:::write_module_registry(registry)
      expect_true(file.exists(temp_registry))
      
      # Test reading written registry
      registry2 <- snippets:::read_module_registry()
      expect_equal(registry2$modules[["test-module-r"]]$module, "test-module")
    }
  )
  
  # Cleanup
  if (file.exists(temp_registry)) {
    unlink(temp_registry)
  }
})

test_that("show_active_modules works with no modules", {
  temp_dir <- tempdir()
  temp_registry <- file.path(temp_dir, "empty_registry.json")
  
  with_mocked_bindings(
    get_module_registry_path = function() temp_registry,
    {
      # Remove any existing registry
      if (file.exists(temp_registry)) unlink(temp_registry)
      
      # Should return empty data frame and show message
      expect_message(
        result <- show_active_modules(type = "r"),
        "No modules are currently installed"
      )
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 0)
    }
  )
})

test_that("get_installed_modules_for_type works correctly", {
  temp_dir <- tempdir()
  temp_registry <- file.path(temp_dir, "test_registry2.json")
  
  with_mocked_bindings(
    get_module_registry_path = function() temp_registry,
    {
      # Create a registry with some modules
      registry <- list(
        modules = list(
          "dplyr-r" = list(module = "dplyr", type = "r"),
          "ggplot2-r" = list(module = "ggplot2", type = "r"),
          "basic-markdown" = list(module = "basic", type = "markdown")
        ),
        last_updated = Sys.time()
      )
      
      snippets:::write_module_registry(registry)
      
      # Test getting modules for type "r"
      r_modules <- snippets:::get_installed_modules_for_type("r")
      expect_length(r_modules, 2)
      expect_true("dplyr" %in% r_modules)
      expect_true("ggplot2" %in% r_modules)
      
      # Test getting modules for type "markdown"
      md_modules <- snippets:::get_installed_modules_for_type("markdown")
      expect_length(md_modules, 1)
      expect_true("basic" %in% md_modules)
      
      # Test getting modules for non-existent type
      empty_modules <- snippets:::get_installed_modules_for_type("python")
      expect_length(empty_modules, 0)
    }
  )
  
  # Cleanup
  if (file.exists(temp_registry)) {
    unlink(temp_registry)
  }
})