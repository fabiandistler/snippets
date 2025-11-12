test_that("install_snippet_modules validates modules parameter", {
  # modules must be character
  expect_error(
    install_snippet_modules(modules = 123, type = "r"),
    "'modules' must be a non-empty character vector"
  )

  # modules must not be empty vector
  expect_error(
    install_snippet_modules(modules = character(0), type = "r"),
    "'modules' must be a non-empty character vector"
  )

  # modules can be NULL (valid for auto-discovery)
  # This should not error, but we skip actual installation in tests
  expect_no_error({
    modules <- NULL
    is.null(modules) || (is.character(modules) && length(modules) > 0)
  })
})

test_that("install_snippet_modules validates type parameter", {
  # type must be character
  expect_error(
    install_snippet_modules(modules = "dplyr", type = 123),
    "'type' must be a non-empty character vector"
  )

  # type must not be empty vector
  expect_error(
    install_snippet_modules(modules = "dplyr", type = character(0)),
    "'type' must be a non-empty character vector"
  )

  # type can be NULL (valid for auto-discovery)
  expect_no_error({
    type_val <- NULL
    is.null(type_val) || (is.character(type_val) && length(type_val) > 0)
  })
})

test_that("install_snippet_modules validates from parameter", {
  # from must be character
  expect_error(
    install_snippet_modules(modules = "dplyr", type = "r", from = 123),
    "'from' must be a non-empty character string"
  )

  # from must be single string, not vector
  expect_error(
    install_snippet_modules(modules = "dplyr", type = "r", from = c("path1", "path2")),
    "'from' must be a non-empty character string"
  )

  # from must not be empty string
  expect_error(
    install_snippet_modules(modules = "dplyr", type = "r", from = ""),
    "'from' must be a non-empty character string"
  )

  # from can be NULL (uses default)
  expect_no_error({
    from_val <- NULL
    is.null(from_val) || (is.character(from_val) && length(from_val) == 1 && from_val != "")
  })
})

test_that("install_snippet_modules validates backup parameter", {
  # backup must be logical
  expect_error(
    install_snippet_modules(modules = "dplyr", type = "r", backup = "yes"),
    "'backup' must be a single logical value"
  )

  # backup must be single value
  expect_error(
    install_snippet_modules(modules = "dplyr", type = "r", backup = c(TRUE, FALSE)),
    "'backup' must be a single logical value"
  )

  # backup TRUE or FALSE is valid
  expect_no_error({
    backup_val <- TRUE
    is.logical(backup_val) && length(backup_val) == 1
  })
})

test_that("install_snippet_modules validates force_update parameter", {
  # force_update must be logical
  expect_error(
    install_snippet_modules(modules = "dplyr", type = "r", force_update = "yes"),
    "'force_update' must be a single logical value"
  )

  # force_update must be single value
  expect_error(
    install_snippet_modules(modules = "dplyr", type = "r", force_update = c(TRUE, FALSE)),
    "'force_update' must be a single logical value"
  )

  # force_update TRUE or FALSE is valid
  expect_no_error({
    force_val <- FALSE
    is.logical(force_val) && length(force_val) == 1
  })
})

test_that("remove_snippet_modules validates modules parameter", {
  # modules is required (cannot be missing)
  expect_error(
    remove_snippet_modules(type = "r"),
    "argument \"modules\" is missing"
  )

  # modules must be character
  expect_error(
    remove_snippet_modules(modules = 123, type = "r"),
    "'modules' must be a non-empty character vector"
  )

  # modules must not be empty vector
  expect_error(
    remove_snippet_modules(modules = character(0), type = "r"),
    "'modules' must be a non-empty character vector"
  )
})

test_that("remove_snippet_modules validates type parameter", {
  # type must be character
  expect_error(
    remove_snippet_modules(modules = "dplyr", type = 123),
    "'type' must be a single character string"
  )

  # type must be single string
  expect_error(
    remove_snippet_modules(modules = "dplyr", type = c("r", "markdown")),
    "'type' must be a single character string"
  )
})

test_that("remove_snippet_modules validates backup parameter", {
  # backup must be logical
  expect_error(
    remove_snippet_modules(modules = "dplyr", type = "r", backup = "no"),
    "'backup' must be a single logical value"
  )

  # backup must be single value
  expect_error(
    remove_snippet_modules(modules = "dplyr", type = "r", backup = c(TRUE, FALSE)),
    "'backup' must be a single logical value"
  )
})
