test_that("install_snippet_modules validates modules parameter", {
  # modules must be character
  expect_error(
    install_snippet_modules(modules = 123, type = "r"),
    "modules.*must be.*character"
  )

  # modules must not be empty vector
  expect_error(
    install_snippet_modules(modules = character(0), type = "r"),
    "modules.*must be.*non-empty"
  )
})

test_that("install_snippet_modules validates type parameter", {
  # type must be character
  expect_error(
    install_snippet_modules(modules = "dplyr", type = 123),
    "type.*must be.*character"
  )

  # type must not be empty vector
  expect_error(
    install_snippet_modules(modules = "dplyr", type = character(0)),
    "type.*must be.*non-empty"
  )
})

test_that("install_snippet_modules validates from parameter", {
  # from must be character
  expect_error(
    install_snippet_modules(modules = "dplyr", type = "r", from = 123),
    "from.*must be.*character"
  )

  # from must be single string, not vector
  expect_error(
    install_snippet_modules(modules = "dplyr", type = "r", from = c("path1", "path2")),
    "from.*must be.*character string"
  )

  # from must not be empty string
  expect_error(
    install_snippet_modules(modules = "dplyr", type = "r", from = ""),
    "from.*must be.*non-empty"
  )
})

test_that("install_snippet_modules validates backup parameter", {
  # backup must be logical
  expect_error(
    install_snippet_modules(modules = "dplyr", type = "r", backup = "yes"),
    "backup.*must be.*logical"
  )

  # backup must be single value
  expect_error(
    install_snippet_modules(modules = "dplyr", type = "r", backup = c(TRUE, FALSE)),
    "backup.*must be.*single.*logical"
  )
})

test_that("install_snippet_modules validates force_update parameter", {
  # force_update must be logical
  expect_error(
    install_snippet_modules(modules = "dplyr", type = "r", force_update = "yes"),
    "force_update.*must be.*logical"
  )

  # force_update must be single value
  expect_error(
    install_snippet_modules(modules = "dplyr", type = "r", force_update = c(TRUE, FALSE)),
    "force_update.*must be.*single.*logical"
  )
})

test_that("remove_snippet_modules validates modules parameter", {
  # modules is required (cannot be missing)
  expect_error(
    remove_snippet_modules(type = "r"),
    "modules.*missing|must be.*non-empty"
  )

  # modules must be character
  expect_error(
    remove_snippet_modules(modules = 123, type = "r"),
    "modules.*must be.*character"
  )

  # modules must not be empty vector
  expect_error(
    remove_snippet_modules(modules = character(0), type = "r"),
    "modules.*must be.*non-empty"
  )
})

test_that("remove_snippet_modules validates type parameter", {
  # type must be character
  expect_error(
    remove_snippet_modules(modules = "dplyr", type = 123),
    "type.*must be.*character"
  )

  # type must be single string
  expect_error(
    remove_snippet_modules(modules = "dplyr", type = c("r", "markdown")),
    "type.*must be.*single"
  )
})

test_that("remove_snippet_modules validates backup parameter", {
  # backup must be logical
  expect_error(
    remove_snippet_modules(modules = "dplyr", type = "r", backup = "no"),
    "backup.*must be.*logical"
  )

  # backup must be single value
  expect_error(
    remove_snippet_modules(modules = "dplyr", type = "r", backup = c(TRUE, FALSE)),
    "backup.*must be.*single.*logical"
  )
})
