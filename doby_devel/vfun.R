# Load necessary package
library(rlang)

# Parse variable names from flexible input
# Accepts unquoted names, character vector, or formula (~x + y)
vparse <- function(...) {
  dots <- enquos(...)
  vars <- NULL

  if (length(dots) == 1) {
    val <- eval_tidy(dots[[1]])

    if (is_formula(val)) {
      vars <- all.vars(f_rhs(val))
    } else if (is.character(val)) {
      vars <- val
    } else {
      abort("Invalid input: expected a formula (~), character vector, or unquoted variable names.")
    }
  } else {
    vars <- unname(sapply(dots, as_name))
  }

  vars
}

# Select columns from a data frame using vparse-style input
vselect <- function(df, ...) {
  vars <- vparse(...)
  df[, vars, drop = FALSE]
}

# Check if variables exist in a data frame
# Returns TRUE if all vars exist, otherwise throws an error
vcheck <- function(df, ...) {
  vars <- vparse(...)
  missing <- vars[!vars %in% names(df)]
  if (length(missing) > 0) {
    abort(paste("Missing variables in data frame:", paste(missing, collapse = ", ")))
  }
  TRUE
}

# Map a function over variable names (e.g., printing or transforming)
vmap <- function(..., .f) {
  vars <- vparse(...)
  lapply(vars, .f)
}

# Rename variables in a data frame using a named character vector
# e.g., vrename(df, c(old1 = "new1", old2 = "new2"))
vrename <- function(df, rename_map) {
  stopifnot(is.character(rename_map), !is.null(names(rename_map)))
  old_names <- names(rename_map)
  new_names <- unname(rename_map)

  matched <- match(old_names, names(df))
  if (any(is.na(matched))) {
    abort("Some names in rename_map do not exist in the data frame.")
  }

  names(df)[matched] <- new_names
  df
}
