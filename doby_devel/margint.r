##' @title Marginal table
##' @param tbl,tbl2 Named arrays (arrays with named dimnames)
##' @param ... Specification of variables, see examples
##' @return A named array
##' @author Søren Højsgaard
##' @name table_operation
##' @examples
##'
##' # Multiple bare names
##' table_marg(Titanic, Survived, Sex)
##' table_marg(Titanic, ~ Survived + Sex)
##' table_marg(Titanic, c("Survived", "Sex"))
##' table_marg(Titanic, c(4, 2))
##'
NULL

##' @rdname table_operation
##' @export

table_marg <- function(tbl, ...) {
  if (!is_named_array(tbl)) {
    stop("Input must be a named array.")
  }

  dots <- substitute(list(...))[-1]

  dims <- NULL

  if (length(dots) == 0) {
    dims <- character(0)  # full margin
  } else if (length(dots) == 1) {
    arg <- dots[[1]]

    if (is.call(arg) && as.character(arg[[1]]) == "~") {
      dims <- all.vars(eval(arg, envir = parent.frame()))
    } else if (is.call(arg) && as.character(arg[[1]]) == "c") {
      dims <- eval(arg, envir = parent.frame())
    } else if (is.symbol(arg)) {
      dims <- eval(arg, envir = parent.frame())
    } else {
      dims <- deparse(arg)
    }
  } else {
    dims <- vapply(dots, deparse, character(1))
  }

  # Support numeric margin
  if (is.numeric(dims)) {
    return(margin.table(tbl, dims))
  }

  # Handle empty set
  if (length(dims) == 0) {
    return(margin.table(tbl))
  }

  dim_names <- names(dimnames(tbl))
  dim_indices <- match(dims, dim_names)

  if (anyNA(dim_indices)) {
    stop("Some dimension names not found: ", paste(dims[is.na(dim_indices)], collapse = ", "))
  }

  margin.table(tbl, dim_indices)
}


##' @examples
##' table_perm(Titanic, Survived, Age, Class, Sex)
##' table_perm(Titanic, ~Survived + Age + Class + Sex)
##' table_perm(Titanic, c("Survived", "Age", "Class", "Sex"))
##' table_perm(Titanic, c(4,3,1,2))
##' ## dims <- c("Survived", "Age", "Class")
##' ## table_perm(Titanic, dims)            


##' @rdname table_operation
##' @export
table_perm <- function(tbl, ...) {
  if (!is_named_array(tbl)) {
    stop("Input must be a named array.")
  }

  dots <- substitute(list(...))[-1]
  dims <- NULL

  if (length(dots) == 0) {
    stop("No dimensions specified.")
  } else if (length(dots) == 1) {
    arg <- dots[[1]]

    if (is.call(arg) && as.character(arg[[1]]) == "~") {
      dims <- all.vars(eval(arg, envir = parent.frame()))
    } else if (is.call(arg) && as.character(arg[[1]]) == "c") {
      dims <- eval(arg, envir = parent.frame())
    } else if (is.symbol(arg)) {
      dims <- eval(arg, envir = parent.frame())
    } else {
      dims <- deparse(arg)
    }
  } else {
    dims <- vapply(dots, deparse, character(1))
  }

  if (is.numeric(dims)) {
    if (length(dims) != length(dim(tbl))) {
      stop("Numeric permutation must include all dimensions.")
    }
    return(aperm(tbl, dims))
  }

  dn <- names(dimnames(tbl))
  if (!all(dims %in% dn)) {
    stop("Some dimension names not found: ", paste(dims[!dims %in% dn], collapse = ", "))
  }
  if (length(dims) != length(dn)) {
    stop("You must specify all dimensions in new order.")
  }

  aperm(tbl, match(dims, dn))
}


##' @rdname table_operation
##' @export
table_perm_ <- function(tbl, dims) {
  if (!is_named_array(tbl)) {
    stop("Input must be a named array.")
  }

  # If numeric
  if (is.numeric(dims)) {
    if (length(dims) != length(dim(tbl))) {
      stop("Numeric permutation must include all dimensions.")
    }
    return(aperm(tbl, dims))
  }

  # If character
  dn <- names(dimnames(tbl))
  if (!all(dims %in% dn)) {
    stop("Some dimension names not found: ", paste(dims[!dims %in% dn], collapse = ", "))
  }
  if (length(dims) != length(dn)) {
    stop("You must specify all dimensions in new order.")
  }

  aperm(tbl, match(dims, dn))
}


expand_table <- function(tbl, full_dims) {
  current_dims <- names(dimnames(tbl))
  current_dimnames <- dimnames(tbl)

  full_dimnames <- lapply(full_dims, function(d) {
    if (d %in% current_dims) {
      current_dimnames[[d]]
    } else {
      # Add a singleton dim with levels from other table
      # We'll fill this in from the OTHER table outside this function
      NULL
    }
  })
  names(full_dimnames) <- full_dims

  # Fill in missing levels from other table (handled outside)
  full_dimnames
}








##' @rdname table_operation
##' @examples
##' sa <- Titanic |> table_marg(Survived, Age)
##' sc <- Titanic |> table_marg(Class, Survived)
##' table_op(sa, sc)
##' 
##' @export
table_op <- function(tbl, tbl2, op = `*`) {
  if (!is_named_array(tbl) || !is_named_array(tbl2)) {
    stop("Both inputs must be named arrays.")
  }

  all_dims <- union(names(dimnames(tbl)), names(dimnames(tbl2)))

  # Ensure common dims have matching levels
  for (d in intersect(names(dimnames(tbl)), names(dimnames(tbl2)))) {
    lv1 <- dimnames(tbl)[[d]]
    lv2 <- dimnames(tbl2)[[d]]
    if (!identical(sort(lv1), sort(lv2))) {
      stop(sprintf(
        "Dimension '%s' has mismatched levels:\n  tbl: %s\n  tbl2: %s",
        d,
        paste(lv1, collapse = ", "),
        paste(lv2, collapse = ", ")
      ))
    }
  }

  # Create a complete dimnames template
  dim_template <- list()
  for (d in all_dims) {
    lv1 <- dimnames(tbl)[[d]]
    lv2 <- dimnames(tbl2)[[d]]
    dim_template[[d]] <- if (!is.null(lv1)) lv1 else lv2
  }

  # Expand both tables to full dimnames
  expand_array <- function(t, template) {
    dims <- sapply(template, function(x) if (is.null(x)) 1 else length(x))
    dimnames_out <- lapply(template, function(x) if (is.null(x)) "" else x)
    array(t, dim = dims, dimnames = dimnames_out)
  }

  tbl1_exp <- expand_array(tbl, dim_template)
  tbl2_exp <- expand_array(tbl2, dim_template)

  # Align order
  tbl1_aligned <- table_perm_(tbl1_exp, names(dim_template))
  tbl2_aligned <- table_perm_(tbl2_exp, names(dim_template))

  # Apply the operation (with 0/0 = 0 rule)
  result <- if (identical(op, `/`)) {
    ifelse(tbl1_aligned == 0 & tbl2_aligned == 0, 0, tbl1_aligned / tbl2_aligned)
  } else {
    op(tbl1_aligned, tbl2_aligned)
  }

  dimnames(result) <- dimnames(tbl1_aligned)
  result
}



align_table <- function(t, target_dimnames) {
  target_dim_order <- names(target_dimnames)

  # Construct full shape
  dims <- sapply(target_dimnames, function(x) if (is.null(x)) 1 else length(x))
  dnms <- lapply(target_dimnames, function(x) if (is.null(x)) "" else x)

  # Build array with those dims, recycling t
  full_array <- array(t, dim = dims, dimnames = dnms)

  # Reorder to match dim order
  table_perm_(full_array, target_dim_order)
}


align_table <- function(t, target_dimnames) {
  target_dim_order <- names(target_dimnames)

  # Construct full shape
  dims <- sapply(target_dimnames, function(x) if (is.null(x)) 1 else length(x))
  dnms <- lapply(target_dimnames, function(x) if (is.null(x)) "" else x)

  # Build array with those dims, recycling t
  full_array <- array(t, dim = dims, dimnames = dnms)

  # Reorder to match dim order
  table_perm_(full_array, target_dim_order)
}



table_op <- function(tbl, tbl2, op = `*`) {
  if (!is_named_array(tbl) || !is_named_array(tbl2)) {
    stop("Both inputs must be named arrays.")
  }

  all_dims <- union(names(dimnames(tbl)), names(dimnames(tbl2)))

  # Check shared dimensions have matching levels
  for (d in intersect(names(dimnames(tbl)), names(dimnames(tbl2)))) {
    lv1 <- dimnames(tbl)[[d]]
    lv2 <- dimnames(tbl2)[[d]]
    if (!identical(sort(lv1), sort(lv2))) {
      stop(sprintf("Dimension '%s' has mismatched levels:\n  tbl: %s\n  tbl2: %s",
                   d, paste(lv1, collapse = ", "), paste(lv2, collapse = ", ")))
    }
  }

  # Construct full dimnames template
  full_dimnames <- list()
  for (d in all_dims) {
    full_dimnames[[d]] <-
      if (!is.null(dimnames(tbl)[[d]])) {
        dimnames(tbl)[[d]]
      } else {
        dimnames(tbl2)[[d]]
      }
  }


  tbl1_aligned <- align_table(tbl, full_dimnames)
  tbl2_aligned <- align_table(tbl2, full_dimnames)

  # Apply op (with 0/0 = 0)
  result <- if (identical(op, `/`)) {
    ifelse(tbl1_aligned == 0 & tbl2_aligned == 0, 0, tbl1_aligned / tbl2_aligned)
  } else {
    op(tbl1_aligned, tbl2_aligned)
  }

  dimnames(result) <- full_dimnames
  result
}



is_named_array <- function(x) {
  is_array <- is.array(x)
  has_dimnames <- !is.null(dimnames(x))
  has_named_dimnames <- has_dimnames && !is.null(names(dimnames(x)))
  all_named <- has_named_dimnames && all(nzchar(names(dimnames(x))))
  is_array && all_named
}



## table_marg <- function(tbl, ...) {
##   if (!is_named_array(tbl)) {
##     stop("Input must be a named array.")
##   }

##   dots <- substitute(list(...))[-1]

##   if (length(dots) == 1) {
##     arg <- dots[[1]]

##     # Formula interface
##     if (is.call(arg) && as.character(arg[[1]]) == "~") {
##       dims <- all.vars(eval(arg, envir = parent.frame()))

##     # Character or numeric vector
##     } else if (is.call(arg) && as.character(arg[[1]]) == "c") {
##       dims <- eval(arg, envir = parent.frame())

##     # Single bare name
##     } else {
##       dims <- deparse(arg)
##     }
##   } else {
##     dims <- vapply(dots, deparse, character(1))
##   }

##   # Handle numeric
##   if (is.numeric(dims)) {
##     return(margin.table(tbl, dims))
##   }

##   dim_names <- names(dimnames(tbl))
##   dim_indices <- match(dims, dim_names)

##   if (anyNA(dim_indices)) {
##     stop("Some dimension names not found: ", paste(dims[is.na(dim_indices)], collapse = ", "))
##   }

##   margin.table(tbl, dim_indices)
## }








## table_perm <- function(tbl, ...) {
##   if (!is_named_array(tbl)) {
##     stop("Input must be a named array.")
##   }

##   dots <- substitute(list(...))[-1]
##   dims <- NULL

##   if (length(dots) == 0) {
##     stop("No dimensions specified.")
##   } else if (length(dots) == 1) {
##     arg <- dots[[1]]

##     if (is.call(arg) && as.character(arg[[1]]) == "~") {
##       dims <- all.vars(eval(arg, envir = parent.frame()))
##     } else if (is.call(arg) && as.character(arg[[1]]) == "c") {
##       dims <- eval(arg, envir = parent.frame())
##     } else if (is.symbol(arg)) {
##       dims <- eval(arg, envir = parent.frame())
##     } else {
##       dims <- deparse(arg)
##     }
##   } else {
##     dims <- vapply(dots, deparse, character(1))
##   }

##   if (is.numeric(dims)) {
##     if (length(dims) != length(dim(tbl))) {
##       stop("Numeric permutation must include all dimensions.")
##     }
##     return(aperm(tbl, dims))
##   }

##   dn <- names(dimnames(tbl))
##   if (!all(dims %in% dn)) {
##     stop("Some dimension names not found: ", paste(dims[!dims %in% dn], collapse = ", "))
##   }
##   if (length(dims) != length(dn)) {
##     stop("You must specify all dimensions in new order.")
##   }

##   aperm(tbl, match(dims, dn))
## }


## table_perm <- function(tbl, ...) {
##   if (!is_named_array(tbl)) {
##     stop("Input must be a named array.")
##   }

##   dots <- substitute(list(...))[-1]
##   dims <- NULL

##   if (length(dots) == 0) {
##     stop("No dimensions specified.")
##   } else if (length(dots) == 1) {
##     arg <- dots[[1]]

##     if (is.call(arg) && as.character(arg[[1]]) == "~") {
##       dims <- all.vars(eval(arg, envir = parent.frame()))
##     } else if (is.call(arg) && as.character(arg[[1]]) == "c") {
##       dims <- eval(arg, envir = parent.frame())
##     } else if (is.symbol(arg)) {
##       dims <- eval(arg, envir = parent.frame())
##     } else {
##       dims <- deparse(arg)
##     }
##   } else {
##     dims <- vapply(dots, deparse, character(1))
##   }

##   if (is.numeric(dims)) {
##     if (length(dims) != length(dim(tbl))) {
##       stop("Numeric permutation must include all dimensions.")
##     }
##     return(aperm(tbl, dims))
##   }

##   dn <- names(dimnames(tbl))
##   if (!all(dims %in% dn)) {
##     stop("Some dimension names not found: ", paste(dims[!dims %in% dn], collapse = ", "))
##   }
##   if (length(dims) != length(dn)) {
##     stop("You must specify all dimensions in new order.")
##   }

##   aperm(tbl, match(dims, dn))
## }

