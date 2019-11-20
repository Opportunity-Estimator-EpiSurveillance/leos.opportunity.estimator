setFactorOrder <- function(x, order=sort(levels(x))) {
  # Returns a factor ordered by `order`.
  # If order is missing, defaults to `levels(x)` if available, else to `sort(unique(x))`
  # Useful for ggplot and elsewhere were ordering is based on the order of the levels

  if (!is.factor(x)) {
    warning("`x` is not a factor. Will coerce.")
    levs <- sort(unique(x))
    if (missing(order))
      order <- levs
  } else {
    levs <- levels(x)
  }

  # any values in order, not in levels(x)
  NotInx <- setdiff(order, levs)

  if (length(NotInx)) {
    warning ("Some values not in x:\n", paste(NotInx, collapse=", "))
  }

  # levels(x) not explicitly named in order
  Remaining <-  setdiff(levs, order)

  order <- c(setdiff(order, NotInx), Remaining)

  factor(x, level=order)
}
