merge_gpar = function(x, y) {
  if(!inherits(x, "gpar") || !inherits(x, "gpar")) {
    rlang::abort("x and y should be gpar")
  }
  x_names = names(x)
  y_names = names(y)
  if (length(unique(x_names)) != length(x_names) || length(unique(y_names)) != length(y_names)) {
    rlang::abort("one or both gpar names not internally unique")
  }
  for (yn in y_names) {
    if (yn %in% x_names) {
      rlang::warn(glue::glue("graphical param `{yn}` already in x, value in y will replace value in x"))
    }
    x[[yn]] = y[[yn]]
  }
  x
}
# %%

