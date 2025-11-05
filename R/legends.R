# %%
add_legends.HeatmapMaker <- function(object, df, col, titles = NULL, ...) {
  if (is.null(titles)) {
    titles <- colnames(df)
    names(titles) <- titles
  } else {
    if (is.null(names(titles))) {
      rlang::abort("titles must be named list")
    }
  }
  if (is.null(names(col))) {
    rlang::abort("col must be named list")
  }
  for (column in colnames(df)) {
    object <- add_legend(object, df[, column], title = titles[column], col = col[[column]], ...)
  }
  object
}

# %%
make_annotation_legends.HeatmapMaker <- function(object, col, ...) {
  if (is.null(names(col))) {
    rlang::abort("col must be named list")
  }
  for (column in colnames(object@annotation_data)[-which(colnames(object@annotation_data) == "location")]) {
    object <- add_legend(object, object@annotation_data[, column], title = column, col = col[[column]], ...)
  }
  object
}

# %%                                                              list
make_heatmap_legend.HeatmapMaker <- function(object, title, col, ...) {
  object@legends[["heatmap"]] <- ComplexHeatmap::Legend(
    col_fun = col,
    title = title,
    ...
  )
  object
}

# %%
add_legend.factor <- function(object, values, title, col, ...) {
  labels <- levels(values)
  lgd <- ComplexHeatmap::Legend(
    labels = labels,
    title = title,
    legend_gp = gpar(fill = col[labels]),
    ...
  )
  object@legends[[title]] <- lgd
  object
}
# %%
