COLORS <- c("#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377", "#68855C", "#9C9C5E", "#A06177", "#8C785D")

assign_colors <- function(df) {
  col <- list()
  i <- 1
  for (column in colnames(df)) {
    values <- levels(df[, column])
    idx <- i:(i + length(values) - 1)
    these <- COLORS[idx]
    names(these) <- values
    col[[column]] <- these
    i <- i + length(values)
  }
  col
}

add_annotations.HeatmapMaker <- function(object, df, location = "bottom", titles = NULL, col = NULL) {
  if (is.null(titles)) {
    titles <- colnames(df)
    names(titles) <- titles
  } else {
    if (is.null(names(titles))) {
      stop("titles must be named list")
    }
  }
  if (is.null(col)) {
    col = assign_colors(df)
  } else {
    if (is.null(names(col))) {
      stop("col must be a list of named vectors")
    }
  }
  object@annotations[[location]] <- ComplexHeatmap::HeatmapAnnotation(
    df = df,
    show_legend = FALSE,
    annotation_name_gp = gpar(fontsize = 8),
    annotation_height = 0.5,
    col = col
  )
  object <- add_legends(object, df, col, titles)

  object
}
# %%
