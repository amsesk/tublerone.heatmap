
# %%
add_legends.HeatmapMaker = function(object, df, col, titles = NULL) {
  if (is.null(titles)) {
    titles = colnames(df)
    names(titles) = titles
  } else {
    if (is.null(names(titles))){
      stop("titles must be named list")
    }
  }
  for (column in colnames(df)){
    object = add_legend(object, df[,column], title=titles[column], col = col[[column]])
  }
  object
}

# %%
add_legend.factor <- function(object, values, title, col) {
  labels <- levels(values)
  lgd <- ComplexHeatmap::Legend(
    labels = labels,
    title = title,
    grid_height = unit(2, "mm"),
    grid_width = unit(3, "mm"),
    labels_gp = gpar(fontsize = 6),
    legend_gp = gpar(fill = col),
    title_gp = gpar(fontsize = 8)
  )
  object@legends[[title]] <- lgd
  object
}
# %%

