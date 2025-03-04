
COLORS <- c("#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377", "#68855C", "#9C9C5E", "#A06177", "#8C785D", '#66c5cc', '#f6cf71', '#f89c74', '#dcb0f2', '#87c55f', '#9eb9f3', '#fe88b1', '#c9db74', '#8be0a4', '#b497e7')

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

add_simple_annotations.HeatmapMaker <- function(object, 
                                         df, 
                                         location = "bottom", 
                                         titles = NULL, 
                                         col = NULL, 
                                         simple_anno_size=unit(0.1, "in"),
                                         annotation_name_gp = gpar(fontsize=8)) {

  # Turn everything into a factor to avoid a slew of errors later
  for (column in colnames(df)) {
    if (!is.factor(df[[column]])) {
      df[[column]] = as.factor(df[[column]])
    }
  }

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
    annotation_name_gp = annotation_name_gp,
    simple_anno_size = simple_anno_size,
    col = col
  )
  object <- add_legends(object, df, col, titles)

  object
}

# %%
add_jittered_rownames.HeatmapMaker <- function(object, 
                                               to_label,
                                               location = "right",
                                               labels_gp = gpar(fontsize=4),
                                               lines_gp = gpar(lwd=0.3)
                                               ) {
  if (any(!to_label %in% rownames(object@data))) {
    mes = "Some provided feature names are not present in the rownames"
    rlang::abort(mes)
  }
  label_pos = which(rownames(object@data) %in% to_label)

  object@annotations[[location]] <- ComplexHeatmap::rowAnnotation(
    feature = ComplexHeatmap::anno_mark(at = label_pos,
                                        labels = to_label,
                                        labels_gp = labels_gp,
                                        lines_gp = lines_gp)
    )
  object
}
# %%

