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

# %% DEPRECATED: add_simple_annotations takes care of creating its own legends
# make_annotation_legends.HeatmapMaker <- function(object, col, ...) {
#   if (is.null(names(col))) {
#     rlang::abort("col must be named list")
#   }
#   for (column in colnames(object@annotation_data)[-which(colnames(object@annotation_data) == "location")]) {
#     object <- add_legend(object, object@annotation_data[, column], title = column, col = col[[column]], ...)
#   }
#   object
# }

# %%                                                              list
make_heatmap_legend.HeatmapMaker <- function(object, title, col, ...) {
  object@legends[["heatmap"]] <- rlang::exec(
    ComplexHeatmap::Legend,
    col_fun = col,
    title = title,
    !!!legend_ht_opt(),
    ...
  )
  object
}

# %%
add_legend.factor <- function(object, values, title, col, legend_gp=NULL, ...) {
  labels <- levels(values)
  if (is.null(legend_gp)) {
    legend_gp = grid::gpar(fill = col[labels])
  } else {
    legend_gp = merge_gpar(legend_gp, grid::gpar(fill = col[labels]))
  }
  lgd <- rlang::exec(
    ComplexHeatmap::Legend,
    labels = labels,
    title = title,
    legend_gp = legend_gp,
    !!!legend_ht_opt(),
    ...
  )
  object@legends[[title]] <- lgd
  object
}

# %%
legend_ht_opt = function() {
  legend_ht_opt = ht_opt(names(ComplexHeatmap::ht_opt)[stringr::str_detect(names(ComplexHeatmap::ht_opt), "^legend_")]) %>%
    purrr::set_names(stringr::str_replace(names(.), "^legend_", "")) %>%
    purrr::discard(\(.x) { is.null(.x) }) %>%
    purrr::discard(\(.x) { is.list(.x) && length(.x)==0 })
  legend_ht_opt[["column_gap"]] = legend_ht_opt[["gap"]][[1]]
  legend_ht_opt[["row_gap"]] = legend_ht_opt[["gap"]][[2]]
  legend_ht_opt = legend_ht_opt[names(legend_ht_opt)!="gap"]
  legend_ht_opt
}
# %%

