#' @importFrom circlize colorRamp2
#' @importFrom ComplexHeatmap Heatmap
#' @param counts A matrix of counts with genes as rows and samples as columns.
#' @param genes A character vector, or something coercible to one, of gene names to include in the heatmap. Default is to include all genes (rows) in the count matrix.
make.HeatmapMaker <- function(object,
                              genes = NULL,
                              scale = TRUE,
                              col = circlize::colorRamp2(c(-2, 0, 2), c("blue", "white", "red")),
                              ...) {
  counts <- object@data
  if (!is.null(genes)) {
    counts <- counts[genes, ]
  }
  if (scale) {
    counts_scale <- t(apply(counts, 1, scale))
    colnames(counts_scale) <- colnames(counts)
    rownames(counts_scale) <- rownames(counts)
    counts <- counts_scale
  }

  top_annotation <- object@annotations[["top"]]
  bottom_annotation <- object@annotations[["bottom"]]

  hm <- ComplexHeatmap::Heatmap(counts,
    col = col,
    bottom_annotation = bottom_annotation,
    top_annotation = top_annotation,
    ...
  )
  object@heatmap <- hm
}
# %%
hm_size_and_position <- function(figure_width,
                                 figure_height,
                                 row_dend_width = unit(0.2, "in"),
                                 row_names_max_width = unit(1, "in"),
                                 column_dend_height = unit(0.2, "in"),
                                 column_names_max_height = unit(1, "in"),
                                 hm_width_scaling = 0.8) {
  LEFT_PADDING <- unit(0.05, "in")
  values <- c()
  values$row_dend_width <- row_dend_width
  values$row_names_max_width <- row_names_max_width
  values$column_dend_height <- column_dend_height
  values$column_names_max_height <- column_names_max_height
  values$width <- (figure_width - row_dend_width - row_names_max_width) * hm_width_scaling
  values$height <- (figure_height - column_dend_height - column_names_max_height)
  values$padding <- unit(c(0, LEFT_PADDING, 0, (figure_width - (values$width + row_dend_width + LEFT_PADDING))), "in")
  values
}
# %%

#' @importFrom circlize colorRamp2
#' @importFrom ComplexHeatmap Legend
#' @import magick
#' @export
#' @param counts A matrix of counts with genes as rows and samples as columns.
#' @param genes A character vector, or something coercible to one, of gene names to include in the heatmap. Default is to include all genes (rows) in the count matrix.
pretty_draw.HeatmapMaker <- function(object,
                                     genes = NULL,
                                     scale = TRUE,
                                     figure_width = unit(6.5, "in"),
                                     figure_height = unit(4.5, "in"),
                                     col = circlize::colorRamp2(c(-2, 0, 2), c("blue", "white", "red")),
                                     show_row_names = FALSE,
                                     legends = NULL,
                                     bottom_annotations = NULL,
                                     top_annotations = NULL,
                                     legend_loc = list("x"=unit(0.85, "npc"), "y"=unit(0.6, "npc")),
                                     ...) {
  legend_title <- ifelse(scale, "Scaled expression", "Expression")
  legend <- Legend(col_fun = col, title = legend_title, title_gp = gpar(fontsize = 8), labels_gp = gpar(fontsize = 6))
  object@legends[["heatmap"]] <- legend
  hm_plt_params <- hm_size_and_position(
    figure_width = unit(7.5, "in"),
    figure_height = unit(5.0, "in"),
    row_names_max_width = unit(0, "in")
  )
  object@heatmap <- make(
    object,
    genes = genes,
    scale = scale,
    col = col,
    show_row_names = show_row_names,
    show_heatmap_legend = FALSE,
    row_dend_width = hm_plt_params$row_dend_width,
    row_names_max_width = hm_plt_params$row_names_max_width,
    column_dend_height = hm_plt_params$column_dend_height,
    column_names_max_height = hm_plt_params$column_names_max_height,
    width = hm_plt_params$width,
    height = hm_plt_params$height,
    ...
  )
  legend_pack = packLegend(list = object@legends)
  draw(object@heatmap,
    padding = hm_plt_params$padding,
  )
  draw(legend_pack, x = legend_loc[["x"]], y = legend_loc[["y"]])

  object
}
# %%

