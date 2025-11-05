#' @importFrom circlize colorRamp2
#' @importFrom ComplexHeatmap Heatmap
#' @param counts A matrix of counts with genes as rows and samples as columns.
#' @param genes A character vector, or something coercible to one, of gene names to include in the heatmap. Default is to include all genes (rows) in the count matrix.
make.HeatmapMaker <- function(object,
                              scale = TRUE,
                              col = circlize::colorRamp2(c(-2, 0, 2), c("blue", "white", "red")),
                              ...) {
  counts <- object@data
  if (scale) {
    rlang::inform("Scaling heatmap matrix")
    counts_scale <- t(apply(counts, 1, scale))
    colnames(counts_scale) <- colnames(counts)
    rownames(counts_scale) <- rownames(counts)
    counts <- counts_scale
  }

  top_annotation <- object@annotations[["top"]]
  bottom_annotation <- object@annotations[["bottom"]]
  right_annotation = object@annotations[["right"]]

  hm <- ComplexHeatmap::Heatmap(counts,
    col = col,
    bottom_annotation = bottom_annotation,
    top_annotation = top_annotation,
    right_annotation = right_annotation,
    ...
  )
  hm
}
# %%
subset_features.HeatmapMaker <- function(object,
                                      features) {
  object@data <- object@data[features, ]
  object
}

# %%
align2.HeatmapMaker <- function(object,
                                figure_width,
                                figure_height,
                                ...
) {}

# %%
align.HeatmapMaker <- function(object,
                               figure_width,
                               figure_height,
                               row_dend_width = unit(0.2, "in"),
                               showing_row_names=FALSE,
                               row_names_max_width = unit(1, "in"),
                               column_dend_height = unit(0.2, "in"),
                               showing_column_names=FALSE,
                               column_names_max_height = unit(1, "in"),
                               width_scaling = 0.8,
                               extra_border = unit(0.05, "in"),
                               horiz_justify = "left"
                               ) {
  ht_opt = ComplexHeatmap::ht_opt
  column_anno_padding = grid::convertUnit(ht_opt$COLUMN_ANNO_PADDING, "in")
  row_anno_padding = grid::convertUnit(ht_opt$ROW_ANNO_PADDING, "in")
  if (!showing_row_names) {
    row_names_max_width = unit(0.0,"in")
  }
  if (!showing_column_names) {
    column_names_max_height = unit(0.0,"in")
  }

  row_annotation_extend = list()
  i = 1
  for (side in c("top", "bottom")) {
    if (!is.null(object@annotations[[side]])) {
      row_annotation_extend[[i]] = as.numeric(convertUnit(object@annotations[[side]]@extended, "in")) 
    }
  }
  for (side in c("left", "right")) {
    if (!is.null(object@annotations[[side]])) {
      row_annotation_extend[[i]] = as.numeric(convertUnit(object@annotations[[side]]@anno_size, "in")) 
    }
  }
  row_annotation_extend = row_annotation_extend %>%
    unlist() %>%
    max() %>%
    unit("in")

  column_annotation_extend=list()
  i=1
  for (side in c("top", "bottom")) {
    if (!is.null(object@annotations[[side]])) {
       column_annotation_extend[[i]] = as.numeric(convertUnit(object@annotations[["top"]]@height, "in"))
    }
  }
  column_annotation_extend = column_annotation_extend %>%
    unlist() %>%
    max() %>%
    unit("in")

  values <- list()
  values$row_dend_width <- row_dend_width
  values$row_names_max_width <- row_names_max_width
  values$column_dend_height <- column_dend_height
  values$column_names_max_height <- column_names_max_height
  values$row_annotation_extend = row_annotation_extend
  values$column_annotation_extend = column_annotation_extend
  values$horizontal_spacing = (row_dend_width + row_names_max_width + values$row_annotation_extend + row_anno_padding + extra_border)
  values$vertical_spacing = (column_dend_height + column_names_max_height + values$column_annotation_extend + column_anno_padding + extra_border)
  values$height <- (figure_height - values$vertical_spacing)

  width = (figure_width)*(width_scaling)
  if ( as.numeric(width + values$horizontal_spacing) > as.numeric(figure_width)) {
    width = width - values$horizontal_spacing 
  } else {
    width = width
  }
  values$width = width

  horiz_padding = (figure_width)*(1-width_scaling)
  if ( as.numeric(horiz_padding - values$horizontal_spacing) < 0) {
    horiz_padding = unit(0, "in")
  } else {
    horiz_padding = horiz_padding - values$horizontal_spacing
  }

# %%

hmm@align_params
# %%
.hms = function(av_pair) {
}
  if (horiz_justify == "left") {
    values$padding <- unit(c(0,
                             0,
                             0,
                             # as.numeric( ((figure_width)*(1-width_scaling)) - values$horizontal_spacing + (values$horizontal_spacing*(1-width_scaling)))
                             # as.numeric( ((figure_width-values$horizontal_spacing)*(1-width_scaling)) )
                             horiz_padding
                             ), "in")
  } else if (horiz_justify == "right") { 
    values$padding <- unit(c(0,
                             # as.numeric( ((figure_width)*(1-width_scaling)) - values$horizontal_spacing + (values$horizontal_spacing*(1-width_scaling))),
                             horiz_padding,
                             0,
                             0
                             ), "in")
  } else {
    rlang::abort("horiz_justify must be either 'left' or 'right'.")
  }
  object@align_params = values
  object
}


# %%
draw.HeatmapMaker = function(object, col = circlize::colorRamp2(c(-2, 0, 2), c("blue", "white", "red")), scale=TRUE, ...) {
    hm <- object %>%
        make(
            scale = scale,
            width = object@align_params$width,
            height = object@align_params$height,
            gap=unit(0.00,"in"),
            col = col,
            row_dend_width=object@align_params$row_dend_width,
            column_dend_height=object@align_params$column_dend_height,
            row_names_max_width=object@align_params$row_names_max_width,
            column_names_max_height=object@align_params$column_names_max_height,
            row_dend_gp=gpar(lwd=0.3),
            column_dend_gp=gpar(lwd=0.3),
            show_heatmap_legend= FALSE,
            ...
        )

    
  ComplexHeatmap::draw(hm, padding = object@align_params$padding)
}

# %%

# %% DEPRECATED: pretty_draw.HeatmapMaker, Use align->draw instead {{{
#' @importFrom circlize colorRamp2
#' @importFrom ComplexHeatmap Legend
#' @import magick
#' @export
#' @param counts A matrix of counts with genes as rows and samples as columns.
#' @param genes A character vector, or something coercible to one, of gene names to include in the heatmap. Default is to include all genes (rows) in the count matrix.
# pretty_draw.HeatmapMaker <- function(object,
#                                      scale = TRUE,
#                                      figure_width = unit(6.5, "in"),
#                                      figure_height = unit(4.5, "in"),
#                                      col = circlize::colorRamp2(c(-2, 0, 2), c("blue", "white", "red")),
#                                      show_row_names = FALSE,
#                                      legends = NULL,
#                                      bottom_annotations = NULL,
#                                      top_annotations = NULL,
#                                      legend_loc = list("x"=unit(0.85, "npc"), "y"=unit(0.6, "npc")),
#                                      ...) {
#   legend_title <- ifelse(scale, "Scaled expression", "Expression")
#   legend <- Legend(col_fun = col, title = legend_title, title_gp = gpar(fontsize = 8), labels_gp = gpar(fontsize = 6))
#   object@legends[["heatmap"]] <- legend
#   hm_plt_params <- hm_size_and_position(
#     figure_height = figure_height,
#     figure_width = figure_width,
#   )
#   object@heatmap <- make(
#     object,
#     genes = genes,
#     scale = scale,
#     col = col,
#     show_row_names = show_row_names,
#     show_heatmap_legend = FALSE,
#     row_dend_width = hm_plt_params$row_dend_width,
#     row_names_max_width = hm_plt_params$row_names_max_width,
#     column_dend_height = hm_plt_params$column_dend_height,
#     column_names_max_height = hm_plt_params$column_names_max_height,
#     width = hm_plt_params$width,
#     height = hm_plt_params$height,
#     ...
#   )
#   legend_pack = packLegend(list = object@legends)
#   draw(object@heatmap,
#     padding = hm_plt_params$padding,
#   )
#   draw(legend_pack, x = legend_loc[["x"]], y = legend_loc[["y"]])
#
#   object
# }
# %%
# }}}

