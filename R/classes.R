library(ComplexHeatmap)
setClassUnion("HeatmapOrNull", c("Heatmap", "NULL"))
setClassUnion("FunctionOrNull", c("function", "NULL"))

setClass(
  "HeatmapMaker",
  representation(
    data = "matrix",
    heatmap_palette = "FunctionOrNull",
    annotation_palettes = "list",
    annotation_data = "list",
    annotations = "list",
    legends = "list",
    heatmaps = "list"
  )
)

# %%
HeatmapMaker <- function(data,
                         heatmap_palette = NULL,
                         row_annotation_data = data.frame(),
                         column_annotation_data = data.frame(),
                         # annotation_data = list(rows=data.frame(), columns=data.frame()),
                         annotation_palettes = list()
                         ) {
  new("HeatmapMaker", 
    data = data, 
    heatmap_palette=heatmap_palette,
    annotation_palettes=annotation_palettes,
    annotation_data = list(row=row_annotation_data, column=column_annotation_data),
    annotations = list(
      # top=new("HeatmapAnnotation"),
      # right=new("HeatmapAnnotation"),
      # bottom=new("HeatmapAnnotation"),
      # left=new("HeatmapAnnotation")
    ),
    legends = list(),
    heatmaps = list()
  )
}
# %%

