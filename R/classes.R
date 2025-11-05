setClass(
  "HeatmapMaker",
  representation(
    data = "matrix",
    heatmap = "Heatmap",
    annotations = "list",
    annotation_data = "data.frame",
    legends = "list",
    align_params = "list"
  )
)


# %%
HeatmapMaker <- function(data, annotations = list(), annotation_data = data.frame(), legends = list(), align_params = list()) {
  new("HeatmapMaker", data = data, heatmap = Heatmap(data), annotations = annotations, annotation_data = annotation_data, legends = legends, align_params = align_params)
}
# %%

