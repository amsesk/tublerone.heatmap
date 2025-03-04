setClass(
  "HeatmapMaker",
  representation(
    data = "matrix",
    heatmap = "Heatmap",
    annotations = "list",
    legends = "list",
    align_params = "list"
  )
)


# %%
HeatmapMaker <- function(data, annotations = list(), legends = list(), align_params = list()) {
  new("HeatmapMaker", data = data, heatmap = Heatmap(data), annotations = annotations, legends = legends, align_params = align_params)
}
# %%

