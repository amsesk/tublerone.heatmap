setClass(
  "HeatmapMaker",
  representation(
    data = "matrix",
    heatmap = "Heatmap",
    annotations = "list",
    legends = "list"
  )
)


# %%
HeatmapMaker <- function(data, annotations = list(), legends = list()) {
  new("HeatmapMaker", data = data, heatmap = Heatmap(data), annotations = annotations, legends = legends)
}
# %%

