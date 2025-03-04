# %% Method for making legends for factor data
setGeneric("add_simple_annotations", function(object, df, ...) standardGeneric("add_simple_annotations"))
setMethod(
  "add_simple_annotations",
  signature("HeatmapMaker", "data.frame"),
  function(object, df, ...) add_simple_annotations.HeatmapMaker(object, df, ...)
)

# %% Method to subset the HeatmapMaker object by feature
setGeneric("subset_features", function(object, features, ...) standardGeneric("subset_features"))
setMethod(
  "subset_features",
  signature("HeatmapMaker", "character"),
  function(object, features, ...) subset_features.HeatmapMaker(object, features, ...)
)

# %% Method adding a subset of rownames as jittered, so they can be seen
setGeneric("add_jittered_rownames", function(object, to_label, ...) standardGeneric("add_jittered_rownames"))
setMethod(
  "add_jittered_rownames",
  signature("HeatmapMaker", "character"),
  function(object, to_label, ...) add_jittered_rownames.HeatmapMaker(object, to_label, ...)
)
# %% Method for making legends for factor data
setGeneric("add_legend", function(object, values, ...) standardGeneric("add_legend"))
setMethod(
  "add_legend",
  signature("HeatmapMaker", "factor"),
  function(object, values, ...) add_legend.factor(object, values, ...)
)

# %% Method for making multiple legends
setGeneric("add_legends", function(object, df, ...) standardGeneric("add_legends"))
setMethod(
  "add_legends",
  signature("HeatmapMaker", "data.frame"),
  function(object, df, ...) add_legends.HeatmapMaker(object, df, ...)
)

# %% Method for adding annotations
setGeneric("make", function(object, ...) standardGeneric("make"))
setMethod(
  "make",
  signature("HeatmapMaker"),
  function(object, ...) make.HeatmapMaker(object, ...)
)

# %% Draw an aligned annotated heatmap with legends
setGeneric("pretty_draw", function(object, ...) standardGeneric("pretty_draw"))
setMethod(
  "pretty_draw",
  signature("HeatmapMaker"),
  function(object, ...) pretty_draw.HeatmapMaker(object, ...)
)

# %% Draw an aligned annotated heatmap without legends
setGeneric("draw", function(object, ...) standardGeneric("draw"))
setMethod(
  "draw",
  signature("HeatmapMaker"),
  function(object, ...) draw.HeatmapMaker(object, ...)
)

# %% Generate the alignment parameters for plotting a heatmap, not including legends
setGeneric("align", function(object, ...) standardGeneric("align"))
setMethod(
  "align",
  signature("HeatmapMaker"),
  function(object, ...) align.HeatmapMaker(object, ...)
)

# %%
