# %% Method for making legends for factor data
setGeneric("add_annotations", function(object, df, ...) standardGeneric("add_annotations"))
setMethod(
  "add_annotations",
  signature("HeatmapMaker", "data.frame"),
  function(object, df, ...) add_annotations.HeatmapMaker(object, df, ...)
)
# %% Method for making multiple legends
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

# %% Method for making multiple legends
setGeneric("pretty_draw", function(object, ...) standardGeneric("pretty_draw"))
setMethod(
  "pretty_draw",
  signature("HeatmapMaker"),
  function(object, ...) pretty_draw.HeatmapMaker(object, ...)
)

# %%

