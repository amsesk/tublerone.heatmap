# %%
library(ComplexHeatmap)


# %%

# Method for legend with continuous data
# setMethod(
#   "make_legend",
#   signature("HeatmapMaker", "numeric", "character", "numeric"),
#   function(object, values, title, fill) "Legend()"
# )

# Method for legend with categorical data
# %%
hmm <- HeatmapMaker()
hmm <- make_legend(hmm, as.factor(annot_df$subject), title = "Subject", fill = 1:4)
hmm@legends

annot_df[,1] = as.factor(annot_df[,1])
annot_df[,1]
annot_df[,2] = as.factor(annot_df[,2])
annot_df[,3] = as.factor(annot_df[,3])
class(annot_df[,1])  signature("HeatmapMaker", "factor", "character", "integer"),
sapply(annot_df, class)
make_legends(hmm, annot_df, titles = c("subject"="Subject", "tissue_type"="Tissue", "cancer_type"="Cancer Type"))

# %%

##############
# %%
# make_legends = function(maker, df, titles, fills, ...) {
#   UseMethod(generic = "make_legends", object = maker)
# }
