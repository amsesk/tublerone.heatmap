repo = file.path(Sys.getenv("HOME"), "dev/tublerone.heatmap")

for (f in list.files(file.path(repo, "R"), full.names=TRUE, pattern = "[.]R$")) {
  source(f)
}
