server <- function(input, output, session) {
  ## Scatterplot
  scatterplotServer("sp01")
  ## Principal Componet Analysis
  pcaServer("pca01")
  ## Before Exploratory Factor Analysis
  efaBeforeServer("efaBefore01")
}
