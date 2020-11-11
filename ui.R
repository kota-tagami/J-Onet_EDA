ui <- navbarPage(
  
  ## App title
  title = "Exploratory Data Analysis for Japanese O*net",
  header = reference,
  footer = span(app_ver, style = "color:#586c7e"), 
  theme = shinytheme("cerulean"),
  
  ## Scatterplot
  tabPanel("Scatter Plot", scatterplotUI("sp01")),
  
  ## Principal Componet Analysis
  tabPanel("Principal Componet Analysis", pcaUI("pca01")),
  
  ## Exploratory Factor Analysis
  navbarMenu(
    "Exploratory Factor Analysis",
    tabPanel("Before EFA", efaBeforeUI("efaBefore01")),
    tabPanel("Main Results")
  ),

  ## Description
  tabPanel("README", includeMarkdown("README.md"))
  
)
