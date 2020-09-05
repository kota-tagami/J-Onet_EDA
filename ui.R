ui <- navbarPage(
  
  ## App title
  title = "Exploratory Data Analysis for Japanese O*net",
  header = reference,
  footer = span(app_ver, style = "color:#586c7e"), 
  theme = shinytheme("cerulean"),
  
  ## Scatterplot
  tabPanel("Scatter Plot", scatterplotUI("sp01")),
  
  ## Analysis 2
  tabPanel("Analysis 2"),
  
  ## Analysis 3
  tabPanel("Analysis 3"),
  
  ## Description
  tabPanel("README", includeMarkdown("README.md"))
  
)
