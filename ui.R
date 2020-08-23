ui <- navbarPage(
  ## App title
  "Exploratory Data Analysis for Japanese O*net",
  
  ## Scatterplot
  tabPanel("Scatter Plot", scatterplotUI),
  
  tabPanel("Analysis 2"),

  theme = shinytheme("cerulean")
)
