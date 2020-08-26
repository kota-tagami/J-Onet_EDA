ui <- navbarPage(
  
  ## App title
  title = "Exploratory Data Analysis for Japanese O*net",
  
  # header = "This is a header",

  footer = span("version: 0.0.0.9000", style = "color:#586c7e"), 
  
  theme = shinytheme("cerulean"),
  
  ## Analysis 1
  ## Scatterplot
  tabPanel(
    "Scatter Plot", 
    fluidPage(
      titlePanel("Scatter Plot"),
      sidebarLayout(
        sidebarPanel(
          h4("Parameters"),
          
          ## Variable selection
          wellPanel(
            h5("X variable"),
            selectInput(
              "spXGroup",
              "Select a variable group:",
              choices = vars_group,
              selected = default_vars_group
            ),
            selectInput(
              "spXVar",
              "Select a variable:",
              choices = default_vars_name,
              selected = head(default_vars_name, 1)
            )
          ), 
          wellPanel(
            h5("Y variable"),
            selectInput(
              "spYGroup",
              "Select a variable group:",
              choices = vars_group,
              selected = default_vars_group
            ),
            selectInput(
              "spYVar",
              "Select a variable:",
              choices = default_vars_name,
              selected = head(default_vars_name, 1)
            )
          ),
          
          ## Smoothing span
          wellPanel(
            h5("Nonparametric Smoothing"),
            sliderInput("spSpan", "Span:",
                        min = 0.1, 
                        max = 0.9, 
                        value = 0.65,
                        step = 0.01),
            helpText("Local Polynomial Regression Fitting is used for the nonparametric smoothing")
          )
        ),
        mainPanel(
          wellPanel(
            ## triger
            actionButton("spBottom", "Show", width = "100%")
          ),
          
          tabsetPanel(
            tabPanel("Plot", plotOutput("spPlot")),
            tabPanel("Data", dataTableOutput("spData"))
          )
        )
      )
    )
  ),
  
  
  ## Analysis 2
  tabPanel("Analysis 2"),
  
  
  ## Analysis 3
  tabPanel("Analysis 3"),
  
  
  ## Description
  tabPanel(
    "README",
    includeMarkdown("README.md")
  )
  
)
