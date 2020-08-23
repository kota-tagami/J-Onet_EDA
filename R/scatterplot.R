source("R/scatterplot_helper.R")

scatterplotUI <- fluidPage(
  titlePanel("Scatter Plot"),
  sidebarLayout(
    ## Input part
    sidebarPanel(
      h4("Parameters"),
      wellPanel(em(textOutput("spXVar")),
                em(textOutput("spYVar")),
                em(textOutput("spSpan")),
                actionButton("spBottom", "Plot")),
      wellPanel(h5("X variable"),
                select_group("X"), 
                select_variable("X")), 
      wellPanel(h5("Y variable"),
                select_group("Y"), 
                select_variable("Y")),
      wellPanel(h5("Nonparametric Smoothing"),
                sliderInput("spSpan", "Span:",
                            min = 0.1, 
                            max = 0.9, 
                            value = 0.65,
                            step = 0.01))
    ),
    
    ## Output part
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("spPlot")),
        tabPanel("Data", dataTableOutput("spData"))
      )
    )
    
  )
)


