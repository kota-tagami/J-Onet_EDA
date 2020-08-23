server <- function(input, output, session) {
  
  ## Scatterplot X variable
  observeEvent(input$spXGroup, {
    vars_name <- make_vars_name(input$spXGroup)
    updateRadioButtons(session, "spXVar", choices = vars_name)
  })
  x_var <- reactive(input$spXVar)
  output$spXVar <- 
    renderText(paste0("X variable is ", input$spXVar))

  ## Scatterplot Y variable
  observeEvent(input$spYGroup, {
    vars_name <- make_vars_name(input$spYGroup)
    updateRadioButtons(session, "spYVar", choices = vars_name)
  })
  y_var <- reactive(input$spYVar)
  output$spYVar <- 
    renderText(paste0("Y variable is ", input$spYVar))

  
  ## Scatterplot data
  sp_data <- eventReactive(input$spBottom, {
    onet_score %>% 
      filter(label %in% c(x_var(), y_var())) %>% 
      select("職業" = occ_name, label, value) %>% 
      pivot_wider(names_from = label, values_from = value)
  })
  output$spData <- renderDataTable(sp_data())
  
  
  ## Scatterplot 
  sp_plot <- eventReactive(sp_data(), {
    x_mean <- mean(sp_data()[[x_var()]], na.rm = T)
    y_mean <- mean(sp_data()[[y_var()]], na.rm = T)
    
    sp_data() %>% 
      ggplot(aes(.data[[x_var()]], .data[[y_var()]])) +
      geom_vline(xintercept = x_mean, linetype = "dotted", color = "grey20") +
      geom_hline(yintercept = y_mean, linetype = "dotted", color = "grey20") +
      geom_point(
        size = 1.5
      ) +
      geom_smooth(
        size = 1,
        linetype = "dashed",
        method = "loess", 
        formula = y ~ x,
        se = FALSE, 
        span = input$span, 
        color = "#83c3e8"
      ) +
      geom_smooth(
        size = 1.5,
        method = "lm", 
        formula = y ~ x,
        se = FALSE, 
        color = "#cd4e2d"
      ) +
      labs(
        title = ""
      )
  })
  output$spPlot <- renderPlot(sp_plot())
  output$spSpan <- 
    renderText(paste0("Smoothing span is ", input$spSpan))
  
}
