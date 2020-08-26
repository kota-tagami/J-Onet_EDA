update_sp_var <- function(session, sp_group, sp_var){
  vars_name <- 
    varslist %>% 
    filter(type == sp_group) %>% 
    distinct(label) %>% 
    pull(label) %>% 
    as.character()
  updateSelectInput(session, sp_var, choices = vars_name)
}

server <- function(input, output, session) {
  ####==============####
  #### Input update ####
  ####==============####

  ##--------------##
  ## Scatter plot ##
  ##--------------##

  ## X variable
  observeEvent(input$spXGroup, 
               update_sp_var(session, input$spXGroup, "spXVar"))
  ## Y variable
  observeEvent(input$spYGroup, 
               update_sp_var(session, input$spYGroup, "spYVar"))

  
  ####=================####
  #### Reactive object ####
  ####=================####
  
  ##--------------##
  ## Scatter plot ##
  ##--------------##
  
  sp_x_var <- reactive(input$spXVar)
  sp_y_var <- reactive(input$spYVar)
  
  sp_data <- eventReactive(input$spBottom, {
    sp_vars <- c(sp_x_var(), sp_y_var())
    rtn_obj <- 
      onet_score %>% 
      filter(label %in% sp_vars) %>% 
      select(occ_name, label, value) %>% 
      pivot_wider(names_from = label, values_from = value) %>% 
      relocate(sp_vars, .after = occ_name) %>% 
      rename("職業" = occ_name) 
    ## The return has only three vars
    ## occ, x, and y (the order is critical)
    return(rtn_obj)
  })
  sp_span <- eventReactive(input$spBottom, input$spSpan)

  ##========##
  ## Output ##
  ##========##
  
  ## Data table
  output$spData <- renderDataTable(sp_data())
  
  ## Scatterplot 
  output$spPlot <- renderPlot({
    x_var <- sp_data() %>% select(2) %>% names()
    y_var <- sp_data() %>% select(3) %>% names()
    x <- sp_data() %>% pull(2)
    y <- sp_data() %>% pull(3)
    x_mean <- mean(x, na.rm = T)
    y_mean <- mean(y, na.rm = T)
    
    r <- cor(x, y, use = "complete.obs")
    
    rtn_obj <- 
      sp_data() %>% 
      rename("x" = 2, "y" = 3) %>% 
      ggplot(aes(x, y)) +
      geom_vline(xintercept = x_mean, linetype = "dotted", color = "grey20") +
      geom_hline(yintercept = y_mean, linetype = "dotted", color = "grey20") +
      geom_point(
        size = 1.5
      ) +
      geom_smooth(
        size = 1,
        method = "loess", 
        formula = y ~ x,
        se = FALSE, 
        span = sp_span(), 
        alpha = 0.8,
        color = "#83c3e8"
      ) +
      geom_smooth(
        size = 1.5,
        method = "lm", 
        formula = y ~ x,
        se = FALSE, 
        alpha = 0.8,
        color = "#cd4e2d"
      ) +
      theme(
        plot.title = element_text(color = "grey20", size = 18),
        plot.caption = element_text(color = "grey20", size = 10),
        axis.title = element_text(color = "grey20", size = 18),
        axis.text = element_text(color = "grey20", size = 16),
      ) +
      labs(
        title = str_c("Pearson's R: ", sprintf("%1.3f", r)),
        x = x_var,
        y = y_var
      )
    
    return(rtn_obj)
  }, width = 600, height = 600)

  ## Parameters check section
  output$spXVar <- 
    renderText(paste0("X variable is ", input$spXVar))
  output$spYVar <- 
    renderText(paste0("Y variable is ", input$spYVar))
  output$spSpan <- 
    renderText(paste0("Smoothing span is ", input$spSpan))
  
}
