##++++++++++++##
#### Helper ####
##++++++++++++##

select_vargroup <- function(id, id_prefix){
  selectInput(
    NS(id, paste0(id_prefix, "Group")),
    "Variable group:",
    choices = vars_group,
    selected = default_vars_group
  )
}
select_varname <- function(id, id_prefix){
  selectInput(
    NS(id, paste0(id_prefix, "Var")),
    "Variable:",
    choices = default_vars_name,
    selected = head(default_vars_name, 1)
  )
}

help_span <- 
  "Local Polynomial Regression Fitting is used for the nonparametric smoothing"



##+++++++++++++++##
#### UI module ####
##+++++++++++++++##

scatterplotUI <- function(id){
  tagList(
    fluidPage(
      add_busy_bar(color = "#FF0000"),
      titlePanel("Scatter Plot"),
      sidebarLayout(
        
        ## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        ## Input section ##
        ##===============##
        sidebarPanel(
          h3("Parameters"),
          wellPanel(
            h4("X variable"),
            select_vargroup(id, "x"),
            select_varname(id, "x")
          ), 
          wellPanel(
            h4("Y variable"),
            select_vargroup(id, "y"),
            select_varname(id, "y")
          ),
          wellPanel(
            h4("Nonparametric Smoothing"),
            sliderInput(
              NS(id, "span"), "Span:",
              min = 0.1, 
              max = 0.9, 
              value = 0.65,
              step = 0.05
            ),
            helpText(help_span)
          )
        ),
        ## <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        
        ## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        ## Output section ##
        ##================##
        mainPanel(
          wellPanel(
            actionButton(NS(id, "button"), "Show results", width = "100%")
          ),
          wellPanel(
            tabsetPanel(
              tabPanel("Plot", plotlyOutput(NS(id, "plot"), width = "700px", height = "700px")),
              tabPanel("Data", dataTableOutput(NS(id, "table")))
            )
          )
        )
        ## <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        
      )
    )
  )
}


##+++++++++++++++++++##
#### Server module ####
##+++++++++++++++++++##

scatterplotServer <- function(id) {
  moduleServer(id, function(input, output, session){
    ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ## Input update section ##
    ##======================##
    x_var_group <- reactive(input$xGroup)
    y_var_group <- reactive(input$yGroup)
    
    get_vars_name <- function(group){
      vars_name <- 
        varslist %>% 
        filter(type == group) %>% 
        distinct(label) %>% 
        pull(label) %>% 
        as.character()
      vars_name
    }
    
    ## X variable
    observeEvent(x_var_group(), {
      vars_name <- get_vars_name(x_var_group())
      updateSelectInput(session, "xVar", choices = vars_name)
    })
    ## Y variable
    observeEvent(y_var_group(), {
      vars_name <- get_vars_name(y_var_group())
      updateSelectInput(session, "yVar", choices = vars_name)
    })
    ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ## Reactive object section ##
    ##=========================##
    x_var <- eventReactive(input$button, input$xVar)
    y_var <- eventReactive(input$button, input$yVar)
    
    data <- eventReactive(input$button, {
      onet_data <- 
        onet_score %>% 
        filter(label %in% c(x_var(), y_var())) %>% 
        select(occ_name, label, value) %>% 
        pivot_wider(names_from = label, values_from = value) %>% 
        relocate(all_of(c(x_var(), y_var())), .after = occ_name) %>% 
        rename("職業" = occ_name) 
      ## The return has only three vars
      ## occ, x, and y (the order is critical)
      onet_data
    })
    
    span <- eventReactive(input$button, input$span)
    ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ## Output section ##
    ##================##
    output$table <- renderDataTable({
      validate(
        need(
          input$xVar != input$yVar,
          message = "Choose different variables for the x and y axis"
        )
      )
      data() %>% 
        datatable(
          filter = "top",
          options = list(
            lengthMenu = c(10, 20, 30, 40, 50), 
            pageLength = 10,
            autoWidth = TRUE
          )
        )
    })
    
    output$plot <- renderPlotly({
      validate(
        need(
          input$xVar != input$yVar,
          message = "Choose different variables for the x and y axis"
        )
      )

      dt <- data()
      x <- dt %>% pull(.data[[x_var()]])
      y <- dt %>% pull(.data[[y_var()]])
      x_mean <- mean(x, na.rm = T)
      y_mean <- mean(y, na.rm = T)

      r <- cor(x, y, use = "complete.obs")

      plot_data <-
        dt %>%
        rename("x" = 2, "y" = 3)

      gg <-
        ggplot(plot_data) +
        geom_vline(xintercept = x_mean, linetype = "dotted", color = "grey60") +
        geom_hline(yintercept = y_mean, linetype = "dotted", color = "grey60") +
        geom_smooth(
          aes(x, y),
          size = 0.5,
          method = "loess",
          formula = y ~ x,
          se = FALSE,
          span = span(),
          alpha = 0.8,
          color = "#83c3e8"
        ) +
        geom_smooth(
          aes(x, y),
          size = 0.8,
          method = "lm",
          formula = y ~ x,
          se = FALSE,
          color = "#cd4e2d"
        ) +
        suppressWarnings(geom_point(
          aes(x, y, text = `職業`, group = `職業`),
          size = 1.5,
          color = "grey40"
        )) +
        theme(
          plot.title = element_text(color = "grey20", size = 18),
          plot.caption = element_text(color = "grey20", size = 10),
          axis.title = element_text(color = "grey20", size = 18),
          axis.text = element_text(color = "grey20", size = 16),
          axis.ticks.x = element_line(color = "grey60"),
          axis.ticks.y = element_line(color = "grey60"),
          panel.border = element_blank(),
        ) +
        labs(
          title = str_c("Pearson's R: ", sprintf("%1.3f", r)),
          x = x_var(),
          y = y_var()
        )

      plot <-
        gg %>%
        ggplotly(tooltip = c("text", "x", "y")) %>%
        plotly::style(hoverinfo = "none", traces = 1:4) %>%
        withr::with_options(
          list(digits = 2, nsmall = 2),
          .
        )

      plot
    })
    ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
  })
}


##++++++++++++++++##
#### Module App ####
##++++++++++++++++##

scatterplotApp <- function(){
  ui <- scatterplotUI("sp00")
  server <- function(session, input, output){
    scatterplotServer("sp00")
  }
  shinyApp(ui, server)
}

