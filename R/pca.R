##++++++++++++##
#### Helper ####
##++++++++++++##
# js <- '
# $(document).on("shiny:value", function(e){
#   if(e.name === "graph"){
#     setTimeout(function(){
#       $("#graph").prev().children()[1].style.display = "none";
#     }, 0);
#   }
# });
# '

##+++++++++++++++##
#### UI module ####
##+++++++++++++++##

checkboxInputTabUI <- function(id, var_group, i){
  tagList(
    p(
      checkboxInput(NS(id, paste0("all", as.character(i))), "すべて")
    ),
    p(
      checkboxGroupInput(
        NS(id, paste0("item", as.character(i))),
        NULL,
        width = "100%",
        inline = T,
        choices = varslist %>% 
          filter(type == var_group) %>% 
          distinct(label) %>% 
          pull(label),
        selected = NULL
      )
    )
  )
}

varsInputUI <- function(id) {
  tagList(
    wellPanel(
      tabsetPanel(
        type = "pills",
        tabPanel(
          "職業興味",
          checkboxInputTabUI(id, "職業興味", 1),
        ),
        tabPanel(
          "仕事価値観",
          checkboxInputTabUI(id, "仕事価値観", 2)
        ),
        tabPanel(
          "スキル",
          checkboxInputTabUI(id, "スキル", 3)
        ),
        tabPanel(
          "知識",
          checkboxInputTabUI(id, "知識", 4)
        ),
        tabPanel(
          "仕事の性質",
          checkboxInputTabUI(id, "仕事の性質", 5)
        ),
        tabPanel(
          "教育と訓練",
          checkboxInputTabUI(id, "教育と訓練", 6)
        )
      )
    ),
    wellPanel(
      actionButton(NS(id, "reset"), "Reset inputs", width = "100%")
    )
  )
}

confirmShowUI <- function(id) {
  tagList(
    wellPanel(
      p("Following variables are selected"),
      verbatimTextOutput(NS(id, "item1")),
      verbatimTextOutput(NS(id, "item2")),
      verbatimTextOutput(NS(id, "item3")),
      verbatimTextOutput(NS(id, "item4")),
      verbatimTextOutput(NS(id, "item5")),
      verbatimTextOutput(NS(id, "item6"))
    ),
    wellPanel(
      actionButton(NS(id, "show"), "Show results", width = "100%")
    )
  )
}

inputUI <- function(id) {
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(width = 5, varsInputUI(id)), 
        mainPanel(width = 7, confirmShowUI(id))
      )
    )
  )
}

outputUI <- function(id) {
  tagList(fluidPage(
    navlistPanel(
      well = T,
      widths = c(2, 10),
      tabPanel(
        "Principal Components", 
        h3("Contribution to variance"),
        plotlyOutput(NS(id, "components"), width = "80%", height = "auto")
      ),
      tabPanel(
        "Variables", 
        h3("Contribution of variables"),
        plotlyOutput(NS(id, "variables"), width = "80%", height = "700px")
      ),
      tabPanel(
        "Occupations", 
        h3("Contribution of individual occupations"),
        plotlyOutput(NS(id, "individuals"), width = "80%", height = "700px")
      ),
      tabPanel(
        "Data Table", 
        h3("Data Table"),
        dataTableOutput(NS(id, "datatable"))
      )
    )
  ))
}

pcaUI <- function(id){
  tagList(fluidPage(
    # tags$head(
    #   tags$script(HTML(js))
    # ),
    titlePanel("Principal Component Analysis"),
    tabsetPanel(
      id = NS(id, "topTabset"),
      type = "pills",
      tabPanel("Input", inputUI(id)),
      tabPanel("Output", value = "outputTab", outputUI(id))
    )
  ))
}


##+++++++++++++++++++##
#### Server module ####
##+++++++++++++++++++##

pcaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    update_checkbox_vars <- function(triger, input_id, var_group) {
      observeEvent(triger(), {
        if(isFALSE(triger())) {
          selected_vars <- character(0)
        } else if(isTRUE(triger())) {
          selected_vars <- 
            varslist %>% 
            filter(type == var_group) %>% 
            distinct(label) %>% 
            pull(label)
        }
        updateCheckboxGroupInput(session, input_id, selected = selected_vars)
      })
    }
    
    update_checkbox_vars(reactive(input$all1), "item1", "職業興味")
    update_checkbox_vars(reactive(input$all2), "item2", "仕事価値観")
    update_checkbox_vars(reactive(input$all3), "item3", "スキル")
    update_checkbox_vars(reactive(input$all4), "item4", "知識")
    update_checkbox_vars(reactive(input$all5), "item5", "仕事の性質")
    update_checkbox_vars(reactive(input$all6), "item6", "教育と訓練")
    
    observeEvent(input$reset, {
      lapply(1:6, function(i) {
        updateCheckboxInput(session, paste0("all", as.character(i)), value = F)
      })
    })
    
    confirm_input <- function(input, label) {
      str_c(input(), collapse = ", ") %>% 
        str_c(label, ., sep = ": ")
    }
    
    output$item1 <- renderText(confirm_input(reactive(input$item1), "職業興味"))
    output$item2 <- renderText(confirm_input(reactive(input$item2), "仕事価値観"))
    output$item3 <- renderText(confirm_input(reactive(input$item3), "スキル"))
    output$item4 <- renderText(confirm_input(reactive(input$item4), "知識"))
    output$item5 <- renderText(confirm_input(reactive(input$item5), "仕事の性質"))
    output$item6 <- renderText(confirm_input(reactive(input$item6), "教育と訓練"))
    
    observeEvent(input$show, {
      updateTabsetPanel(session, "topTabset", selected = "outputTab")
    })
    
    
    dat <- eventReactive(input$show, {
      vars <- c(
        input$item1, input$item2, input$item3, 
        input$item4, input$item5, input$item6
      )
      onet_score %>% 
        filter(label %in% vars) %>% 
        select(occ_name, label, value) %>% 
        pivot_wider(names_from = label, values_from = value) %>% 
        column_to_rownames(var = "occ_name") %>% 
        scale()
    })
    
    result <- reactive({
      prcomp(dat(), scale. = F)
      # PCA(dat(), scale.unit = T, ncp = 20, graph = F)
    })
    
    
    output$datatable <- renderDataTable({
      vars_name <- 
        dat() %>% 
        attr("dimnames") %>% 
        pluck(2)
      
      dat() %>% 
        datatable(
          filter = "top",
          extensions = 'FixedColumns',
          options = list(
            lengthMenu = c(10, 20, 30, 40, 50), 
            pageLength = 10,
            autoWidth = F,
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 1)
          )
        ) %>% 
        formatRound(
          columns = vars_name,
          digits = 3
        )
    })

    
    output$components <- renderPlotly({
      gg <- 
        result() %>% 
        broom::tidy(matrix = "eigenvalues") %>% 
        ggplot(aes(x = PC, y = percent)) +
        geom_line() +
        geom_point() +
        scale_x_continuous(breaks = seq(-100, 100, 1)) +
        coord_cartesian(xlim = c(1, NA)) +
        labs(x = "Principal Component", y = "%")
      
      gg %>% 
        ggplotly(
          tooltip = c("x", "y")
        ) %>% 
        withr::with_options(
          list(digits = 2, nsmall = 2),
          .
        ) %>% 
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 700,
            height = 600
          )
        )
      
    })
    
    
    output$variables <- renderPlotly({
      gg <- 
        result() %>% 
        broom::tidy(matrix = "rotation") %>% 
        pivot_wider(
          names_from = "PC", 
          names_prefix = "PC", 
          values_from = "value"
        ) %>% 
        left_join(varslist %>% select(type, label), by = c("column" = "label")) %>% 
        # plotly::highlight_key(~column, "Select a variable") %>% 
        ggplot() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
        geom_segment(
          aes(x = 0, y = 0, xend = PC1, yend = PC2, color = type, group = column)
        ) +
        geom_point(
          aes(x = PC1, y = PC2, color = type, group = column)
        ) +
        labs(
          x = "PC1", y = "PC2", color = NULL
        ) 
      
      gg %>% 
        ggplotly(
          tooltip = c("x", "y", "type", "vars")
        ) %>% 
        # highlight(
        #   on = "plotly_click",
        #   off = "plotly_doubleclick",
        #   selectize = T,
        #   persistent = TRUE,
        #   selected = attrs_selected(
        #     showlegend = F,
        #     mode = "lines+markers",
        #     marker = list(symbol = "x")
        #   )
        # ) %>% 
        layout(
          legend = list(
            orientation = "h",
            x = 0.5,
            y = 100
          )
        ) %>% 
        withr::with_options(
          list(digits = 2, nsmall = 2),
          .
        ) %>% 
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 700,
            height = 600
          )
        )
      
    })
    
    
    output$individuals <- renderPlotly({
      gg <- 
        result() %>% 
        broom::augment() %>% 
        plotly::highlight_key(~.rownames, "Select a occupation") %>%
        ggplot() +
        geom_vline(xintercept = 0, linetype = "dotted", color = "grey60") +
        geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
        geom_point(
          aes(x = .fittedPC1, y = .fittedPC2, text = .rownames)
        ) +
        labs(
          x = "PC1", y = "PC2", color = NULL
        ) 
      
      
      gg %>% 
        ggplotly(
          tooltip = c("x", "y", "text")
        ) %>% 
        highlight(
          on = "plotly_click",
          off = "plotly_doubleclick",
          selectize = T,
          persistent = T,
          selected = attrs_selected(
            showlegend = F,
            mode = "lines+markers",
            marker = list(symbol = "x")
          )
        ) %>%
        withr::with_options(
          list(digits = 1),
          .
        ) %>% 
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 700,
            height = 600
          )
        )
      
    })
    
    
  })
}


##++++++++++++++++##
#### Module App ####
##++++++++++++++++##

pcaApp <- function(){
  ui <- pcaUI("pca01")
  server <- function(session, input, output){
    pcaServer("pca01")
  }
  shinyApp(ui, server)
}


# pcaApp()
