##+++++++++++++++##
#### UI module ####
##+++++++++++++++##
outputEfaMainUI <- function(id) {
  tagList(fluidPage(
    navlistPanel(
      well = T,
      widths = c(2, 10),
      tabPanel(
        "Factor loading", 
        h3("Factor loading"),
        tabsetPanel(
          tabPanel(
            "Plot",
            plotOutput(NS(id, "graph_loadings"), 
                         width = "1000px", height = "800px")
          ),
          tabPanel(
            "Table",
            dataTableOutput(NS(id, "data_loadings_table"))
          )
        )
      ),
      tabPanel(
        "Factor score", 
        h3("Factor score"),
        tabsetPanel(
          tabPanel(
            "Table",
            dataTableOutput(NS(id, "factor_score_table"))
          )
        )
      )
    )
  ))
}

efaMainUI <- function(id){
  tagList(fluidPage(
    add_busy_bar(color = "#FF0000"),
    titlePanel("Exploratory Factor Analysis: "),
    h2("Main results"),
    helpText(caution_msg),
    tabsetPanel(
      id = NS(id, "topTabset"),
      type = "pills",
      tabPanel(
        "Input", 
        wellPanel(
          h4("Options about exploratory factor analysis"),
          fluidRow(
            column(
              width = 3,
              numericInput(NS(id, "nfactors"), 
                           "Number of factors: ", 
                           3, min = 1, max = 100)
            ),
            column(
              width = 3,
              selectInput(NS(id, "estimation"), 
                          "Estimation: ", 
                          c("minres"),
                          selected = "minres",
                          selectize = F)
            ),
            column(
              width = 3,
              selectInput(NS(id, "rotation"), 
                          "Rotation: ", 
                          c("oblimin"),
                          selected = "oblimin",
                          selectize = F)
            ),
            column(
              width = 3,
              numericInput(NS(id, "cutoff"), 
                           "Cutoff point: ", 
                           0.3, min = 0, max = 1, step = 0.05)
            )
          )
        ),
        h4("Variable selection"),
        inputPcaEfaUI(id)
      ),
      tabPanel("Output", value = "outputTab", outputEfaMainUI(id))
    )
  ))
}


##+++++++++++++++++++##
#### Server module ####
##+++++++++++++++++++##
efaMainServer <- function(id){
  moduleServer(id, function(input, output, session){
    
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
    
    
    ## EFA
    
    data_efa <- eventReactive(input$show, {
      vars <- c(
        input$item1, input$item2, input$item3, 
        input$item4, input$item5, input$item6
      )
      onet_score %>% 
        filter(label %in% vars) %>% 
        left_join(
          .,
          varslist %>% 
            select(label, item_id),
          by = "label"
        ) %>%  
        select(occ_id, item_id, value) %>% 
        pivot_wider(names_from = item_id, values_from = value) %>% 
        column_to_rownames(var = "occ_id")
    })
    
    
    n_of_factors <- reactive(input$nfactors)
    method_estimation <- reactive(input$estimation)
    method_rotation <- reactive(input$rotation)
    cutoff_point <- reactive(as.numeric(input$cutoff))
    
    
    res_efa <- eventReactive(input$show, {
      psych::fa(
        data_efa(), nfactors = n_of_factors(), 
        fm = method_estimation(), rotate = method_rotation(),
        use = "complete.obs"
      )
    })

    data_loadings <- eventReactive(input$show, {
      psych::fa.sort(res_efa())$loadings %>% 
        unclass() %>% 
        as_tibble(rownames = "item_id") %>% 
        rename_with(~ str_replace_all(., "MR", "F")) 
    })
    
    data_loadings_table <- eventReactive(input$show, {
      data_loadings() %>% 
        left_join(
          .,
          varslist %>% select(type, label, item_id),
          by = "item_id"
        ) %>% 
        relocate(type, label, item_id) %>% 
        arrange(type, label) %>% 
        mutate(
          item_id = item_id %>% fct_inorder()
        )
    })
    
    output$data_loadings_table <- renderDataTable({
      data_loadings_table() %>%
        DT::datatable(
          rownames = F,
          filter = "top",
          extensions = c("Buttons"),
          options = list(
            dom = "lBfrtip",
            buttons = c("csv", "excel")
          )
        ) %>%
        DT::formatRound(columns = -c(1:3))
    })
    
    graph_loadings <- eventReactive(input$show, {
      set.seed(1)
      
      tblgraph_loadings <-
        data_loadings() %>% 
        pivot_longer(
          -c(item_id), 
          names_to = "factor"
        ) %>% 
        mutate(value = if_else(
          abs(value) <= cutoff_point(), NA_real_, value
        )) %>% 
        drop_na() %>% 
        relocate(factor, item_id, value) %>% 
        as_tbl_graph() %>% 
        activate(nodes) %>% 
        left_join(
          .,
          varslist %>% select(type, label, item_id),
          by = c("name" = "item_id")
        ) %>% 
        mutate(
          factor = if_else(name %>% str_detect("F"), 1, 0),
          label = if_else(factor == 1, name, label %>% as.character())
        )
      
      graph_loadings <- 
        tblgraph_loadings %>% 
        ggraph(layout = "fr") +
        geom_edge_link(
          aes(color = value),
          width = 1.5,
          arrow = arrow(length = unit(3, "mm")),
          start_cap = circle(8, "mm"),
          end_cap = circle(8, "mm")
        ) +
        geom_node_text(
          aes(label = label), 
          size = 4,
          check_overlap = F
        ) +
        geom_node_label(
          data = . %>% 
            filter(factor == 1),
          aes(label = label), 
          size = 6
        ) +
        scale_edge_color_gradient2(
          guide = guide_edge_colorbar(
            title = "Factor loadings", 
            nbin = 300,
            barwidth = 20
          )
        ) +
        theme_graph(border = F, base_family = "noto sans jp") +
        labs(
          caption = str_c("Note: The cutoff point is", cutoff_point(), sep = " ")
        ) +
        theme(
          legend.position = "top"
        ) 
      
      graph_loadings
    })
    
    output$graph_loadings <- renderPlot(graph_loadings())
    
    factor_score_table <- eventReactive(input$show, {
      factor_score_table <- 
        psych::factor.scores(data_efa(), res_efa()) %>% 
        pluck("scores") %>% 
        as_tibble(rownames = "occ_id") %>% 
        rename_with(~ str_replace_all(., "MR", "F")) %>% 
        mutate(
          occ_id = occ_id %>% as.numeric(),
        ) %>% 
        left_join(
          onet_score %>% distinct(occ_id, occ_name), 
          .,
          by = "occ_id"
        )
      factor_score_table
    })
    
    
    output$factor_score_table <- renderDataTable({
      factor_score_table() %>% 
        DT::datatable(
        rownames = F,
        filter = "top",
        extensions = c("Buttons"),
        options = list(
          dom = "lBfrtip",
          buttons = c("csv", "excel")
        )
      ) %>%
        DT::formatRound(columns = -c(1:2))
      
    })
  })
}



##++++++++++++++++##
#### Module App ####
##++++++++++++++++##

efaMainApp <- function(){
  ui <- efaMainUI("efaMain01")
  server <- function(session, input, output){
    efaMainServer("efaMain01")
  }
  shinyApp(ui, server)
}


# efaMainApp()
