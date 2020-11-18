##+++++++++++++++##
#### UI module ####
##+++++++++++++++##
outputEfaBeforeUI <- function(id) {
  tagList(fluidPage(
    navlistPanel(
      well = T,
      widths = c(2, 10),
      tabPanel(
        "KMO", 
        h3("The Kaiser, Meyer, Olkin Measure of Sampling Adequacy"),
        plotlyOutput(NS(id, "kmo"), width = "80%", height = "700px")
      ),
      tabPanel(
        "Scree Plot", 
        h3("Scree Plot"),
        plotlyOutput(NS(id, "scree"), width = "80%", height = "700px")
      ),
      tabPanel(
        "VSS", 
        h3("Very Simple Structure"),
        verbatimTextOutput(NS(id, "vss"))
      )
    )
  ))
}

efaBeforeUI <- function(id){
  tagList(fluidPage(
    add_busy_bar(color = "#FF0000"),
    titlePanel("Before Exploratory Factor Analysis: "),
    h2("The decision of the number of factors"),
    helpText(caution_msg),
    tabsetPanel(
      id = NS(id, "topTabset"),
      type = "pills",
      tabPanel("Input", inputPcaEfaUI(id)),
      tabPanel("Output", value = "outputTab", outputEfaBeforeUI(id))
    )
  ))
}


##+++++++++++++++++++##
#### Server module ####
##+++++++++++++++++++##
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

efaBeforeServer <- function(id){
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
    
    selected_items <- eventReactive(input$show, {
      vars <- c(
        input$item1, input$item2, input$item3, 
        input$item4, input$item5, input$item6
      )
      varslist %>% 
        filter(label %in% vars) %>% 
        distinct(item_id) %>%
        mutate(
          x_texts = if_else(
            row_number() %in% seq(2, n(), by = 2),
            item_id %>% as.character(), ""
          ),
          y_texts = if_else(
            row_number() %in% seq(1, n(), by = 2),
            item_id %>% as.character(), ""
          ),
        )
    })
    
    
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
    
    
    ## KMO
    res_kmo <- eventReactive(input$show, {
      psych::KMO(data_efa())
    })
    
    output$kmo <- renderPlotly({
      data_kmo <- 
        res_kmo()$MSAi %>% 
        enframe() %>% 
        left_join(
          .,
          varslist %>% 
            select(type, label, item_id),
          by = c("name" = "item_id")
        ) %>% 
        mutate(
          name = name %>% fct_inorder(),
          text = str_c(type, ": ", label),
        ) 
      
      ave_kmo <- res_kmo()$MSA
      x_labs <- selected_items()$x_texts
      title_kmo <- 
        str_c("Overall MSA is", sprintf("%.2f", ave_kmo), sep = " ")
      
      plot_kmo <- 
        data_kmo %>%
        plot_ly(
          x = ~ name
        ) %>% 
        add_trace(
          y = ~ value,
          text = ~ text,
          type = "bar",
          name = "Values"
        ) %>% 
        add_trace(
          y = ave_kmo, 
          type = "scatter", 
          mode = "line", 
          name = "Average"
        ) %>% 
        layout(
          title = list(
            text = title_kmo
          ),
          xaxis = list(
            title = "Variables",
            tickangle = 270
          ),
          yaxis = list(
            title = "MSA"
          )
        )

      plot_kmo
    })
    
    
    ## Screeplot
    res_scree <- eventReactive(input$show, {
      quiet(
        psych::fa.parallel(data_efa(), plot = F, fa = "fa", use = "complete.obs")
      )
    })
    
    output$scree <- renderPlotly({
      title_scree <-  str_c(
        "The parallel analysis suggests that the number of factors is",
        res_scree()$nfact, 
        sep = " "
      )
      
      data_scree <- 
        list(
          "FA Actual Data" = res_scree()$fa.values,
          "FA Simulated Data" = res_scree()$fa.sim,
          "FA Resampled Data" = res_scree()$fa.simr
        ) %>% 
        bind_rows() %>% 
        mutate(fa_n = row_number() %>% as.numeric()) %>% 
        pivot_longer(-fa_n)
      
      plot_scree <- 
        data_scree %>% 
        plot_ly(
          x = ~ fa_n
        ) %>% 
        add_trace(
          y = ~ value,
          name = ~ name,
          type = "scatter", 
          mode = "markers+lines"
        ) %>% 
        add_trace(
          x = ~ c(0, fa_n),
          y = 1, 
          type = "scatter", 
          mode = "linse",
          showlegend = F
        ) %>% 
        layout(
          title = list(
            text = title_scree
          ),
          xaxis = list(
            title = "The number of factors",
            tickmode = "linear",
            tick0 = 2,
            dtick = 2
          ),
          yaxis = list(
            title = "Eigen values"
          ),
          legend = list(x = 0.7, y = 0.95),
          margin = list(
            t = 50, b = 80, l = 80, r = 80
          )
        )
      
      plot_scree
    })
    
    
    ## VSS
    res_vss <- eventReactive(input$show, {
      psych::VSS(data_efa(), n = 8, plot = F)
    })
      
    output$vss <- renderPrint({
      print(res_vss())
    })
     
  })
}


##++++++++++++++++##
#### Module App ####
##++++++++++++++++##

efaBeforeApp <- function(){
  ui <- efaBeforeUI("efaBefore01")
  server <- function(session, input, output){
    efaBeforeServer("efaBefore01")
  }
  shinyApp(ui, server)
}


# efaBeforeApp()
