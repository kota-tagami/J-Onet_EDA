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

inputPcaEfaUI <- function(id) {
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(width = 5, varsInputUI(id)), 
        mainPanel(width = 7, confirmShowUI(id))
      )
    )
  )
}
