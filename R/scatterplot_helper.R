select_group <- function(id) {
  selectInput(
    paste0("sp", id, "Group"),
    "Select a variable group:",
    choices = vars_group,
    selected = default_vars_group
  )
}

select_variable <- function(id) {
  radioButtons(
    paste0("sp", id, "Var"),
    "Select a variable:",
    choices = default_vars_name
  )
}

