make_vars_name <- function(.type){
  varslist %>% 
    filter(type == .type) %>% 
    distinct(label) %>% 
    pull(label) %>% 
    as.character()
}