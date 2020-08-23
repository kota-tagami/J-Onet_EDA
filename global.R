## ggplot theme
font_add_google("Noto Sans JP", "noto sans jp")
showtext_auto()
theme_set(theme_few(base_size = 20, base_family = "noto sans jp"))

## Data
onet_score <-
  "Data/onet_score.csv" %>% 
  read_csv() %>% 
  mutate_at(vars(name, type, label), ~ fct_inorder(.)) %>% 
  rename("occ_id" = 1, "occ_name" = 2)

## Variable list
varslist <- 
  onet_score %>% 
  distinct(name, type, label)

## Variable group
vars_group <- 
  varslist %>% 
  distinct(type) %>% 
  pull(type) %>% 
  as.character()

## Default variable
default_vars_group <- head(vars_group, 1)
default_vars_name <- 
  varslist %>% 
  filter(type == default_vars_group) %>% 
  pull(label) %>% 
  as.character() 
