source("libraries.R")

##===========##
## Reference ##
##===========##
app_ver <- str_c("version: ", "0.0.0.9003")

onet_ver <- "ver.1.8"
dl_date <- "2020年8月20日"

reference <- 
  str_c(
    "出典：",
    "独立行政法人労働政策研究・研修機構（JILPT）作成",
    "「職業情報データベース 簡易版数値系ダウンロードデータ",
    " ",
    onet_ver,
    "」",
    "\n",
    "職業情報提供サイト（日本版O-NET）より",
    dl_date,
    "にダウンロード",
    "\n",
    "（https://shigoto.mhlw.go.jp/User/download）を加工して作成"
  )


##==============##
## ggplot theme ##
##==============##
font_add_google("Noto Sans JP", "noto sans jp")
showtext_auto()
theme_set(theme_few(base_size = 20, base_family = "noto sans jp"))

##=============##
## Data import ##
##=============##
onet_score <-
  "Data/onet_score.csv" %>% 
  read_csv() %>% 
  mutate_at(vars(name, type, label), ~ fct_inorder(.)) %>% 
  rename("occ_id" = 1, "occ_name" = 2)

## Variable list
varslist <- 
  onet_score %>% 
  distinct(name, type, label) %>% 
  mutate(
    type_en = type %>% 
      fct_recode(!!!c(
        "Occupational Interest" = "職業興味",
        "Job Value" = "仕事価値観",
        "Skill" = "スキル",
        "Knowledge" = "知識",
        "Job Quality" = "仕事の性質",
        "Education and Training" = "教育と訓練"
      )),
  ) %>% 
  group_by(type) %>% 
  mutate(
    item_id = type_en %>% 
      abbreviate(2L) %>% 
      str_c(., row_number(), sep = "-") %>% 
      fct_inorder(),
  ) %>% 
  ungroup()


## Variable group
vars_group <- 
  varslist %>% 
  distinct(type) %>% 
  pull(type) %>% 
  as.character()

default_vars_group <- 
  vars_group %>% 
  head(1)

##===============##
## Global for UI ##
##===============##
default_vars_name <- 
  varslist %>% 
  filter(type == default_vars_group) %>% 
  pull(label) %>% 
  as.character() 


##================##
## Helper Modules ##
##================##
source("R/helper_input_pca_efa.R")
caution_msg <- 
  "* The contents are subject to change without notice."
