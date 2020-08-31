library(tidyverse)
library(readxl)

## Onetウェブサイトからダウンロードしたデータを読み込む
onet_score_00 <-
  "IPD_DL_numeric_1_8.xlsx" %>% 
  str_c("Data", ., sep = "/") %>% 
  read_excel(
    sheet = 1,
    col_names = T,
    .name_repair = "unique",
    skip  = 19
  ) %>% 
  select(
    - `20`,
    id_row = `...2`, 
    everything()
  ) %>% 
  mutate(
    ## バージョン8では西洋料理調理人（コック）のカッコが半角になっている
    IPD_02_01_001 = IPD_02_01_001 %>% 
      str_replace_all("\\(", "（") %>% 
      str_replace_all("\\)", "）")
  )

## アプリで使用する変数のリストとラベル
varlist <- 
  "onet_varlist.xlsx" %>% 
  str_c("Data", ., sep = "/") %>% 
  read_excel() %>% 
  rename(dist_value = `...4`)


## appで使用する変数を選択し、ロングにする
onet_score_01 <- 
  onet_score_00 %>% 
  select(contains(varlist$`IPD-ID`)) %>% 
  pivot_longer(-c(1, 2)) %>% 
  left_join(., varlist, by = c("name" = "IPD-ID")) %>% 
  relocate(value, .after = last_col()) %>% 
  mutate(across(c(`IPD_02_01_001`, name, type, label), fct_inorder))


## 教育と訓練なし
onet_score_01_1 <-
  onet_score_01 %>% 
  filter(type != "教育と訓練") %>% 
  select(-dist_value)
  

## 教育と訓練を連続値化
onet_score_01_2 <-
  onet_score_01 %>% 
  filter(type == "教育と訓練", dist_value != "NA") %>%
  separate(
    col = label,
    into = c("label", "item"),
    sep = "_"
  ) %>% 
  mutate(label = label %>% fct_inorder()) %>% 
  mutate(dist_value = dist_value %>% as.numeric()) %>% 
  group_by(IPD_01_01_001, IPD_02_01_001, type, label) %>% 
  summarise(
    value = weighted.mean(dist_value, w = value, na.rm = T),
    .groups = "drop"
  ) %>% 
  mutate(
    label = case_when(
      label == "学歴" ~ str_c(label, "(平均教育年数)"),
      label == "入職前の訓練期間" ~ str_c(label, "(平均年数)"),
      label == "入職前の実務経験" ~ str_c(label, "(平均年数)"),
      label == "入職後の訓練期間" ~ str_c(label, "(平均年数)")
    ) %>% 
      fct_inorder(),
    value = case_when(
      label %>% str_detect("学歴") ~ value,
      TRUE ~ value/12
    ) 
  )


## 結合
onet_score_02 <- 
  bind_rows(onet_score_01_1, onet_score_01_2) %>% 
  arrange(IPD_01_01_001, name) 


## save
write_csv(onet_score_02, "Data/onet_score.csv")
