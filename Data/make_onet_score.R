library(tidyverse)
library(readxl)

## Onetウェブサイトからダウンロードしたデータを読み込む
onet_score_00 <-
  "IPD_DL_numeric_1_8.xlsx" %>% 
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
varlist <- read_excel("onet_varlist.xlsx")


## appで使用する変数を選択し、ロングにする
onet_score_01 <- 
  onet_score_00 %>% 
  select(contains(varlist$`IPD-ID`)) %>% 
  pivot_longer(-c(1, 2)) %>% 
  left_join(., varlist, by = c("name" = "IPD-ID")) %>% 
  relocate(value, .after = last_col())


## save
write_csv(onet_score_01, "Data/onet_score.csv")
