#rm(list=ls())
library(tidyverse)

df_data <- readRDS('rtt_data.rds') %>% 
  mutate(Period = as.Date(paste0(str_sub(Filename, 1, 6), '01'), format = '%Y%m%d'), .before = 1)
df_detail_wide <- readRDS('current_wl_wide_format.rds') %>% 
  mutate(Period = as.Date(paste0(str_sub(Filename, 1, 6), '01'), format = '%Y%m%d'), .before = 1)
df_detail_long <- readRDS('current_wl_long_format.rds') %>% 
  mutate(Period = as.Date(paste0(str_sub(Filename, 1, 6), '01'), format = '%Y%m%d'), .before = 1)

fnGetOpenPathways <- function(period_start, period_end, 
                              providers, group_providers = TRUE, 
                              rtt_part, treatment_function_codes,
                              )
