#rm(list=ls())
library(tidyverse)

df_data <- readRDS('rtt_data.rds') %>% 
  mutate(Period = as.Date(paste0(str_sub(Filename, 1, 6), '01'), format = '%Y%m%d'), .before = 1)
df_detail_wide <- readRDS('current_wl_wide_format.rds') %>% 
  mutate(Period = as.Date(paste0(str_sub(Filename, 1, 6), '01'), format = '%Y%m%d'), .before = 1)
df_detail_long <- readRDS('current_wl_long_format.rds') %>% 
  mutate(Period = as.Date(paste0(str_sub(Filename, 1, 6), '01'), format = '%Y%m%d'), .before = 1)


df_tfc_lu <- data.frame(Treatment_Function_Code = c('C_100', 'C_101', 'C_110', 'C_120', 'C_130', 
                                                    'C_140', 'C_150', 'C_160', 'C_170', 'C_300', 
                                                    'C_301', 'C_320', 'C_330', 'C_340', 'C_400', 
                                                    'C_410', 'C_430', 'C_502', 'C_999', 'X01', 
                                                    'X02', 'X03', 'X04', 'X05', 'X06'),
                        Treatment_Function_Name = c('General Surgery', 'Urology', 'Trauma & Orthopaedics', 
                                                    'ENT', 'Ophthalmology', 'Oral Surgery', 
                                                    'Neurosurgery', 'Plastic Surgery', 'Cardiothoracic Surgery', 
                                                    'General Medicine', 'Gastroenterology', 'Cardiology', 
                                                    'Dermatology', 'Thoracic Medicine', 'Neurology', 
                                                    'Rheumatology', 'Geriatric Medicine', 'Gynaecology', 
                                                    'Total', 'Other', 'Other - Medical Services', 
                                                    'Other - Mental Health Services', 'Other - Paediatric Services', 'Other - Surgical Services', 
                                                    'Other - Other Services'))
df_rtt_type_lu <- data.frame(RTT_Part = c('PART_1A', 'PART_1B', 'PART_2', 'PART_2A', 'PART_3'),
                             RTT_Part_Desc = c('Completed Pathways For Admitted Patients',
                                               'Completed Pathways For Non-Admitted Patients',
                                               'Incomplete Pathways', 'New RTT Periods - All Patients', 
                                               'Incomplete Pathways with DTA'))

# fnGetOpenPathways <- function(period_start, period_end, 
#                               provider_codes, group_providers = TRUE, 
#                               rtt_part, treatment_function_codes,
#                               )


# Admitted Waiting List
df_data %>% 
  dplyr::filter(Provider_Code %in% c('RH8', 'RBZ')) %>% 
  dplyr::filter(RTT_Part == 'PART_2A') %>%
  group_by(Period, Treatment_Function_Code) %>%
  summarise(Wk00_18 = sum(Wk00_18, na.rm = TRUE),
            Grand_Total = sum(Grand_Total, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup()

# Non-Admitted Waiting List
df_data %>% 
  dplyr::filter(Provider_Code %in% c('RH8', 'RBZ')) %>% 
  dplyr::filter(RTT_Part %in% c('PART_2', 'PART_2A')) %>%
  select(-RTT_Part_Desc) %>%
  pivot_wider(names_from = RTT_Part, values_from = c(Wk00_18, Grand_Total), values_fill = 0) %>%
  mutate(Wk00_18 = Wk00_18_PART_2 - Wk00_18_PART_2A,
         Grand_Total = Grand_Total_PART_2 - Grand_Total_PART_2A) %>%
  group_by(Period, Treatment_Function_Code) %>%
  summarise(Wk00_18 = sum(Wk00_18, na.rm = TRUE),
            Grand_Total = sum(Grand_Total, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup()

# Non-Admitted Clock Starts
df_data %>% 
  dplyr::filter(Provider_Code %in% c('RH8', 'RBZ')) %>% 
  dplyr::filter(RTT_Part == 'PART_3') %>%
  group_by(Period, Treatment_Function_Code) %>%
  summarise(Grand_Total = sum(Grand_Total, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup()

# Admitted Clock Stops
df_data %>% 
  dplyr::filter(Provider_Code %in% c('RH8', 'RBZ')) %>% 
  dplyr::filter(RTT_Part == 'PART_1A') %>%
  group_by(Period, Treatment_Function_Code) %>%
  summarise(Wk00_18 = sum(Wk00_18, na.rm = TRUE),
            Grand_Total = sum(Grand_Total, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup()

# Non-Admitted Clock Stops
df_data %>% 
  dplyr::filter(Provider_Code %in% c('RH8', 'RBZ')) %>% 
  dplyr::filter(RTT_Part == 'PART_1B') %>%
  group_by(Period, Treatment_Function_Code) %>%
  summarise(Wk00_18 = sum(Wk00_18, na.rm = TRUE),
            Grand_Total = sum(Grand_Total, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup()




# Incomplete outpatient pathways = [PART_2: Incomplete Pathways] - [PART_2A: Incomplete Pathways with DTA]
