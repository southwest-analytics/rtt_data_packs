#rm(list=ls())
library(tidyverse)
library(timeDate)
library(lubridate)

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
df_adm_wl <- df_data %>% 
  dplyr::filter(Provider_Code %in% c('RH8', 'RBZ')) %>% 
  dplyr::filter(RTT_Part == 'PART_2A') %>%
  group_by(Period, Treatment_Function_Code) %>%
  summarise(Wk00_18 = sum(Wk00_18, na.rm = TRUE),
            Grand_Total = sum(Grand_Total, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup()

# Non-Admitted Waiting List
df_nonadm_wl <- df_data %>% 
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
df_nonadm_starts <- df_data %>% 
  dplyr::filter(Provider_Code %in% c('RH8', 'RBZ')) %>% 
  dplyr::filter(RTT_Part == 'PART_3') %>%
  group_by(Period, Treatment_Function_Code) %>%
  summarise(Grand_Total = sum(Grand_Total, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup()

# Admitted Clock Stops
df_adm_stops <- df_data %>% 
  dplyr::filter(Provider_Code %in% c('RH8', 'RBZ')) %>% 
  dplyr::filter(RTT_Part == 'PART_1A') %>%
  group_by(Period, Treatment_Function_Code) %>%
  summarise(Wk00_18 = sum(Wk00_18, na.rm = TRUE),
            Grand_Total = sum(Grand_Total, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup()

# Non-Admitted Clock Stops
df_nonadm_stops <- df_data %>% 
  dplyr::filter(Provider_Code %in% c('RH8', 'RBZ')) %>% 
  dplyr::filter(RTT_Part == 'PART_1B') %>%
  group_by(Period, Treatment_Function_Code) %>%
  summarise(Wk00_18 = sum(Wk00_18, na.rm = TRUE),
            Grand_Total = sum(Grand_Total, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup()

# Working days
bank_holidays <- as.Date(c('2022-01-03', '2022-04-15', '2022-04-18', '2022-05-02', '2022-06-02', '2022-06-03', '2022-08-29', '2022-09-19', '2022-12-26', '2022-12-27', 
                           '2021-01-01', '2021-04-02', '2021-04-05', '2021-05-03', '2021-05-31', '2021-08-30', '2021-12-27', '2021-12-28', 
                           '2020-01-01', '2020-04-10', '2020-04-13', '2020-05-04', '2020-05-25', '2020-08-31', '2020-12-25', '2020-12-28', 
                           '2019-01-01', '2019-04-19', '2019-04-22', '2019-05-06', '2019-05-27', '2019-08-26', '2019-12-25', '2019-12-26', 
                           '2018-01-01', '2018-03-30', '2018-04-02', '2018-05-07', '2018-05-28', '2018-08-27', '2018-12-25', '2018-12-26', 
                           '2017-01-02', '2017-04-14', '2017-04-17', '2017-05-01', '2017-05-29', '2017-08-28', '2017-12-25', '2017-12-26', 
                           '2016-01-01', '2016-01-25', '2016-01-28', '2016-01-02', '2016-01-30', '2016-01-29', '2016-01-26', '2016-01-27'))


df_working_days <- data.frame(date = as.Date(timeDate::timeSequence(from = as.Date(min(df_data$Period)), 
                                                                               to = as.Date(max(df_data$Period) + 
                                                                                              lubridate::dmonths(1) - 
                                                                                              lubridate::ddays(1))))) %>%
  mutate(working_day = timeDate::isBizday(as.timeDate(date), holidays = bank_holidays))

df_working_days %>%
  mutate(month = as.Date(format(df_working_days$date, '%Y-%m-01'))) %>%
  filter(working_day==TRUE) %>%
  group_by(month) %>%
  summarise(working_days = n()) %>%
  ungroup()
         
hist()  
  