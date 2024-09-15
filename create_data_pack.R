# 0. Load libraries and define functions ----
# ═══════════════════════════════════════════
library(tidyverse)
library(timeDate)
library(lubridate)
library(flextable)
library(officer)
library(plotly)

# * 0.1. Parameters
provider_code_selection <- c('RH8', 'RBZ')
organisation_name <- 'Royal Devon University Healthcare (RDUH)'
selected_period <- months(11)
wl_start_date <- as.Date('2022-04-01')
conversion_period <- months(23)
output_dir <- 'output'
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
b_interactive = TRUE

fnTimeseriesPlot <- function(df, str_title, str_x_axis, str_y_axis, b_interactive = FALSE){
  if(!b_interactive){
    plt <- ggplot() %+%
      theme_bw(base_size = 12) %+%
      theme(plot.title = element_text(hjust = .5),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) %+%
      labs(title = str_title, x = str_x_axis, y = str_y_axis) %+%
      geom_line(data = df, aes(x = Period, y = Grand_Total), colour = '#005EB8', linewidth = 1) %+%
      geom_point(data = df, aes(x = Period, y = Grand_Total), 
                 shape = 21, fill = '#FFFFFF', color = '#005EB8', size = 2, stroke = 1.5) %+%
      scale_x_date(date_labels = format('%b-%y'))
  } else {
    plt <- plotly::plot_ly() %>%
      add_trace(data = df,
                type = 'scatter',
                mode = 'markers+lines',
                x = ~Period,
                y = ~Grand_Total,
                line = list(color = '#005EB8'),
                marker = list(line = list(color = '#005EB8', width = 2),
                              color = '#FFFFFF')) %>%
      layout(title =str_title,
             xaxis = list(title = str_x_axis,
                          dtick = "M6", tickformat='%b-%y'),
             yaxis = list(title = str_y_axis))
  }
  return(plt)
}
  
fnHistogramPlot <- function(df, str_title, str_x_axis, str_y_axis, b_interactive = FALSE){
  if(!b_interactive){
    plt <- ggplot() %+%
      theme_bw(base_size = 12) %+%
      theme(plot.title = element_text(hjust = .5)) %+%
      labs(title = str_title, x = str_x_axis, y = str_y_axis) %+%
      geom_histogram(data = df,
                     aes(x = Grand_Total), fill = '#005EB8', color = '#003087')
  } else {
    plt <- plotly::plot_ly() %>%
      add_trace(data = df,
                type = 'histogram',
                x = ~Grand_Total,
                marker = list(line = list(width = 1, color = '#003087'),
                              color = '#005EB8')) %>%
    layout(title =str_title,
             xaxis = list(title = str_x_axis),
             yaxis = list(title = str_y_axis))
  }
  return(plt)
}

  
# 1. Load data ----
# ═════════════════

# * 1.1. Load concise historic data ----
# ──────────────────────────────────────
df_data <- readRDS('rtt_data.rds') %>% 
  mutate(Period = as.Date(paste0(str_sub(Filename, 1, 6), '01'), format = '%Y%m%d'), .before = 1)

# * 1.2. Load detailed current month data ----
# ────────────────────────────────────────────
# * * 1.2.1. Wide format ----
df_detail_wide <- readRDS('current_wl_wide_format.rds') %>% 
  mutate(Period = as.Date(paste0(str_sub(Filename, 1, 6), '01'), format = '%Y%m%d'), .before = 1)
# * * 1.2.1. Long format ----
df_detail_long <- readRDS('current_wl_long_format.rds') %>% 
  mutate(Period = as.Date(paste0(str_sub(Filename, 1, 6), '01'), format = '%Y%m%d'), .before = 1)

# * 1.3. Load lookup data ----
# ────────────────────────────
# * * 1.3.1 Treatment function codes ----
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
# * * 1.3.2 RTT sections ----
df_rtt_type_lu <- data.frame(RTT_Part = c('PART_1A', 'PART_1B', 'PART_2', 'PART_2A', 'PART_3'),
                             RTT_Part_Desc = c('Completed Pathways For Admitted Patients',
                                               'Completed Pathways For Non-Admitted Patients',
                                               'Incomplete Pathways', 'New RTT Periods - All Patients', 
                                               'Incomplete Pathways with DTA'))


# * * 1.3.3 Working days ----
bank_holidays <- as.Date(c('2022-01-03', '2022-04-15', '2022-04-18', '2022-05-02', '2022-06-02', '2022-06-03', '2022-08-29', '2022-09-19', '2022-12-26', '2022-12-27', 
                           '2021-01-01', '2021-04-02', '2021-04-05', '2021-05-03', '2021-05-31', '2021-08-30', '2021-12-27', '2021-12-28', 
                           '2020-01-01', '2020-04-10', '2020-04-13', '2020-05-04', '2020-05-25', '2020-08-31', '2020-12-25', '2020-12-28', 
                           '2019-01-01', '2019-04-19', '2019-04-22', '2019-05-06', '2019-05-27', '2019-08-26', '2019-12-25', '2019-12-26', 
                           '2018-01-01', '2018-03-30', '2018-04-02', '2018-05-07', '2018-05-28', '2018-08-27', '2018-12-25', '2018-12-26', 
                           '2017-01-02', '2017-04-14', '2017-04-17', '2017-05-01', '2017-05-29', '2017-08-28', '2017-12-25', '2017-12-26', 
                           '2016-01-01', '2016-01-25', '2016-01-28', '2016-01-02', '2016-01-30', '2016-01-29', '2016-01-26', '2016-01-27'))
# Start and end dates
start_date <- as.Date(min(df_data$Period))
end_date <- as.Date(max(df_data$Period))

# Create a date frame of dates covering data period classified as working or non-working day 
df_working_days <- data.frame(date = as.Date(timeDate::timeSequence(from = start_date,
                                                                    to = end_date + months(1) - days(1)))) %>%
  mutate(working_day = timeDate::isBizday(as.timeDate(date), holidays = bank_holidays))

# Group by month and calculate the number of working days in that month
df_monthly_working_days <- df_working_days %>%
  mutate(month = as.Date(format(df_working_days$date, '%Y-%m-01'))) %>%
  filter(working_day==TRUE) %>%
  group_by(month) %>%
  summarise(working_days = n()) %>%
  ungroup()
                                                                    
# 2. Process data ----
# ════════════════════

# * 2.1. Admitted waiting list ----
# ─────────────────────────────────
df_adm_wl <- df_data %>% 
  dplyr::filter(Provider_Code %in% provider_code_selection) %>% 
  dplyr::filter(RTT_Part == 'PART_2A') %>%
  group_by(Period, Treatment_Function_Code) %>%
  summarise(Wk00_18 = sum(Wk00_18, na.rm = TRUE),
            Grand_Total = sum(Grand_Total, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup()

# * 2.2. Non-admitted waiting list ----
# ─────────────────────────────────────
df_nonadm_wl <- df_data %>% 
  dplyr::filter(Provider_Code %in% provider_code_selection) %>% 
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

# * 2.3. Non-admitted clock starts ----
# ─────────────────────────────────────
df_nonadm_starts <- df_data %>% 
  dplyr::filter(Provider_Code %in% provider_code_selection) %>% 
  dplyr::filter(RTT_Part == 'PART_3') %>%
  group_by(Period, Treatment_Function_Code) %>%
  summarise(Grand_Total = sum(Grand_Total, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup()

# * 2.4. Admitted clock stops ----
# ────────────────────────────────
df_adm_stops <- df_data %>% 
  dplyr::filter(Provider_Code %in% provider_code_selection) %>% 
  dplyr::filter(RTT_Part == 'PART_1A') %>%
  group_by(Period, Treatment_Function_Code) %>%
  summarise(Wk00_18 = sum(Wk00_18, na.rm = TRUE),
            Grand_Total = sum(Grand_Total, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup()

# * 2.5. Non-admitted clock stops ----
# ────────────────────────────────────
df_nonadm_stops <- df_data %>% 
  dplyr::filter(Provider_Code %in% provider_code_selection) %>% 
  dplyr::filter(RTT_Part == 'PART_1B') %>%
  group_by(Period, Treatment_Function_Code) %>%
  summarise(Wk00_18 = sum(Wk00_18, na.rm = TRUE),
            Grand_Total = sum(Grand_Total, na.rm = TRUE),
            .groups = 'keep') %>%
  ungroup()

# 3. Create data pack ----
# ════════════════════════
selected_tfc <- 'C_110'

# * 3.1. Historic activity ----
# ─────────────────────────────

# * * 3.1.1. Non-admitted clock starts ----
df_plot_data <- df_nonadm_starts

df_table_data <- df_plot_data %>% 
  dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
  reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                    Mean = mean(x = Grand_Total),
                    Std.Dev. = sd(x = Grand_Total),
                    n = n())) %>%
  mutate(Statistic = names(value),
         period = paste0(format(start_date, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
  select(period, Statistic, value) %>%
  bind_rows(
    df_plot_data %>% 
      dplyr::filter(Treatment_Function_Code == selected_tfc & 
                      Period >= end_date - selected_period &
                      Period <= end_date) %>% 
      reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                          Mean = mean(x = Grand_Total),
                          Std.Dev. = sd(x = Grand_Total),
                          n = n())) %>%
      mutate(Statistic = names(value),
             period = paste0(format(end_date - selected_period, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
      select(period, value, Statistic)
  ) %>%
  pivot_wider(names_from = period, values_from = value)

ftbl_nonadm_starts <- flextable(data = df_table_data) %>%
  theme_booktabs() %>%
  add_header_row(top = TRUE, values = 'Statistics for Non-Admitted Clock Starts',colwidth = 3) %>%
  align(i = 1, part = 'header', align = 'center') %>%
  set_table_properties(layout = "autofit", width = .9) %>%
  colformat_double(j = 2:3, digits = 1) %>%
  colformat_double(i = 8, j = 2:3, digits = 0) %>%
  hline(i = c(5,7), border = fp_border(color = "gray")) %>%
  bold(bold = TRUE, part = "header")

# * * * 3.1.1.1. Time series - Entire data set ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Clock Starts at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Non-Admitted Clock Starts'
#b_interactive = FALSE
plt_nonadm_starts_ts_entire <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                                str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.1.2. Time series - Selected time period ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Clock Starts at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_starts_ts_selected <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                          Period >= end_date - selected_period &
                                                                                          Period <= end_date),
                                                  str_title, str_x_axis, str_y_axis, b_interactive)
# * * * 3.1.1.3. Histogram - Entire data set ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Clock Starts at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Non-Admitted Clock Starts'
str_y_axis <- 'Frequency'
#b_interactive = TRUE
plt_nonadm_starts_hs_entire <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                               str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.1.4. Histogram - Selected time period ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Clock Starts at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_starts_hs_selected <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                      Period >= end_date - selected_period &
                                                                                      Period <= end_date),
                                                 str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.1.5. Output RObject ----
save(list = c('plt_nonadm_starts_ts_entire', 'plt_nonadm_starts_ts_selected', 
              'plt_nonadm_starts_hs_entire', 'plt_nonadm_starts_hs_selected',
              'ftbl_nonadm_starts'),
     file = paste0(output_dir, '/non_admitted_clock_starts.Robj'))

# * * 3.1.2. Non-admitted clock starts adjusted for working days ----
df_plot_data <- df_nonadm_starts %>% 
  left_join(df_monthly_working_days, by = c('Period' = 'month')) %>%
  mutate(Grand_Total = Grand_Total / working_days * 21)

df_table_data <- df_plot_data %>% 
  dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
  reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                    Mean = mean(x = Grand_Total),
                    Std.Dev. = sd(x = Grand_Total),
                    n = n())) %>%
  mutate(Statistic = names(value),
         period = paste0(format(start_date, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
  select(period, Statistic, value) %>%
  bind_rows(
    df_plot_data %>% 
      dplyr::filter(Treatment_Function_Code == selected_tfc & 
                      Period >= end_date - selected_period &
                      Period <= end_date) %>% 
      reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                        Mean = mean(x = Grand_Total),
                        Std.Dev. = sd(x = Grand_Total),
                        n = n())) %>%
      mutate(Statistic = names(value),
             period = paste0(format(end_date - selected_period, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
      select(period, value, Statistic)
  ) %>%
  pivot_wider(names_from = period, values_from = value)

ftbl_nonadm_starts_wda <- flextable(data = df_table_data) %>%
  theme_booktabs() %>%
  add_header_row(top = TRUE, values = 'Statistics for Non-Admitted Clock Starts Adjusted for Working Days and Standardised to a 21 Working Day Month',colwidth = 3) %>%
  align(i = 1, part = 'header', align = 'center') %>%
  set_table_properties(layout = "autofit", width = .9) %>%
  colformat_double(j = 2:3, digits = 1) %>%
  colformat_double(i = 8, j = 2:3, digits = 0) %>%
  hline(i = c(5,7), border = fp_border(color = "gray")) %>%
  bold(bold = TRUE, part = "header")

# * * * 3.1.2.1. Time series - Entire data set ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Clock Starts Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Non-Admitted Clock Starts (Working Day Adjusted)'
#b_interactive = FALSE
plt_nonadm_starts_wda_ts_entire <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                                    str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.2.2. Time series - Selected time period ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Clock Starts Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_starts_wda_ts_selected <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                            Period >= end_date - selected_period &
                                                                                            Period <= end_date),
                                                      str_title, str_x_axis, str_y_axis, b_interactive)
# * * * 3.1.2.3. Histogram - Entire data set ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Clock Starts Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Non-Admitted Clock Starts (Working Day Adjusted)'
str_y_axis <- 'Frequency'
#b_interactive = TRUE
plt_nonadm_starts_wda_hs_entire <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                               str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.2.4. Histogram - Selected time period ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Clock Starts Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_starts_wda_hs_selected <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                      Period >= end_date - selected_period &
                                                                                      Period <= end_date),
                                                 str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.2.5. Output RObject ----
save(list = c('plt_nonadm_starts_wda_ts_entire', 'plt_nonadm_starts_wda_ts_selected', 
              'plt_nonadm_starts_wda_hs_entire', 'plt_nonadm_starts_wda_hs_selected',
              'ftbl_nonadm_starts_wda'),
     file = paste0(output_dir, '/non_admitted_clock_starts_wda.Robj'))

# * * 3.1.3. Non-admitted clock stops ----
df_plot_data <- df_nonadm_stops

df_table_data <- df_plot_data %>% 
  dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
  reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                    Mean = mean(x = Grand_Total),
                    Std.Dev. = sd(x = Grand_Total),
                    n = n())) %>%
  mutate(Statistic = names(value),
         period = paste0(format(start_date, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
  select(period, Statistic, value) %>%
  bind_rows(
    df_plot_data %>% 
      dplyr::filter(Treatment_Function_Code == selected_tfc & 
                      Period >= end_date - selected_period &
                      Period <= end_date) %>% 
      reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                        Mean = mean(x = Grand_Total),
                        Std.Dev. = sd(x = Grand_Total),
                        n = n())) %>%
      mutate(Statistic = names(value),
             period = paste0(format(end_date - selected_period, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
      select(period, value, Statistic)
  ) %>%
  pivot_wider(names_from = period, values_from = value)

ftbl_nonadm_stops <- flextable(data = df_table_data) %>%
  theme_booktabs() %>%
  add_header_row(top = TRUE, values = 'Statistics for Non-Admitted Clock Stops',colwidth = 3) %>%
  align(i = 1, part = 'header', align = 'center') %>%
  set_table_properties(layout = "autofit", width = .9) %>%
  colformat_double(j = 2:3, digits = 1) %>%
  colformat_double(i = 8, j = 2:3, digits = 0) %>%
  hline(i = c(5,7), border = fp_border(color = "gray")) %>%
  bold(bold = TRUE, part = "header")

# * * * 3.1.3.1. Time series - Entire data set ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Non-Admitted Clock Stops'
#b_interactive = FALSE
plt_nonadm_stops_ts_entire <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                               str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.3.2. Time series - Selected time period ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_stops_ts_selected <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                       Period >= end_date - selected_period &
                                                                                       Period <= end_date),
                                                 str_title, str_x_axis, str_y_axis, b_interactive)
# * * * 3.1.3.3. Histogram - Entire data set ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Non-Admitted Clock Stops'
str_y_axis <- 'Frequency'
#b_interactive = TRUE
plt_nonadm_stops_hs_entire <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                              str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.3.4. Histogram - Selected time period ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_stops_hs_selected <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                  Period >= end_date - selected_period &
                                                                                  Period <= end_date),
                                                 str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.3.5. Output RObject ----
save(list = c('plt_nonadm_stops_ts_entire', 'plt_nonadm_stops_ts_selected', 
              'plt_nonadm_stops_hs_entire', 'plt_nonadm_stops_hs_selected',
              'ftbl_nonadm_stops'),
     file = paste0(output_dir, '/non_admitted_clock_stops.Robj'))

# * * 3.1.4. Non-admitted clock stops adjusted for working days ----
df_plot_data <- df_nonadm_stops %>% 
  left_join(df_monthly_working_days, by = c('Period' = 'month')) %>%
  mutate(Grand_Total = Grand_Total / working_days * 21)

df_table_data <- df_plot_data %>% 
  dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
  reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                    Mean = mean(x = Grand_Total),
                    Std.Dev. = sd(x = Grand_Total),
                    n = n())) %>%
  mutate(Statistic = names(value),
         period = paste0(format(start_date, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
  select(period, Statistic, value) %>%
  bind_rows(
    df_plot_data %>% 
      dplyr::filter(Treatment_Function_Code == selected_tfc & 
                      Period >= end_date - selected_period &
                      Period <= end_date) %>% 
      reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                        Mean = mean(x = Grand_Total),
                        Std.Dev. = sd(x = Grand_Total),
                        n = n())) %>%
      mutate(Statistic = names(value),
             period = paste0(format(end_date - selected_period, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
      select(period, value, Statistic)
  ) %>%
  pivot_wider(names_from = period, values_from = value)

ftbl_nonadm_stops_wda <- flextable(data = df_table_data) %>%
  theme_booktabs() %>%
  add_header_row(top = TRUE, values = 'Statistics for Non-Admitted Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month',colwidth = 3) %>%
  align(i = 1, part = 'header', align = 'center') %>%
  set_table_properties(layout = "autofit", width = .9) %>%
  colformat_double(j = 2:3, digits = 1) %>%
  colformat_double(i = 8, j = 2:3, digits = 0) %>%
  hline(i = c(5,7), border = fp_border(color = "gray")) %>%
  bold(bold = TRUE, part = "header")


# * * * 3.1.4.1. Time series - Entire data set ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Non-Admitted Clock Stops (Working Day Adjusted)'
#b_interactive = FALSE
plt_nonadm_stops_wda_ts_entire <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                                    str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.4.2. Time series - Selected time period ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_stops_wda_ts_selected <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                            Period >= end_date - selected_period &
                                                                                            Period <= end_date),
                                                      str_title, str_x_axis, str_y_axis, b_interactive)
# * * * 3.1.4.3. Histogram - Entire data set ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Non-Admitted Clock Stops (Working Day Adjusted)'
str_y_axis <- 'Frequency'
#b_interactive = TRUE
plt_nonadm_stops_wda_hs_entire <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                                   str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.4.4. Histogram - Selected time period ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_stops_wda_hs_selected <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                      Period >= end_date - selected_period &
                                                                                      Period <= end_date),
                                                     str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.4.5. Output RObject ----
save(list = c('plt_nonadm_stops_wda_ts_entire', 'plt_nonadm_stops_wda_ts_selected', 
              'plt_nonadm_stops_wda_hs_entire', 'plt_nonadm_stops_wda_hs_selected',
              'ftbl_nonadm_stops_wda'),
     file = paste0(output_dir, '/non_admitted_clock_stops_wda.Robj'))

# * * 3.1.5. Admitted clock stops ----
df_plot_data <- df_adm_stops

df_table_data <- df_plot_data %>% 
  dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
  reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                    Mean = mean(x = Grand_Total),
                    Std.Dev. = sd(x = Grand_Total),
                    n = n())) %>%
  mutate(Statistic = names(value),
         period = paste0(format(start_date, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
  select(period, Statistic, value) %>%
  bind_rows(
    df_plot_data %>% 
      dplyr::filter(Treatment_Function_Code == selected_tfc & 
                      Period >= end_date - selected_period &
                      Period <= end_date) %>% 
      reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                        Mean = mean(x = Grand_Total),
                        Std.Dev. = sd(x = Grand_Total),
                        n = n())) %>%
      mutate(Statistic = names(value),
             period = paste0(format(end_date - selected_period, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
      select(period, value, Statistic)
  ) %>%
  pivot_wider(names_from = period, values_from = value)

ftbl_adm_stops <- flextable(data = df_table_data) %>%
  theme_booktabs() %>%
  add_header_row(top = TRUE, values = 'Statistics for Admitted Clock Stops',colwidth = 3) %>%
  align(i = 1, part = 'header', align = 'center') %>%
  set_table_properties(layout = "autofit", width = .9) %>%
  colformat_double(j = 2:3, digits = 1) %>%
  colformat_double(i = 8, j = 2:3, digits = 0) %>%
  hline(i = c(5,7), border = fp_border(color = "gray")) %>%
  bold(bold = TRUE, part = "header")

# * * * 3.1.5.1. Time series - Entire data set ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Admitted Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Admitted Clock Stops'
#b_interactive = FALSE
plt_adm_stops_ts_entire <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                               str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.5.2. Time series - Selected time period ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Admitted Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_adm_stops_ts_selected <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                       Period >= end_date - selected_period &
                                                                                       Period <= end_date),
                                                 str_title, str_x_axis, str_y_axis, b_interactive)
# * * * 3.1.5.3. Histogram - Entire data set ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Admitted Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Admitted Clock Stops'
str_y_axis <- 'Frequency'
#b_interactive = TRUE
plt_adm_stops_hs_entire <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                              str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.5.4. Histogram - Selected time period ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Admitted Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_adm_stops_hs_selected <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                 Period >= end_date - selected_period &
                                                                                 Period <= end_date),
                                                str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.5.5. Output RObject ----
save(list = c('plt_adm_stops_ts_entire', 'plt_adm_stops_ts_selected', 
              'plt_adm_stops_hs_entire', 'plt_adm_stops_hs_selected',
              'ftbl_adm_stops'),
     file = paste0(output_dir, '/admitted_clock_stops.Robj'))

# * * 3.1.6. Admitted clock stops adjusted for working days ----
df_plot_data <- df_adm_stops %>% 
  left_join(df_monthly_working_days, by = c('Period' = 'month')) %>%
  mutate(Grand_Total = Grand_Total / working_days * 21)

df_table_data <- df_plot_data %>% 
  dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
  reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                    Mean = mean(x = Grand_Total),
                    Std.Dev. = sd(x = Grand_Total),
                    n = n())) %>%
  mutate(Statistic = names(value),
         period = paste0(format(start_date, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
  select(period, Statistic, value) %>%
  bind_rows(
    df_plot_data %>% 
      dplyr::filter(Treatment_Function_Code == selected_tfc & 
                      Period >= end_date - selected_period &
                      Period <= end_date) %>% 
      reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                        Mean = mean(x = Grand_Total),
                        Std.Dev. = sd(x = Grand_Total),
                        n = n())) %>%
      mutate(Statistic = names(value),
             period = paste0(format(end_date - selected_period, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
      select(period, value, Statistic)
  ) %>%
  pivot_wider(names_from = period, values_from = value)

ftbl_adm_stops_wda <- flextable(data = df_table_data) %>%
  theme_booktabs() %>%
  add_header_row(top = TRUE, values = 'Statistics for Admitted Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month',colwidth = 3) %>%
  align(i = 1, part = 'header', align = 'center') %>%
  set_table_properties(layout = "autofit", width = .9) %>%
  colformat_double(j = 2:3, digits = 1) %>%
  colformat_double(i = 8, j = 2:3, digits = 0) %>%
  hline(i = c(5,7), border = fp_border(color = "gray")) %>%
  bold(bold = TRUE, part = "header")

# * * * 3.1.6.1. Time series - Entire data set ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Admitted Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Admitted Clock Stops (Working Day Adjusted)'
#b_interactive = FALSE
plt_adm_stops_wda_ts_entire <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                                   str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.6.2. Time series - Selected time period ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Admitted Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_adm_stops_wda_ts_selected <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                           Period >= end_date - selected_period &
                                                                                           Period <= end_date),
                                                     str_title, str_x_axis, str_y_axis, b_interactive)
# * * * 3.1.6.3. Histogram - Entire data set ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Admitted Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Admitted Clock Stops (Working Day Adjusted)'
str_y_axis <- 'Frequency'
#b_interactive = TRUE
plt_adm_stops_wda_hs_entire <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                                  str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.6.4. Histogram - Selected time period ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Admitted Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_adm_stops_wda_hs_selected <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                     Period >= end_date - selected_period &
                                                                                     Period <= end_date),
                                                    str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.6.5. Output RObject ----
save(list = c('plt_adm_stops_wda_ts_entire', 'plt_adm_stops_wda_ts_selected', 
              'plt_adm_stops_wda_hs_entire', 'plt_adm_stops_wda_hs_selected',
              'ftbl_adm_stops_wda'),
     file = paste0(output_dir, '/admitted_clock_stops_wda.Robj'))

# * * 3.1.7. Non-admitted open pathways ----
df_plot_data <- df_nonadm_wl

# * * * 3.1.7.1. Time series - Entire data set ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Waiting List Size at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Non-Admitted Waiting List Size'
#b_interactive = FALSE
plt_nonadm_wl_size_ts_entire <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                               str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.7.2. Time series - Selected time period ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Waiting List Size at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_wl_size_ts_selected <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                       Period >= wl_start_date &
                                                                                       Period <= end_date),
                                                 str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.7.3. Output RObject ----
save(list = c('plt_nonadm_wl_size_ts_entire', 'plt_nonadm_wl_size_ts_selected'),
     file = paste0(output_dir, '/non_admitted_wl_size.Robj'))

# * * 3.1.8. Admitted open pathways ----
df_plot_data <- df_adm_wl

# * * * 3.1.8.1. Time series - Entire data set ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Admitted Waiting List Size at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Non-Admitted Waiting List Size'
#b_interactive = FALSE
plt_adm_wl_size_ts_entire <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                                 str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.8.2. Time series - Selected time period ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Admitted Waiting List Size at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_adm_wl_size_ts_selected <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                         Period >= wl_start_date &
                                                                                         Period <= end_date),
                                                   str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.8.3. Output RObject ----
save(list = c('plt_adm_wl_size_ts_entire', 'plt_adm_wl_size_ts_selected'),
     file = paste0(output_dir, '/admitted_wl_size.Robj'))

# * * 3.1.9. Change in waiting list ----
df_plot_data <- df_nonadm_starts %>% 
  dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
  mutate(Period, Clock_Starts = Grand_Total, .keep = 'none') %>%
  left_join(
    df_adm_stops %>% dplyr::filter(Treatment_Function_Code == selected_tfc) %>%
      bind_rows(df_nonadm_stops %>% dplyr::filter(Treatment_Function_Code == selected_tfc)) %>%
      group_by(Period) %>% 
      summarise(Clock_Stops = sum(Grand_Total)) %>%
      ungroup() %>%
      left_join(
        df_adm_wl %>% dplyr::filter(Treatment_Function_Code == selected_tfc) %>%
          bind_rows(df_nonadm_wl %>% dplyr::filter(Treatment_Function_Code == selected_tfc)) %>% 
          group_by(Period) %>% 
          summarise(Waiting_List_Size = sum(Grand_Total)) %>%
          ungroup(),
        by = 'Period'),
    by = 'Period') %>%
  mutate(Delta = Clock_Starts - Clock_Stops,
         Observed_Delta = lead(Waiting_List_Size) - Waiting_List_Size,
         Cum_Delta = cumsum(Delta))
df_plot_data$Expected_Waiting_List_Size = df_plot_data %>% head(1) %>% .$Waiting_List_Size
df_plot_data <- df_plot_data %>% mutate(Expected_Waiting_List_Size = Expected_Waiting_List_Size + Cum_Delta)

# * * * 3.1.9.1 Difference between observed and expected change in waiting list ----
df <- df_plot_data %>% select(Period, Delta, Observed_Delta) %>% pivot_longer(cols = 2:3, names_to = 'Metric', values_to = 'Value')
str_title <- str_wrap(sprintf('Difference between Difference in Clock Starts and Clock Stops (Delta) and the Difference in Consecutive Waiting List Sizes (Observed Delta) of %s (%s) Admitted and Non-Admitted Pathways Combined at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Change in Waiting List Size'
#b_interactive <- TRUE

if(!b_interactive){
  plt <- ggplot() %+%
    theme_bw(base_size = 12) %+%
    theme(plot.title = element_text(hjust = .5),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) %+%
    labs(title = str_title, x = str_x_axis, y = str_y_axis) %+%
    geom_line(data = df, aes(x = Period, y = Value, group = Metric, colour = Metric)) %+%
    scale_x_date(date_labels = format('%b-%y')) %+%
    scale_color_manual(values = c('Delta' = '#005EB8', 'Observed_Delta' = '#8A1538'), labels = c('Delta', 'Observed_Delta'))
} else {
  plt <- plotly::plot_ly() %>%
    add_trace(data = df %>% dplyr::filter(Metric == 'Delta'),
              type = 'scatter',
              mode = 'lines',
              name = 'Delta',
              x = ~Period,
              y = ~Value,
              line = list(color = '#005EB8')) %>%
    add_trace(data = df %>% dplyr::filter(Metric == 'Observed_Delta'),
              type = 'scatter',
              mode = 'lines',
              name = 'Observed Delta',
              x = ~Period,
              y = ~Value,
              line = list(color = '#8A1538')) %>%
    layout(title =str_title,
           xaxis = list(title = str_x_axis,
                        dtick = "M6", tickformat='%b-%y'),
           yaxis = list(title = str_y_axis))
}
plt_observed_vs_expected_waiting_list_changes <- plt

# * * * 3.1.9.2 Difference between expected and observed waiting list size ----
df <- df_plot_data %>% select(Period, Expected_Waiting_List_Size, Waiting_List_Size) %>% pivot_longer(cols = 2:3, names_to = 'Metric', values_to = 'Value')
str_title <- str_wrap(sprintf('Difference between Expected Waiting List Size (using Delta) and the Actual Waiting List Size of %s (%s) Admitted and Non-Admitted Pathways Combined at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Waiting List Size'
#b_interactive <- TRUE

if(!b_interactive){
  plt <- ggplot() %+%
    theme_bw(base_size = 12) %+%
    theme(plot.title = element_text(hjust = .5),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) %+%
    labs(title = str_title, x = str_x_axis, y = str_y_axis) %+%
    geom_line(data = df, aes(x = Period, y = Value, group = Metric, colour = Metric)) %+%
    scale_x_date(date_labels = format('%b-%y')) %+%
    scale_color_manual(values = c('Expected_Waiting_List_Size' = '#005EB8', 'Waiting_List_Size' = '#8A1538'), labels = c('Expected Waiting List Size', 'Waiting List Size'))
} else {
  plt <- plotly::plot_ly() %>%
    add_trace(data = df %>% dplyr::filter(Metric == 'Expected_Waiting_List_Size'),
              type = 'scatter',
              mode = 'lines',
              name = 'Expected Waiting List Size',
              x = ~Period,
              y = ~Value,
              line = list(color = '#005EB8')) %>%
    add_trace(data = df %>% dplyr::filter(Metric == 'Waiting_List_Size'),
              type = 'scatter',
              mode = 'lines',
              name = 'Waiting List Size',
              x = ~Period,
              y = ~Value,
              line = list(color = '#8A1538')) %>%
    layout(title =str_title,
           xaxis = list(title = str_x_axis,
                        dtick = "M6", tickformat='%b-%y'),
           yaxis = list(title = str_y_axis))
}
plt_expected_vs_actual_waiting_list_size <- plt

# * * * 3.1.9.3. Output RObject ----
save(list = c('plt_observed_vs_expected_waiting_list_changes', 'plt_expected_vs_actual_waiting_list_size'),
     file = paste0(output_dir, '/wl_delta.Robj'))

# * * 3.1.10. Conversion Rate ----
df_plot_data <- df_adm_stops %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                 Period >= end_date - conversion_period &
                                                 Period <= end_date) %>%
  mutate(Period, Admitted_Clock_Stops = Grand_Total, .keep = 'none') %>%
  left_join(
    df_nonadm_stops %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                        Period >= end_date - conversion_period &
                                        Period <= end_date) %>%
      mutate(Period, NonAdmitted_Clock_Stops = Grand_Total, .keep = 'none'),
    by = 'Period')

# * * * 3.1.10.1 Conversion rate to admitted ----
df <- df_plot_data %>% pivot_longer(cols = 2:3, names_to = 'Metric', values_to = 'Value')
str_title <- str_wrap(sprintf('Conversion Rate from Non-Admitted Pathway to Admitted Pathways of %s (%s) at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Clock Stops'
#b_interactive <- TRUE

if(!b_interactive){
  plt <- ggplot() %+%
    theme_bw(base_size = 12) %+%
    theme(plot.title = element_text(hjust = .5),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) %+%
    labs(title = str_title, x = str_x_axis, y = str_y_axis) %+%
    geom_bar(data = df, aes(x = Period, y = Value, group = Metric, fill = Metric), stat = 'identity') %+%
    scale_x_date(date_labels = format('%b-%y')) %+%
    scale_fill_manual(values = c('Admitted_Clock_Stops' = '#005EB8', 'NonAdmitted_Clock_Stops' = '#8A1538'), labels = c('Admitted Clock Stops', 'Non-Admitted Clock Stops'))
} else {
  plt <- plotly::plot_ly() %>%
    add_trace(data = df %>% dplyr::filter(Metric == 'NonAdmitted_Clock_Stops'),
              type = 'bar',
              name = 'Non-Admitted Clock Stops',
              x = ~Period,
              y = ~Value,
              marker = list(color = '#8A1538')) %>%
    add_trace(data = df %>% dplyr::filter(Metric == 'Admitted_Clock_Stops'),
              type = 'bar',
              name = 'Admitted Clock Stops',
              x = ~Period,
              y = ~Value,
              marker = list(color = '#005EB8')) %>%
    layout(barmode = 'stack',
           title =str_title,
           xaxis = list(title = str_x_axis,
                        dtick = "M6", tickformat='%b-%y'),
           yaxis = list(title = str_y_axis))
}
plt_conversion_rate_to_adm <- plt

n_adm_stops <- sum(df_plot_data$Admitted_Clock_Stops, na.rm = TRUE)
n_nonadm_stops <- sum(df_plot_data$NonAdmitted_Clock_Stops, na.rm = TRUE)
conversion_rate_to_adm <- n_adm_stops / (n_adm_stops + n_nonadm_stops)

# * * * 3.1.10.2. Output RObject ----
save(list = c('plt_conversion_rate_to_adm', 'conversion_rate_to_adm'),
     file = paste0(output_dir, '/conv_to_adm.Robj'))

# * * 3.1.11. Proxy clock stops ----
df_proxy_data <- df_nonadm_wl %>% dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
  mutate(Period, Treatment_Function_Code, NonAdmWL = Grand_Total, .keep = 'none') %>%
  left_join(df_nonadm_starts %>% dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
              mutate(Period, Adds = Grand_Total, ConvRate = conversion_rate_to_adm, .keep = 'none'), by = 'Period') %>%
  left_join(df_adm_wl %>% dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
              mutate(Period, AdmWL = Grand_Total, .keep = 'none'), by = 'Period') %>%
  mutate(NonAdmDelta = lead(NonAdmWL) - NonAdmWL,
         AdmDelta = lead(AdmWL) - AdmWL,
         AdmProxyAdds = round(Adds * ConvRate),
         NonAdmProxyAdds = Adds - AdmProxyAdds,
         NonAdmProxyRems = NonAdmProxyAdds - NonAdmDelta,
         AdmProxyRems = AdmProxyAdds - AdmDelta)

# * * * 3.1.11.1. Output RObject ----
save(list = c('df_proxy_data'), file = paste0(output_dir, '/proxy_data.Robj'))
write.csv(df_proxy_data, paste0(output_dir, '/proxy_data.csv'), row.names = FALSE)

# * * 3.1.12. Non-admitted clock starts ----
df_plot_data <- df_proxy_data %>% mutate(Grand_Total = NonAdmProxyAdds)

df_table_data <- df_plot_data %>% 
  dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
  reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                    Mean = mean(x = Grand_Total),
                    Std.Dev. = sd(x = Grand_Total),
                    n = n())) %>%
  mutate(Statistic = names(value),
         period = paste0(format(start_date, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
  select(period, Statistic, value) %>%
  bind_rows(
    df_plot_data %>% 
      dplyr::filter(Treatment_Function_Code == selected_tfc & 
                      Period >= end_date - selected_period &
                      Period <= end_date) %>% 
      reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                        Mean = mean(x = Grand_Total),
                        Std.Dev. = sd(x = Grand_Total),
                        n = n())) %>%
      mutate(Statistic = names(value),
             period = paste0(format(end_date - selected_period, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
      select(period, value, Statistic)
  ) %>%
  pivot_wider(names_from = period, values_from = value)

ftbl_nonadm_proxy_starts <- flextable(data = df_table_data) %>%
  theme_booktabs() %>%
  add_header_row(top = TRUE, values = 'Statistics for Non-Admitted Proxy Clock Starts',colwidth = 3) %>%
  align(i = 1, part = 'header', align = 'center') %>%
  set_table_properties(layout = "autofit", width = .9) %>%
  colformat_double(j = 2:3, digits = 1) %>%
  colformat_double(i = 8, j = 2:3, digits = 0) %>%
  hline(i = c(5,7), border = fp_border(color = "gray")) %>%
  bold(bold = TRUE, part = "header")

# * * * 3.1.12.1. Time series - Entire data set ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Proxy Clock Starts at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Non-Admitted Proxy Clock Starts'
#b_interactive = FALSE
plt_nonadm_proxy_starts_ts_entire <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                                str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.12.2. Time series - Selected time period ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Proxy Clock Starts at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_proxy_starts_ts_selected <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                        Period >= end_date - selected_period &
                                                                                        Period <= end_date),
                                                  str_title, str_x_axis, str_y_axis, b_interactive)
# * * * 3.1.12.3. Histogram - Entire data set ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Proxy Clock Starts at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Non-Admitted Proxy Clock Starts'
str_y_axis <- 'Frequency'
#b_interactive = TRUE
plt_nonadm_proxy_starts_hs_entire <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                               str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.12.4. Histogram - Selected time period ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Porxy Clock Starts at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_proxy_starts_hs_selected <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                  Period >= end_date - selected_period &
                                                                                  Period <= end_date),
                                                 str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.12.5. Output RObject ----
save(list = c('plt_nonadm_proxy_starts_ts_entire', 'plt_nonadm_proxy_starts_ts_selected', 
              'plt_nonadm_proxy_starts_hs_entire', 'plt_nonadm_proxy_starts_hs_selected',
              'ftbl_nonadm_proxy_starts'),
     file = paste0(output_dir, '/non_admitted_proxy_clock_starts.Robj'))

# * * 3.1.13. Non-admitted Proxy clock starts adjusted for working days ----
df_plot_data <- df_proxy_data %>% 
  mutate(Grand_Total = NonAdmProxyAdds) %>%
  left_join(df_monthly_working_days, by = c('Period' = 'month')) %>%
  mutate(Grand_Total = Grand_Total / working_days * 21)

df_table_data <- df_plot_data %>% 
  dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
  reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                    Mean = mean(x = Grand_Total),
                    Std.Dev. = sd(x = Grand_Total),
                    n = n())) %>%
  mutate(Statistic = names(value),
         period = paste0(format(start_date, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
  select(period, Statistic, value) %>%
  bind_rows(
    df_plot_data %>% 
      dplyr::filter(Treatment_Function_Code == selected_tfc & 
                      Period >= end_date - selected_period &
                      Period <= end_date) %>% 
      reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1)),
                        Mean = mean(x = Grand_Total),
                        Std.Dev. = sd(x = Grand_Total),
                        n = n())) %>%
      mutate(Statistic = names(value),
             period = paste0(format(end_date - selected_period, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
      select(period, value, Statistic)
  ) %>%
  pivot_wider(names_from = period, values_from = value)

ftbl_nonadm_proxy_starts_wda <- flextable(data = df_table_data) %>%
  theme_booktabs() %>%
  add_header_row(top = TRUE, values = 'Statistics for Non-Admitted Proxy Clock Starts Adjusted for Working Days and Standardised to a 21 Working Day Month',colwidth = 3) %>%
  align(i = 1, part = 'header', align = 'center') %>%
  set_table_properties(layout = "autofit", width = .9) %>%
  colformat_double(j = 2:3, digits = 1) %>%
  colformat_double(i = 8, j = 2:3, digits = 0) %>%
  hline(i = c(5,7), border = fp_border(color = "gray")) %>%
  bold(bold = TRUE, part = "header")

# * * * 3.1.13.1. Time series - Entire data set ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Proxy Clock Starts Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Non-Admitted Proxy Clock Starts (Working Day Adjusted)'
#b_interactive = FALSE
plt_nonadm_proxy_starts_wda_ts_entire <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                                    str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.13.2. Time series - Selected time period ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Proxy Clock Starts Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_proxy_starts_wda_ts_selected <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                            Period >= end_date - selected_period &
                                                                                            Period <= end_date),
                                                      str_title, str_x_axis, str_y_axis, b_interactive)
# * * * 3.1.13.3. Histogram - Entire data set ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Proxy Clock Starts Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Non-Admitted Proxy Clock Starts (Working Day Adjusted)'
str_y_axis <- 'Frequency'
#b_interactive = TRUE
plt_nonadm_proxy_starts_wda_hs_entire <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                                   str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.13.4. Histogram - Selected time period ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Proxy Clock Starts Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_proxy_starts_wda_hs_selected <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                      Period >= end_date - selected_period &
                                                                                      Period <= end_date),
                                                     str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.13.5. Output RObject ----
save(list = c('plt_nonadm_proxy_starts_wda_ts_entire', 'plt_nonadm_proxy_starts_wda_ts_selected', 
              'plt_nonadm_proxy_starts_wda_hs_entire', 'plt_nonadm_proxy_starts_wda_hs_selected',
              'ftbl_nonadm_proxy_starts_wda'),
     file = paste0(output_dir, '/non_admitted_proxy_clock_starts_wda.Robj'))

# * * 3.1.14. Non-admitted clock stops ----
df_plot_data <- df_proxy_data %>% mutate(Grand_Total = NonAdmProxyRems)

df_table_data <- df_plot_data %>% 
  dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
  reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1), na.rm = TRUE),
                    Mean = mean(x = Grand_Total, na.rm = TRUE),
                    Std.Dev. = sd(x = Grand_Total, na.rm = TRUE),
                    n = n())) %>%
  mutate(Statistic = names(value),
         period = paste0(format(start_date, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
  select(period, Statistic, value) %>%
  bind_rows(
    df_plot_data %>% 
      dplyr::filter(Treatment_Function_Code == selected_tfc & 
                      Period >= end_date - selected_period &
                      Period <= end_date) %>% 
      reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1), na.rm = TRUE),
                        Mean = mean(x = Grand_Total, na.rm = TRUE),
                        Std.Dev. = sd(x = Grand_Total, na.rm = TRUE),
                        n = n())) %>%
      mutate(Statistic = names(value),
             period = paste0(format(end_date - selected_period, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
      select(period, value, Statistic)
  ) %>%
  pivot_wider(names_from = period, values_from = value)

ftbl_nonadm_proxy_stops <- flextable(data = df_table_data) %>%
  theme_booktabs() %>%
  add_header_row(top = TRUE, values = 'Statistics for Non-Admitted Proxy Clock Stops',colwidth = 3) %>%
  align(i = 1, part = 'header', align = 'center') %>%
  set_table_properties(layout = "autofit", width = .9) %>%
  colformat_double(j = 2:3, digits = 1) %>%
  colformat_double(i = 8, j = 2:3, digits = 0) %>%
  hline(i = c(5,7), border = fp_border(color = "gray")) %>%
  bold(bold = TRUE, part = "header")

# * * * 3.1.14.1. Time series - Entire data set ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Proxy Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Non-Admitted Proxy Clock Stops'
#b_interactive = FALSE
plt_nonadm_proxy_stops_ts_entire <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                               str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.14.2. Time series - Selected time period ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Proxy Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_proxy_stops_ts_selected <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                       Period >= end_date - selected_period &
                                                                                       Period <= end_date),
                                                 str_title, str_x_axis, str_y_axis, b_interactive)
# * * * 3.1.14.3. Histogram - Entire data set ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Proxy Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Non-Admitted Proxy Clock Stops'
str_y_axis <- 'Frequency'
#b_interactive = TRUE
plt_nonadm_proxy_stops_hs_entire <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                              str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.14.4. Histogram - Selected time period ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Proxy Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_proxy_stops_hs_selected <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                 Period >= end_date - selected_period &
                                                                                 Period <= end_date),
                                                str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.14.5. Output RObject ----
save(list = c('plt_nonadm_proxy_stops_ts_entire', 'plt_nonadm_proxy_stops_ts_selected', 
              'plt_nonadm_proxy_stops_hs_entire', 'plt_nonadm_proxy_stops_hs_selected',
              'ftbl_nonadm_proxy_stops'),
     file = paste0(output_dir, '/non_admitted_proxy_clock_stops.Robj'))

# * * 3.1.15. Non-admitted proxy clock stops adjusted for working days ----
df_plot_data <- df_proxy_data %>% 
  mutate(Grand_Total = NonAdmProxyRems) %>%
  left_join(df_monthly_working_days, by = c('Period' = 'month')) %>%
  mutate(Grand_Total = Grand_Total / working_days * 21)

df_table_data <- df_plot_data %>% 
  dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
  reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1), na.rm = TRUE),
                    Mean = mean(x = Grand_Total, na.rm = TRUE),
                    Std.Dev. = sd(x = Grand_Total, na.rm = TRUE),
                    n = n())) %>%
  mutate(Statistic = names(value),
         period = paste0(format(start_date, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
  select(period, Statistic, value) %>%
  bind_rows(
    df_plot_data %>% 
      dplyr::filter(Treatment_Function_Code == selected_tfc & 
                      Period >= end_date - selected_period &
                      Period <= end_date) %>% 
      reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1), na.rm = TRUE),
                        Mean = mean(x = Grand_Total, na.rm = TRUE),
                        Std.Dev. = sd(x = Grand_Total, na.rm = TRUE),
                        n = n())) %>%
      mutate(Statistic = names(value),
             period = paste0(format(end_date - selected_period, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
      select(period, value, Statistic)
  ) %>%
  pivot_wider(names_from = period, values_from = value)

ftbl_nonadm_proxy_stops_wda <- flextable(data = df_table_data) %>%
  theme_booktabs() %>%
  add_header_row(top = TRUE, values = 'Statistics for Non-Admitted Proxy Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month',colwidth = 3) %>%
  align(i = 1, part = 'header', align = 'center') %>%
  set_table_properties(layout = "autofit", width = .9) %>%
  colformat_double(j = 2:3, digits = 1) %>%
  colformat_double(i = 8, j = 2:3, digits = 0) %>%
  hline(i = c(5,7), border = fp_border(color = "gray")) %>%
  bold(bold = TRUE, part = "header")


# * * * 3.1.15.1. Time series - Entire data set ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Proxy Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Non-Admitted Proxy Clock Stops (Working Day Adjusted)'
#b_interactive = FALSE
plt_nonadm_proxy_stops_wda_ts_entire <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                                   str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.15.2. Time series - Selected time period ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Non-Admitted Proxy Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_proxy_stops_wda_ts_selected <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                           Period >= end_date - selected_period &
                                                                                           Period <= end_date),
                                                     str_title, str_x_axis, str_y_axis, b_interactive)
# * * * 3.1.15.3. Histogram - Entire data set ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Proxy Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Non-Admitted Proxy Clock Stops (Working Day Adjusted)'
str_y_axis <- 'Frequency'
#b_interactive = TRUE
plt_nonadm_proxy_stops_wda_hs_entire <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                                  str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.15.4. Histogram - Selected time period ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Non-Admitted Proxy Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_nonadm_proxy_stops_wda_hs_selected <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                     Period >= end_date - selected_period &
                                                                                     Period <= end_date),
                                                    str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.15.5. Output RObject ----
save(list = c('plt_nonadm_proxy_stops_wda_ts_entire', 'plt_nonadm_proxy_stops_wda_ts_selected', 
              'plt_nonadm_proxy_stops_wda_hs_entire', 'plt_nonadm_proxy_stops_wda_hs_selected',
              'ftbl_nonadm_proxy_stops_wda'),
     file = paste0(output_dir, '/non_admitted_proxy_clock_stops_wda.Robj'))

# * * 3.1.16. Admitted proxy clock stops ----
df_plot_data <- df_proxy_data %>% mutate(Grand_Total = AdmProxyRems)

df_table_data <- df_plot_data %>% 
  dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
  reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1), na.rm = TRUE),
                    Mean = mean(x = Grand_Total, na.rm = TRUE),
                    Std.Dev. = sd(x = Grand_Total, na.rm = TRUE),
                    n = n())) %>%
  mutate(Statistic = names(value),
         period = paste0(format(start_date, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
  select(period, Statistic, value) %>%
  bind_rows(
    df_plot_data %>% 
      dplyr::filter(Treatment_Function_Code == selected_tfc & 
                      Period >= end_date - selected_period &
                      Period <= end_date) %>% 
      reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1), na.rm = TRUE),
                        Mean = mean(x = Grand_Total, na.rm = TRUE),
                        Std.Dev. = sd(x = Grand_Total, na.rm = TRUE),
                        n = n())) %>%
      mutate(Statistic = names(value),
             period = paste0(format(end_date - selected_period, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
      select(period, value, Statistic)
  ) %>%
  pivot_wider(names_from = period, values_from = value)

ftbl_adm_proxy_stops <- flextable(data = df_table_data) %>%
  theme_booktabs() %>%
  add_header_row(top = TRUE, values = 'Statistics for Admitted Proxy Clock Stops',colwidth = 3) %>%
  align(i = 1, part = 'header', align = 'center') %>%
  set_table_properties(layout = "autofit", width = .9) %>%
  colformat_double(j = 2:3, digits = 1) %>%
  colformat_double(i = 8, j = 2:3, digits = 0) %>%
  hline(i = c(5,7), border = fp_border(color = "gray")) %>%
  bold(bold = TRUE, part = "header")

# * * * 3.1.16.1. Time series - Entire data set ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Admitted Proxy Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Admitted Proxy Clock Stops'
#b_interactive = FALSE
plt_adm_proxy_stops_ts_entire <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                            str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.16.2. Time series - Selected time period ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Admitted Proxy Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_adm_proxy_stops_ts_selected <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                    Period >= end_date - selected_period &
                                                                                    Period <= end_date),
                                              str_title, str_x_axis, str_y_axis, b_interactive)
# * * * 3.1.16.3. Histogram - Entire data set ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Admitted Proxy Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Admitted Proxy Clock Stops'
str_y_axis <- 'Frequency'
#b_interactive = TRUE
plt_adm_proxy_stops_hs_entire <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                           str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.16.4. Histogram - Selected time period ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Admitted Proxy Clock Stops at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_adm_proxy_stops_hs_selected <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                              Period >= end_date - selected_period &
                                                                              Period <= end_date),
                                             str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.16.5. Output RObject ----
save(list = c('plt_adm_proxy_stops_ts_entire', 'plt_adm_proxy_stops_ts_selected', 
              'plt_adm_proxy_stops_hs_entire', 'plt_adm_proxy_stops_hs_selected',
              'ftbl_adm_proxy_stops'),
     file = paste0(output_dir, '/admitted_proxy_clock_stops.Robj'))

# * * 3.1.17. Admitted proxy clock stops adjusted for working days ----
df_plot_data <- df_proxy_data %>% 
  mutate(Grand_Total = AdmProxyRems) %>%
  left_join(df_monthly_working_days, by = c('Period' = 'month')) %>%
  mutate(Grand_Total = Grand_Total / working_days * 21)

df_table_data <- df_plot_data %>% 
  dplyr::filter(Treatment_Function_Code == selected_tfc) %>% 
  reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1), na.rm = TRUE),
                    Mean = mean(x = Grand_Total, na.rm = TRUE),
                    Std.Dev. = sd(x = Grand_Total, na.rm = TRUE),
                    n = n())) %>%
  mutate(Statistic = names(value),
         period = paste0(format(start_date, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
  select(period, Statistic, value) %>%
  bind_rows(
    df_plot_data %>% 
      dplyr::filter(Treatment_Function_Code == selected_tfc & 
                      Period >= end_date - selected_period &
                      Period <= end_date) %>% 
      reframe(value = c(quantile(x = Grand_Total, probs = c(0, .25, .5, .75, 1), na.rm = TRUE),
                        Mean = mean(x = Grand_Total, na.rm = TRUE),
                        Std.Dev. = sd(x = Grand_Total, na.rm = TRUE),
                        n = n())) %>%
      mutate(Statistic = names(value),
             period = paste0(format(end_date - selected_period, '%b-%y'), ' to ', format(end_date, '%b-%y'))) %>%
      select(period, value, Statistic)
  ) %>%
  pivot_wider(names_from = period, values_from = value)

ftbl_adm_proxy_stops_wda <- flextable(data = df_table_data) %>%
  theme_booktabs() %>%
  add_header_row(top = TRUE, values = 'Statistics for Admitted Proxy Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month',colwidth = 3) %>%
  align(i = 1, part = 'header', align = 'center') %>%
  set_table_properties(layout = "autofit", width = .9) %>%
  colformat_double(j = 2:3, digits = 1) %>%
  colformat_double(i = 8, j = 2:3, digits = 0) %>%
  hline(i = c(5,7), border = fp_border(color = "gray")) %>%
  bold(bold = TRUE, part = "header")

# * * * 3.1.17.1. Time series - Entire data set ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Admitted Proxy Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month'
str_y_axis <- 'Admitted Proxy Clock Stops (Working Day Adjusted)'
#b_interactive = FALSE
plt_adm_proxy_stops_wda_ts_entire <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                                str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.17.2. Time series - Selected time period ----
str_title <- str_wrap(sprintf('Time Series of %s (%s) Admitted Proxy Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_adm_proxy_stops_wda_ts_selected <- fnTimeseriesPlot(df = df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                        Period >= end_date - selected_period &
                                                                                        Period <= end_date),
                                                  str_title, str_x_axis, str_y_axis, b_interactive)
# * * * 3.1.17.3. Histogram - Entire data set ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Admitted Proxy Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Admitted Proxy Clock Stops (Working Day Adjusted)'
str_y_axis <- 'Frequency'
#b_interactive = TRUE
plt_adm_proxy_stops_wda_hs_entire <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc),
                                               str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.17.4. Histogram - Selected time period ----
str_title <- str_wrap(sprintf('Histogram of %s (%s) Admitted Proxy Clock Stops Adjusted for Working Days and Standardised to a 21 Working Day Month at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(end_date - selected_period, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
plt_adm_proxy_stops_wda_hs_selected <- fnHistogramPlot(df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc &
                                                                                  Period >= end_date - selected_period &
                                                                                  Period <= end_date),
                                                 str_title, str_x_axis, str_y_axis, b_interactive)

# * * * 3.1.17.5. Output RObject ----
save(list = c('plt_adm_proxy_stops_wda_ts_entire', 'plt_adm_proxy_stops_wda_ts_selected', 
              'plt_adm_proxy_stops_wda_hs_entire', 'plt_adm_proxy_stops_wda_hs_selected',
              'ftbl_adm_proxy_stops_wda'),
     file = paste0(output_dir, '/admitted_proxy_clock_stops_wda.Robj'))

# * * 3.1.18. Non-Admitted maximum sustainable list size ----
df_plot_data <- df_nonadm_wl %>% 
  mutate(Achievement = Wk00_18 / Grand_Total, 
         Standard = factor(if_else(Achievement >= 0.92, 'Achieved', 'Failed'), levels = c('Achieved', 'Failed')))

# * * * 3.1.18.1. Non-Admitted maximum sustainable list size scatter plot ----
df <- df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc)
str_title <- str_wrap(sprintf('Scatterplot of Non-Admitted Waiting List against 92%% Standard Achievement of %s (%s) at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Non-Admitted Waiting List Size'
str_y_axis <- '92% Standard Achievement'
#b_interactive <- FALSE

if(!b_interactive){
  plt <- ggplot() %+%
    theme_bw(base_size = 12) %+%
    theme(plot.title = element_text(hjust = .5)) %+%
    labs(title = str_title, x = str_x_axis, y = str_y_axis) %+%
    guides(fill = guide_legend(title = '92% Standard')) %+%
    geom_point(data = df, aes(x = Grand_Total, y = Achievement, fill = Standard), shape = 21, size = 4, color = 'black') %+%
    scale_fill_manual(values = c('Achieved' = '#005EB8CE', 'Failed' = '#8A1538CE'), drop = FALSE) %+%
    scale_y_continuous(labels = scales::percent)
} else {
  plt <- plotly::plot_ly() %>%
    add_trace(data = df %>% dplyr::filter(Standard == 'Achieved'),
              type = 'scatter',
              mode = 'markers',
              name = 'Achieved',
              x = ~Grand_Total,
              y = ~Achievement,
              size = 4,
              marker = list(color = '#005EB8')) %>%
    add_trace(data = df %>% dplyr::filter(Standard == 'Failed'),
              type = 'scatter',
              mode = 'markers',
              name = 'Failed',
              x = ~Grand_Total,
              y = ~Achievement,
              size = 4, 
              marker = list(color = '#8A1538')) %>%
    layout(title =str_title,
           xaxis = list(title = str_x_axis),
           yaxis = list(title = str_y_axis,
                        tickformat = '.0%'))
}
plt_nonadm_max_wl_size_scatter <- plt

# * * * 3.1.18.2. Non-Admitted maximum sustainable list size time series ----
str_title <- str_wrap(sprintf('Time Series of Non-Admitted Waiting List against 92%% Standard Achievement of %s (%s) at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month' 
str_y_axis <- 'Non-Admitted Waiting List Size'
#b_interactive <- TRUE

if(!b_interactive){
  plt <- ggplot() %+%
    theme_bw(base_size = 12) %+%
    theme(plot.title = element_text(hjust = .5)) %+%
    labs(title = str_title, x = str_x_axis, y = str_y_axis) %+%
    guides(fill = guide_legend(title = '92% Standard')) %+%
    geom_point(data = df, aes(x = Period, y = Grand_Total, fill = Standard), shape = 21, size = 4, color = 'black') %+%
    scale_fill_manual(values = c('Achieved' = '#005EB8CE', 'Failed' = '#8A1538CE'), drop = FALSE)
} else {
  plt <- plotly::plot_ly() %>%
    add_trace(data = df %>% dplyr::filter(Standard == 'Achieved'),
              type = 'scatter',
              mode = 'markers',
              name = 'Achieved',
              x = ~Period,
              y = ~Grand_Total,
              text = ~paste("Achievement: ", round(Achievement*100, 1), '%'),
              size = 4,
              marker = list(color = '#005EB8')) %>%
    add_trace(data = df %>% dplyr::filter(Standard == 'Failed'),
              type = 'scatter',
              mode = 'markers',
              name = 'Failed',
              x = ~Period,
              y = ~Grand_Total,
              text = ~paste("Achievement: ", round(Achievement*100, 1), '%'),
              size = 4, 
              marker = list(color = '#8A1538')) %>%
    layout(title =str_title,
           xaxis = list(title = str_x_axis,
                        dtick = "M6", tickformat='%b-%y'),
           yaxis = list(title = str_y_axis))
}
plt_nonadm_max_wl_size_ts <- plt

# * * * 3.1.18.3. Output RObject ----
save(list = c('plt_nonadm_max_wl_size_scatter', 'plt_nonadm_max_wl_size_ts'),
     file = paste0(output_dir, '/non_admitted_max_list_size.Robj'))

# * * 3.1.19. Admitted maximum sustainable list size ----
df_plot_data <- df_adm_wl %>% 
  mutate(Achievement = Wk00_18 / Grand_Total, 
         Standard = factor(if_else(Achievement >= 0.92, 'Achieved', 'Failed'), levels = c('Achieved', 'Failed')))

# * * * 3.1.19.1. Admitted maximum sustainable list size scatter plot ----
df <- df_plot_data %>% dplyr::filter(Treatment_Function_Code == selected_tfc)
str_title <- str_wrap(sprintf('Scatterplot of Admitted Waiting List against 92%% Standard Achievement of %s (%s) at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Admitted Waiting List Size'
str_y_axis <- '92% Standard Achievement'
#b_interactive <- FALSE

if(!b_interactive){
  plt <- ggplot() %+%
    theme_bw(base_size = 12) %+%
    theme(plot.title = element_text(hjust = .5)) %+%
    labs(title = str_title, x = str_x_axis, y = str_y_axis) %+%
    guides(fill = guide_legend(title = '92% Standard')) %+%
    geom_point(data = df, aes(x = Grand_Total, y = Achievement, fill = Standard), shape = 21, size = 4, color = 'black') %+%
    scale_fill_manual(values = c('Achieved' = '#005EB8CE', 'Failed' = '#8A1538CE'), drop = FALSE) %+%
    scale_y_continuous(labels = scales::percent)
} else {
  plt <- plotly::plot_ly() %>%
    add_trace(data = df %>% dplyr::filter(Standard == 'Achieved'),
              type = 'scatter',
              mode = 'markers',
              name = 'Achieved',
              x = ~Grand_Total,
              y = ~Achievement,
              size = 4,
              marker = list(color = '#005EB8')) %>%
    add_trace(data = df %>% dplyr::filter(Standard == 'Failed'),
              type = 'scatter',
              mode = 'markers',
              name = 'Failed',
              x = ~Grand_Total,
              y = ~Achievement,
              size = 4, 
              marker = list(color = '#8A1538')) %>%
    layout(title =str_title,
           xaxis = list(title = str_x_axis),
           yaxis = list(title = str_y_axis,
                        tickformat = '.0%'))
}
plt_adm_max_wl_size_scatter <- plt

# * * * 3.1.18.2. Admitted maximum sustainable list size time series ----
str_title <- str_wrap(sprintf('Time Series of Admitted Waiting List against 92%% Standard Achievement of %s (%s) at %s for the Period %s to %s',
                              df_tfc_lu$Treatment_Function_Name[df_tfc_lu$Treatment_Function_Code==selected_tfc],
                              selected_tfc, organisation_name, format(start_date, '%b-%y'), format(end_date, '%b-%y')),
                      width = 100)
str_x_axis <- 'Month' 
str_y_axis <- 'Admitted Waiting List Size'
#b_interactive <- TRUE

if(!b_interactive){
  plt <- ggplot() %+%
    theme_bw(base_size = 12) %+%
    theme(plot.title = element_text(hjust = .5)) %+%
    labs(title = str_title, x = str_x_axis, y = str_y_axis) %+%
    guides(fill = guide_legend(title = '92% Standard')) %+%
    geom_point(data = df, aes(x = Period, y = Grand_Total, fill = Standard), shape = 21, size = 4, color = 'black') %+%
    scale_fill_manual(values = c('Achieved' = '#005EB8CE', 'Failed' = '#8A1538CE'), drop = FALSE)
} else {
  plt <- plotly::plot_ly() %>%
    add_trace(data = df %>% dplyr::filter(Standard == 'Achieved'),
              type = 'scatter',
              mode = 'markers',
              name = 'Achieved',
              x = ~Period,
              y = ~Grand_Total,
              text = ~paste("Achievement: ", round(Achievement*100, 1), '%'),
              size = 4,
              marker = list(color = '#005EB8')) %>%
    add_trace(data = df %>% dplyr::filter(Standard == 'Failed'),
              type = 'scatter',
              mode = 'markers',
              name = 'Failed',
              x = ~Period,
              y = ~Grand_Total,
              text = ~paste("Achievement: ", round(Achievement*100, 1), '%'),
              size = 4, 
              marker = list(color = '#8A1538')) %>%
    layout(title =str_title,
           xaxis = list(title = str_x_axis,
                        dtick = "M6", tickformat='%b-%y'),
           yaxis = list(title = str_y_axis))
}
plt_adm_max_wl_size_ts <- plt

# * * * 3.1.18.3. Output RObject ----
save(list = c('plt_adm_max_wl_size_scatter', 'plt_adm_max_wl_size_ts'),
     file = paste0(output_dir, '/admitted_max_list_size.Robj'))
