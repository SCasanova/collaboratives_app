library(tidyverse)
library(personalfix)


tuition <- read_csv('FY22 Rates.csv') %>% 
  janitor::clean_names() %>% 
  mutate(across(per_diem_member:total_tuition_non_member, fix_num)) %>% 
  group_by(collaborative) %>% 
  summarise(per_diem_in_district = mean(per_diem_member, na.rm = T),
            school_year_in_district = mean(school_year_member_180_days, na.rm = T),
            summer_in_district = mean(summer_member, na.rm = T),
            total_tuition_in_district = mean(total_tuition_member, na.rm = T),
            per_diem_out_district = mean(per_diem_non_member, na.rm = T),
            school_year_out_district = mean(school_year_non_member_180_days, na.rm = T),
            summer_out_district = mean(summer_non_member, na.rm = T),
            total_tuition_out_district = mean(total_tuition_non_member, na.rm = T),
            summer_days= mean(summer_days, na.rm = T)
            ) %>% 
  dplyr::slice(1:15) %>% 
  ungroup() %>% 
  mutate(across(.fns = function(x){ifelse(is.nan(x), NA, x)})) %>% 
  rowwise() %>% 
  mutate(total_tuition_in_district =sum(school_year_in_district,summer_in_district, na.rm = T),
         total_tuition_out_district = sum(school_year_out_district,summer_out_district))

all_tuition <- read_csv('FY22 Rates.csv') %>% 
  janitor::clean_names() %>% 
  mutate(across(per_diem_member:total_tuition_non_member, fix_num)) %>% 
  filter(!is.na(grade_level)) %>% 
  group_by(collaborative, grade_level) %>% 
  summarise(per_diem_in_district = mean(per_diem_member, na.rm = T),
            school_year_in_district = mean(school_year_member_180_days, na.rm = T),
            summer_in_district = mean(summer_member, na.rm = T),
            total_tuition_in_district = mean(total_tuition_member, na.rm = T),
            per_diem_out_district = mean(per_diem_non_member, na.rm = T),
            school_year_out_district = mean(school_year_non_member_180_days, na.rm = T),
            summer_out_district = mean(summer_non_member, na.rm = T),
            total_tuition_out_district = mean(total_tuition_non_member, na.rm = T)
            ) %>% 
  ungroup() %>% 
  mutate(across(.fns = function(x){ifelse(is.nan(x), NA, x)})) %>% 
  rowwise() %>% 
  mutate(total_tuition_in_district =sum(school_year_in_district,summer_in_district, na.rm = T),
         total_tuition_out_district = sum(school_year_out_district,summer_out_district)) %>% 
  pivot_wider(values_from =  per_diem_in_district:total_tuition_out_district, names_from = grade_level) %>% 
  janitor::clean_names()
  


para_salaries <- read_csv('collaborative_conditions.csv') %>% 
  janitor::clean_names() %>% 
  select(collaborative, work_year:vacation_days, health_pct) %>% 
  dplyr::slice(1:17)


director_details <- read_csv('collaborative_directors.csv') %>% 
  janitor::clean_names() %>% 
  dplyr::select(collaborative,additional_compensation,number_of_years_as_exec_dir:percent_med,board_of_directors)

fy2022_data <- read_csv('Collaboratives - FY22.csv') %>% 
  janitor::clean_names() %>% 
  select(-c(which_union_prof,which_union_non_rof, board_of_directors))

fy2022_salaries <- read_csv('FY22 Salaries.csv') %>% 
  janitor::clean_names() %>% 
  select(collaborative, bachelors_step_1:ph_d_top_step) %>% 
  mutate(across(.fns = function(x)ifelse(x == 0, NA, x)),
         lower_right = pmax(masters_top_step, masters_15_top_step, masters_30_top_step, cags_top_step,ph_d_top_step, na.rm = T ))

fy2022_financials <- read_csv('collaborative_financials.csv') %>% 
  janitor::clean_names() %>% 
  select(-c(collaborative_region))

enrollment <- read_csv('Enrollment.csv') %>% 
  janitor::clean_names() %>% 
  rename(collaborative = collaborative_name)

diversity <- read_csv('Enrollment_Diversity.csv') %>% 
  janitor::clean_names() %>% 
  rename(collaborative = collaborative_name)


collaborative_data <- fy2022_data %>% 
  full_join(fy2022_financials, by = 'collaborative') %>% 
  left_join(tuition, by = 'collaborative') %>% 
  left_join(all_tuition, by = 'collaborative') %>% 
  left_join(para_salaries, by = 'collaborative') %>% 
  left_join(fy2022_salaries, by = 'collaborative') %>% 
  left_join(director_details, by = 'collaborative') %>% 
  left_join(enrollment, by = 'collaborative') %>% 
  left_join(diversity, by = 'collaborative')


write_csv(collaborative_data, 'collaborative_data.csv') 


read_csv('FY23 private.csv') %>%
  janitor::clean_names() %>% 
  mutate(coll = ifelse( school != 'CREST', 'Private', 'Collaborative')) %>% 
  group_by(coll) %>% 
  summarise(across(autism_intensive:learning_disabilities, ~ mean(.x, na.rm = TRUE))) %>% 
  t() %>% 
  janitor::row_to_names(1) %>% 
  data.frame() %>% 
  mutate(across(Collaborative:Private, fix_num)) %>% 
  write_csv('private_daily_rates.csv')
 
