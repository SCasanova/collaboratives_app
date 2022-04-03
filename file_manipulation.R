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
  select(collaborative, work_year:vacation_days) %>% 
  dplyr::slice(1:17)


director_salaries <- read_csv('collaborative_directors.csv') %>% 
  janitor::clean_names() %>% 
  dplyr::select(collaborative,fy20_base_salary:additional_compensation,number_of_years_as_exec_dir:x25)

fy2022_data <- read_csv('Collaboratives - FY22.csv') %>% 
  janitor::clean_names() %>% 
  select(-c(which_union_prof,which_union_non_rof, board_of_directors))

fy2022_salaries <- read_csv('FY22 Salaries.csv') %>% 
  janitor::clean_names() %>% 
  select(collaborative, bachelors_step_1:ph_d_top_step)

fy2022_salaries$lower_right <- pmax(fy2022_salaries$masters_top_step,
                                    fy2022_salaries$masters_15_top_step ,
                                    fy2022_salaries$masters_30_top_step ,
                                    fy2022_salaries$cags_top_step ,
                                    fy2022_salaries$ph_d_top_step)

fy2022_financials <- read_csv('collaborative_financials.csv') %>% 
  janitor::clean_names() %>% 
  select(-c(collaborative_region))


collaborative_data <- fy2022_data %>% 
  full_join(fy2022_financials, by = 'collaborative') %>% 
  left_join(tuition, by = 'collaborative') %>% 
  left_join(all_tuition, by = 'collaborative') %>% 
  left_join(para_salaries, by = 'collaborative') %>% 
  left_join(fy2022_salaries, by = 'collaborative')


write_csv(collaborative_data, 'collaborative_data.csv')  
 
