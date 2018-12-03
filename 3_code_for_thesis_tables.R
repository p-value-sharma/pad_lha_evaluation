# packages
library(purrr)
library(dplyr)
library(tidyr)
library(broom.mixed)

# importing processed data  ####
hcc_denom_pre <- read.csv(here::here('intermediates/hcc_denom_pre.csv')) %>% 
  mutate(date = as.character(date),
         date = as.Date(date, format = '%Y-%m-%d'))

geographicLHApopn <- read.csv(here::here('intermediates', 'geographicLHApopn.csv'))

pharmanetData_cleaned <- read.csv(here::here('intermediates', 'pharmanetData_cleaned.csv')) %>% 
  left_join(., geographicLHApopn %>% 
              select(LHA_NUM, geographic), by = 'LHA_NUM') %>% 
  mutate(geographic = Hmisc::capitalize(as.character(geographic)),
         geographic = factor(geographic, levels = c('Remote', 'Rural', 'Urban/rural', 'Metro')),
         coverageLevel = factor(coverageLevel, levels = c('Low', 'Medium', 'High')))


UTIlinked_data <- read.csv(here::here('intermediates', 'UTIlinked_data.csv')) %>% 
  mutate(coverageLevel = factor(coverageLevel, levels = c('Low', 'Medium', 'High')))

multileveldata2 <- read.csv(here::here('intermediates', 'multileveldata2.csv'))


sensitivity_supp_table <- readRDS(file = here::here('intermediates', 
                                                    'sensitivity_supp_table.RDS'))

chron_time_nb2_model <- readRDS(file = here::here('intermediates', 'DOS_mlm.RDS'))

model_outputs_list <- readRDS(file = here::here('data/intermediates', 'bs_glmmTMB_final_10000.rds'))

data_for_linear_regression <- read.csv(file = here::here('intermediates', 'data_for_linear_regression.csv')) %>% 
  mutate(coverageLevel = factor(coverageLevel, levels = c('Low', 'Medium', 'High')),
         date = as.Date(as.character(date), format = '%Y-%m-%d')) %>% 
  arrange(coverageLevel)

DOS_linear_reg <- readRDS(file = here::here('intermediates', 'DOS_linear_reg.RDS'))

# defined functions ####

# enclose number with brackets 
encl_brackets <- function(x) {
  paste0('(', x, ')')
}

indent_function <- function(indent, x) {
  if(indent == 'subindent') {
    paste0('        ', x)
  }
  else {
    paste0('    ', x)
  }
}


top_characteristic_pad <- function(x, y) {
  bind_rows(data.frame(characteristic = as.character(x), Low = ' ', Medium = ' ', High = ' ', Total = ' '), y) 
}

# Table 1 - Geographic  ####
# LEFT number of lhas by size spread by intervention group
geographic_pre <- pharmanetData_cleaned %>% 
  filter(!is.na(coverageLevel)) %>% 
  group_by(coverageLevel, geographic) %>% 
  distinct(LHA_NUM) %>% 
  count() %>% 
  ungroup() %>% 
  complete(geographic,coverageLevel, fill = list(n = 0)) %>% 
  mutate(coverageLevel = factor(coverageLevel, levels = c('Low', 'Medium', 'High')))


geog_left <- geographic_pre %>% 
  group_by(coverageLevel) %>% 
  mutate(n_coverageLevel = sum(n)) %>% 
  ungroup() %>% 
  mutate(prop = encl_brackets(formatC(100*n/n_coverageLevel, digits = 1, format = 'f')),
         n = paste(n, prop, sep = ' ')) %>% 
  select(geographic:n) %>% 
  spread(coverageLevel, n)


geog_bottom <- geographic_pre %>% 
  group_by(coverageLevel) %>% 
  summarise(n = sum(n)) %>% 
  add_row(coverageLevel = 'Overall', n = sum(.$n)) %>% 
  mutate(n = paste(n, '(100.0)', sep = ' ')) %>% 
  spread(coverageLevel, n) %>% 
  mutate(geographic = 'Total LHAs') %>% 
  select(-Overall)
  
geographic_right <- geographic_pre %>% 
  group_by(geographic) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  add_row(geographic = 'Total LHAs', n = sum(.$n)) %>% 
  mutate(n_overall = last(n), 
         prop = encl_brackets(formatC(100*n/n_overall, digits = 1, format = 'f')),
         n = paste(n, prop, sep = ' ')) %>% 
  select(geographic:n) %>% 
  rename(Overall = n)

geographic_core <- geog_left %>% 
  bind_rows(geog_bottom) %>% 
  left_join(., geographic_right, by = 'geographic')


n_nursing_homes <- hcc_denom_pre %>%
  group_by(coverageLevel) %>%
  distinct(PROVIDER_ID) %>%
  count() %>%
  bind_rows(
    hcc_denom_pre %>%
      distinct(PROVIDER_ID) %>%
      count() %>%
      mutate(coverageLevel = 'Overall')) %>% 
  ungroup() %>% 
  mutate(coverageLevel = factor(coverageLevel, 
                                levels = c('Low', 'Medium', 'High', 'Overall'))) %>% 
  spread(coverageLevel, n) %>% 
  mutate_at(vars(Low, Medium, High, Overall), 
            funs(
              paste(., 
                    encl_brackets(formatC(100*./Overall, format = 'f', digits = 1))))) %>% 
  mutate(characteristic = 'Number of nursing homes') %>% 
  select(characteristic, everything())
  

nh_size_covLevel <- hcc_denom_pre %>%
  mutate(coverageLevel = factor(coverageLevel, levels = c('Low', 'Medium', 'High'))) %>% 
  group_by(coverageLevel, PROVIDER_ID) %>%
  summarise(max = max(CLIENT_COUNT)) %>% 
  mutate(size = cut(max, breaks = c(0, 29, 99, Inf),
                    labels = c('Small', 'Medium', 'Large')))  %>% 
  group_by(coverageLevel, size) %>% 
  count() 
  

# Table 1 - prescriber section    #####
uti_link_prescriber_pre <- UTIlinked_data %>% 
  filter(!is.na(coverageLevel)) %>% 
  mutate(coverageLevel = factor(coverageLevel, 
                                levels = c('Low', 'Medium', 'High'))) %>% 
  group_by(coverageLevel) %>% 
  distinct(PRAC_STUDY_ID) %>% 
  count() %>% 
  ungroup() %>% 
  add_row(coverageLevel = 'Overall', n = sum(.$n))


prescriberType_formatting <- data.frame(prescriber_type = as.character(unique(pharmanetData_cleaned$prescriber_type)),
           prescriber_type2 = c('Physician', 'Other','Nurse practitioner', 'Pharmacist', 'Other', 
                                'Other', 'Other'))


unique_linked_prescribers <- UTIlinked_data %>% 
  filter(!is.na(coverageLevel)) %>%
  mutate(coverageLevel = factor(coverageLevel, 
                                levels = c('Low', 'Medium', 'High'))) %>% 
  group_by(coverageLevel, prescriber_type) %>% 
  distinct(PRAC_STUDY_ID) %>% 
  count() %>% 
  # adding our prescriber categorization
  left_join(., prescriberType_formatting, by = 'prescriber_type') %>% 
  group_by(coverageLevel, prescriber_type2) %>% 
  summarise(n = sum(n)) %>% 
  # adding zeros for groups that didn't appear in a coveragegroup (consistency)
  complete(prescriber_type2, fill = list(n = 0)) %>% 
  group_by(prescriber_type2) %>% 
  # adding an overall for prescriberType 
  nest() %>% 
  mutate(data = map(data, ~ .x %>% 
                      add_row(coverageLevel = 'Overall', n = sum(.$n)))) %>% 
  unnest() %>% 
  # adding the totals to make proportions 
  left_join(., uti_link_prescriber_pre, by = 'coverageLevel') %>% 
  mutate(value = paste(formatC(n.x, format = 'd', big.mark = ','),
                       encl_brackets(formatC(100*n.x/n.y,
                                             format = 'f', digits = 1)),
                       sep = ' ')) %>% 
  select(prescriber_type2, coverageLevel, value) %>% 
  bind_rows(uti_link_prescriber_pre %>% 
              mutate(prescriber_type2 = 'Number of unique prescribers',
                     n = paste(formatC(n, format = 'd', big.mark = ','),
                               '(100.0)',
                               sep = ' ')) %>% 
              rename(value = n)) %>% 
  group_by(prescriber_type2) %>% 
  nest() %>% 
  mutate(data = map(data, ~.x %>% 
                      spread(coverageLevel, value))) %>% 
  unnest()
  
# Table 1 - Resident characteristics #####
residents_number_pre <- hcc_denom_pre %>% 
  group_by(coverageLevel, date) %>% 
  summarise(population = sum(CLIENT_COUNT)) %>%
  group_by(date) %>% 
  mutate(population_overall = sum(population)) 

residents_number_overall <- residents_number_pre %>% 
  distinct(date, population_overall) %>% 
  ungroup() %>% 
  summarise(population = mean(population_overall)) %>% 
  mutate(coverageLevel = 'Overall')

residents_number_core <- residents_number_pre %>% 
  group_by(coverageLevel) %>% 
  summarise(population = mean(population)) %>% 
  bind_rows(., residents_number_overall) 
  
# female gender 
female_residents_number_pre <- hcc_denom_pre %>% 
  filter(GENDER_ADJ == 'F') %>% 
  group_by(coverageLevel, date) %>% 
  summarise(population = sum(CLIENT_COUNT)) %>%
  group_by(date) %>% 
  mutate(population_overall = sum(population)) 

female_residents_number_overall <- female_residents_number_pre %>% 
  distinct(date, population_overall) %>% 
  ungroup() %>% 
  summarise(population = mean(population_overall)) %>% 
  mutate(coverageLevel = 'Overall')

female_residents_number_core <- female_residents_number_pre %>% 
  group_by(coverageLevel) %>% 
  summarise(population = mean(population)) %>% 
  bind_rows(., female_residents_number_overall) %>% 
  left_join(residents_number_core, by = 'coverageLevel') %>% 
  mutate(prop = 100*population.x/population.y,
         population.x = formatC(population.x, big.mark = ',', format = 'd'),
         prop = encl_brackets(formatC(prop, format = 'f', digits = 1)),
         population.x = paste(population.x, prop, sep = ' ')) %>% 
  select(coverageLevel, population.x) %>% 
  spread(coverageLevel, population.x) %>% 
  mutate(characteristic = 'Average number of female residents*') %>% 
  select(characteristic, everything())

residents_with_linked_rx <- UTIlinked_data %>%
  group_by(coverageLevel) %>% 
  distinct(moh_id) %>% 
  count() %>% 
  ungroup() %>% 
  add_row(coverageLevel = 'Overall', n = sum(.$n)) %>% 
  left_join(., residents_number_core, by = 'coverageLevel') %>% 
  mutate(coverageLevel = factor(coverageLevel, 
                                levels = c('Low', 'Medium', 'High', 'Overall')),
         n = paste(formatC(n, format = 'd', big.mark = ','),
                   encl_brackets(formatC(100*n/population,
                                         big.mark = ',',
                                         digits = 1,
                                         format = 'f')),
                   sep = ' '),
         characteristic = 'Unique residents with a UTI-linked prescription') %>% 
  select(coverageLevel,n, characteristic) %>% 
  spread(coverageLevel,n)


# age group
agegrp_residents_number_pre <- hcc_denom_pre %>% 
  group_by(coverageLevel, date, CUSTOM_AGE_GROUP) %>% 
  summarise(population = sum(CLIENT_COUNT)) %>%
  group_by(date, CUSTOM_AGE_GROUP) %>% 
  mutate(population_overall = sum(population)) 

agegrp_residents_number_overall <- agegrp_residents_number_pre %>% 
  group_by(CUSTOM_AGE_GROUP) %>% 
  distinct(date, population_overall) %>%
  summarise(population = mean(population_overall)) %>% 
  mutate(coverageLevel = 'Overall')

agegrp_residents_number_core <- agegrp_residents_number_pre %>% 
  group_by(coverageLevel, CUSTOM_AGE_GROUP) %>% 
  summarise(population = mean(population)) %>% 
  bind_rows(., agegrp_residents_number_overall) %>% 
  left_join(., residents_number_core, by = 'coverageLevel') %>% 
  mutate(prop = 100*population.x/population.y,
         population.x = formatC(population.x, big.mark = ',', format = 'd'),
         prop = encl_brackets(formatC(prop, format = 'f', digits = 1)),
         population.x = paste(population.x, prop, sep = ' ')) %>% 
  select(coverageLevel, population.x, CUSTOM_AGE_GROUP) %>%
  group_by(CUSTOM_AGE_GROUP) %>% 
  nest() %>% 
  mutate(data = purrr::map(data, ~ .x %>% 
                      spread(coverageLevel, population.x))) %>% 
  unnest() %>% 
  rename(characteristic = CUSTOM_AGE_GROUP) %>% 
  select(characteristic, everything())


# Combining sections for Table 1 ####

residents_number_proper <- residents_number_core %>% 
  spread(coverageLevel, population) %>% 
  mutate_at(vars(Low, Medium, High, Overall), 
            funs(paste(formatC(., format = 'd', big.mark = ','), 
                       encl_brackets(formatC(100*./Overall, digits = 1, format = 'f')),
                       sep = ' '))) %>% 
  mutate(characteristic = 'Average number of residents in study period') %>% 
  select(characteristic, everything())


prescriber_section <- unique_linked_prescribers %>% 
  add_row(.before = T, prescriber_type2 = 'Prescribers',
          Low = ' ', Medium = ' ', High = ' ', Overall = ' ') %>% 
  rename(characteristic = prescriber_type2)

geog_section_proper <- geographic_core %>% 
  rename(characteristic = geographic) %>% 
  add_row(characteristic = 'Geographic', Low = ' ', Medium = ' ', High = ' ',
          Overall = ' ', .before = T) %>% 
  bind_rows(n_nursing_homes)

residents_section <- residents_number_proper %>% 
  bind_rows(female_residents_number_core) %>%
  bind_rows(residents_with_linked_rx) %>% 
  add_row(characteristic = 'Age', Low = ' ', Medium = ' ', High = ' ', Overall = ' ') %>% 
  bind_rows(agegrp_residents_number_core) %>% 
  select(characteristic, Low, Medium, High, Overall) %>% 
  add_row(characteristic = 'Residents', Low = ' ', Medium = ' ', High = ' ',
          Overall = ' ', .before = T)


combined_table1_thesis <- geog_section_proper %>% 
  bind_rows(., residents_section) %>% 
  bind_rows(., prescriber_section) 


write.table(combined_table1_thesis, file = here::here('tables/thesis_tab1.txt'), 
            row.names = F, sep = '|', quote = F)



# Table 2 - Pharmanet data ####
rxn_overall_pre <- pharmanetData_cleaned %>%
  filter(!is.na(coverageLevel)) %>% 
  group_by(coverageLevel) %>% 
  count() %>% 
  ungroup() %>% 
  add_row(coverageLevel = 'Total', n = sum(.$n))

rxn_uti_linked_only_pre <- UTIlinked_data %>% 
  group_by(coverageLevel) %>% 
  count() %>% 
  ungroup() %>% 
  add_row(coverageLevel = 'Total', n = sum(.$n)) 

cvgLevel_order <- c('Low', 'Medium', 'High', 'Total')

rxn_overall <- rxn_overall_pre %>% 
  mutate(n = formatC(n, format = 'd', big.mark = ','),
         n = paste(n, '(100)', sep = ' '),
         coverageLevel = factor(coverageLevel, levels = cvgLevel_order)) %>%
  spread(coverageLevel, n) %>% 
  mutate(characteristic = 'Total number of prescriptions') %>% 
  select(characteristic, everything())

rxn_nitro_overall_pre <- pharmanetData_cleaned %>%
  filter(!is.na(coverageLevel), FINAL_ATC == 'J01XE01') %>% 
  group_by(coverageLevel) %>% 
  count() %>% 
  ungroup() %>% 
  add_row(coverageLevel = 'Total', n = sum(.$n)) 

rxn_nitro_overall <- rxn_nitro_overall_pre %>% 
  left_join(., rxn_overall_pre, by = 'coverageLevel') %>% 
  mutate(prop = encl_brackets(formatC(100*n.x/n.y, digits = 1, format = 'f')),
         n.x = formatC(n.x, format = 'd', big.mark = ','),
         n.x = paste(n.x, prop, sep = ' '),
         coverageLevel = factor(coverageLevel, levels = cvgLevel_order)) %>% 
  select(coverageLevel:n.x) %>% 
  spread(coverageLevel, n.x) %>% 
  mutate(characteristic = 'All nitrofurantoin') %>% 
  select(characteristic, everything())

 
rxn_uti_linked_only <- rxn_uti_linked_only_pre %>%  
  left_join(., rxn_overall_pre, by = 'coverageLevel') %>% 
  mutate(prop = encl_brackets(formatC(100*n.x/n.y, digits = 1, format = 'f')),
         n.x = formatC(n.x, format = 'd', big.mark = ','),
         n.x = paste(n.x, prop, sep = ' '),
         coverageLevel = factor(coverageLevel, levels = cvgLevel_order)) %>% 
  select(coverageLevel:n.x) %>% 
  spread(coverageLevel, n.x) %>% 
  mutate(characteristic = 'UTI-linked prescriptions') %>% 
  select(characteristic, everything())

rxn_dx_specific_UTIlinked <- UTIlinked_data %>% 
  group_by(coverageLevel, dx) %>% 
  count() %>% 
  ungroup() %>% # I always seem to forget to do this 
  complete(coverageLevel, nesting(dx), fill = list(n = 0)) %>% 
  bind_rows(., UTIlinked_data %>% 
              group_by(dx) %>% 
              summarise(coverageLevel = 'Total', n = n())) %>% 
  left_join(., rxn_uti_linked_only_pre, by = 'coverageLevel') %>% 
  mutate(prop = encl_brackets(formatC(100*n.x/n.y, digits = 1, format = 'f')),
         n.x = formatC(n.x, format = 'd', big.mark = ','),
         n.x = paste(n.x, prop, sep = ' '),
         coverageLevel = factor(coverageLevel, levels = cvgLevel_order)) %>% 
  select(coverageLevel:n.x) %>%
  mutate(coverageLevel = factor(coverageLevel, 
                                levels = c('Low', 'Medium', 'High', 'Total'))) %>% 
  group_by(dx) %>% 
  nest() %>% 
  mutate(data = map(data, ~ .x %>% 
                      spread(coverageLevel, n.x))) %>% 
  unnest() %>% 
  rename(characteristic = dx) %>% 
  mutate(characteristic = indent_function(x = characteristic, indent = 'indent'))

rxn_uti_nitro_only <- UTIlinked_data %>% 
  filter(FINAL_ATC == 'J01XE01') %>% 
  group_by(coverageLevel) %>% 
  count() %>% 
  ungroup() %>% 
  add_row(coverageLevel = 'Total', n = sum(.$n)) %>% 
  left_join(., rxn_nitro_overall_pre, by = 'coverageLevel') %>% 
  mutate(prop = encl_brackets(formatC(100*n.x/n.y, digits = 1, format = 'f')),
         n.x = formatC(n.x, format = 'd', big.mark = ','),
         n.x = paste(n.x, prop, sep = ' '),
         coverageLevel = factor(coverageLevel, levels = cvgLevel_order)) %>% 
  select(coverageLevel:n.x) %>%
  mutate(coverageLevel = factor(coverageLevel, 
                                levels = c('Low', 'Medium', 'High', 'Total'))) %>% 
  spread(coverageLevel, n.x) %>% 
  mutate(characteristic = 'UTI-linked nitrofurantoin') %>% 
  select(characteristic, everything())

table2_pre <- rxn_overall %>% 
  bind_rows(rxn_nitro_overall) %>% 
  bind_rows(rxn_uti_linked_only) %>% 
  add_row(characteristic = 'Clinical indication', Low = ' ', Medium = ' ', High = ' ', Total = ' ') %>% 
  bind_rows(rxn_dx_specific_UTIlinked) %>% 
  bind_rows(rxn_uti_nitro_only)

table2_pre$characteristic[5:9] <- paste0(table2_pre$characteristic[5:9], '*')
table2_pre$characteristic[10] <- paste0(table2_pre$characteristic[10], '#')

write.table(table2_pre, file = here::here('tables', 'thesis_tab2_abx.txt'), 
            row.names = F, sep = '|', quote = F)

# Table 3 - DOS linear model summary ####
table3_pre <- broom::tidy(DOS_linear_reg, conf.int = T) %>% 
  select(-c(statistic:p.value)) %>% 
  mutate_if(is.numeric, formatC, format = 'f', digits = 2) %>% 
  mutate(estimate = paste0(estimate, ' (', std.error, ')'),
         conf.low = paste0(conf.low, ' — ', conf.high)) %>% 
  select(-c(std.error, conf.high))

tab3_proper_names <- c('Baseline DOS rate for control (May 2015)', 'Medium coverage group baseline difference',
                       'High coverage group baseline difference', 
                       'Control group pre-intervention trend', 
                       'Control group post-intervention trend',
                       'Medium coverage group pre-intervention trend difference', 
                       'High coverage group pre-intervention trend difference', 
                       'Medium coverage group post-intervention trend difference', 
                       'High coverage group post-intervention trend difference')


proper_term_names <- data.frame(term = table3_pre$term, tab3_proper_names)

table3_pre$term  <- proper_term_names$tab3_proper_names[match(table3_pre$term, proper_term_names$term)]


write.table(table3_pre, file = here::here('tables', 'thesis_table3_model_coef.txt'), 
            quote = F, sep = '|', row.names = F, col.names = c('Term', 'Estimate (SE)', '95% CI'))





# Table 4 - Average rates of change of fitted values from DOS MLM ####

# Supplementary Table for Sensitivity####
# term proper names is created in table 3

sensitivity_table_term_proper_names <- c('Number of prescription rate for control in May 2015',
                                         'Medium coverage group baseline difference',
                       'High coverage group baseline difference', 'Control group overall trend', 
                       'Medium coverage group trend in study period', 
                       'High coverage group trend in study period', 'Control group trend in post-intervention period',
                       'Medium coverage group post-intervention trend difference', 
                       'High coverage group post-intervention trend difference')


sensitivity_supp_table$term2 <- sensitivity_table_term_proper_names

sensitivity_output_table <- sensitivity_supp_table %>% 
  select(model_linked_only:term2) %>% 
  select(term2, model_linked_only, model_0.6, model_0.7, model_0.8)

write.table(x = sensitivity_output_table, file = here::here('tables', 'thesis_sensitivity_coefs.txt'),
            quote = F, sep = '|', row.names = F)





# Supplementary table of MLM coef ####
pt_estimates_MLM <- tidy(chron_time_nb2_model, effects = 'fixed') %>% 
  select(term, estimate)

fixed_effects_CIs <- map(model_outputs_list, 
                         ~ .x %>% tidy(.x, effects = 'fixed') %>% 
      select(term, estimate)) %>% 
  bind_rows() %>% 
  group_by(term) %>% 
  tidyr::nest() %>% 
  mutate(ci_low = map_dbl(data, ~ quantile(.x$estimate, probs = 0.025)),
         ci_high = map_dbl(data, ~ quantile(.x$estimate, probs = 0.975)),
         nrow = map_dbl(data, ~nrow(.x))) %>% 
  select(-data) %>% 
  mutate_if(is.numeric, formatC, format = 'f', digits = 2)

pt_estimates_MLM %>% 
  left_join(., fixed_effects_CIs, by = 'term') %>% 
  select(-nrow) %>% 
  mutate_if(is.numeric, formatC, format = 'f', digits = 2)

# removed flextable script ####

table2_typology <- data.frame(col_keys = c('characteristic', 'Low', 'Medium', 'High', 'Total'),
                              colA = c(' ', rep('n (%)', times = 4)),
                              colB = c('Prescription characteristics', 'Low', 'Medium', 'High', 'Total')) %>% 
  mutate_all(as.character)


remove_border <- officer::fp_border(width = 0, color = 'white')
overall_bottom_border <- officer::fp_border(width = 1.5, color = 'black')
header_bottom_border <- officer::fp_border(width = 1, color = 'black')

table2_ft <- flextable(table2_pre) %>% 
  set_header_df(table2_typology, key = 'col_keys') %>% 
  merge_h(part = 'header') %>% 
  theme_vanilla() %>% 
  align(align = 'center', part = 'header') %>% 
  align(j = ~ characteristic, align = 'left', part = 'all') %>% 
  border(border = remove_border, part = 'all') %>% 
  border(i = nrow(table2_pre), border.bottom = overall_bottom_border, part = 'body') %>% 
  border(i = 1, border.top = header_bottom_border, part = 'body') %>%
  border(i = 1, j = 2:5, border.bottom = header_bottom_border, part = 'header') %>% 
  autofit()

table2_ft



table3_typology <- data.frame(col_keys = c('term', 'estimate', 'conf.low'), 
                              colA = c('Model term', 'Estimate (DOS)', '95% confidence interval')) %>% 
  mutate_all(as.character)

tab3_ft <- flextable(data = table3_pre, col_keys = names(table3_pre)) %>% 
  set_header_df(table3_typology, key = 'col_keys') %>% 
  theme_vanilla() %>% 
  fontsize( part = 'all', size = 12) %>% 
  bold(bold = T, part = 'header') %>% 
  align(j = 1, align = 'left', part = 'all') %>% 
  border(border = remove_border, part = 'all') %>% 
  border(part = 'header', border.bottom = header_bottom_border) %>% 
  border(i = nrow(table3_pre), part = 'body', border.bottom = overall_bottom_border) %>% 
  autofit()

tab3_ft

# unique prescriber removed ####

prescriber_pre <- pharmanetData_cleaned %>% 
  filter(!is.na(coverageLevel)) %>% 
  group_by(coverageLevel) %>% 
  distinct(PRAC_STUDY_ID) %>% 
  count() %>% 
  ungroup() %>% 
  add_row(coverageLevel = 'Overall', n = sum(.$n))


unique_abx_prescribers <- pharmanetData_cleaned %>% 
  filter(!is.na(coverageLevel)) %>%
  mutate(coverageLevel = factor(coverageLevel, 
                                levels = c('Low', 'Medium', 'High'))) %>% 
  group_by(coverageLevel, prescriber_type) %>% 
  distinct(PRAC_STUDY_ID) %>% 
  count() %>% 
  # adding our prescriber categorization
  left_join(., prescriberType_formatting, by = 'prescriber_type') %>% 
  group_by(coverageLevel, prescriber_type2) %>% 
  summarise(n = sum(n)) %>% 
  # adding zeros for groups that didn't appear in a coveragegroup (consistency)
  complete(prescriber_type2, fill = list(n = 0)) %>% 
  group_by(prescriber_type2) %>% 
  # adding an overall for prescriberType 
  nest() %>% 
  mutate(data = map(data, ~ .x %>% 
                      add_row(coverageLevel = 'Overall', n = sum(.$n)))) %>% 
  unnest() %>% 
  # adding the totals to make proportions 
  left_join(., prescriber_pre, by = 'coverageLevel') %>% 
  mutate(value = paste(formatC(n.x, format = 'd', big.mark = ','),
                       encl_brackets(formatC(100*n.x/n.y,
                                             format = 'f', digits = 1)),
                       sep = ' ')) %>% 
  select(prescriber_type2, coverageLevel, value) %>% 
  bind_rows(prescriber_pre %>% 
              mutate(prescriber_type2 = 'Number of unique prescribers',
                     n = paste(formatC(n, format = 'd', big.mark = ','),
                               '(100.0)',
                               sep = ' ')) %>% 
              rename(value = n)) %>% 
  group_by(prescriber_type2) %>% 
  nest() %>% 
  mutate(data = map(data, ~.x %>% 
                      spread(coverageLevel, value))) %>% 
  unnest()



