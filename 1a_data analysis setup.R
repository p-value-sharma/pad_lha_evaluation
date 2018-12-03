# Analysis of PAD intervention 
# Prateek Sharma

# Step 0. Using RStudio open "Prateek_Thesis.Rproj"  
library(dplyr)
library(tidyr)
library(purrr)


# 1 Importing data ####
lhacoveragelevels <- read.csv(paste(here::here(), "data/PAD-UTI LHA Coverage levels.csv", 
                                    sep = "/")) %>% 
  mutate(prop = round(100*numInterventionLTRCs/num_of_LTCFs_in_2017, digits = 1), 
         coverageLevel = cut(prop, breaks = c(NA,-Inf, 1,66,100), 
                             labels = c('Low', 'Medium', "High")),
         coverageLevel = factor(coverageLevel)) 

hcc_denom_raw <- readxl::read_excel(here::here('data/HCC denominator data_Oct162018.xlsx'), 
                                    col_types = c('text', 'numeric', 'numeric', 'text', 'text', 'numeric')) %>% 
  # October is formatted wrong
  mutate(YEAR_MONTH = ifelse(YEAR_MONTH == '2015.1', '2015.10', YEAR_MONTH),
    YEAR_MONTH = ifelse(YEAR_MONTH == '2016.1', '2016.10', YEAR_MONTH)) %>% 
  mutate(YEAR_MONTH = paste0(YEAR_MONTH, '.01'),
         YEAR_MONTH = as.Date(YEAR_MONTH, format = '%Y.%m.%d')) %>% 
  # study period starts June 2015
  filter(YEAR_MONTH >= as.Date('2015-06-01')) %>% 
  arrange(YEAR_MONTH) %>% 
  left_join(., data.frame(AGE_GROUP3 = unique(.$AGE_GROUP3), 
                          CUSTOM_AGE_GROUP = c('80+','15-64', '80+', '15-64', '65-80', 
                                               '15-64', '65-80', '15-64', '80+', '65-80',
                                               '15-64', '15-64', '15-64', '15-64', '15-64', '15-64')), 
            by = 'AGE_GROUP3') %>% 
  rename(date = YEAR_MONTH, LHA_NUM = PROVIDER_LHA) %>% 
  left_join(., lhacoveragelevels %>% 
              select(LHA_NUM, coverageLevel), by = 'LHA_NUM')

write.csv(hcc_denom_raw, file = here::here('intermediates/hcc_denom_pre.csv'), row.names = F)


studyT_to_calendarT <- data.frame(study_time = 0:21, 
                                  date = seq(from = as.Date('2015-06-01'), 
                                             to = as.Date('2017-03-01'), by = 'month'))

hcc_denom <- hcc_denom_raw %>% 
  group_by(date, LHA_NUM, CUSTOM_AGE_GROUP, coverageLevel) %>% 
  summarise(population = sum(CLIENT_COUNT)) %>% 
  left_join(., hcc_denom_raw %>% 
              group_by(LHA_NUM) %>%
              distinct(PROVIDER_ID) %>%
              summarise(n_NHs_LHA = n()), by = 'LHA_NUM') %>% 
  # adding study time
  left_join(., studyT_to_calendarT, by = 'date') %>% 
  ungroup() %>%
  mutate(days_in_month = lubridate::days_in_month(date), 
         population = as.numeric(population)) %>% 
  select(date, study_time, LHA_NUM, coverageLevel, population, days_in_month) 


hcc_denom_coverageLevel <- hcc_denom %>% 
  group_by(date, study_time, coverageLevel) %>% 
  summarise(population = sum(population), days_in_month = first(days_in_month))


# adding HSDA and HA to Pharmanet data 
lha_translator <- readxl::read_excel("O:/BCCDC/Groups/Analytics_Resources/Data/Health Regions/HA HSDA LHA CHSA Hierarchy 2017.xlsx", 
                                     sheet = "lha_2017")

lha_translator <- lha_translator %>% 
  select(HA_ID, LHA97_NAME, LHA97_NUM, HSDA_ID) %>% 
  arrange(LHA97_NUM) %>% 
  rename(LHA_NUM = LHA97_NUM) %>% 
  select(-LHA97_NAME)


geographicLHApopn <- read.csv(paste(here::here(), "data/LHA_pop_2017.csv", sep = '/')) 

geographicLHApopn <- geographicLHApopn %>% 
  rename(LHA_NAME = Local.Health.Area, popn = Total, year = Year) %>% 
  left_join(., lhacoveragelevels, by = "LHA_NUM") %>% 
  mutate(geographic = cut(popn, breaks = c(0,10001,40001,190001,Inf), 
                          labels = c("remote","rural","urban/rural","metro"))) %>% 
  select(-LHA_NAME.y) %>% 
  rename(LHA_NAME = LHA_NAME.x)

pharmanetData_raw <- read.csv(here::here('data/pharmanetDataMay1_2018.csv'))
    
dxgroup_defn <- data.frame(dxgroup = c(rep('UTI-incl symptoms', 5), 'Unlinked'), 
           dx = c('Cystitis', 'Prostatitis', 'Pyelonephritis',
                  'Other disorders of urethra and urinary tract', 
                  'Symptoms involving urinary system', 'Z2.Unlinked'))   

pharmanetData_pre <- pharmanetData_raw %>% 
  rename(LHA_NUM = lha, DSPD_DAYS_SPLY = DISPENSING_DSPD_DAYS_SPLY, 
         prescriber_type = PRSCR_PRAC_PROF) %>% # renaming columns
  mutate(ATC_3 = substr(FINAL_ATC, 1, 4),
    ATC_4 = substr(FINAL_ATC, 1, 5), # making an ATC_4 column
    date = as.Date(date, origin = '1899-12-30'), # creating a date column
    month = format(date, '%m'), # creating a month column
    LHA_NUM = as.numeric(as.character(LHA_NUM))) %>% 
  left_join(., dxgroup_defn, by = 'dx') %>% 
  mutate(dxgroup = as.character(dxgroup)) %>% 
  # want data from the start of June 2015
  filter(date >= '2015-06-01') %>% 
  left_join(., subset(lhacoveragelevels, 
                      select = c("LHA_NUM","coverageLevel", 'prop')),
            by = "LHA_NUM") %>% 
  left_join(., lha_translator, by = 'LHA_NUM') %>% 
  mutate(HSDA_ID = as.factor(HSDA_ID), HA_ID = as.factor(HA_ID)) %>% 
  rename(prop.coverage = prop)

pharmanetData_pre$dxgroup[is.na(pharmanetData_pre$dxgroup)] <- 'Nitro various reasons'

LHAs_not_in_HCC <- pharmanetData_pre %>% 
  filter(dxgroup == 'UTI-incl symptoms') %>% 
  distinct(LHA_NUM) %>% 
  anti_join(., hcc_denom_raw %>% distinct(LHA_NUM), by = 'LHA_NUM') %>% 
  pull()


pharmanetData <- pharmanetData_pre %>% 
  filter(!LHA_NUM %in% LHAs_not_in_HCC)


BCnursinghomes <-  read.csv(file = paste(here::here(), 'data/ltrcListadminareas.csv', 
                                         sep = '/'))

BCnursinghomes2 <- BCnursinghomes %>% 
  left_join(., subset(lhacoveragelevels, select = c("LHA_NUM","coverageLevel")),
            by = "LHA_NUM") %>% 
  mutate(nh_size = cut(total.beds, breaks = c(1, 29, 99, Inf), 
                       labels = c('small', 'medium', 'large')),
         FSA = substr(postal.code, 1, 3))


UTIlinked_data <- pharmanetData %>% 
  filter(dxgroup == 'UTI-incl symptoms', !is.na(coverageLevel)) %>% 
  droplevels()




interv_nh_list <- read.csv(file = paste(here::here(), 'data/Senior\'s Advocate LTRC List Jan 2017 (admin areas) 23-Jun.csv', sep = '/'))

interv_fsa <- interv_nh_list %>% 
  select(-c(X:X.1, index)) %>% 
  mutate(FSA = substr(postalCode, 1, 3), date = as.Date(as.character(date), format = '%m/%d/%Y')) %>%
  group_by(intervention, FSA) %>% 
  summarise(n = n(), minDate = min(date)) %>% 
  arrange(FSA, intervention) %>% 
  filter(!is.na(intervention))


interv_fsa %>% 
  filter(intervention == T) 

length(unique(interv_fsa$FSA)) # 190 FSAs in BC, 128 FSA have a nursing home, 88 FSA visited 



# 2a Data setup for OLS regression ####
data_for_linear_regression <- UTIlinked_data %>% 
  group_by(month, year,  coverageLevel) %>% 
  summarise(totalDDD = sum(DDD), 
            totalDOS = sum(DSPD_DAYS_SPLY), 
            totalrx_n = n()) %>% 
  filter(coverageLevel != 'NA') %>% 
  mutate(date = as.Date(paste(year, month, '01', sep = '-'), format = '%Y-%m-%d')) %>% 
  # there are 22 months in the study
  # studyT is created in the data import step
  left_join(., studyT_to_calendarT, by = 'date') %>% 
  mutate(post_trend = pmax(0, study_time-12,0)) %>% 
  filter(study_time != 'NA') %>% 
  ungroup() %>% 
  select(-c(month, year)) %>% 
  arrange(study_time, coverageLevel) %>% 
  left_join(., hcc_denom_coverageLevel, 
            by = c('coverageLevel', 'date', 'study_time')) %>% 
  mutate(DOS_rate = 1000*totalDOS/population/days_in_month,
         rx_n_rate = 1000*totalrx_n/population/days_in_month)

# 2b Data setup for OLS sensitivity analysis ####

sens.approp <- addmargins(xtabs(~ dx + dxgroup, data = pharmanetData))
sens.approp_nitro_only <- addmargins(xtabs(~ dx + dxgroup, data = pharmanetData[pharmanetData$FINAL_ATC == 'J01XE01',]))

write.table(sens.approp, here::here('tables/sens.approp_all_drugs_June2015toMar2017.txt'), 
            sep = '|', row.names = T)
write.table(sens.approp_nitro_only, here::here('tables/sens.approp_nitro_only.txt'), 
            sep = '|', row.names = T)

all_months <- tidyr::expand(pharmanetData, LHA_NUM, month, year)

unlinked_nitro <- pharmanetData %>% 
  filter(dxgroup == 'Unlinked' & FINAL_ATC == 'J01XE01') %>% 
  group_by(coverageLevel, month, year) %>% 
  summarise(unlinked_nitro_DOS = sum(DSPD_DAYS_SPLY), unlinked_nitro_DDD = sum(DDD),
            unlinked_nitro_rx_n = n()) %>% 
  mutate(date = paste(year, month, '01', sep = '-'),
         date = as.Date(date, format = '%Y-%m-%d')) %>% 
  ungroup() %>% 
  select(-c(month, year))


# ‘True’ UTI Rx_N = Total linked Rx_N +
# Unlinked nitrofurantoin Rx_N * (Prop exclusive use of nitro for UTI) *(1/ Prop nitro of unlinked) 

constants = seq(from = 0.8, to = 0.6, by = -0.1)

# data_for_linear_regression from 2a and unlinked_nitro from 2b
list_for_sensitivity_analysis <- lapply(X = constants, FUN = function(x) {
  sensitivity_data <- data_for_linear_regression %>% 
    left_join(., unlinked_nitro, by = c('coverageLevel', 'date')) %>% 
    rename(linked_rx_n = totalrx_n) %>% 
    mutate(trueUTI_rx_n = linked_rx_n + unlinked_nitro_rx_n*(1/x), constant = x) %>% 
    select(coverageLevel, study_time, post_trend, trueUTI_rx_n, constant) %>% 
    mutate(coverageLevel = factor(coverageLevel, levels = c('Low', 'Medium', 'High'))) %>% 
    left_join(., hcc_denom_coverageLevel, 
              by = c('coverageLevel', 'study_time')) %>% 
    mutate(true_rx_n_rate = 1000*trueUTI_rx_n/population/days_in_month)})

names(list_for_sensitivity_analysis) <- c('perc_80', 'perc_70', 'perc_60')

saveRDS(object = list_for_sensitivity_analysis, 
        file = here::here('intermediates','list_for_sensitivity_analysis_25Sep2018.RDS'))

unlinked_nitro_prescribers <- pharmanetData %>% 
  filter(dxgroup == 'Unlinked' & FINAL_ATC == 'J01XE01' & date < as.Date('2016-06-01')) %>% 
  select(PRAC_STUDY_ID) %>% 
  group_by(PRAC_STUDY_ID) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(rx_n_tot = sum(n)) %>% 
  arrange(desc(n))


linked_nitro_prescribers <- pharmanetData %>% 
  filter(dxgroup != 'Unlinked' & FINAL_ATC == 'J01XE01' & date < as.Date('2016-06-01')) %>% 
  select(PRAC_STUDY_ID) %>% 
  group_by(PRAC_STUDY_ID) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(rx_n_tot = sum(n)) %>% 
  arrange(desc(n))


# return all rows from x where there are not matching values in y, keeping just columns from x
# report how many prescribers have unlinked prescriptions of total number of prescriptions (bc linked prescribers can apprarently have
# substantial amounts of unlinked prescribing)
unlinked_nitro_prescribers %>% 
  filter(n < 5) %>% 
  mutate(mean_n = mean(n))

unlinked_nitro_prescribers %>% 
  filter(n > 5) %>% 
  mutate(mean_n = mean(n))


linked_nitro_prescribers %>% 
  filter(n < 5) %>% 
  mutate(mean_n = mean(n))

linked_nitro_prescribers %>% 
  filter(n > 5) %>% 
  mutate(mean_n = mean(n))

linked_nitro_prescribers %>% 
  mutate(mean_n = mean(n))

unlinked_nitro_prescribers %>% 
  mutate(mean_n = mean(n))



unlinked_nitro_prescribers %>% 
  filter(n < 5) %>% 
  semi_join(linked_nitro_prescribers %>% 
              filter(n < 5), by = 'PRAC_STUDY_ID')

unlinked_nitro_prescribers %>% 
  filter(n > 5) %>% 
  semi_join(linked_nitro_prescribers %>% 
              filter(n > 5), by = 'PRAC_STUDY_ID')




unlinked_nitro_prescribers %>% 
  semi_join(linked_nitro_prescribers, by = 'PRAC_STUDY_ID')
  





# 3 Setting up data for multilevel analysis ####

LHA_lvl_hccdenom <- hcc_denom %>% 
  group_by(LHA_NUM, date, study_time) %>% 
  summarise(population = sum(population), 
            days_in_month = first(days_in_month)) %>% 
  # need to ungroup for the complete function to work
  ungroup() %>% 
  # complete helps us add months with missing values 
  complete(LHA_NUM, nesting(date, study_time, days_in_month),
           fill = list(population = 0))


interv_LHAs <- geographicLHApopn %>% 
  select(LHA_NUM, MINdate)

multileveldata <- UTIlinked_data %>% 
  left_join(interv_LHAs, by = 'LHA_NUM') %>% 
  mutate(MINdate = as.Date(MINdate, format = '%m/%d/%Y'),
         intervention = ifelse(!is.na(MINdate), T, F)) %>% 
  group_by(month, year, LHA_NUM, intervention) %>% 
  summarise(linked_DDD = sum(DDD), 
            linked_DOS = sum(DSPD_DAYS_SPLY), linked_rx_n = n(), 
            MINdate = min(MINdate), prop.coverage = min(prop.coverage)) %>% 
  mutate(date = as.Date(paste(year, month, '01', sep = '-'), format = '%Y-%m-%d'), 
         interv_year = format(MINdate, '%Y'),
         interv_mo = as.numeric(format(MINdate, '%m')), 
         MINdate = as.Date(paste(interv_year, interv_mo, '01', sep = '-'),
                           format = '%Y-%m-%d')) %>% 
  rename(minDate = MINdate) %>% 
  arrange(year, month, LHA_NUM) 

# distribution of intervention months 
month_distribution <- multileveldata %>% 
  ungroup() %>% 
  distinct(LHA_NUM, intervention, minDate) %>% 
  filter(minDate < as.Date('2016-10-01', format = '%Y-%m-%d') | is.na(minDate), intervention == T) %>% 
  mutate(interv_mo = as.numeric(format(minDate, '%m'))) %>% 
  group_by(interv_mo) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(sum = sum(n), prop = round(n/sum, digits = 3), 
         n_control_LHAs = length(unique(multileveldata$LHA_NUM[multileveldata$intervention == FALSE])),
         n_months = round(prop*n_control_LHAs, digits = 0)) %>% 
  select(interv_mo, n_months)

# make a vector of the month distribution 
initial_month_seq <- unlist(mapply(FUN = rep, month_distribution$interv_mo, month_distribution$n_months))


multileveldata2 <- multileveldata %>% 
  ungroup() %>% 
  # complete adds months with no linked prescribing
  # LHA_NUM should only have intervention status, minDate, interv_year, interv_mo
  # that actually appears in the data, no new combinations
  complete(nesting(LHA_NUM, intervention, minDate, interv_year, 
                   interv_mo, prop.coverage), 
           nesting(month, year, date), 
           fill = list(linked_DDD = 0, linked_DOS = 0, linked_rx_n = 0)) %>% 
  left_join(., LHA_lvl_hccdenom, 
            by = c('LHA_NUM', 'date'))

saveRDS(multileveldata2, 
        file = here::here('data/intermediates', 'multileveldata2.rds'))



# 4 exporting data for Table creation ####
dir.create(path = here::here('intermediates'))

write.csv(pharmanetData, row.names = F, 
          file = here::here('intermediates', 'pharmanetData_cleaned.csv'))

write.csv(UTIlinked_data, row.names = F, 
          file = here::here('intermediates', 'UTIlinked_data.csv'))

write.csv(multileveldata2, row.names = F, 
          file = here::here('intermediates', 'multileveldata2.csv'))

write.csv(geographicLHApopn, row.names = F, 
          file = here::here('intermediates', 'geographicLHApopn.csv'))

write.csv(x = BCnursinghomes2, 
          file = here::here('intermediates', 'bcnursinghomes.csv'), row.names = F)

write.csv(x = data_for_linear_regression, 
          file = here::here('intermediates', 'data_for_linear_regression.csv'), 
          row.names = F)




# DEPRACTED - adding controls to multilevel data ####


# This isn't needed anymore, if i'm just doing a single baseline study


# intervention LHAs data for merging with the control LHAs list
interv_multilevel_data <-  multileveldata2[multileveldata2$intervention == TRUE,] %>% 
  mutate(interv_mo = as.numeric(format(minDate, '%m')))


set.seed(3) # this needs to be run with any code that has 
#any randomness to get the same output in different sessions
random_month_seq <-  sample(initial_month_seq, size = length(initial_month_seq), replace = F)
d = data.frame(V1 = 1:length(unique(multileveldata$LHA_NUM[multileveldata$intervention == FALSE])))
d[,1:10000] <- NA # when this is 10,000 the code to create list for analysis will take 10 min to run

# create random permutations of the interv_mo sequence
d <- lapply(d, function(x) sample(random_month_seq, size = length(random_month_seq), replace = F)) 
# adding LHA_NUM to the permutations
d <- Map(cbind, d, LHA_NUM = list(unique(multileveldata$LHA_NUM[multileveldata$intervention == F]))) 

# this adds colnames to the data.frames in the list
d1 <- lapply(d, function(x) {
  colnames(x) <- c('interv_mo', 'LHA_NUM')
  x = as.data.frame(x)
  return(x)})

potential_interv_months = d1[!duplicated(as.list(d1))] # removing sequences that are duplicates

list_for_analysis <- d1 %>% 
  # adding the possible control interv_mo to the control LHAs
  map(~left_join(.x, multileveldata2[multileveldata2$intervention == F,] %>% 
                   select(-interv_mo), 
                 by = 'LHA_NUM')) %>% 
  # adding minDate to the control LHAs lists
  map(~mutate(.x, minDate = as.Date(paste('01', interv_mo, '2016', sep = '-'), 
                                    format = '%d-%m-%Y'))) %>% 
  # adding intervention facilities to control LHAs
  map(~rbind(.x, interv_multilevel_data)) %>% 
  # study_time relative to intervention start, 12 months before intervention (t = 0) 
  map(~mutate(.x, study_time = as.numeric(round(difftime(date,minDate, units = "days")/30)) + 12)) %>% 
  # filtering timepoints before 12 months before intervention
  map(~filter(.x, study_time >= 0)) %>% 
  # adding an index variable for post-intervention observations
  map(~mutate(.x, post_trend = pmax(0, study_time-12,0))) %>% 
  # removing unecessary columns
  map(~select(.x, -c(minDate, date, interv_mo)))

# this saves the list, so that the previous code doesn't need to be run again 
saveRDS(object = list_for_analysis, file = 
          paste(here::here(), 'data/intermediates/list_for_mlm.rds', sep = '/'))



