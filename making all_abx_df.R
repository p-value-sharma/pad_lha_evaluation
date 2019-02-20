library(purrr)
library(dplyr)

# getting list of called Prateek_thesis
files <- list.files(here::here('data'))[grepl(x = list.files(here::here('data')), 
                            pattern = 'Prateek_thesis_')]

# LHAs_not-in_HCC is from 1a_data analysis setup file

# reading them into a list
combined_files <- lapply(files, function(x) read.csv(file = here::here('data', x)))

allabx_data_pre <-  map(combined_files, ~ .x %>% 
                      
                      filter(PLAN_B == 'Y', # filtering for Pharmanet Plan B
                             !lha %in% LHAs_not_in_HCC) %>%  # removing LHAs w/o NH residents
                      mutate(date = as.Date(date, format = '%m/%d/%Y')) %>%
                      # removing observations before June 2015
                      filter(date >= as.Date("2015-01-01"))) %>% 
  # combining into one dataframe
  bind_rows() %>%  
  mutate(year = format(date, '%Y'),
         date2 = paste('01', month, year, sep = '-'),
         date2 = as.Date(date2, format = '%d-%m-%Y')) %>% 
  # aggregating by Month and LHA
  group_by(date2, lha) %>% 
  summarise(totalrx_n = n(),
            totalDDD = sum(DDD),
            totalDOS = sum(DISPENSING_DSPD_DAYS_SPLY)) %>% 
  rename(date = date2, LHA_NUM = lha)

# writing to intermediate csv
write.csv(x = allabx_data_pre, file = here::here('intermediates', 'allabx_data_pre'),
          row.names = F)
