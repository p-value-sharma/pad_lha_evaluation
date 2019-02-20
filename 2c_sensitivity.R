library(broom)
library(purrr)
library(tidyr)
library(dplyr)

# intermediate files to load for sensitivity analysis ####
# from 1a_data_analysis_setup (2b section)
# derived from data_for_linear_regression
list_for_sensitivity_analysis <- readRDS(file = here::here('intermediates/list_for_sensitivity_analysis_24Dec2018.RDS'))

binary_sensitivity_data <- list_for_sensitivity_analysis %>% 
  # reorganizing as binary intervention 
  map(~ .x %>% 
        mutate(intervention = ifelse(coverageLevel == 'Low', 'Control', 'Intervention')) %>% 
        group_by(intervention, study_time, post_trend, constant, 
                 date, days_in_month) %>% 
        summarise(trueUTI_rx_n = sum(trueUTI_rx_n),
                  trueUTI_DDD = sum(trueUTI_DDD),
                  population = sum(population)) %>% 
        ungroup() %>% 
        mutate_at(vars(trueUTI_rx_n, trueUTI_DDD), 
                  funs(rate = .*1000/population/days_in_month))) 

# from 2a_OLS.R
rx_n_binary_OLS <- readRDS(file = here::here('intermediates', 'rx_n_binary_OLS.RDS'))
binary_DDD_linear_reg <- readRDS(file = here::here('intermediates', 'binary_DDD_linear_reg.RDS'))

binary_intervention_data <- read.csv(here::here('intermediates', 'binary_intervention_data.csv')) %>% 
  mutate(date = as.Date(date))


allabx_data_for_OLS <- read.csv(file = here::here('intermediates', 'allabx_data_for_OLS.csv')) %>% 
  mutate(coverageLevel = factor(coverageLevel, levels = c('Low', 'Medium', 'High')),
         date = as.Date(as.character(date), format = '%Y-%m-%d')) %>% 
  arrange(coverageLevel) 

# constants is also created in 1a_data analysis setup
constants = c(1, seq(from = 0.8, to = 0.6, by = -0.2))

# sanity checking sensitivity values #####
linked_and_nitro <- binary_intervention_data %>% 
  group_by(date) %>% 
  summarise(linked_DDD = sum(totalDDD)) %>% 
  left_join(., otherdx_and_unlinked_nitro %>% 
              group_by(date) %>% 
              summarise(otherdx_unlinked_nitro_DDD = sum(otherdx_unlinked_nitro_DDD)),
            by = 'date') %>% 
  mutate(raw_sensitivity = linked_DDD + otherdx_unlinked_nitro_DDD,
         test_sensitivity = linked_DDD + otherdx_unlinked_nitro_DDD*(1/0.6))

sanity_check <- allabx_data_for_OLS %>% 
  group_by(date) %>% 
  summarise(totalDDD = sum(totalDDD)) %>% 
  left_join(linked_and_nitro, by = 'date') %>% 
  mutate(prop_linked = linked_DDD/totalDDD,
         prop_nitro_added = raw_sensitivity/totalDDD,
         prop_0.6 = test_sensitivity/totalDDD)


View(sanity_check)
# sensitivity model ####
rx_n_sensitivity <- lapply(X = binary_sensitivity_data, FUN = function(j) 
  lm(trueUTI_rx_n_rate ~ intervention + study_time + post_trend + intervention * study_time + intervention * post_trend, 
     data = j))

DDD_sensitivity <- lapply(X = binary_sensitivity_data, FUN = function(j) 
  lm(trueUTI_DDD_rate ~ intervention + study_time + post_trend + intervention * study_time + intervention * post_trend, 
     data = j))


# summarizing model parameters ####
tidied_model_rx_n <- lapply(rx_n_sensitivity, function(x) broom::tidy(x, conf.int = T) %>% 
                         mutate_at(vars(estimate, conf.low, conf.high), formatC, 
                                   format = 'f', digits = 2, big.mark = ',') %>% 
                         mutate(estimate = paste0(estimate, ' (', conf.low, ' - ', conf.high, ')')) %>% 
                         select(estimate))


tidied_model_DDD <- lapply(DDD_sensitivity, function(x) broom::tidy(x, conf.int = T) %>% 
                              mutate_at(vars(estimate, conf.low, conf.high), formatC, 
                                        format = 'f', digits = 2, big.mark = ',') %>% 
                              mutate(estimate = paste0(estimate, ' (', conf.low, ' - ', conf.high, ')')) %>% 
                              select(estimate))

# combining model parameters into one table
sensitivity_estimates_DDD <- do.call("cbind", tidied_model_DDD) %>% 
  cbind(., broom::tidy(binary_DDD_linear_reg, conf.int = T) %>% 
          mutate_at(vars(estimate, conf.low, conf.high), formatC, 
                    format = 'f', digits = 2, big.mark = ',') %>% 
          mutate(estimate = paste0(estimate, ' (', conf.low, ' - ', conf.high, ')')) %>% 
          select(term, estimate)) 


colnames(sensitivity_estimates_DDD) <- c(paste('model', constants, sep = '_'), 
                                     'term', 'model_linked_only')

sensitivity_supp_table <- sensitivity_estimates_DDD %>% 
  select(term, model_linked_only, everything())


# readying data for data visualization ####

# when having issues with fitted values 
# check whether the order is the same as the dataframe for the regression
sensitivity_fitted_values_DDD <- lapply(X = DDD_sensitivity, FUN = function(x) 
  binary_intervention_data %>% 
    select(intervention, date) %>% 
    arrange(intervention, date) %>% 
    mutate(fitted = fitted(x)))

sensitivity_ggplot <- do.call('rbind', sensitivity_fitted_values_DDD) %>% 
  mutate(type = as.character(rep(c(1, 0.8, 0.6), each = 44))) %>% 
  bind_rows(binary_intervention_data %>% 
              select(date, intervention) %>% 
              mutate(fitted = fitted(binary_DDD_linear_reg), 
                     type = 'Original')) %>% 
  mutate(type = factor(type, levels = c('Original', '1', '0.8', '0.6')))


# saving output###
saveRDS(sensitivity_ggplot, file = here::here('intermediates', 'sensitivity_ggplot.RDS'))
saveRDS(sensitivity_supp_table, file = here::here('intermediates', 'sensitivity_supp_table.RDS'))

# the sensitivity results are visualized in 3_code_for_thesis

