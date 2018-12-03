library(broom)

# intermediate files to load for sensitivity analysis
# from 1a_data_analysis_setup (2b section)
list_for_sensitivity_analysis <- readRDS(file = here::here('intermediates/list_for_sensitivity_analysis_25Sep2018.RDS'))

# from 2a_OLS.R
rx_n_linear_reg <- readRDS(file = here::here('intermediates', 'rx_n_linear_reg.RDS'))

# constants is also created in 1a_data analysis setup
constants = seq(from = 0.8, to = 0.6, by = -0.1)




# 4f. sensitivity analysis ####
sensitivity_models <- lapply(X = list_for_sensitivity_analysis, FUN = function(j) 
  lm(true_rx_n_rate ~ coverageLevel + study_time + post_trend + coverageLevel * study_time + coverageLevel * post_trend, 
     data = j))


tidied_model <- lapply(sensitivity_models, function(x) broom::tidy(x, conf.int = T) %>% 
                         mutate_at(vars(estimate, conf.low, conf.high), formatC, 
                                   format = 'f', digits = 2, big.mark = ',') %>% 
                         mutate(estimate = paste0(estimate, ' (', conf.low, ' - ', conf.high, ')')) %>% 
                         select(estimate))

sensitivity_estimates <- do.call("cbind", tidied_model) %>% 
  cbind(., broom::tidy(rx_n_linear_reg, conf.int = T) %>% 
          mutate_at(vars(estimate, conf.low, conf.high), formatC, 
                    format = 'f', digits = 2, big.mark = ',') %>% 
          mutate(estimate = paste0(estimate, ' (', conf.low, ' - ', conf.high, ')')) %>% 
          select(term, estimate)) 


colnames(sensitivity_estimates) <- c(paste('model', constants, sep = '_'), 
                                     'term', 'model_linked_only')

sensitivity_supp_table <- sensitivity_estimates %>% 
  select(term, model_linked_only, everything())

sensitivity_fitted_values <- lapply(X = sensitivity_models, FUN = function(x) 
  data_for_linear_regression %>% 
    select(coverageLevel, date) %>% 
    mutate(fitted = fitted(x)))

sensitivity_ggplot <- do.call('rbind', sensitivity_fitted_values) %>% 
  mutate(type = as.character(rep(c(0.8, 0.7, 0.6), each = 66))) %>% 
  bind_rows(data_for_linear_regression %>% 
              select(date, coverageLevel) %>% 
              mutate(fitted = fitted(rx_n_linear_reg), type = 'Original')) %>% 
  mutate(type = factor(type, levels = c('Original', '0.8', '0.7', '0.6')))


fitted(rx_n_linear_reg)


# saving output###
saveRDS(sensitivity_ggplot, file = here::here('intermediates', 'sensitivity_ggplot.RDS'))
saveRDS(sensitivity_supp_table, file = here::here('intermediates', 'sensitivity_supp_table.RDS'))

# the sensitivity results are visualized in 3_code_for_thesis




list_for_sensitivity_analysis %>% 
  map(., ~.x %>% 
        group_by(coverageLevel, study_time) %>% 
        summarise(mean = mean(true_rx_n_rate)) %>% 
        filter(study_time == 0))