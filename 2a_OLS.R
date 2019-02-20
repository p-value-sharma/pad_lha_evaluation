library(ggplot2)


# importing data ####
data_for_linear_regression <- read.csv(file = here::here('intermediates', 'data_for_linear_regression.csv')) %>% 
  mutate(coverageLevel = factor(coverageLevel, levels = c('Low', 'Medium', 'High')),
         date = as.Date(as.character(date), format = '%Y-%m-%d')) %>% 
  arrange(coverageLevel)

allabx_data_for_OLS <- read.csv(file = here::here('intermediates', 'allabx_data_for_OLS.csv')) %>% 
  mutate(coverageLevel = factor(coverageLevel, levels = c('Low', 'Medium', 'High')),
         date = as.Date(as.character(date), format = '%Y-%m-%d')) %>% 
  arrange(coverageLevel) 


# UTI-linked - OLS segmented regression with binary intervention groups #####
binary_intervention_data <- data_for_linear_regression %>% 
  group_by(study_time, post_trend, days_in_month, intervention, date) %>% 
  summarise(population = sum(population), totalDDD = sum(totalDDD),
            totalDOS = sum(totalDOS), totalrx_n = sum(totalrx_n)) %>% 
  ungroup() %>% 
  mutate_at(vars(starts_with('total')),
            funs(rate = .*1000/population/days_in_month))

binary_DOS_linear_reg <- lm(totalDOS_rate~intervention + study_time + post_trend +
                       intervention*study_time + intervention*post_trend, 
                     data = binary_intervention_data)


binary_DDD_linear_reg <- lm(totalDDD_rate~intervention + study_time + post_trend +
                              intervention*study_time + intervention*post_trend, 
                            data = binary_intervention_data)

# durbin watson test 
car::durbinWatsonTest(binary_DDD_linear_reg)

acf(residuals(binary_DDD_linear_reg))
pacf(residuals(binary_DDD_linear_reg))


# all abx - OLS segmented regression with binary intervention groups ####
binary_all_abx <- allabx_data_for_OLS %>% 
  mutate(intervention = ifelse(coverageLevel == 'Low', 0, 1)) %>% 
  group_by(date, study_time, post_trend, intervention) %>% 
  summarise(totalDDD = sum(totalDDD),
            totalDOS = sum(totalDOS),
            totalrx_n = sum(totalrx_n)) %>% 
  # deseasonalizing data
  group_by(intervention) %>% 
  tidyr::nest() %>% 
  mutate(tsDDD = map(data, ~.x %>% pull(totalDDD) %>% 
                    ts(., start = c(2015,1), end = c(2017,3),
                       frequency = 12)),
         stl5 = map(tsDDD, ~ stl(.x, s.window = 5)),
         stl7 = map(tsDDD, ~ stl(.x, s.window = 7)),
         stl13 = map(tsDDD, ~ stl(.x, s.window = 13)),
         stl_df = map(stl13, ~as.data.frame(.x$time.series))) %>% 
  select(intervention, data, stl_df) %>% 
  tidyr::unnest() %>% 
  mutate(total_deseasonalized_DDD = trend + remainder) %>% 
  select(-c(seasonal, trend, remainder)) %>% 
  filter(study_time >= 0) %>% 
  # adding population
  left_join(., hcc_denom_coverageLevel %>% 
              ungroup() %>% 
              mutate(intervention = ifelse(coverageLevel == "Low", 0, 1)) %>% 
              group_by(date, study_time, intervention) %>% 
              summarise(population = sum(population), 
                        days_in_month = first(days_in_month)), 
            by = c('intervention', 'date', 'study_time')) %>% 
  mutate_at(vars(starts_with('total')),
            funs(rate = .*1000/population/days_in_month))
  
allabx_DOS_linear_reg <- lm(totalDOS_rate~intervention + study_time + post_trend +
                              intervention*study_time + intervention*post_trend, 
                            data = binary_all_abx)

allabx_DDD_regular <- lm(totalDDD_rate~intervention + study_time + post_trend +
                          intervention*study_time + intervention*post_trend, 
                        data = binary_all_abx)

allabx_DDD_deseas <- lm(total_deseasonalized_DDD_rate~intervention + study_time + post_trend +
                              intervention*study_time + intervention*post_trend, 
                            data = binary_all_abx)

# looks like an autocorrelation correction is needed

car::durbinWatsonTest(allabx_DDD_deseas)
acf(residuals(allabx_DDD_deseas))
pacf(residuals(allabx_DDD_deseas))


# weaker autoregressive process, but AR1, ACF tails off
acf(residuals(allabx_DOS_linear_reg))
pacf(residuals(allabx_DOS_linear_reg))



# UTI-linked - OLS segmented regression with coverage groups ####
# add all the terms into the model, because the reduced interaction form messes up with
# factor variables (orders them alphabetically)
DOS_linear_reg <- lm(DOS_rate~coverageLevel + study_time + post_trend +
                       coverageLevel*study_time + coverageLevel*post_trend, 
                     data = data_for_linear_regression)

# durbin watson test 
car::durbinWatsonTest(DOS_linear_reg)

# binary_intervention data comes UTI-linked OLS
rx_n_binary_OLS <- lm(totalrx_n_rate~intervention + study_time + post_trend +
                        intervention*study_time + intervention*post_trend, 
                      data = binary_intervention_data)

saveRDS(DOS_linear_reg, file = here::here('intermediates', 'DOS_linear_reg.RDS'))
saveRDS(rx_n_binary_OLS, file = here::here('intermediates', 'rx_n_binary_OLS.RDS'))


# GLS segmented regression with coverage groups ####
DOS_gls_reg <- nlme::gls(DOS_rate~study_time+coverageLevel + study_time + 
                           post_trend + coverageLevel*study_time + coverageLevel*post_trend, 
                         data = data_for_linear_regression, 
                         correlation = nlme::corAR1())





# exporting models ####



saveRDS(binary_DDD_linear_reg, file = here::here('intermediates', 'binary_DDD_linear_reg.RDS'))
saveRDS(binary_DOS_linear_reg, file = here::here('intermediates', 'binary_DOS_linear_reg.RDS'))

saveRDS(allabx_DOS_linear_reg, file = here::here('intermediates', 'allabx_DOS_linear_reg.RDS'))
saveRDS(allabx_DDD_linear_reg, file = here::here('intermediates', 'allabx_DDD_deseas.RDS'))
        
saveRDS(DOS_linear_reg, file = here::here('intermediates', 'DOS_linear_reg.RDS'))
saveRDS(rx_n_linear_reg, file = here::here('intermediates', 'rx_n_linear_reg.RDS'))

write.csv(binary_intervention_data, 
          here::here('intermediates/binary_intervention_data.csv'), row.names = F)
write.csv(binary_all_abx, here::here('intermediates', 'binary_all_abx.csv'),
          row.names = F)


# Self help - how to interpret the trend coefficients ####

# these are the absolute trends in the groups from the fitted values 
# you can see the model coefficients are relative
# i think the absolute values are helpful bc/ people see the relative 
# differences in the graph, but can get mislead by directly interpretting the
# coefficients

binary_all_abx %>% 
  mutate(fitted = fitted(allabx_DDD_GLS),
         interv_period = ifelse(post_trend == 0, 0, 1)) %>% 
  group_by(intervention, interv_period) %>% 
  mutate(fitted2 = lag(fitted)) %>% 
  ungroup() %>% 
  mutate(diff = fitted - fitted2) %>% 
  select(intervention, study_time, fitted:diff) %>% 
  group_by(intervention, interv_period) %>%
  summarise(trend_coef = last(diff))

broom::tidy(allabx_DDD_GLS) %>% 
  select(term:estimate) %>% 
  mutate(estimate = round(estimate, digits = 4))
