library(ggplot2)

data_for_linear_regression <- read.csv(file = here::here('intermediates', 'data_for_linear_regression.csv')) %>% 
  mutate(coverageLevel = factor(coverageLevel, levels = c('Low', 'Medium', 'High')),
         date = as.Date(as.character(date), format = '%Y-%m-%d')) %>% 
  arrange(coverageLevel)


# OLS segmented regression with coverage groups ####
# add all the terms into the model, because the reduced interaction form messes up with
# factor variables (orders them alphabetically)
DOS_linear_reg <- lm(DOS_rate~coverageLevel + study_time + post_trend +
                       coverageLevel*study_time + coverageLevel*post_trend, 
                     data = data_for_linear_regression)

# durbin watson test 
car::durbinWatsonTest(DOS_linear_reg)

rx_n_linear_reg <- lm(rx_n_rate~coverageLevel + study_time + post_trend +
                        coverageLevel*study_time + coverageLevel*post_trend, 
                      data = data_for_linear_regression)

saveRDS(DOS_linear_reg, file = here::here('intermediates', 'DOS_linear_reg.RDS'))
saveRDS(rx_n_linear_reg, file = here::here('intermediates', 'rx_n_linear_reg.RDS'))

# GLS segmented regression with coverage groups ####
DOS_gls_reg <- nlme::gls(DOS_rate~study_time+coverageLevel + study_time + 
                           post_trend + coverageLevel*study_time + coverageLevel*post_trend, 
                         data = data_for_linear_regression, 
                         correlation = nlme::corAR1())


