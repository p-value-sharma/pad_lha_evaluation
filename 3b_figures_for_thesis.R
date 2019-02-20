library(ggplot2)
library(dplyr)


# importing OLS models and data #####
allabx_DDD_deseas <- readRDS(file = here::here('intermediates', 'allabx_DDD_deseas.RDS'))
binary_DDD_linear_reg <- readRDS(file = here::here('intermediates', 'binary_DDD_linear_reg.RDS'))

binary_all_abx <- read.csv(here::here('intermediates', 'binary_all_abx.csv')) %>% 
  mutate(intervention = as.character(intervention),
         intervention = ifelse(intervention == '0', 'Control', 'Intervention'),
         date = as.Date(as.character(date), format = '%Y-%m-%d'))

binary_intervention_data <- read.csv(file = here::here('intermediates', 'binary_intervention_data.csv')) %>% 
  mutate(date = as.Date(as.character(date), format = '%Y-%m-%d')) 

sensitivity_ggplot <- readRDS(file = here::here('intermediates', 
                                                'sensitivity_ggplot.RDS')) %>% 
  filter(type %in% c('Original', '1')) %>% 
  mutate(type = droplevels(type),
         intervention = ifelse(intervention == 0, 'Control', 'Intervention'))


levels(sensitivity_ggplot$type) = c('UTI-linked only', 
                                    'Nitrofurantoin with\nnon-UTI indications added')

#sensitivity_ggplot$coverageLevel <- factor(sensitivity_ggplot$coverageLevel, 
#                                           levels = c('Low', 'Medium', 'High'))
#levels(sensitivity_ggplot$coverageLevel) <- c('Control', 'Medium', 'High')

interv_start_annotation <- as.Date('2016-07-01')
interv_end_annotation <- as.Date('2017-03-01')

# setting up all abx counterfactuals #####
# getting counterfactual and fitted values
counterfactual_values_allabx <- data.frame(date = seq(from = as.Date('2016-07-01'), 
                                                      to = as.Date('2017-03-01'), 
                                                      by = 'month'),
                                           # using the names should give much more predictable results 
                                           # plus if it changes it will crash rather than give misleading results
                                           Control_counterfactual = coef(allabx_DDD_deseas)['(Intercept)'] + coef(allabx_DDD_deseas)['study_time']*13:21,
                                           Intervention_counterfactual = (coef(allabx_DDD_deseas)['(Intercept)'] + coef(allabx_DDD_deseas)['intervention']) + (coef(allabx_DDD_deseas)['study_time']+coef(allabx_DDD_deseas)['intervention:study_time'])*13:21) %>% 
  gather(key = intervention, value = value, -date) %>% 
  mutate(intervention = gsub(x = intervention, pattern = '_counterfactual', replacement = ''),
         type = 'counterfactual')


fitted_and_counterfactual_allabx_DDD <- binary_all_abx %>% 
  distinct(intervention, date) %>% 
  mutate(value = fitted(allabx_DDD_deseas), 
         type = 'fitted') %>% 
  bind_rows(., counterfactual_values_allabx) %>% 
  bind_rows(., binary_all_abx %>% 
              select(intervention, totalDDD_rate, date) %>% 
              mutate(type = 'Observed') %>% 
              rename(value = totalDDD_rate)) %>% 
  mutate(type = as.character(type))


# renaming for figure
fitted_and_counterfactual_allabx_DDD$type[fitted_and_counterfactual_allabx_DDD$type == 'counterfactual'] = 'Trend without intervention'
fitted_and_counterfactual_allabx_DDD$type[fitted_and_counterfactual_allabx_DDD$type == 'fitted'] = 'Trend with intervention'

# break.vec is specified bc/ the default plot shows the months before and after the data 
# that we have collected
break.vec <- seq(from = min(binary_all_abx$date), 
                 to = max(binary_all_abx$date), by = "month")

#fitted_and_counterfactual_DDD$coverageLevel <- factor(fitted_and_counterfactual_DDD$coverageLevel,
#                                                      levels = c('Low', 'Medium', 'High'))
#levels(fitted_and_counterfactual_DDD$coverageLevel) <- c('Control', 'Medium','High')

#coveragegrp_colours <- c('Control' = '#1b9e77', 'Medium' = '#d95f02', 'High' = '#7570b3')

# Figure 1 - all abx ####
ggplot(subset(fitted_and_counterfactual_allabx_DDD, type != 'Observed'), 
       aes(x = date, y = value, color = intervention))+
  # interventoin shaded period
  annotate(geom = 'rect', xmin = as.Date('2016-06-15'),
           xmax = as.Date('2017-03-01'), 
           ymin = -Inf, ymax = Inf, alpha = 0.2, 
           fill = 'grey50')+
  geom_line(aes(linetype = type), size = 2) +
  geom_point(data = subset(fitted_and_counterfactual_allabx_DDD, type == 'Observed'),
             aes(x = date, y = value, fill = intervention),
             pch=21, color = 'black', size = 2, show.legend = F)+
  scale_color_grey()+
  scale_fill_grey()+
  labs(x = 'Date', y = 'Monthly DDD/1,000 residents/day', color = 'Group', 
       shape = 'Observed values',
       linetype = 'Fitted trend', 
       caption = 'Grey shading indicates the intervention period')+
  #scale_color_manual(values = coveragegrp_colours)+
  scale_x_date(date_labels = '%b %y', breaks = break.vec)+
  expand_limits(y=0)+
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust=1), 
        axis.title = element_text(face = 'bold'),
        legend.text=element_text(size=11),
        plot.caption = element_text(size = 7),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold'))+
  guides(linetype = guide_legend(title.position = 'top', 
                                 title.hjust = 0.5, nrow = 1),
         color = guide_legend(title.position = 'top', title.hjust = 0.5),
         shape = guide_legend(title.position = 'top', title.hjust = 0.5))+
  ggsave(filename = 'DDD_all_abx.png', device = 'png',
         path = here::here('figures'),
         width = 17, height = 12,
         units = 'cm', dpi = 320)



# setting up UTI linked counterfactuals ####
counterfactual_values_uti_linkedDDD <- data.frame(date = seq(from = as.Date('2016-07-01'), 
                                                             to = as.Date('2017-03-01'), 
                                                             by = 'month'),
                                                  # using the names should give much more predictable results 
                                                  # plus if it changes it will crash rather than give misleading results
                                                  Control_counterfactual = coef(binary_DDD_linear_reg)['(Intercept)'] + coef(binary_DDD_linear_reg)['study_time']*13:21,
                                                  Intervention_counterfactual = (coef(binary_DDD_linear_reg)['(Intercept)'] + coef(binary_DDD_linear_reg)['intervention']) + (coef(binary_DDD_linear_reg)['study_time']+coef(binary_DDD_linear_reg)['intervention:study_time'])*13:21) %>% 
  gather(key = intervention, value = value, -date) %>% 
  mutate(intervention = gsub(x = intervention, pattern = '_counterfactual', replacement = ''),
         type = 'counterfactual')


fitted_and_counterfactual_utilinkedDDD <- binary_intervention_data %>% 
  distinct(intervention, date) %>% 
  mutate(value = fitted(binary_DDD_linear_reg), 
         type = 'fitted',
         intervention = as.character(intervention),
         intervention = ifelse(intervention == '0',
                               'Control', 'Intervention')) %>% 
  bind_rows(., counterfactual_values_uti_linkedDDD) %>% 
  bind_rows(., binary_intervention_data %>% 
              select(intervention, totalDDD_rate, date) %>% 
              mutate(type = 'Observed',
                     intervention = as.character(intervention),
                     intervention = ifelse(intervention == '0', 
                                           'Control', 'Intervention')) %>% 
              rename(value = totalDDD_rate)) %>% 
  mutate(type = as.character(type))


# renaming for figure
fitted_and_counterfactual_utilinkedDDD$type[fitted_and_counterfactual_utilinkedDDD$type == 'counterfactual'] = 'Trend without intervention'
fitted_and_counterfactual_utilinkedDDD$type[fitted_and_counterfactual_utilinkedDDD$type == 'fitted'] = 'Trend with intervention'

# break.vec is specified bc/ the default plot shows the months before and after the data 
# that we have collected
break.vec <- seq(from = min(binary_intervention_data$date), 
                 to = max(binary_intervention_data$date), by = "month")

# figure 2 - uti-linked abx ####
ggplot(subset(fitted_and_counterfactual_utilinkedDDD, type != 'Observed'), 
       aes(x = date, y = value, color = intervention))+
  # intervention shaded period
  annotate(geom = 'rect', 
           xmin = interv_start_annotation,
           xmax = interv_end_annotation, 
           ymin = -Inf, ymax = Inf, alpha = 0.05, 
           fill = 'grey50')+
  geom_line(aes(linetype = type), size = 2) +
  geom_point(data = subset(fitted_and_counterfactual_utilinkedDDD, type == 'Observed'),
             aes(x = date, y = value, fill = intervention),
             pch=21, color = 'black', size = 2, show.legend = F)+
  scale_color_grey()+
  scale_fill_grey()+
  labs(x = 'Date', y = 'Monthly DDD/1,000 residents/day', color = 'Group', 
       shape = 'Observed values',
       linetype = 'Fitted trend', 
       caption = 'Grey shading indicates the intervention period')+
  #scale_color_manual(values = coveragegrp_colours)+
  scale_x_date(date_labels = '%b %y', breaks = break.vec)+
  expand_limits(y=0)+
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust=1), 
        axis.title = element_text(face = 'bold'),
        legend.text=element_text(size=11),
        plot.caption = element_text(size = 7),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold'))+
  guides(linetype = guide_legend(title.position = 'top', 
                                 title.hjust = 0.5, nrow = 1),
         color = guide_legend(title.position = 'top', title.hjust = 0.5),
         shape = guide_legend(title.position = 'top', title.hjust = 0.5))+
  ggsave(filename = 'DDD_uti_linked_binary.png', device = 'png',
         path = here::here('figures'),
         width = 17, height = 12,
         units = 'cm', dpi = 320)


# Sensitivity analysis figure ####
# break.vec is from figure 1 

ggplot(sensitivity_ggplot, aes(x = date, y = fitted, 
                               color = intervention, shape = type), size = 2)+
  annotate(geom = 'rect', 
           xmin = interv_start_annotation,
           xmax = interv_end_annotation, 
           ymin = -Inf, ymax = Inf, alpha = 0.05, 
           fill = 'grey50')+
  geom_point(size = 2)+
  geom_line()+
  labs(x = 'Date', y = 'Monthly DDDs/1,000 residents/day', color = 'Group', 
       shape = 'Analysis type', 
       caption = 'Grey shading indicates the intervention period')+
  # coveragegrp_colours is from figure 1 
  #scale_color_manual(values = coveragegrp_colours)+
  scale_shape_manual(values = c('UTI-linked only' = 4, 
                                'Nitrofurantoin with\nnon-UTI indications added' = 17))+
  scale_x_date(date_labels = '%b %y', breaks = break.vec)+
  scale_color_grey()+
  expand_limits(y = 0)+
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust=1), 
        axis.title.y = element_text(size = 9),
        axis.title = element_text(face = 'bold'),
        legend.title = element_text(face = 'bold'),
        legend.text=element_text(size=12),
        legend.position="bottom",
        legend.key.size = unit(1.5, 'lines'),
        plot.caption = element_text(size = 7))+
  guides(color = guide_legend(title.position = 'top', 
                                 title.hjust = 0.5),
         shape = guide_legend(title.position = 'top', 
                              title.hjust = 0.5))+
  ggsave(filename = 'thesis_sensitivity_figure.png', device = 'png',
         path = here::here('figures'),
         width = 17, height = 10,
         units = 'cm', dpi = 320)


