library(ggplot2)

sensitivity_ggplot <- readRDS(file = here::here('intermediates', 'sensitivity_ggplot.RDS'))
sensitivity_ggplot$coverageLevel <- factor(sensitivity_ggplot$coverageLevel, 
                                           levels = c('Low', 'Medium', 'High'))
levels(sensitivity_ggplot$coverageLevel) <- c('Control', 'Medium', 'High')
levels(sensitivity_ggplot$type) <- c('Original', '80%', '70%', '60%')

interv_start_annotation <- as.Date('2016-07-01')
interv_end_annotation <- as.Date('2017-03-01')

# setting up Figure 1 #####
# getting counterfactual and fitted values
counterfactual_values_DOS <- data.frame(date = seq(from = as.Date('2016-07-01'), 
                                                   to = as.Date('2017-03-01'), 
                                                   by = 'month'),
                                        # using the names should give much more predictable results 
                                        # plus if it changes it will crash rather than give misleading results
                                        Low_counterfactual = coef(DOS_linear_reg)['(Intercept)'] + coef(DOS_linear_reg)['study_time']*13:21,
                                        Medium_counterfactual = (coef(DOS_linear_reg)['(Intercept)'] + coef(DOS_linear_reg)['coverageLevelMedium']) + (coef(DOS_linear_reg)['study_time']+coef(DOS_linear_reg)['coverageLevelMedium:study_time'])*13:21,
                                        High_counterfactual = (coef(DOS_linear_reg)['(Intercept)'] + coef(DOS_linear_reg)['coverageLevelHigh']) + (coef(DOS_linear_reg)['study_time']+coef(DOS_linear_reg)['coverageLevelHigh:study_time'])*13:21) %>% 
  gather(key = coverageLevel, value = value, -date) %>% 
  mutate(coverageLevel = gsub(x = coverageLevel, pattern = '_counterfactual', replacement = ''),
         type = 'counterfactual')


fitted_and_counterfactual_DOS <- data_for_linear_regression %>% 
  select(coverageLevel, date) %>% 
  mutate(value = fitted(DOS_linear_reg), type = 'fitted') %>% 
  bind_rows(., counterfactual_values_DOS) %>% 
  bind_rows(., data_for_linear_regression %>% 
          select(coverageLevel, DOS_rate, date) %>% 
          mutate(type = 'Observed') %>% 
          rename(value = DOS_rate)) %>% 
  mutate(type = as.character(type))


# renaming for figure
fitted_and_counterfactual_DOS$type[fitted_and_counterfactual_DOS$type == 'counterfactual'] = 'Trend without intervention'
fitted_and_counterfactual_DOS$type[fitted_and_counterfactual_DOS$type == 'fitted'] = 'Trend with intervention'

# break.vec is specified bc/ the default plot shows the months before and after the data 
# that we have collected
break.vec <- seq(from = min(data_for_linear_regression$date), 
                 to = max(data_for_linear_regression$date), by="month")
fitted_and_counterfactual_DOS$coverageLevel <- factor(fitted_and_counterfactual_DOS$coverageLevel,
                                                      levels = c('Low', 'Medium', 'High'))
levels(fitted_and_counterfactual_DOS$coverageLevel) <- c('Control', 'Medium','High')

coveragegrp_colours <- c('Control' = '#1b9e77', 'Medium' = '#d95f02', 'High' = '#7570b3')

# Figure 1 - minimal zoom ####
ggplot(subset(fitted_and_counterfactual_DOS, type != 'Observed'), 
       aes(x = date, y = value, color = coverageLevel))+
  annotate(geom = 'rect', xmin = interv_start_annotation,
           xmax = interv_end_annotation, 
           ymin = -Inf, ymax = Inf, alpha = 0.2, 
           fill = 'grey50')+
  geom_line(aes(linetype = type), size = 2) +
  labs(x = 'Date', y = 'Monthly DOS/1,000 residents/day', color = 'Group', 
       shape = 'Observed values',
       linetype = 'Fitted trend', 
       caption = 'Grey shading indicates the intervention period')+
  scale_color_manual(values = coveragegrp_colours)+
  scale_x_date(date_labels = '%b %y', breaks = break.vec)+
  scale_y_continuous(limits = c(4,11), 
                     breaks = seq(4,11,1))+
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust=1), 
        axis.title = element_text(face = 'bold'),
        legend.text=element_text(size=11),
        plot.caption = element_text(size = 7),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold'))+
  guides(linetype = guide_legend(title.position = 'top', 
                                 title.hjust = 0.5, nrow = 2),
         color = guide_legend(title.position = 'top', title.hjust = 0.5),
         shape = guide_legend(title.position = 'top', title.hjust = 0.5))+
  ggsave(filename = 'thesis_DOS_model_zoomed_vis.png', device = 'png',
         path = here::here('figures'),
         width = 17, height = 12,
         units = 'cm', dpi = 320)


# Figure 1 - with zero in y axis #####
ggplot(subset(fitted_and_counterfactual_DOS, type != 'Observed'), 
       aes(x = date, y = value, color = coverageLevel))+
  annotate(geom = 'rect',
           xmin = interv_start_annotation, 
           xmax = interv_end_annotation, 
           ymin = -Inf, ymax = Inf, alpha = 0.2, 
           fill = 'grey50')+
  geom_line(aes(linetype = type), size = 2) +
  geom_point(data = subset(fitted_and_counterfactual_DOS, type == 'Observed'), 
             aes(x = date, y = value, group = coverageLevel, shape = coverageLevel),
             size = 2, color = 'black')+
  labs(x = 'Date', y = 'Monthly DOS/1,000 residents/day', color = 'Group', shape = 'Observed values',
       linetype = 'Fitted trend', caption = 'Grey shading indicates the intervention period')+
  scale_color_manual(values = coveragegrp_colours)+
  scale_x_date(date_labels = '%b %y', breaks = break.vec)+
  expand_limits(y = 0) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust=1), 
        axis.title = element_text(face = 'bold'),
        legend.text=element_text(size=11),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold'))+
  guides(linetype = guide_legend(title.position = 'top', title.hjust = 0.5, nrow = 2),
         color = guide_legend(title.position = 'top', title.hjust = 0.5),
         shape = guide_legend(title.position = 'top', title.hjust = 0.5))+
  ggsave(filename = 'thesis_DOS_model_vis_w_0_in_yaxis.png', device = 'png',
       path = here::here('figures'),
       width = 17, height = 12,
       units = 'cm', dpi = 320)

# Sensitivity analysis figure ####
# break.vec is from figure 1 

ggplot(sensitivity_ggplot, aes(x = date, y = fitted, 
                               color = coverageLevel, shape = type), size = 2)+
  annotate(geom = 'rect', 
           xmin = interv_start_annotation,
           xmax = interv_end_annotation, 
           ymin = -Inf, ymax = Inf, alpha = 0.2, 
           fill = 'grey50')+
  geom_point(size = 2)+
  geom_line()+
  labs(x = 'Date', y = 'Monthly number of prescriptions/1,000 residents/day', color = 'Coverage level', 
       shape = 'Prop. nitrofurantoin\nof unlinked', 
       caption = 'Grey shading indicates the intervention period')+
  # coveragegrp_colours is from figure 1 
  scale_color_manual(values = coveragegrp_colours)+
  scale_shape_manual(values = c('Original' = 4, 
                                '80%' = 17, '70%' = 19, '60%' = 15))+
  scale_x_date(date_labels = '%b %y', breaks = break.vec)+
  expand_limits(y = 0)+
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust=1), 
        axis.title.y = element_text(size = 9),
        axis.title = element_text(face = 'bold'),
        legend.title = element_text(face = 'bold'),
        legend.text=element_text(size=12),
        plot.caption = element_text(size = 7))+
  ggsave(filename = 'thesis_sensitivity_figure.png', device = 'png',
         path = here::here('figures'),
         width = 17, height = 10,
         units = 'cm', dpi = 320)


