
# 2 Assessing factors for missingness ####
# how do prescribers break down by linkage status
prescriber_type_breakdown <- pharmanetData %>% 
  group_by(coverageLevel, prescriber_type, dxgroup) %>% 
  summarise(n = n()) %>% # n prescriptions by prescriber type
  group_by(coverageLevel, prescriber_type) %>% 
  mutate(rxs_type_coverageLevel = sum(n), prop1 = round(n*100/rxs_type_coverageLevel, digits = 1)) %>% 
  group_by(coverageLevel) %>% 
  mutate(rxs_in_coveragelevel = sum(n), prop2 = round(n*100/rxs_in_coveragelevel, digits = 1)) %>% 
  arrange(coverageLevel, dxgroup, desc(n))

# prop1 is the proportion of prescriptions that are linked out of the total
# number of prescribers of that type in that coverage level # e.g. 61.6% of physician prescribing is unlinked in low

# prop2 is the proportion of prescriptions by that prescriber type # e.g. nurse practitioners account for 0.2% of unlinked prescribing  
# and 0.6% of linked prescribing (see who is getting linked in that coveragelevel)

write.table(prescriber_type_breakdown, paste(here::here(), "misc", "prescribe_type_brkdwn.txt", sep = "/"), 
            sep = ",", quote = FALSE, row.names = F)




# 3a. Checking if seasonality in prescribing #####
UTI_seasonality_check <- UTIlinked_data %>% 
  group_by(month, year) %>% 
  summarise(DDD = sum(DDD)) %>% 
  mutate(date = as.Date(paste('01', month, year, sep = '-'), format = '%d-%m-%Y'))



# hard to see visually any seasonality, bc/ of trend
ggplot(UTI_seasonality_check, aes(x = date, y = DDD))+
  geom_point()+
  lims(y = c(0, 6000))


UTI_seasonality_ts <- ts(UTI_seasonality_check$DDD, start = c(2015,1), frequency = 12)
plot(diff(UTI_seasonality_ts)) # differenced to remove trend


acf(UTI_seasonality_ts, lag.max = 16) # AR sharp
acf(UTI_seasonality_ts, type = 'partial', lag.max = 16) #MA sharp?

acf(diff(UTI_seasonality_ts))
acf(diff(UTI_seasonality_ts), type = 'partial')


# auto.arima thinks AR1 structure
forecast::auto.arima(UTI_seasonality_ts, stepwise = F, num.cores = 4)




# 3b Visualizing the influence of prescribers in LHA prescribing #####
lha_dominance_prescriber_plot <- UTIlinked_data %>% 
  group_by(month, year, LHA_NUM, PRAC_STUDY_ID, HA_ID, coverageLevel) %>% 
  summarise(n = n()) %>% 
  group_by(month, year, LHA_NUM, HA_ID) %>% 
  mutate(rx_n_lha = sum(n)) %>% 
  ungroup() %>% 
  mutate(prop = n/rx_n_lha) %>% 
  filter(rx_n_lha > 1) %>% 
  arrange(desc(prop)) %>% 
  group_by(PRAC_STUDY_ID, LHA_NUM, HA_ID, coverageLevel) %>% 
  summarise(mean = mean(prop), rxs = sum(n)) %>% 
  left_join(., 
            geographicLHApopn %>% 
              select(LHA_NUM, LHA_NAME, geographic), by = 'LHA_NUM') %>% 
  arrange(PRAC_STUDY_ID, desc(rxs)) %>% 
  ungroup() %>% 
  mutate(PRAC_STUDY_ID = as.factor(PRAC_STUDY_ID), LHA_NUM = as.factor(LHA_NUM), 
         geographic = as.factor(Hmisc::capitalize(as.character(geographic))), HA_ID = as.character(HA_ID))

ha_translator <- data.frame(HA_ID = 1:5, HA_NAME = c('Interior', 'Fraser','Vancouver Coastal','Vancouver Island', 'Northern'))
lha_dominance_prescriber_plot$HA_NAME <- as.factor(ha_translator$HA_NAME[match(lha_dominance_prescriber_plot$HA_ID, ha_translator$HA_ID)])
lha_dominance_prescriber_plot$geographic <- factor(lha_dominance_prescriber_plot$geographic, levels = c('Remote', 'Rural', 'Urban/rural', 'Metro'), ordered = T)


ggplot(lha_dominance_prescriber_plot, aes(x = PRAC_STUDY_ID, y = mean, size = rxs, shape = coverageLevel, color = geographic))+
  geom_point()+
  labs(size = 'Number of prescriptions', color = 'Population geography', shape = 'Intervention status', y = 'Mean of monthly proportion of prescriber\'s prescriptions of total UTI prescribing in LHA')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_colour_brewer(type = "qual", palette = 6, direction = 1)+
  ggsave('prescriber dominance bc jan 2015-March 2017.png', plot = last_plot(), 
         path = paste(here::here(),'figures', sep = '/'),
         scale = 1, width = 15, height = 10, units = 'in', 
         dpi = 300, limitsize = TRUE)


ggplot(lha_dominance_prescriber_plot, aes(x = PRAC_STUDY_ID, y = mean, size = rxs, shape = coverageLevel))+
  geom_point()+
  labs(size = 'Number of prescriptions', color = 'Population geography', shape = 'Intervention status', y = 'Mean of monthly proportion of prescriber\'s prescriptions of total UTI prescribing in LHA')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_colour_brewer(type = "qual", palette = 6, direction = 1)+
  facet_wrap(~geographic)+
  ggsave('prescriber dominance geographic jan 2015-March 2017.png', plot = last_plot(), 
         path = paste(here::here(),'figures', sep = '/'),
         scale = 1, width = 15, height = 10, units = 'in', 
         dpi = 300, limitsize = TRUE)


ggplot(lha_dominance_prescriber_plot, aes(x = PRAC_STUDY_ID, y = mean, size = rxs, shape = coverageLevel, color = geographic))+
  geom_point()+
  labs(size = 'Number of prescriptions', color = 'Population geography', shape = 'Intervention status', y = 'Mean of monthly proportion of prescriber\'s prescriptions of total UTI prescribing in LHA')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_colour_brewer(type = "qual", palette = 6, direction = 1)+
  scale_shape(solid = FALSE)+
  facet_wrap(HA_NAME~LHA_NAME)+
  ggsave('prescriber dominance lha jan 2015-March 2017.png', plot = last_plot(), 
         path = paste(here::here(),'figures', sep = '/'),
         scale = 1, width = 15, height = 12, units = 'in', 
         dpi = 300, limitsize = TRUE)



# 3c. amount of non-FFS prescribers ####

prescribers_UTI_linked <- pharmanetData %>% 
  filter(dxgroup == 'UTI-incl symptoms') %>% 
  group_by(dxgroup, PRAC_STUDY_ID, LHA_NUM) %>% 
  count()


prescribers_unlinked <- pharmanetData %>% 
  filter(dxgroup == 'Unlinked') %>% 
  group_by(dxgroup, PRAC_STUDY_ID, LHA_NUM) %>% 
  count() 

# the number of prescribers in a LHA that never had a UTI-linked prescription in the study period
prescribers_unlinked %>% 
  # return all rows from x where there are not matching values in y, keeping just columns from x.
  anti_join(., prescribers_UTI_linked, by = 'PRAC_STUDY_ID') %>% 
  group_by(LHA_NUM) %>% 
  count() %>% 
  arrange(desc(nn))

# the number of prescribers in a LHA that had UTI-linked prescriptions 
prescribers_UTI_linked



# 3c. looking at duration of UTI-linked prescriptions  ####
uti_duration_ridge <- UTIlinked_data %>% 
  mutate(post = as.factor(ifelse(date < as.Date('2016-06-01'), 'Pre', 'Post'))) %>% 
  filter(coverageLevel != 'NA')


ggplot(uti_duration_ridge, aes(x = DSPD_DAYS_SPLY, y = coverageLevel, fill = post)) +
  ggridges::geom_density_ridges2(scale = 1)+
  scale_x_log10(expand = c(0.01, 0))+
  scale_y_discrete(expand = c(0.01, 0))+
  theme_minimal()+
  scale_fill_brewer(palette = 'Set1')+
  facet_wrap(~post)+
  labs(x = 'log(dispensed days)', y = '')
ggsave('UTI_duration_ridgeline.png', plot = last_plot(), 
       path = paste(here::here(),'figures', sep = '/'),
       scale = 1, width = NA, height = NA, dpi = 300, limitsize = TRUE)

linked_only <- UTIlinked_data %>% 
  filter(dxgroup == 'UTI-incl symptoms', date < as.Date('2016-06-01')) 

plot(density(UTIlinked_data$DSPD_DAYS_SPLY), log = 'x', main = 'Distribution of Days of Supply for UTI-linked prescriptions in the pre-intervention period',
     xlab = 'Days of Supply')



ggplot(uti_duration_ridge, aes(x = DSPD_DAYS_SPLY, y = post, fill = post)) +
  ggridges::geom_density_ridges2(scale = 1)+
  scale_x_log10(expand = c(0.01, 0))+
  scale_y_discrete(expand = c(0.01, 0))+
  theme_minimal()+
  scale_fill_brewer(palette = 'Set1')+
  labs(x = 'log(DOS)', y = 'Density', fill = 'Intervention era')+
  ggsave('UTI_duration_ridgeline.png', plot = last_plot(), 
         path = paste(here::here(),'figures', sep = '/'),
         scale = 1, width = 7, height = 7, dpi = 600, units = 'in', limitsize = TRUE)


# 3d. how much nitrofurantoin (J01XE01) is unlinked in this time period ####
nitro_unlink_trends <- pharmanetData %>% 
  filter(FINAL_ATC == 'J01XE01', coverageLevel != 'NA') %>% 
  group_by(coverageLevel, dxgroup, FINAL_ATC, month, year) %>% 
  summarise(n = n()) %>% 
  group_by(coverageLevel, month, year) %>% 
  mutate(sum_cvg_lvl = sum(n)) %>% 
  ungroup() %>% 
  mutate(date = as.Date(paste('01',month, year, sep = '-'), format = '%d-%m-%Y'),
         prop = round(100*n/sum_cvg_lvl, digits = 1))

ggplot(nitro_unlink_trends, aes(x = date, y = prop, fill = dxgroup))+
  geom_bar(stat = 'identity')+
  facet_wrap(~coverageLevel)+
  #geom_smooth(se = F)+
  lims(y = c(0,100))+
  ggsave('nitro_linking.png', plot = last_plot(), 
         path = paste(here::here(),'figures', sep = '/'),
         scale = 1, width = 15, height = 10, units = 'in', 
         dpi = 300, limitsize = TRUE)




# 3e. chronologic time visualization LHA level ####
chron_time_LHA <- UTIlinked_data %>% 
  group_by(LHA_NUM, month, year) %>% 
  summarise(rx_n = n(), totalDDD = sum(DDD), totalDOS = sum(DSPD_DAYS_SPLY)) %>% 
  left_join(., geographicLHApopn %>% 
              select(LHA_NUM, MINdate), by = 'LHA_NUM') %>% 
  ungroup() %>% 
  mutate(intervention = ifelse(!is.na(MINdate), 'Intervention', 'Control'), 
         date = as.Date(paste('01', month, year, sep = '-'), format = '%d-%m-%Y'),
         MINdate = as.Date(MINdate, format = '%m/%d/%Y'))

set.seed(20)
chron_time_LHA <- chron_time_LHA %>% 
  filter(LHA_NUM %in% c(sample(x = pull(chron_time_LHA %>% 
                                          filter(intervention == 'Intervention') %>% 
                                          distinct(LHA_NUM)), size = 12, replace = F), pull(chron_time_LHA %>% 
                                                                                              filter(intervention == 'Control') %>% 
                                                                                              distinct(LHA_NUM))))

point_dates <- chron_time_LHA %>% 
  distinct(LHA_NUM, MINdate, intervention) %>% 
  mutate(MINdate = gsub(pattern = '\\d\\d$', x = paste(MINdate), replacement = '01'),
         MINdate = as.Date(MINdate, format = '%Y-%m-%d')) %>% 
  left_join(., chron_time_LHA %>% 
              select(LHA_NUM, date, totalDDD) %>% 
              rename(MINdate = date), by = c('LHA_NUM', 'MINdate'))

ggplot(chron_time_LHA, aes(x = date, y = totalDDD, group = LHA_NUM, color = intervention))+
  geom_line(size = 1)+
  geom_point(data = point_dates, aes(x = MINdate, y = totalDDD, group = LHA_NUM), size = 3)+
  labs(x = 'Date', y = 'total DDDs')+
  facet_wrap(~intervention)+
  theme_bw()+
  ggsave(filename = 'chron_time_LHA.png', device = 'png',
         path = paste(here::here(),'figures', sep = '/'))






# 3f. trends in prescribing by UTI-diagnosis type ####
unique(UTIlinked_data$dxgroup)

UTI_dx_graph_data <- UTIlinked_data %>% 
  filter(dxgroup == 'UTI-incl symptoms') %>% 
  group_by(month, year, dx, LHA_NUM) %>%
  summarise(DOS = sum(DSPD_DAYS_SPLY)) %>% 
  ungroup() %>% 
  mutate(date = as.Date(paste(year, month, '01', sep = '-')), format = '%Y-%m-%d')

ggplot(UTI_dx_graph_data, aes(x = date, y = DOS, color = dx))+
  geom_line()+
  facet_wrap(~LHA_NUM)

