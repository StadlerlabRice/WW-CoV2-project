
# Plotting data from 2 csv files (between 2 weeks) for comparision of consistent increase or decreases //
# Written to troubleshoot any specific method aberrations for a subset of samples that could be resulted in systemic decreases of 
# both N1 and N2 for those set of samples

# lauren Bauhs brought up 12 samples that seemed to be kept for extra 30 mins in fridge before bead beating

fldr <- 'excel files/Weekly data to HHD/'

datin <- c('1221 Rice', '1228 Rice') %>%   # get these two csv files
  str_c(fldr, ., '.csv') %>% 
  map2_dfr(., 
           c(1221, 1228),  # Add the date as a number
           ~ read_csv(.x) %>% 
            mutate(weekid = .y, .before = 1))

datsumm <- datin %>% 
  group_by(weekid, Target_Name, WWTP) %>% 
  # mutate(mean_copies_per_liter_WW = mean(Copies_Per_Liter_WW), 
  #        .after = 1) %>% 
  summarize(mean_copies_per_liter_WW = mean(Copies_Per_Liter_WW, na.rm = T)) %>% 
  ungroup() %>% 
  
  mutate(weekid_plot = jitter(weekid))  # add some random noise for better plotting
  # mutate(WWTP_labels = if_else())

datdecrid <- datsumm %>% 
  select(weekid, Target_Name, WWTP, mean_copies_per_liter_WW) %>%
  pivot_wider(names_from = weekid, values_from = mean_copies_per_liter_WW) %>% 
  rename(w1228 = '1228', w1221 = '1221') %>% 
  filter(w1228 < w1221) %>% 
  pull(WWTP) %>% 
  unique() %>% 
  str_c(collapse = '|')

# jitter plot
ggplot(datsumm, aes(weekid_plot, mean_copies_per_liter_WW, colour = Target_Name)) + 
  geom_point() + 
  geom_line(aes(group = interaction(WWTP, Target_Name)),
            alpha = .2)

ggsave('qPCR analysis/Extra graphs/1221 to 1228 jitter.png')

# plot by wwtp

ggplot(datin, aes(WWTP, Copies_Per_Liter_WW, colour = as.factor(weekid))) + 
  geom_point() + 
  facet_wrap(~ Target_Name, nrow = 2) + 
  geom_line(data = datin %>% filter(str_detect(WWTP, datdecrid)), aes(group = WWTP), colour = 'black')

ggsave('qPCR analysis/Extra graphs/1221 vs 1228.png', width = 12, height = 4)
