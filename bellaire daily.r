
# Script for Bellaire daily samples plotting (Basmah;s data)
# Run general functions.R or calculations_multiple_runs.R to load all dependancies

# Data import ----

# Import from complete data
in_dat <- read_sheet(sheeturls$complete_data, sheet = 'Bellaire daily and others')

# Data processing ----

plt_dat <- in_dat %>% 
  filter(str_detect(Facility, 'BT')) %>% 
  mutate(across(WWTP, as.numeric)) %>% 
  group_by(WWTP, `Target Name`) %>% 
  mutate(mean_copies_WW = mean(`Copies/l WW`), sd_copies_WW = sd(`Copies/l WW`))

# Plotting ----
{ggplot(plt_dat, 
               aes(WWTP, `Copies/l WW`, colour = `Target Name`)) + 
  geom_jitter(alpha = .3, width = .2) + 
  geom_point(aes(y = mean_copies_WW), size = 2) + 
    geom_errorbar(aes(ymin = mean_copies_WW - sd_copies_WW, ymax = mean_copies_WW + sd_copies_WW), width = .1) + 
    scale_x_continuous(breaks = c(1:14))
} %>% 
  format_logscale_y() %>% 
  format_classic() %>% 
  print()
