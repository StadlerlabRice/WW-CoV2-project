
# Script for Bellaire daily samples plotting (Basmah;s data)
# Run general functions.R or calculations_multiple_runs.R to load all dependancies

# Data import ----

# Import from complete data
in_dat <- read_sheet(sheeturls$complete_data, sheet = 'Bellaire daily and others')

day0_date <- '15/9/20'

# Data processing ----

plt_dat <- in_dat %>% 
  filter(str_detect(Facility, 'BT')) %>% 
  mutate(across(WWTP, as.numeric)) %>% 
  group_by(WWTP, `Target Name`) %>% 
  arrange(WWTP) %>% 
  mutate(mean_copies_WW = mean(`Copies/l WW`, na.rm = T), sd_copies_WW = sd(`Copies/l WW`, na.rm = T) ,
         date_only = dmy(day0_date) + WWTP,
         'Sampling date' = str_c( wday(date_only, label = T), '\n', month(date_only), '/', day(date_only))  %>% as_factor()
  )

# Plotting ----
plt.all <- {ggplot(plt_dat, 
               aes(WWTP, `Copies/l WW`, colour = `Target Name`)) + 
  geom_jitter(alpha = .3, width = .2) + 
  geom_point(aes(y = mean_copies_WW), size = 2) +
    geom_line(aes(y = mean_copies_WW)) +
    geom_errorbar(aes(ymin = mean_copies_WW - sd_copies_WW, ymax = mean_copies_WW + sd_copies_WW), width = .1) + 
    scale_x_continuous(breaks = c(1:14))
} %>% 
  format_logscale_y() %>% 
  format_classic() %>% 
  print()

# N1-N2 plot with line included
plt.ns <- {ggplot(plt_dat %>% 
                    filter(str_detect(`Target Name`, 'N')), 
                   aes(`Sampling date`, `Copies/l WW`, colour = `Target Name`)) + 
    geom_jitter(alpha = .3, width = .2) + 
    geom_point(aes(y = mean_copies_WW), size = 2) +
    geom_line(aes(y = mean_copies_WW, group = `Target Name`)) +
    geom_errorbar(aes(ymin = mean_copies_WW - sd_copies_WW, ymax = mean_copies_WW + sd_copies_WW), width = .1) + 
    # theme(legend.position = 'bottom') +
    ggtitle('Daily samples from Bellaire WWTP')
} %>% 
  # format_logscale_y() %>% 
  format_classic() %>% 
  print()

ggsave('qPCR analysis/Extra graphs/Bellaire daily/withline_N1N2.pdf', height = 4, width = 8)
ggsave('qPCR analysis/Extra graphs/Bellaire daily/withline_N1N2.png', height = 4, width = 8)
