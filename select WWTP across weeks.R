# Run the weekly comparison first
# Run the weekly_comparisons.Rmd relevant sections - to get to this variable: normalized_copies_N12

# selecting 3 WWTP
zach3 <- map(normalized_copies_N12, ~filter(., str_detect(WWTP, '47|NW|TT')))

# anonymization vector
anony_replace <- 1:3 %>% str_c('WWTP ', .) %>% setNames(c('47', 'NW', 'TT'))
anony_replace

zach3anony <- map(zach3, ~mutate(., 'WWTP' = str_replace_all(WWTP, anony_replace) )
)

plot_mean_sd_jitter(zach3anony, long_format = F, sample_var = extra_categories, exclude_sample = T, x_var = Week, y_var = `Viral load per capita per day`, facet_var = WWTP, colour_var = Target, ylabel = 'Viral load per capita per day', title_text = 'June 22 - August 17: 2020') + geom_line(aes(group = Target)) + facet_wrap(facets = ~WWTP, scales = 'free_x')

ggsave('qPCR analysis/Extra graphs/June-Aug_47,NW,TT.png', width = 12, height = 4)
