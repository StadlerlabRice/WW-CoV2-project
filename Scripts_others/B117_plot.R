# B117 variant plotting

# input ----

dt_in <- read_sheet(sheeturls$data_dump, sheet = 'dd.WW112_B117_Test-B117')

# process ----

dt_mid <- dt_in %>% 
  mutate(WWTP_ID = if_else(biological_replicates == ''|is.na(biological_replicates), str_c(Sample_name, '.', assay_variable) , 
                           str_c(Sample_name, '.', assay_variable, '-', biological_replicates)),
         Sample_full_name = str_c(Sample_name, '.', assay_variable) %>%  fct_relevel('NTC.0', 'WHC.0', 'B117.0', 'MIX.0'),
         .before = 1)  # add a unique column for each row to match to the complete data (for merge)
  
# plot ----

plt1 <- ggplot(dt_mid, aes(Sample_full_name, Copies_per_L_WW, colour = Variant)) + 
  geom_point()

lgplt1 <- plt1 %>% format_logscale_y() %>% print()

# output ----

ggsave('qPCR analysis/Extra graphs/B117 trial1-log.png', width = 7, height = 4)
ggsave('qPCR analysis/Extra graphs/B117 trial1.png', plot = plt1, width = 7, height = 4)
