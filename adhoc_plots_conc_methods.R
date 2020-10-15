# Ad hoc plots for concentration methods

# Dilution factors plot
plt.dilution <- {ggplot(vol_R, aes(Sample_name, dilution_factor, colour = Target)) + geom_jitter() + ggtitle('Conc. methods2 - Dilution factors')} %>% format_logscale_y() %>% format_classic()  %>% print()

ggsave('qPCR analysis/Methods paper/Archive/dilution factors.png', plt.dilution, width = 8, height = 4)


# Prelim plot after dilution
{ggplot(vol_R, aes(Sample_name, dilution_factor, colour = Target)) + geom_jitter() + ggtitle('Conc. methods2 - preliminary plot')} %>% format_logscale_y() %>% format_classic()  %>% print()


# Prelim plot after dilution - copies per ul RNA
prelim_copy_RNA <- {ggplot(processed_quant_data, aes(Sample_name, `Copy #`, colour = Target)) + 
    geom_jitter() + 
    ggtitle('Conc. methods2 - copies/ul RNA- preliminary plot')} %>% 
  format_logscale_y() %>%
  format_classic()  %>% 
  print()

ggsave('qPCR analysis/Methods paper/Archive/conc_methods_2_RNA.png', prelim_copy_RNA, width = 8, height = 4)


# copies per L ww plot
prelim_copy_WW <- {ggplot(processed_quant_data, aes(Sample_name, Recovered, colour = Target)) + 
    geom_jitter() + 
    ggtitle('Conc. methods2 - copies/L WW - preliminary plot')} %>% 
  format_logscale_y() %>% 
  format_classic()  %>% 
  print()

ggsave('qPCR analysis/Methods paper/Archive/conc_methods_2_WW.png', prelim_copy_WW, width = 8, height = 4)


# N1-N2 zoom on NTC
plotly::ggplotly(prelim_copy_RNA, dynamicTicks = T)


# NTC DI focus analysis ----

source('./general_functions.R') # Source the general_functions file

read_these_sheets <- c( 'dd.WW52_Conc method 2b_N1N2 + BCoV2',
                        'dd.WW53_Conc Meth 2_N1N2',
                        'dd.WW54_Conc Meth 2_BCoV2',
                        'dd.WW55_S38 boil_BCoV2',
                        'WW79_Conc Methods 2a_pMMoV_Std58',
                        'WW80_Conc Methods 2b_pMMoV_Std59')

list_raw_quant_data <- map(read_these_sheets, 
                           ~ read_sheet(sheeturls$data_dump, sheet = .) %>% 
                             select(-any_of(c('Conc(copies/µL)', 'Concentration'))) %>% 
                             rename('Accepted Droplets' = any_of('AcceptedDroplets'))
                           )

# bind multiple reads and clean up names
raw_quant_data <- bind_rows(list_raw_quant_data) %>% 
  rename(Sample_name = any_of('Sample Name')) %>% # if name is according to old convension, this will rename it 
  # unite('Biobot_id', c(Sample_name, assay_variable), sep = '', remove = F) %>%
  select(-`Well Position`) %>% 
  mutate_at('assay_variable', as.character) %>% 
  mutate_at('biological_replicates', ~str_replace_na(., '')) %>% 
  mutate_at('Tube ID', ~str_remove(., "\\.")) %>% 
  unite('Label_tube', c('Sample_name', 'Tube ID'), sep = "", remove = F) %>%  # make a unique column for matching volumes 
  mutate(across(Sample_name, ~ fct_relevel(.x, 'NTC', after = Inf)),
         Sample_status = if_else(str_detect(Sample_name, 'NTC') | str_detect(assay_variable, 'DI'), 'Controls', 'Regular'),
         across(assay_variable, ~ if_else(.x == '0', 'NTC', .x) )) %>% 
  filter(!str_detect(Sample_name, 'Vacboil|Std'))

# filter negative controls
negative_controls <- raw_quant_data %>% 
  filter(str_detect(Sample_name, 'NTC') | str_detect(assay_variable, 'DI')) %>% 
  select(Sample_name, assay_variable, `Copy #`, Positives, Negatives, `Accepted Droplets`, everything())

# plots ----
plt.positives_neg_controls <- {ggplot(negative_controls, aes(Sample_name, Positives, colour = Target, shape = assay_variable)) + 
  geom_jitter() + 
  # geom_point(aes(y = `Accepted Droplets`), shape = '.') + 
  ggtitle('Droplet count for negative samples')} %>% 
  format_classic() %>% 
  print()

plt.positives_neg_controls + ylim(c(0,20))

plt.positives_all <- {ggplot(raw_quant_data, aes(Sample_name, Positives, colour = Target)) + 
    geom_jitter() + 
    facet_grid(~ Sample_status, scales = 'free') + 
    # geom_point(aes(y = `Accepted Droplets`), shape = '.') + 
    ggtitle('Droplet count for negative samples')} %>% 
  format_classic() %>% 
  print()

ggsave('qPCR analysis/Methods paper/Archive/negative controls.png', plot = plt.positives_neg_controls, width = 8, height = 4)
ggsave('qPCR analysis/Methods paper/Archive/negative controls zoom.png', plot = plt.positives_neg_controls + ylim(c(0,20)), width = 8, height = 4)
ggsave('qPCR analysis/Methods paper/Archive/positive_droplets.png', plot = plt.positives_all, width = 8, height = 4)


# output data ----
write_csv(negative_points, 'qPCR analysis/Methods paper/Archive/negative_points_conc methods2.csv', na = '')
