# investigating blanks

# prelims ----
# running generation functions presumed

rts <- c('dd.WW126_Schools+WWTP_N1N2', 'dd.WW127_Schools+WWTP_B117', 'dd.WW128_Schools_N1N2', 
         'dd.WW129_0210_Con+0209_Schools_Rerun', 'dd.WW130_0209_WWTP_B117Redo', 'dd.WW131_0209_WWTP_N1N2Redo')

# input ----

# Get and merge the two datasets from data dump
raw_quant_data <- map_dfr(rts, 
                          ~ read_sheet(sheeturls$data_dump, sheet = .x, range = 'A:L') %>%
                            filter(str_detect(assay_variable, '^DI|BLANK') | str_detect(Sample_name, 'NTC')) %>% 
                            mutate(run_ID = str_extract(.x, 'dd.WW([:digit:]*)'), .before = 1,
                                   across(where(is.list), as.character),
                                   across(biological_replicates, as.numeric),
                                   across(assay_variable, ~ str_replace(., '^0$', 'NTC')))
)

# plot ----
ggplot(raw_quant_data, 
       aes(x = assay_variable, y = Positives, colour = run_ID)) + 
  geom_point() 

ggsave('qPCR analysis/Extra graphs/dd.126-131_DI-blanks.png')
