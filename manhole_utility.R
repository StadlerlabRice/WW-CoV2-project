source('1-processing_functions.R', encoding = 'UTF-8')

sheet <- 'dd.WW86_1203_Manehole+redos'
output <- '1203 Rice Manhole Report'

#-----------------------------------------------------

data <- read_sheet(sheeturls$data_dump, sheet)
lookup_table <- read_sheet(sheeturls$biobot_id, 'All manhole')
manhole_codes <- lookup_table %>% pull('Facility SYMBOL')

filtered <- data %>% filter(assay_variable%in%manhole_codes | Sample_name == 'NTC') %>%
  select(Sample_name, assay_variable, biological_replicates, Target, Positives, AcceptedDroplets) %>%
  mutate(Full_name = paste(Sample_name,assay_variable, sep="/")) %>%
  mutate(Negativeresult = Positives == 0, Positiveresult = Positives > 3, Unclearresult = Positives > 0 & Positives<=3) %>%
  mutate(Resultsymbol = case_when(Positives == 0 ~ "-",
                                  Positives >= 3 ~ "+",
                                  Positives > 0 & Positives < 3 ~ "?")) %>%
  group_by(Full_name) %>%
  mutate(Report_symbol = paste0(Resultsymbol, collapse=""))

quick_report <- filtered %>% select(Full_name, Report_symbol) %>% distinct() %>% arrange(Full_name)

write_csv(quick_report, path = str_c('excel files/Weekly data to HHD/', output, '.csv'), na = '') # output CSV file
