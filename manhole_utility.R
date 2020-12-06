source('1-processing_functions.R', encoding = 'UTF-8')

sheet <- 'dd.WW86_1203_Manehole+redos'

#-----------------------------------------------------


data <- read_sheet(sheeturls$data_dump, sheet)
lookup_table <- read_sheet(sheeturls$biobot_id, 'All manhole')
