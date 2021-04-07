# g.15 reading directly from csv in googledrive

# Work in progress : need to test
read_gdrive_csv <- function(read_this_sheet)
{
  library(googledrive)
  
  # read_this_sheet <- c( 'dd.WW123_0201_Schools+WWTP_B117_Rerun', 'dd.WW138_0301_SCHOOLS_N1N2')
  
  # run_ids_to_read <- read_this_sheet %>% str_extract('dd.WW[:digit:]*') %>% paste0(collapse = '|')
  csv_to_read <- read_this_sheet %>% str_c('.csv')
  
  list_all_csv_files_in_folder <- drive_get(id = 'https://drive.google.com/drive/u/0/folders/1aIek7-aqHe2l7EnUfZZUtox44bj3SZVQ') %>%
    drive_ls()
    
  csv_dribble <- list_all_csv_files_in_folder %>%
    filter(str_detect(name, csv_to_read))  # detect all the run ids
          
  # download csvs temporarily
  drive_download(csv_dribble)
  
  # read csvs
  lst_output <- read_csv(csv_to_read)
  
  # clean up
  unlink(csv_to_read)
  
  return(lst_output) # return a list of all the csv files read in
}