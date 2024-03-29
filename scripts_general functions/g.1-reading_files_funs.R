# general functions to read files
# reading files and manipulating columns

# read in the excel file (from row 36 onwards)
readqpcr <- function(flnm)
{
  fl <- flnm %>%  
    excel_sheets() %>% 
    set_names(.,.) %>% 
    map(read_excel, path = flnm, skip = 38)
  
  # convert CT values into numeric 
  class(fl$Results$CT) <- 'numeric'
  fl
}


# Gets the 96 well layout with template names matching the experiment ID from filename in a google sheet
get_template_for <- function(bait, sheet_url = sheeturls$templates)
{ # Looking for WWx or Stdx - example WW21 or Std7 within the filename; Assumes plate spans from row B to N (1 row below the matching ID)
  
  # Finding the plate to be read
  plate_names_row <- read_sheet(sheet_url, sheet = 'Plate layouts', 
                                range = 'C:C', col_types = 'c', 
                                col_names = FALSE)
  
  m_row <- plate_names_row %>% unlist() %>% as.character() %>% 
    # find the row with standard beginings matching the filename
    str_detect(., str_c('^', bait %>% str_match('^(WW|Std|dd.WW)[:alnum:]*') %>% .[1]) ) %>% 
    which() + 1 # returns the row number where '<>' is supposed to be found
  
  range_to_get <- str_c('B', m_row + 1, ':N', m_row + 9) # extrapolate to where the grid is
     # IMPORTANT : FIRST ROW SHOULD BE EMPTY LINE!!
  
  # Eror message and terminate if plate ID is not unique
  if(length(m_row) > 1) stop( str_c('Plate ID of :', bait, 'repeats in', paste0(m_row, collapse = ' & '), 'row numbers. Please fix and re-run the script', sep = ' '))
  
  # read the template corresponding to the file name
  plate_template_raw <- read_sheet(sheet_url, sheet = 'Plate layouts', range = range_to_get)
  
  # checking if row and column names are unique, else throw an error 
  plate_template_raw %>% {if((.[,'<>']) %>% anyDuplicated | 
                             colnames(.) %>% anyDuplicated) 
    stop(str_c(bait, ' : plate layout\'s row letters and column numbers are not unique. Please correct it and rerun'))}
  
  # Convert the 96 well into a single column, alongside the Well position
  plate_template <- read_plate_to_column(plate_template_raw, 'Sample_name') # convert plate template (Sample_names) into a single vector, columnwise
  
  }
  