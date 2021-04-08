# read user input parameters from a google sheet

# These parameters are things that change every week
# Ex: file name, title of the complete data etc.


# Check for ## RUN LOG ----

param_colm1 <- read_sheet(sheeturls$user_inputs, range = 'A1:A', col_names = FALSE) # read the first column of the user inputs (first visible sheet)
check_for_run_log <- param_colm1 %>% 
  pull(1) %>% 
  str_detect('## RUN LOG') # check which cell in the first column matches the run log start key

first_visible_sheetname <- sheet_properties(sheeturls$user_inputs) %>%  # get all sheets along with properties (within the user inputs googlsheet)
  filter(visible == TRUE) %>%  # retain only visible sheets
  pull(name) %>%  # Get the name of the 
  .[1] # first sheet 


# Ask for re-run permission ----

range_to_read <- if(any(check_for_run_log)) {
   str_c('A1:', which(check_for_run_log) - 3) # if RUN LOG key appears, read till 3 rows before it

  rerun_sheet_key <- menu(c('Yes', 'No'), 
                          title = paste("The sheet with the name", 
                                        first_visible_sheetname, 
                                        "has already been read and has a ## RUN LOG. Do you want to re-run and write another LOG?", 
                                        sep=" "))
  if (rerun_sheet_key == 2){
    # stop("Cancel selected, script aborted.")
    print(str_c("Cancen selected. script aborted without running : ", first_visible_sheetname))
  }
}


# Input sheet ----

user_param_table <- read_sheet(sheeturls$user_inputs, range = range_to_read) %>% # read user inputs (first visible sheet)
  .[-1,-1]  # Remove the first row (second row in the sheet) which is description and first column which is row headers
  

# Parse data ----

# Takes a single column of a tibble and assigns the contents to variables named as the column names
# Convenient handle to obtain parameters from a google sheet

# Takes each column of a tibble as a native type (by pull()) and assigns to a variable named after .colm_name
tbl.colm_to_assigmnent <- function(.singl_colm, .colm_name = NULL)
{ 
  
  # throw an error if no column name is found
  if(is.null(.colm_name)) stop('No column name found, check and provide the `.colm_nam` argument to the function `tbl.colm_to_assignment`')
  
  
  # Clean the column and make a vector
  colm.val <- .singl_colm %>% 
    # remove trailing whiteshaces from sheet
    .[!sapply(., function(x) is.null(x)||is.na(x))] %>% # Remove NAs if a vector and NULLs if a list
    as_vector() # convert to a vector

  
  # Assign variable
  eval(call2(`<-`, .colm_name, colm.val), envir = global_env()) # assign a new variable .colm_name into the global environment
}


# Make variables from sheet ----

# Run a vectorized function to turn tibble column name--contents into variable name--values
map2(user_param_table, 
     colnames(user_param_table),
     ~ tbl.colm_to_assigmnent(.x, .y)
     # ~ eval_tidy(call2(`<-`, .y, .x[!sapply(.x, function(x) is.null(x)||is.na(x))], envir = global_env()))
     )

# Evaluate the function calls (which are read in as strings)
samples_to_remove <- eval(parse_expr(samples_to_remove))
  # Can be generalized later with a if(is.call(..) ) check

# Designate the sheet name as title_name
title_name <- first_visible_sheetname