# read user input parameters from a google sheet

# These parameters are things that change every week
# Ex: file name, title of the complete data etc.

# Input sheet ----

user_param_table <- read_sheet(sheeturls$user_inputs) %>% 
  .[-1,-1]  # Remove the first row (second row in the sheet) : description and first column : row headers
  

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


# Make variables ----

# Run a vectorized function to turn tibble column name -- contents into variable name -- values
map2(user_param_table, 
     colnames(user_param_table),
     ~ tbl.colm_to_assigmnent(.x, .y)
     # ~ eval_tidy(call2(`<-`, .y, .x[!sapply(.x, function(x) is.null(x)||is.na(x))], envir = global_env()))
     )
