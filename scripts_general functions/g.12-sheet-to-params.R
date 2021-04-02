# read user input parameters from a google sheet

# These parameters are things that change every week
# Ex: file name, title of the complete data etc.

# Input sheet ----

user_param_table <- read_sheet(sheeturls$user_inputs) %>% 
  .[-1,-1]  # Remove the first row (second row in the sheet) : description and first column : row headers
  

# Parse data ----

# Takes a single column of a tibble and assigns the contents to variables named as the column names
# Convenient handle to obtain parameters from a google sheet
tbl.colm_to_assigmnent <- function(.singl_colm, .colm_name = NULL)
{ # this will not work with vectorization since each column enters this function as a vector (not a tibble)
  
  # print(1)
  # cleaned the column
  # colm.val <- .singl_colm %>%  # remove trailing nas - whitespaces from the sheet
  #   {case_when(is_tibble(.) ~ drop_na(.) %>% pull(1),  # removes NA from a tibble and get the contents of the column as a vector
  #              is_list(.) ~ .[!sapply(., is.null)] %>% as_vector(),  # removes NULL from lists and converts to vector
  #              is_vector(.) ~ .[!is.na(.)] )} # removes NA from a vector
  
  # Use this instead
  colm.val <- .singl_colm %>% .[!sapply(., function(x) is.null(x)||is.na(x))]
  
  # get the column name
  colm.nm <- if(is.null(.colm_name)) 
    colnames(cleaned_colm) else # get the name of the column from the tibble
      .colm_name  # take the name
  # throw an error if no column name is found
  if(is.null(colm.nm)) stop('No column name found, check and provide the `.colm_nam` argument to the function `cleaned_colm`')
  
  # Assign variable
  eval(call2(`<-`, colm.nm, colm.val), envir = global_env()) # assign a new variable colm.nm into the global environment
}


# Make variables ----

# Run a vectorized function to turn tibble column name -- contents into variable name -- values
map2(user_param_table, 
     colnames(user_param_table),
     ~ tbl.colm_to_assigmnent(.x, .y)
     # ~ eval_tidy(call2(`<-`, .y, .x[!is.na(.x)]))
     )
