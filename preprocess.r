library(readxl)
library(tidyverse)
library(Dict)

## Read in Data -----------------------------------
dat = tibble(read.csv('Wave1-8_A-E.csv'))
request_lookup = read_excel('Combined_Request_Lookup.xlsx', skip=1)
data_dictionary = read_excel('Combined_Data_Dictionary.xlsx', skip=1)
demo_survey = read_excel('Comparison of Demographic Surveys.xlsx', skip=1)


# Generate demographic categories
demo_data <- demo_survey[1:44,]
values <- c('Likert', 'Likert', 'Numeric Factor', 'Categorical Factor', 'Binary',
            'Numeric Factor', NA, 'Categorical Factor', 'Y/N/O', 'Categorical Factor',
            'Binary', 'Binary', 'Categorical Factor', 'Numeric Factor', 'Numeric Factor',
            'Binary', 'Categorical Factor', 'Y/N/O', 'Numeric Factor', 'Y/N/O', 'Categorical Factor',
            'Y/N/O', 'Categorical Factor', 'Categorical Factor', 'Categorical Factor', 
            'Categorical Factor', 'Likert')

val_types <- tibble(values, unique(demo_data$Text))


# Processing data dictionary tracking
dict_tracking = read_excel('COVID_Data_Dictionaries_tracking.xlsx')
dict_tracking = dplyr::rename(dict_tracking, Descriptive_label = ...1)
dict_tracking = dict_tracking[!is.na(dict_tracking$Request_Label),]

dummy_waves <- colnames(data_dictionary %>% select(8:22))


## Main preprocessing step:
# Get unique_request_labels
response_rows= request_lookup[(request_lookup$Scores==0) & (request_lookup$DataType=='integer'),]
measures = unique(response_rows$Request_Label)
demographics = measures[startsWith(measures, 'Dem')]
measures = measures[!startsWith(measures, 'Dem')]
demographic_varnames <- response_rows[startsWith(response_rows$unified_variable_names, 'Dem'),]$ElementDescription

# Generate select categories
cats = character()
for (mea in measures){
  cats = c(cats, get_descriptive_labels(dict_tracking, mea))
}

# Get descriptive labels
get_descriptive_labels <- function(dict_tracking, label){
  tmp = dict_tracking[dict_tracking$Request_Label==label, 'Descriptive_label']
  shape = dim(tmp)
  if (shape[1] == 0) {
    return(label)
  } else {
    return(pull(tmp))
  }
}

# Remove outlier for age:
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}



# Find the waves that have data for a given measure---------------------------------------------------------
findWaves <- function(measure){
  
  # Gets the rows of data for given measure
  rows <- subset(response_rows, Request_Label %in% measure)
  
  wave_cols <- rows %>% select(starts_with("wave"))
  wave_exist <- vector()
  i <- 1
  
  for(col in wave_cols){
    for (row in col){
      if (is.na(row)){}
      else{
        wave_exist[i] <- colnames(wave_cols[i])
      }
    }
    i <- i + 1
  }  
  
  if (length(wave_exist) == 0){
    return(NA)
  }
  
  #Remove NA
  wave_exist <- wave_exist[! wave_exist %in% NA]

  return(wave_exist)
}

# Grab the questions associated with given measure----------------------------------------------------------
grabQuestions <- function(measure){
  
  # Gets the rows of data for given measure
  rows <- response_rows[(response_rows$Request_Label == measure),]
  #rows <- subset(data_dictionary, Measure %in% measure)
  return(rows$ElementDescription)
}

# Retrieve Text ---------------------------------------------------------------------------------------------
get_text <- function(dat, data_dictionary, variable){
  state_text = data_dictionary[data_dictionary$Variable==variable, 'Text']$Text
  test = unlist(strsplit(state_text, ';'))
  v_num = character()
  v_char = character()
  for (txt in test){
    tmp <- txt %>% str_trim(.)
    for (val_key in strsplit(tmp, '=')){
      num = str_trim(val_key[1])
      char = str_trim(val_key[2])
      v_num <- c(v_num, num)
      v_char <- c(v_char, char)
    }
  }
  out <- character()
  for (val in dat[,variable]){
    out <- c(out, v_char[val])
  }
  return(out)
}

# Plot raw data
