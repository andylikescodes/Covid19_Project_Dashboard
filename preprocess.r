library(readxl)
library(tidyverse)
library(Dict)

# Read in Data
request_lookup = read_excel('Combined_Request_Lookup.xlsx', skip=1)
dict_tracking = read_excel('COVID_Data_Dictionaries_tracking.xlsx')
data_dictionary = read_excel('Combined_Data_Dictionary.xlsx', skip=1)

labels <- unlist(unique(dict_tracking['Descriptive_Label']))
labels <- labels[-c(41,40,39)]
tracking <- unlist(unique(dict_tracking['Request_Label']))
dict_measures <- unique(data_dictionary$Measure)
measures <- intersect(tracking, dict_measures)



# Find the waves that have data for a given measure---------------------------------------------------------
findWaves <- function(measure){
  
  # Gets the rows of data for given measure
  rows <- data_dictionary[data_dictionary$Measure %in% measure,]
  rows <- subset(data_dictionary, Measure %in% measure)
  
  
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
  rows <- data_dictionary[data_dictionary$Measure %in% measure,]
  rows <- subset(data_dictionary, Measure %in% measure)
  questions <- vector()

  for(i in 1:nrow(rows)){
    questions[i] <- rows[i,4]
  }
    
  return(questions)
}


# Create measure/wave/question tibble

generateQ_Wave_Map <- function(){
  
  waves <- list()
  questions <- list()
  for(i in 1:length(measures)){
    temp <- measures[i]
    wave <- findWaves(temp)
    waves[i] <- list(wave)
    qs <- grabQuestions(temp)
    questions[i] <- list(qs)
  }
  
  question_wave_map <- tibble(measure=labels, wave=waves, questions= questions)
  return(question_wave_map)
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


