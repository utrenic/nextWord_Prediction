library(stringi)                # character string processing package 
library(stringr)
library(dplyr)                  # a grammar of data manipluation 
library(knitr)                  # a general purpose tool for dynamic report generation in R
library(quanteda)               # Quantitative Analysis of Textual Data
library(RColorBrewer)           # ColorBrewer Palettes
library(rapportools)

# load up data from the precomputed files
badwords_lines = readRDS("badwords.rds")
uni_words <- readRDS("uni_words.rds")
bi_words <- readRDS("bi_words.rds")
tri_words <- readRDS("tri_words.rds")
four_words <- readRDS("four_words.rds")

# prediction function for next word
predict_next_word <- function(input, n=10) {
  # check the input to see if it it empty
  if (rapportools::is.empty(input)) {
    prediction = uni_words %>% select(nextWord, Prop) %>% arrange(Prop)     
    print.noquote("empty input: output is based on unigram sample probability")
    return (prediction[1:n,])
  }
  #now, input is not empty
  corp <- corpus(input)
  # tokenize the input
  txt <- tokens_tolower(
    tokens(corp, what = "word1",
           remove_punct = TRUE,
           remove_symbols = TRUE,
           remove_numbers = TRUE,
           remove_url = TRUE) %>%
      tokens_remove(badwords_lines))
  
  parsedInput = as.list(txt)$text1
  #
  plen = length(parsedInput)
  #
  #print.noquote(paste0("Input: ", parsedInput, sep=''))
  #print.noquote(paste0("Length: ", plen, sep=''))
  
  if (parsedInput[1] %in% four_words$word_1 &
      parsedInput[2] %in% four_words$word_2 &
      parsedInput[3] %in% four_words$word_3) {
    print.noquote("Use fourgram")
    prediction = four_words %>% filter(word_1 %in% parsedInput[1] &
                                         word_2 %in% parsedInput[2] &
                                         word_3 %in% parsedInput[3]) %>%
      select(nextWord, Prop) %>% 
      arrange(desc(Prop)) 
    # if nextWord is NA, it will use n-1 gram to predict
    if (is.na(prediction$nextWord[1])) {
      # use n-1 to predict = trigram in this case
      parsedInput_1 = paste(parsedInput[2], parsedInput[3], sep = ' ') 
      prediction = predict_next_word(parsedInput_1)
    }
    
  } else if (parsedInput[1] %in% tri_words$word_1 &
             parsedInput[2] %in% tri_words$word_2) {
    print.noquote("Use trigram")                  
    prediction = tri_words %>% filter(word_1 %in% parsedInput[1] &
                                        word_2 %in% parsedInput[2]) %>%
      select(nextWord, Prop) %>% 
      arrange(desc(Prop))
    # if nextWord is NA, it will use n-1 gram to predict
    if (is.na(prediction$nextWord[1])) {
      # use n-1 to predict = bigram in this case
      parsedInput_1 = paste(parsedInput[2]) 
      prediction = predict_next_word(parsedInput_1)
    }
    
  } else if (parsedInput[1] %in% bi_words$word_1) {
    print.noquote("Use bigram")        
    prediction = bi_words %>% filter(word_1 %in% parsedInput[1]) %>%
      select(nextWord, Prop) %>%
      arrange(desc(Prop)) 
    
  } else {
    print.noquote("Use unigram inside")        
    prediction = uni_words %>% select(nextWord, Prop) %>%  
      arrange(desc(Prop))
  }
  
  return(prediction[1:n,])
}


