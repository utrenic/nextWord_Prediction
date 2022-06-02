# nextWord_Prediction
Data Science Specialization Capstone Project - Next Word Prediction

This project was the last project of a 10 course Data Science track given by Johns Hopkins University. It was about a next-word prediction algorithm. A <html href=https://kleung.shinyapps.io/nextWord/>shiny app</html> is used to demonstrate such word prediction algorithm. It takes an input as a phrase (i.e. multiple words) in a text box and outputs a prediction of the next word accordingly. Swiftkey, an industrial partner, had provided us with the data. It can be downloaded <html http://127.0.0.1:61997/rmd_output/1/%3Chttps://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip>here</html>.

The job was to clean and analyze a large corpus of unstructured text and build a word prediction model on the web using shiny library in R. Since the corpus is too large, we had created the samples for twitter, news, and blogs and then we combined these samples (8000 lines samples on each type of data) into a combined corpus. We then created tokens of the corpus by removing the punctuation, symbols, numbers, url and profanity words that can be downloaded from the web. After that, we generated n-gram words (4-gram word, 3-gram word, 2-gram word, and unigram word) and save them into R objects for faster lookup. We implemented a version of stupid backoff algorithm for next word prediction and the prediction is based on the ranked sample frequency of those words. For a given phrase, we tokenized it first like the way we generated n-gram word files. Then, we looked up next word based on first n-1 grams. For example, if the input phrase contains 3-grams, we will match the first three gram in the 4-gram table. If there are a match of the first 3-grams, we simply return the 4th-gram based on maximizing the frequency of occurrence in those filtered set of words. If there is no match, we simply use the last two-gram to do that word prediction. The process is recursively done and may end up with having 1-gram word in the worse case scenario. The result seems make sense for many simple input phrases like “I love”, or “One of the”, etc. Note, the accuracy of the prediction of next word purely depends on the sample we created.


