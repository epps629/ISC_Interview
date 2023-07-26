# Load required packages
library(tidyverse)
library(quanteda)

# Read survey data into a data frame (replace "survey_data.csv" with your actual file name)
survey_data <- read_csv("survey_data.csv")

# Perform preprocessing and create a corpus
corpus <- corpus(survey_data$Responses)  # Assuming the survey responses are in a column named "Responses"

# Perform text cleaning and preprocessing
corpus_clean <- corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_separators = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_tolower()

# Perform qualitative analysis
# Example: Calculate word frequencies
word_freq <- dfm(corpus_clean) %>%
  dfm_trim(min_termfreq = 5) %>%  # Adjust the minimum term frequency as needed
  topfeatures(n = 10, sort = TRUE)

# Print the word frequencies
print(word_freq)

# Example: Perform topic modeling using Latent Dirichlet Allocation (LDA)
lda_model <- corpus_clean %>%
  tokens_remove(c("common", "words", "to", "exclude")) %>%  # Add any additional words to exclude
  dfm() %>%
  convert(to = "topicmodels")

topics <- textstat_topics(lda_model, n = 5)  # Specify the number of topics to extract

# Print the identified topics
print(topics)
