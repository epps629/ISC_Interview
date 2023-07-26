# Load required libraries
library(tm)      # Text mining
library(dplyr)   # Data manipulation
library(ggplot2) # Data visualization

# Define the focus group data (replace with correct data)
focus_group <- c(
  "Participant 1: I really liked the new program. It's innovative and useful.",
  "Participant 2: The program site design could be improved. It's not user-friendly.",
  "Participant 3: I found the program to be ineffective.",
  "Participant 4: The technical support was excellent. They were very responsive.",
  "Participant 5: I didn't understand how to get in touch with the TCTAC. It needs better instructions.
  
)

# Create a document-term matrix
corpus <- Corpus(VectorSource(focus_group))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
dtm <- DocumentTermMatrix(corpus)

# Convert the document-term matrix to a data frame
dtm_df <- as.data.frame(as.matrix(dtm))
dtm_df <- dtm_df %>%
  mutate(Document = row.names(.)) %>%
  select(Document, everything())

# Perform coding and categorization
dtm_df$coding <- NA
dtm_df$coding[dtm_df$Document == "Participant 1"] <- "Positive"
dtm_df$coding[dtm_df$Document == "Participant 2"] <- "Negative"
dtm_df$coding[dtm_df$Document == "Participant 3"] <- "Negative"
dtm_df$coding[dtm_df$Document == "Participant 4"] <- "Positive"
dtm_df$coding[dtm_df$Document == "Participant 5"] <- "Negative"

# Analyze and visualize the coding results
coding_summary <- dtm_df %>%
  group_by(coding) %>%
  summarize(count = n())

# Print the coding summary
print(coding_summary)

# Visualize the coding summary
ggplot(coding_summary, aes(x = coding, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Coding", y = "Count", title = "Coding Summary")
