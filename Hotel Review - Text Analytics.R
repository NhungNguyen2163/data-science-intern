install.packages("ggthemes")
install.packages("qdap")
install.packages("dplyr")
install.packages('tm')
install.packages('wordcloud')
install.packages('plotrix')
install.packages('ggplot2')
install.packages('reshape2')
install.packages('quanteda')
install.packages('cld2')
install.packages("topicmodels")
install.packages("textclean")
install.packages("stringdist")
install.packages("parallel")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("wordcloud2")
install.packages("ggraph")
install.packages("tidygraph")
install.packages("igraph")
install.packages("servr")
install.packages("LDAvis")
library(LDAvis)
library(scales)
library(forcats)
library(servr)
library(ldatuning)
library(tidygraph)
library(RColorBrewer)
library(wordcloud2)
library(cowplot)
library(tidytext)
library(tidyverse)
library(topicmodels)
library(textstem)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(quanteda)
library(cld2)

# ====================== 1. READING AND EXPLORING DATA
# Call and name file
reviews <- read.csv("C:/Users/Nhung/Downloads/R/Forecasting/assignment/HotelsData.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
names(reviews)
reviews

# Check empty lines
empty_lines_count <- sum(reviews$Text.1 == "")
print(empty_lines_count)

# Check the distribution of English review - make sure representativeness
reviews$lang <- cld2::detect_language(reviews$Text.1)
table(reviews$lang)
reviews$lang
english_reviews <- reviews %>%  # Filter to use only English review
  filter(lang == "en")
print(paste("English reviews:", nrow(english_reviews))) # Print number os english reviews
head(english_reviews)
english_reviews$lang <- NULL


# ====================== 2. CREATING A SAMPLE DATASET
# Take a 2000 reviews sample dataset
set.seed(770)
sample_data <- sample_n(english_reviews, 2000)
nrow(sample_data)  
str(sample_data)
class(sample_data)

# Check the distribution of review score
summary(reviews$Review.score)
summary(english_reviews$Review.score)
summary(sample_data$Review.score)

# Create frequency table for Review.score
review_counts <- table(sample_data$Review.score)
review_data <- data.frame(
  Score = as.numeric(names(review_counts)),
  Count = as.numeric(review_counts)
)
review_data$Percentage <- review_data$Count / sum(review_data$Count) * 100 
# Create bar chart 
ggplot(review_data, aes(x = factor(Score), y = Count)) +
  geom_bar(stat = "identity", fill = "#4472C4") +
  geom_text(aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")), 
            vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Distribution of score in sample dataset",
       x = "Review Score",
       y = "Number of reviews") +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


# ==================== 3. DIVIDING SAMPLE BY SENTIMENT
# Create subset base on review score
posScore <- sample_data %>% filter(Review.score >= 4) # positive score
negScore <- sample_data %>% filter(Review.score <= 2) # negative score 
neutralScore <- sample_data %>% filter(Review.score == 3) # neutral score

# Save each group to CSV
#write.csv(posScore, "posScore.csv", row.names = FALSE)
#write.csv(negScore, "negScore.csv", row.names = FALSE)
#write.csv(neutralScore, "neutralScore.csv", row.names = FALSE)


# =================== 4. TEXT PREPROCESSING FUNCTIONS
# Function to check SPECIAL CHARACTERS
special_chars <- function(x) {
  x1 <- sapply(x, as.character)
  x2 <- paste(x1, collapse = " ")
  x3 <- unlist(strsplit(x2, split = ""))
  x3 <- x3[grepl("[^A-Za-z\\s]", x3)]
  x4 <- sort(unique(x3))
  return(x4)
}

# Function to REPLACE CONTRACTIONS
replace_contractions <- function(text) {
  text <- gsub("n't\\b", " not", text)      # can't → can not, didn't → did not
  text <- gsub("'s\\b", " is", text)        # it's → it is, he's → he is
  text <- gsub("'re\\b", " are", text)      # they're → they are
  text <- gsub("'ll\\b", " will", text)     # you'll → you will
  text <- gsub("'ve\\b", " have", text)     # I've → I have
  text <- gsub("'d\\b", " would", text)     # I'd → I would
  text <- gsub("\\bwon't\\b", "will not", text)  # won't → will not
  return(text)
}

# Function to CLEAN & LEMMATIZE
clean_and_lemmatize_corpus <- function(texts) {
  corpus <- VCorpus(VectorSource(texts))                              # [0] Create corpus
  corpus <- tm_map(corpus, content_transformer(replace_contractions)) # [1] Replace contractions
  corpus <- tm_map(corpus, content_transformer(tolower))              # [2] Convert to lowercase
  corpus <- tm_map(corpus, content_transformer(function(text) {       # [3] Replace punctuation with space
    gsub("[[:punct:]]", " ", text)
  })) 
  all_stopwords <- unique(c(stopwords("en"), stop_words$word))
  corpus <- tm_map(corpus, removeWords, all_stopwords)                # [4] Remove standard English stopwords
  corpus <- tm_map(corpus, removeNumbers)                             # [5] Remove numbers
  corpus <- tm_map(corpus, content_transformer(function(text) {       # [6] Replace special characters
    gsub("[^A-Za-z]", " ", text)
  }))
  corpus <- tm_map(corpus, content_transformer(function(text) {       # [7] Normalize whitespace
    gsub("^\\s+|\\s+$", "", gsub("\\s+", " ", text))
  }))
  special_chars_remain <- special_chars(corpus)                       # [8] Check remaining special characters
  cat("Special characters remaining after cleaning:", 
      ifelse(length(special_chars_remain) == 0, "None\n", paste(special_chars_remain, collapse = ", ")))
  corpus <- tm_map(corpus, content_transformer(function(text) {       # [9] Lemmatize text
    lemmatize_strings(text)
  }))
  return(corpus)
}

pos <- clean_and_lemmatize_corpus(posScore$Text.1)
neg <- clean_and_lemmatize_corpus(negScore$Text.1)
neutral <- clean_and_lemmatize_corpus(neutralScore$Text.1)
sample <- clean_and_lemmatize_corpus(sample_data$Text.1)


# Get top 100 common words
get_top_words <- function(corpus_cleaned, top_n = 100) {
  text_vector <- sapply(corpus_cleaned, content)
  df <- data.frame(text = text_vector, stringsAsFactors = FALSE)
  top_words <- df %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) %>%
    head(top_n)
  return(top_words)
}
top100_words <- get_top_words(sample)
write_csv(top100_words, "C:/Users/Nhung/Downloads/R/Forecasting/assignment/top100_words_sample.csv")

#List of custom stopwords
custom_stopwords <- c( "just", "also", "ahead",'use',"my", "london", "say", "even", "give",
                       'make', 'ask', "stay", "nice", "little", "lot", 'think', 'id', "tube", "etc",'hotel',
                       "one", "tell", "get", "day", "take", "can", "much", "many", "two", "thing",
                       "however", "need", "back", "find", 'another', "book", "room", "king cross",
                       "blah", "see", "want", "time", "turn", "know", "night","document", "visit", 
                       "similar", 'school', "us", "everything", "minute", "english", "review", "experience",
                       "lot", "business", "trip", "travel", "st", "hour", "station", 
                       "min", "convent", "hyde", "th", "con", "road", "leave", "am", "pm", "street",
                       "oxford", "bridge", "morning", "spring")

# Function to REMOVE CUSTOM STOPWORDS
remove_custom_stopwords <- function(corpus, stopwords_vec) {
  corpus <- tm_map(corpus, removeWords, stopwords_vec)
  # Normalize whitespace again (after delete the stopwords)
  corpus <- tm_map(corpus, content_transformer(function(text) {
    text <- gsub("\\s+", " ", text) 
    text <- trimws(text)            
    return(text)
  }))
  return(corpus)
}

pos_clean <- remove_custom_stopwords(pos, custom_stopwords)
neg_clean <- remove_custom_stopwords(neg, custom_stopwords)
neutral_clean <- remove_custom_stopwords(neutral, custom_stopwords)
sample_clean <- remove_custom_stopwords(sample, custom_stopwords)
print(sample_clean[[11]]$content)

# ============================== 5. EXTRACT TOP TERMS AND VISUALIZATION FUNCTIONS
# Function to GET TOP WORDS
get_top_terms <- function(corpus, top_n = 50, min_word_length = 3) {
  # Create DTM
  dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(min_word_length, Inf)))
  term_matrix <- as.matrix(dtm)
  term_freq <- colSums(term_matrix)
  term_freq_sorted <- sort(term_freq, decreasing = TRUE)
  top_terms <- head(term_freq_sorted, top_n)
  df_top_terms <- data.frame(term = names(top_terms), freq = top_terms)
  return(df_top_terms)
}

# Function to PLOT BAR CHART FOR TOP WORDS
plot_top_terms <- function(term_df, title_suffix = "", top_n = 20) {
  plot_df <- head(term_df, top_n)
  ggplot(plot_df, aes(x = reorder(term, freq), y = freq)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Top", top_n, "Most Frequent Words", title_suffix),
         x = "Word", y = "Frequency") + theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
                                                                axis.title = element_text(face = "bold"))
}

# Function to CREATE WORD CLOUD
plot_wordcloud <- function(term_df, max_words = 50, title = "Word Cloud") {
  set.seed(770)
  # Plot word cloud
  wordcloud(words = term_df$term,
            freq = term_df$freq,
            max.words = max_words,
            random.order = FALSE,
            colors = brewer.pal(8, "Dark2"),
            scale = c(4, 1))
  rot.per = 0
  title(title, col.main = "black")
}

# Function to GENERATE & ANALYZE BIGRAMS
generate_bigrams <- function(corpus, top_n = 20) {
  texts <- sapply(corpus, content)
  df <- data.frame(text = texts, stringsAsFactors = FALSE)
  bigrams_df <- df %>%     # Tokenize into bigrams
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
  bigram_counts <- bigrams_df %>%    # Count bigram frequencies
    count(bigram, sort = TRUE)
  return(head(bigram_counts, top_n))
}

# Function to PLOT BIGRAMS
plot_bigrams <- function(bigram_df, title_suffix = "", top_n = 20) {
  plot_df <- head(bigram_df, top_n)
  ggplot(plot_df, aes(x = reorder(bigram, n), y = n)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    coord_flip() +
    labs(title = paste("Top", top_n, "Most Frequent Bigrams", title_suffix),
         x = "Bigram", y = "Frequency") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

# [1] Extract top term
pos_terms <- get_top_terms(pos_clean)
neg_terms <- get_top_terms(neg_clean)
neutral_terms <- get_top_terms(neutral_clean)
sample_terms <- get_top_terms(sample_clean)

plot_top_terms(pos_terms, title_suffix = "(Positive Reviews)")
plot_top_terms(neg_terms, title_suffix = "(Negative Reviews)")
plot_top_terms(neutral_terms, title_suffix = "(Neutral Reviews)")
plot_top_terms(sample_terms, title_suffix = "(Sample Reviews)")

plot_wordcloud(pos_terms, title = "Positive Reviews")
plot_wordcloud(neg_terms, title = "Negative Reviews")
plot_wordcloud(neutral_terms, title = "Neutral Reviews")
plot_wordcloud(sample_terms, title = "Sample Reviews")

# [2] n-bigram
pos_bigrams <- generate_bigrams(pos_clean)
neg_bigrams <- generate_bigrams(neg_clean)
neutral_bigrams <- generate_bigrams(neutral_clean)
sample_bigrams <- generate_bigrams(sample_clean)

plot_bigrams(pos_bigrams, title_suffix = "(Positive Reviews)")
plot_bigrams(neg_bigrams, title_suffix = "(Negative Reviews)")
plot_bigrams(sample_bigrams, title_suffix = "(Sample Reviews)")

# [3] Trigram
# Function to GENERATE & ANALYZE TRIGRAM
generate_trigrams <- function(corpus, top_n = 20) {
  texts <- sapply(corpus, content)
  df <- data.frame(text = texts, stringsAsFactors = FALSE)
  trigrams_df <- df %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3)
  trigram_counts <- trigrams_df %>%
    count(trigram, sort = TRUE)
  return(head(trigram_counts, top_n))
}

# Function to PLOT TRIGRAM
plot_trigrams <- function(trigram_df, title_suffix = "", top_n = 15) {
  plot_df <- head(trigram_df, top_n)
  ggplot(plot_df, aes(x = reorder(trigram, n), y = n)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    coord_flip() +
    labs(title = paste("Top", top_n, "Most Frequent Trigrams", title_suffix),
         x = "Trigram", y = "Frequency") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

pos_trigrams <- generate_trigrams(pos_clean)
neg_trigrams <- generate_trigrams(neg_clean)
sample_trigrams <- generate_trigrams(sample_clean)

plot_trigrams(pos_trigrams, title_suffix = "(Positive Reviews)")
plot_trigrams(neg_trigrams, title_suffix = "(Negative Reviews)")
plot_trigrams(sample_trigrams, title_suffix = "(Sample Reviews)")

# [4] Common word between positive and negavtive in pyramid chart
create_ggplot_pyramid <- function(corpus1, corpus2, corpus1_name = "Positive", 
                                  corpus2_name = "Negative", top_n = 30,
                                  min_word_length = 5, normalize = TRUE) {
  dtm1 <- DocumentTermMatrix(corpus1, control = list(wordLengths = c(min_word_length, Inf)))
  dtm2 <- DocumentTermMatrix(corpus2, control = list(wordLengths = c(min_word_length, Inf)))
  doc_count1 <- length(corpus1)
  doc_count2 <- length(corpus2)
  term_freq1 <- colSums(as.matrix(dtm1))
  term_freq2 <- colSums(as.matrix(dtm2))
  common_terms <- intersect(names(term_freq1), names(term_freq2))
  
  df <- data.frame(
    term = common_terms,
    freq1_raw = term_freq1[common_terms],
    freq2_raw = term_freq2[common_terms],
    stringsAsFactors = FALSE)
  
  # Normalize
  if (normalize) {
    df$freq1 <- df$freq1_raw / doc_count1 * 100
    df$freq2 <- df$freq2_raw / doc_count2 * 100
    y_axis_label <- "Occurrences per 100 documents"
    title_suffix <- " (Normalized)"} else {
    df$freq1 <- df$freq1_raw
    df$freq2 <- df$freq2_raw
    y_axis_label <- "Frequency"
    title_suffix <- ""}
  
  df$delta <- abs(df$freq1 - df$freq2)
  top_terms <- df %>% arrange(desc(delta)) %>% head(top_n)

  plot_data <- rbind(
    data.frame(term = top_terms$term, freq = -top_terms$freq2, group = corpus2_name),  # Negative (left)
    data.frame(term = top_terms$term, freq = top_terms$freq1, group = corpus1_name)    # Positive (right)
  )
  plot_data$term <- factor(plot_data$term, levels = rev(top_terms$term))
  subtitle <- paste0(corpus1_name, ": ", doc_count1, " docs, ", corpus2_name, ": ", doc_count2, " docs")
 
  ggplot(plot_data, aes(x = term, y = freq, fill = group)) +
    geom_bar(stat = "identity", width = 0.7) +
    coord_flip() +
    scale_y_continuous(labels = function(x) abs(x),breaks = pretty(c(-max(top_terms$freq2), max(top_terms$freq1)))) +
    labs(
      title = paste0("Words in Common", title_suffix),
      subtitle = subtitle,
      x = "",
      y = y_axis_label,
      fill = "") +
    theme_minimal() +
    scale_fill_manual(values = c(
      "Positive" = "forestgreen",
      "Negative" = "firebrick")) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      axis.text.y = element_text(size = 11, face = "bold"),  # Center-aligned term labels
      legend.position = "top") + geom_hline(yintercept = 0, linetype = "solid", color = "black")
}

gg_pyramid_norm <- create_ggplot_pyramid(pos_clean, neg_clean, corpus1_name = "Positive", corpus2_name = "Negative", 
                                         top_n = 25, normalize = TRUE)
print(gg_pyramid_norm)

# [5] Not in common words of pos & neg review
dtm_pos <- DocumentTermMatrix(pos_clean, control = list(wordLengths = c(3, Inf)))
dtm_neg <- DocumentTermMatrix(neg_clean, control = list(wordLengths = c(3, Inf)))
terms_pos <- Terms(dtm_pos)
terms_neg <- Terms(dtm_neg)
common_terms <- intersect(terms_pos, terms_neg)

unique_pos_terms <- setdiff(terms_pos, common_terms) # Find uniuque word
unique_neg_terms <- setdiff(terms_neg, common_terms)
pos_freq <- colSums(as.matrix(dtm_pos))[unique_pos_terms]
neg_freq <- colSums(as.matrix(dtm_neg))[unique_neg_terms]

top_unique_pos <- sort(pos_freq, decreasing = TRUE)[1:50] # Select top 50
top_unique_neg <- sort(neg_freq, decreasing = TRUE)[1:50]

unique_pos_terms_df <- data.frame(term = names(top_unique_pos), freq = as.numeric(top_unique_pos))
unique_neg_terms_df <- data.frame(term = names(top_unique_neg), freq = as.numeric(top_unique_neg))

plot_wordcloud(unique_pos_terms_df, title = "Unique Positive Words")
plot_wordcloud(unique_neg_terms_df, title = "Unique Negative Words")


# ======================= TOPIC MODELLING uisng LDA ===========================
dtm_sample <- DocumentTermMatrix(sample_clean, control = list(wordLengths = c(3, Inf)))

dtm_sample_clean <- removeSparseTerms(dtm_sample, 0.998) # remove sparse words (< 0.2%)

dim(dtm_sample_clean)

result_sample <- FindTopicsNumber(
  dtm_sample_clean,
  topics = seq(from = 3, to = 20, by = 1),
  metrics = c("CaoJuan2009", "Arun2010", "Griffiths2004", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 770),
  mc.cores = 2L,
  verbose = TRUE)
FindTopicsNumber_plot(result_sample)

lda_sample <- LDA(dtm_sample_clean, k = 7, method = "Gibbs", control = list(seed = 770, iter = 5000))
table(topics(lda_sample))

phi <- exp(lda_sample@beta)             
theta <- lda_sample@gamma               
vocab <- colnames(dtm_sample_clean)      
doc_length <- slam::row_sums(dtm_sample_clean)
term_frequency_sample <- slam::col_sums(dtm_sample_clean)

json_lda_pos <- createJSON(
  phi = phi,
  theta = theta,
  vocab = vocab,
  doc.length = doc_length,
  term.frequency = term_frequency_sample)
dir.create("vis_pos", showWarnings = FALSE)
serVis(json_lda_pos, out.dir = "vis_pos", open.browser = TRUE)

# Select top 10 words to analyze topic label
topic_terms <- terms(lda_sample, 10)
for (i in 1:ncol(topic_terms)) {
  cat(sprintf(" Topic %d:\n", i))
  cat(paste(topic_terms[, i], collapse = ", "), "\n\n")
}

topic_labels <- c(    # Assign topic labels
  "Food & Beverage Experience",             
  "Service Quality & Staff",       
  "Atmosphere",    
  "Price, Offers & Included Services",        
  "Location & Accessibility",       
  "Front Desk Operations",
  "Room Comfort"
)

topic_table <- data.frame(
  Topic = paste("Topic", 1:7),
  Top_Terms = sapply(1:7, function(i) paste(topic_terms[, i], collapse = ", ")),
  Label = topic_labels,
  stringsAsFactors = FALSE
)

print(topic_table)



# ======================= TOPIC MODELLING AND SENTIMENT ANALYSIS ===========================
# Assign cleaned reviews and create review IDs
sample_data$cleaned_review <- sapply(sample_clean, content)
sample_data <- sample_data %>%
  mutate(review_id = row_number())

# Assign topic from LDA model to each review
topic_assignments <- data.frame(
  review_id = 1:nrow(sample_data),
  topic = topics(lda_sample))
sample_data <- left_join(sample_data, topic_assignments, by = "review_id")

# Define topic labels 
sample_data <- sample_data %>%
  mutate(topic_label = topic_labels[topic])

# Assign SCORE BASE SENTIMENT
sample_data <- sample_data %>%
  mutate(sentiment = case_when(
    Review.score >= 4 ~ "positive",
    Review.score <= 2 ~ "negative",
    TRUE ~ "neutral"
  ))

# Calculate Average Rating by Topic
avg_rating <- sample_data %>%
  filter(!is.na(Review.score)) %>%
  group_by(topic_label) %>%
  summarise(avg_score = mean(Review.score), .groups = "drop")

plot_avg_rating <- function() {
  ggplot(avg_rating, aes(x = topic_label, y = avg_score)) +
    geom_bar(stat = "identity", fill = "#4472C4") +
    geom_text(aes(label = round(avg_score, 2)), 
              vjust = -0.5, 
              color = "black", 
              size = 4) +
    labs(title = "Average Rating by Topic",
         x = "Topic",
         y = "Average Rating") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 15, hjust = 1)
    ) +
    ylim(0, 5) 
}

# Sentiment Distribution by Topic
sentiment_dist <- sample_data %>%
  group_by(topic_label, sentiment) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(topic_label) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()

plot_sentiment_dist <- function() {
  ggplot(sentiment_dist, aes(x = topic_label, y = count, fill = sentiment)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(percent, 1), "%")),
              position = position_stack(vjust = 0.5),
              color = "white",
              size = 3.5) +
    scale_fill_manual(values = c(
      "positive" = "#006400",
      "neutral" = "#FFD700",
      "negative" = "#B22222"
    )) +
    labs(title = "Sentiment Distribution by Topic",
         x = "Topic",
         y = "Review Count") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 15, hjust = 1),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

# Rating Distribution by Topic
rating_dist <- sample_data %>%
  filter(!is.na(Review.score)) %>%
  count(topic_label, Review.score) %>%
  group_by(topic_label) %>%
  mutate(percentage = n / sum(n)) %>%
  ungroup()

plot_rating_dist <- function() {
  ggplot(rating_dist, aes(x = topic_label, y = percentage, fill = factor(Review.score))) +
    geom_bar(stat = "identity", position = "fill") +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(
      values = c(
        "1" = "#B22222",  # red dark
        "2" = "#F08080",  # red light
        "3" = "#FFD700",  # yellow
        "4" = "#66C266",  # green soft
        "5" = "#006400"   # green dark
      ),
      name = "Rating"
    ) +
    labs(title = "Rating % per Topic", x = "Topic", y = "Percentage") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 15, hjust = 1)
    )
}

# Tokenize and filter words
prepare_tokens <- function() {
  tokens <- sample_data %>%     # Extract tokens
    unnest_tokens(word, cleaned_review) %>%
    filter(str_detect(word, "^[a-z]+$")) %>%
    anti_join(stop_words, by = "word") %>%
    select(review_id, word)
  
  word_counts <- tokens %>%   # Count word frequencies
    count(word, sort = TRUE)
  tokens_filtered <- tokens %>% # Filter out rare words (less than 5 occurrences)
    filter(word %in% word_counts$word[word_counts$n >=5])
  tokens_filtered <- tokens_filtered %>%  # Add sentiment
    left_join(sample_data %>% select(review_id, sentiment), by = "review_id")
  return(tokens_filtered)
}

# TF-IDF analysis
calculate_tfidf <- function(tokens_filtered) {
  tfidf_tokens <- tokens_filtered %>%
    count(review_id, word, sort = TRUE) %>%
    bind_tf_idf(term = word, document = review_id, n = n)
  tfidf_tokens <- tfidf_tokens %>%   # Add sentiment
    left_join(sample_data %>% select(review_id, sentiment), by = "review_id")
  return(tfidf_tokens)
}

# Word clouds by sentiment
create_sentiment_wordcloud <- function(tfidf_tokens, sentiment_label, color_name) {
  top_words <- tfidf_tokens %>%
    filter(sentiment == sentiment_label) %>%
    slice_max(tf_idf, n = 100)

  wordcloud(words = top_words$word,
            freq = top_words$tf_idf,
            max.words = 100,
            scale = c(3, 0.8),
            colors = brewer.pal(8, color_name),
            random.order = FALSE)
}

# TF-IDF visualization with sentiment lexicon
plot_tfidf_with_lexicon <- function(tfidf_tokens) {
  bing <- get_sentiments("bing")   # Get Bing sentiment lexicon
  tfidf_sentiment <- tfidf_tokens %>%
    inner_join(bing, by = "word", suffix = c("_review", "_lexicon")) %>%
    rename(polarity = sentiment_lexicon)

  top_tfidf <- tfidf_sentiment %>%         # Plot top words by review sentiment and lexicon polarity
    group_by(sentiment_review, polarity) %>%
    slice_max(tf_idf, n = 10) %>%
    ungroup()

  ggplot(top_tfidf, aes(x = reorder_within(word, tf_idf, sentiment_review), 
                        y = tf_idf, fill = polarity)) + geom_col() + facet_wrap(~ sentiment_review, scales = "free")+
    coord_flip() + scale_x_reordered() + 
    labs(title = "Top TF-IDF Words by Sentiment and Polarity", x = NULL, y = "TF-IDF", fill = "Polarity") + theme_minimal()
}

# Top words by polarity
plot_top_polarity_words <- function(tfidf_tokens) {
  bing <- get_sentiments("bing")
  tfidf_sentiment <- tfidf_tokens %>%
    inner_join(bing, by = "word", suffix = c("_review", "_lexicon")) %>%
    rename(polarity = sentiment_lexicon)
  
  top25_tfidf <- tfidf_sentiment %>%   # Top 25 words by polarity
    filter(polarity %in% c("positive", "negative")) %>%
    group_by(polarity) %>%
    slice_max(tf_idf, n = 25) %>%
    ungroup()
  
  ggplot(top25_tfidf, aes(x = reorder_within(word, tf_idf, polarity), 
                          y = tf_idf, fill = polarity)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ polarity, scales = "free") +
    coord_flip() +
    scale_x_reordered() +
    scale_fill_manual(values = c(
      "positive" = "#006400",  # dark green
      "negative" = "#B22222"   # dark red
    )) +
    labs(title = "Top 15 TF-IDF Words by Polarity", x = "Word", y = "TF-IDF") +
    theme_minimal()
}
# Top words by review sentiment
plot_top_sentiment_words <- function(tfidf_tokens) {
  top_sentiment_words <- tfidf_tokens %>%
    group_by(sentiment) %>%
    slice_max(tf_idf, n = 20) %>%
    ungroup()
  
  ggplot(top_sentiment_words, aes(x = reorder_within(word, tf_idf, sentiment), 
                                  y = tf_idf, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ sentiment, scales = "free") +
    coord_flip() +
    scale_x_reordered() +
    scale_fill_manual(values = c(
      "positive" = "#006400",  # dark green
      "neutral"  = "#FFD700",  # yellow
      "negative" = "#B22222"   # dark red
    )) +
    labs(title = "Top TF-IDF Words by Sentiment", x = NULL, y = "TF-IDF") +
    theme_minimal()
}

# [1] Generate plots for topic and sentiment analysis
plot_avg_rating()
plot_sentiment_dist()
plot_rating_dist()

# [2] Text analysis workflow
tokens_filtered <- prepare_tokens()
tfidf_tokens <- calculate_tfidf(tokens_filtered)

# [3] Generate wordclouds
create_sentiment_wordcloud (tfidf_tokens, "positive", "Greens")
create_sentiment_wordcloud (tfidf_tokens, "neutral", "YlOrBr")
create_sentiment_wordcloud (tfidf_tokens, "negative", "Reds")

# [4] Generate TF-IDF plots
plot_tfidf_with_lexicon(tfidf_tokens)
plot_top_polarity_words(tfidf_tokens)
plot_top_sentiment_words(tfidf_tokens)

plot_topic_bigrams_by_sentiment <- function(data, topic_name, sentiments = c("negative", "neutral"), top_n = 15) {
  bigram_data <- data %>%
    filter(topic_label == topic_name, sentiment %in% sentiments) %>%
    unnest_tokens(bigram, cleaned_review, token = "ngrams", n = 2) %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !is.na(word1), !is.na(word2)) %>%
    unite(bigram, word1, word2, sep = " ") %>%
    count(sentiment, bigram, sort = TRUE) %>%
    group_by(sentiment) %>%
    slice_max(n, n = top_n) %>%
    ungroup()
  
  ggplot(bigram_data, aes(x = reorder_within(bigram, n, sentiment), y = n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    facet_wrap(~ sentiment, scales = "free_y") +
    scale_x_reordered() +
    scale_fill_manual(values = c(
      "negative" = "#B22222",
      "neutral" = "#FFD700",
      "positive" = "#228B22"
    )) +
    labs(title = paste("Top Bigrams in", shQuote(topic_name), "Reviews"),
         x = NULL, y = "Frequency") +
    theme_minimal()
}

plot_topic_bigrams_by_sentiment(sample_data, "Room Comfort", sentiments = c("negative", "neutral"))
plot_topic_bigrams_by_sentiment(sample_data, "Front Desk Operations", sentiments = c("negative", "neutral"))

# Word cloud for each topic
plot_topic_wordcloud <- function(data, topic_name, min_freq = 10, max_words = 50, color_palette = "Dark2") {
  topic_words <- data %>%
    filter(topic_label == topic_name) %>%
    unnest_tokens(word, cleaned_review) %>%
    anti_join(stop_words, by = "word") %>%
    count(word, sort = TRUE) %>%
    filter(n >= min_freq)
  par(mar = c(1, 1, 5, 1))
  wordcloud(words = topic_words$word,
            freq = topic_words$n,
            max.words = max_words,
            scale = c(4, 0.8),  # kích thước chữ
            colors = brewer.pal(8, color_palette),
            random.order = FALSE)
  title(paste("", topic_name), col.main = "black")
}
plot_topic_wordcloud(sample_data, "Room Comfort")
plot_topic_wordcloud(sample_data, "Front Desk Operations")
plot_topic_wordcloud(sample_data, "Service Quality & Staff")
plot_topic_wordcloud(sample_data, "Atmosphere")
plot_topic_wordcloud(sample_data, "Location & Accessibility")
plot_topic_wordcloud(sample_data, "Food & Beverage Experience")
plot_topic_wordcloud(sample_data, "Price, Offers & Included Services")

