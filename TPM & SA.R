library(rvest)
library(dplyr)
library(tidytext)
library(stringr)
library(hunspell)
library(textstem)
library(textclean)
library(tm)
library(ggplot2)
library(wordcloud)
library(SnowballC)
library(topicmodels)

links <- c(
  "https://www.sportspro.com/news/saudi-arabia-surj-sports-investment-euroleague-cycling-danny-townsend-december-2024/",
  "https://www.sportspro.com/news/saudi-arabia-pif-sports-investment-company/",
  "https://frontofficesports.com/pif-saudi-arabia-investment-2024/",
  "https://www.bloomberg.com/news/articles/2024-03-08/saudi-arabia-preparing-new-wave-of-private-sports-investment",
  "https://www.nytimes.com/athletic/5237849/2024/02/02/saudi-arabia-sport-investments/",
  "https://theconversation.com/more-than-money-the-geopolitics-behind-saudi-arabias-sports-strategy-240512",
  "https://www.playthegame.org/news/the-expansion-of-saudi-investments-in-sport-from-football-to-esport/",
  "https://www.bbc.com/sport/67713269",
  "https://teachmideast.org/saudi-investment-in-sports/"
)
webscrap_article <- function(url) {
  tryCatch({
  page <- read_html(url)
    title <- page %>% html_node("h1") %>% html_text(trim = TRUE)
    content <- page %>% html_nodes("p") %>% html_text(trim = TRUE)
    full_content <- paste(content, collapse = " ")
    data.frame(url = url, title = title, content = full_content, stringsAsFactors = FALSE)
    
  }, error = function(e) {
    message(paste("Error scraping:", url, "\n", e$message))
    return(data.frame(url = url, title = NA, content = NA, stringsAsFactors = FALSE))
  })
}


results <- bind_rows(lapply(links, webscrap_article))
print("Scrapped Content")
print(results)

results$content_cleaned<- results$content %>%
  str_to_lower() %>%  
  str_replace_all("[^a-z\\s]", "") %>%  
  str_squish()
print("Step 2: Cleaned content")
print(results$content_cleaned)

text1 <- tibble(id = seq_along(results$content_cleaned), text = results$content_cleaned)
tokens <- text1 %>%
  unnest_tokens(word, text)

print("Tokens")
print(tokens)


no_stop<-tokens %>%
  anti_join(stop_words,by="word")
print("Removing Words")
print(no_stop)
View(no_stop)

tokens$lemma <- textstem::lemmatize_words(tokens$word)
print("Lemmatization")
print(tokens$lemma)
View(tokens)

tokens$handlecontraction <- textclean::replace_contraction(tokens$word)
print(tokens$handlecontraction)
View(tokens)


tokens$checkspell <- hunspell_check(tokens$word)
print(tokens$checkspell)
View(tokens)

write.csv(tokens, "C:/Users/Sudipta/Documents/TPM and SA.csv", row.names = FALSE)

tokens_no_stop <- no_stop %>%
  mutate(lemma = textstem::lemmatize_words(word)) %>%
  group_by(id)

dtm <- tokens_no_stop %>%
  count(id, lemma) %>%
  cast_dtm(document = id, term = lemma, value = n)
print(dtm)

tfidf <- weightTfIdf(dtm)
tfidf

tfidf_matrix <- as.matrix(tfidf)
mean_tfidf <- rowMeans(tfidf_matrix)
tfidf_data <- data.frame(term = names(mean_tfidf), tfidf = mean_tfidf)
tfidf_data <- tfidf_data[order(tfidf_data$tfidf, decreasing = TRUE), ]

ggplot(tfidf_data, aes(x = reorder(term, tfidf), y = tfidf)) +
  geom_bar(stat = "identity", fill = "#2E86C1") +
  labs(title = "TF-IDF Scores for Sample Documents",
       x = "Term", y = "TF-IDF Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))


Model_lda <- LDA(dtm, k = 4, control = list(seed = 1234))
Model_lda

beta_topics <- tidy(Model_lda, matrix = "beta")
beta_topics

terms(Model_lda, 10)

beta_top_terms <- beta_topics %>%
  group_by (topic) %>%
  slice_max (beta, n = 10) %>%
  ungroup () %>%
  arrange (topic, -beta)

beta_top_terms %>%
  mutate (term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col (show.legend = FALSE) +
  facet_wrap (~ topic, scales = "free") +
  scale_y_reordered()

word_freq <- colSums(as.matrix(dtm))  
word_freq <- sort(word_freq, decreasing = TRUE) 
word_freq_df <- data.frame(
  word = names(word_freq),
  frequency = word_freq
)
head(word_freq_df)
top_words <- head(word_freq_df, 20)

ggplot(top_words, aes(x = reorder(word, -frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequent Words",
    x = "Words",
    y = "Frequency"
  ) +
  theme_minimal()

set.seed(123)
wordcloud(
  words = names(word_freq),         
  freq = word_freq,                
  min.freq = 2,                    
  max.words = 100,                 
  random.order = FALSE,            
  rot.per = 0.35,                   
  colors = brewer.pal(8, "Dark2")  
)