install.packages("pacman")

# List the necessary packages
pacman::p_load("tidyverse", "bigrquery", "stringr", "tidytext", "widyr", "irlba", "broom")

bq_auth(use_oob = True)

# Set the project ID
project <- "yourprojectname"

# DEFINE SQL QUERY TO RETRIEVE DATA
sql <- "SELECT
  stories.title AS title,
  stories.text AS text
FROM
  `bigquery-public-data.hacker_news.full` AS stories
WHERE
  stories.deleted IS NULL
LIMIT
  500000"

# EXECUTE THE QUERY AND STORE THE RESULT
hacker_news_raw <- bq_project_query(query = sql, x = project)

# DOWNLOAD THE QUERY RESULTS INTO A DATAFRAME
hacker_news_raw_df <- bq_table_download(hacker_news_raw)

hacker_news_raw_df %>% filter(!is.na(text)) %>% tail()

# CLEAN AND PREPROCESS THE TEXT DATA
hacker_news_text <- hacker_news_raw_df %>%
  # CONVERT TO TIBBLE FOR EASIER MANIPULATION
  as_tibble() %>%
  mutate(
    # REPLACE EMPTY TITLES WITH NA
    title = na_if(title, ""),
    # USE TITLE IF AVAILABLE, OTHERWISE USE TEXT
    text = coalesce(title, text)
  ) %>%
  # REMOVE THE TITLE COLUMN AS IT'S NOW MERGED WITH TEXT
  select(-title) %>%
  mutate(
    # REPLACE HTML ENTITIES WITH PROPER CHARACTERS
    text = str_replace_all(text, "&quot;|&#x2F;|&#x27;", "'"),
    # REPLACE MORE HTML ENTITIES
    text = str_replace_all(text, "&#x2F;", "/"),
    # REMOVE HTML LINK TAGS
    text = str_replace_all(text, "<a(.*?)>", " "),
    # REMOVE GREATER THAN AND LESS THAN HTML ENTITIES
    text = str_replace_all(text, "&gt;|&lt;", " "),
    # REMOVE ANY REMAINING HTML TAGS
    text = str_replace_all(text, "<[^>]*>", " "),
    # ADD A UNIQUE IDENTIFIER FOR EACH POST
    postID = row_number()
  )

# CALCULATE UNIGRAM PROBABILITIES
unigram_probs <- hacker_news_text %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

# CREATE TIDY SKIPGRAMS
tidy_skipgrams <- hacker_news_text %>%
    unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
    mutate(ngramID = row_number()) %>%
    unite(skipgramID, postID, ngramID) %>%
    unnest_tokens(word, ngram)

# CALCULATE SKIPGRAM PROBABILITIES
skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

# CALCULATE NORMALIZED PROBABILITIES
normalized_prob <- skipgram_probs %>%
  # KEEP ONLY WORD PAIRS THAT OCCUR MORE THAN 20 TIMES
  filter(n > 20) %>%
  # RENAME COLUMNS FOR CLARITY
  rename(word1 = item1, word2 = item2) %>%
  # JOIN WITH UNIGRAM PROBABILITIES FOR THE FIRST WORD
  left_join(
    unigram_probs %>%
      select(word1 = word, p1 = p),
    by = "word1"
  ) %>%
  # JOIN WITH UNIGRAM PROBABILITIES FOR THE SECOND WORD
  left_join(
    unigram_probs %>%
      select(word2 = word, p2 = p),
    by = "word2"
  ) %>%
  # CALCULATE THE NORMALIZED PROBABILITY
  mutate(p_together = p / p1 / p2)

# FIND WORDS SIMILAR TO "FACEBOOK"
normalized_prob %>%
  filter(word1 == "facebook") %>%
  arrange(-p_together)

# FIND WORDS SIMILAR TO "SCALA"
normalized_prob %>%
  filter(word1 == "scala") %>%
  arrange(-p_together)

# CREATE A PMI MATRIX
pmi_matrix <- normalized_prob %>%
  # CALCULATE PMI (POINTWISE MUTUAL INFORMATION)
  mutate(pmi = log10(p_together)) %>%
  # CREATE A SPARSE MATRIX WITH WORDS AS ROWS/COLUMNS AND PMI AS VALUES
  cast_sparse(word1, word2, pmi)

# REDUCE THE SPARSE MATRIX USING SINGULAR VALUE DECOMPOSITION
# PERFORM TRUNCATED SVD, KEEPING 256 DIMENSIONS
pmi_svd <- irlba(pmi_matrix, 256, maxit = 1e3)

# CREATE THE WORD VECTORS
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

# DEFINE A FUNCTION TO SEARCH FOR SYNONYMS
search_synonyms <- function(word_vectors, selected_vector) {
  # COMPUTE COSINE SIMILARITIES
  similarities <- word_vectors %*% selected_vector %>%
    # CONVERT TO TIBBLE
    as_tibble(rownames = "row_names") %>%
    rename(
      token = row_names,
      similarity = V1
    )
  # SORT BY DESCENDING SIMILARITY
  similarities %>%
    arrange(-similarity)
}

# FIND SYNONYMS FOR "FACEBOOK"
facebook <- search_synonyms(word_vectors, word_vectors["facebook", ])
facebook

# # FIND A WORD THAT IS RELATED TO AMAZON IN A SIMILAR WAT THAT IPHONE IS RELATED TO MICROSOFT
mystery_product <- word_vectors["iphone", ] - word_vectors["apple", ] + word_vectors["microsoft", ]
search_synonyms(word_vectors, mystery_product)

# # FIND A WORD THAT IS RELATED TO GOOGLE IN A SIMILAR WAT THAT IPHONE IS RELATED TO APPLE
mystery_product <- word_vectors["iphone", ] - word_vectors["apple", ] + word_vectors["google", ]
search_synonyms(word_vectors, mystery_product)

# FIND A WORD THAT IS RELATED TO AMAZON IN A SIMILAR WAT THAT IPHONE IS RELATED TO APPLE
mystery_product <- word_vectors["iphone", ] - word_vectors["apple", ] + word_vectors["amazon", ]
search_synonyms(word_vectors, mystery_product)
