---
title: "Addendum: Structural Topic Modeling"
output: html_notebook
---

This notebook starts with the preprocessed, cleaned and combined data.

```{r}
# Data frame and pipe utilities:
library(dplyr)    #has to be after plyr if used
library(tidyr)

# String, text and data tools:
library(tidyverse)
library(stringr)
library(tidytext)
library(scales)
library(purrr)

# Visualisation:
library(ggplot2)
library(ggthemes)

# Graph engine for correlation plotting
library(igraph)

# Topic modeling
library(topicmodels)
library(lsa)

# Text mining libraries
library(tm)
library(quanteda)
# library(SnowballC)
# Required as LDA config and/or dependencies
library(ldatuning)

# Structural Topic Models
library(stm)


```

```{r}

###############################################################################
# Load data
###############################################################################

# Required data for this workbook:
# tdf.doc.words.clean
export2018           <- readRDS("~/R/NDA/Thesis/export2018.RDS")
export2017           <- readRDS("~/R/NDA/Thesis/export2017.RDS")
export2016           <- readRDS("~/R/NDA/Thesis/export2016.RDS")
export2015           <- readRDS("~/R/NDA/Thesis/export2015.RDS")
# export2014           <- readRDS("~/R/NDA/Thesis/export2014.RDS")

# Concatenate 2015-2018
tdf.doc.words.clean  <- tibble(Student = NA, word = NA, n = NA, total = NA)
tdf.doc.words.clean  <- rbind(export2015, export2016, export2017, export2018)

# Import metadata
metadata.full        <- readRDS("~/R/NDA/Thesis/metadata.full2014-2018.RDS")

# Remove 2014 data
metadata.15.18       <- metadata.full[metadata.full$Year != 2014, ]
tdf.metadata         <- metadata.15.18[, c(1,2, 4:7)]
tdf.metadata         <- tdf.metadata[complete.cases(tdf.metadata), ]

# Bring in year counts in case needed
tdf.yearcounts       <- readRDS("~/R/NDA/Thesis/tdf.yearcounts2014-2018.RDS")
tdf.yearcounts.15.18 <- tdf.yearcounts[1:4, ]

```

```{r}

###############################################################################
# DOCUMENT TERM RANKING # currently not used
###############################################################################

# Add ranks for words based on (sorted) row number per Student
freq.by.rank <- 
  tdf.doc.words.clean %>%
  group_by(Student)   %>%
  mutate(rank = row_number(), `term frequency` = n/total)

```

```{r}
###############################################################################
# SIMPLE WORD COUNTS
###############################################################################
tdf.simple.word.counts <- tdf.doc.words.clean %>% count(word, sort = TRUE)

###############################################################################
# WORD COUNTS PER PROGRAM
###############################################################################
# left join the program metadata back
tdf.programs.clean <- merge(tdf.doc.words.clean, tdf.metadata, by = "Student")

# Add word counts per program
tdf.programs.word.counts <-  
  tdf.programs.clean %>% count(word, Program, sort = TRUE)


```

```{r}
###############################################################################
# TF-IDF PER PROGRAM
###############################################################################
tdf.programs.tfidf <- 
  tdf.programs.clean[ tdf.programs.clean$n > 5, 2:7] %>% 
  bind_tf_idf(word, Program, n) %>%
  group_by(Program)             %>%
  top_n(10)                     %>% # defaults to tf_idf as wt
  ungroup()                     %>%
  mutate(word = reorder(word, tf_idf))

# Plot per program
plot.tfidf.program <-
  tdf.programs.tfidf                          %>%
    ggplot(aes(word, tf_idf, fill = Program)) +
      geom_col(show.legend = FALSE)           +
      facet_wrap(~Program, scales = "free")   +
      coord_flip()


```

```{r}

# Term corpus frequency and document frequency distributions for a single year

dfm.documents.2018 <-  
  tdf.programs.clean[tdf.programs.clean$Year == 2018, 1:3]         %>%
  count(Student, word, sort = TRUE) %>%
  cast_dfm(Student, word, n)


# # Take 2018 data to plot term corpus frequency
# dfm.documents.topn.2018 <-  
#   tdf.programs.clean[tdf.programs.clean$Year == 2018, 1:3] #  %>%
#     # count(Student, word, sort = TRUE) %>%
#     # top_n(10000) # defaults to n

# Take top N words by frequency
dfm.documents.topn.2018 <-  
  tdf.programs.clean[tdf.programs.clean$Year == 2018, 1:3]       %>%
  group_by(word) %>%  
  summarise(., sum(n)) %>%
  top_n(10000)

colnames(dfm.documents.topn.2018) <- c("word", "n")

hist(dfm.documents.topn.2018$n, 
       main = "Corpus frequency distribution of terms in 2018", 
       xlab = "Term corpus frequency", 
       ylab = "Terms" ,
       freq = TRUE,
       breaks = 180,
       col = "lightblue" )

```


```{r}
###############################################################################
# Prepare for topic modeling
###############################################################################
# This section generates the term matrix object used in the topic modeling. 


###############################################################################
# Cast to wide Document Term Matrix (DFM), reduce sparse terms
###############################################################################

# Cast to Quanteda wide format per document
dfm.documents <-  
  tdf.programs.clean[, 1:3]         %>%
  count(Student, word, sort = TRUE) %>%
  cast_dfm(Student, word, n)

# Cast to Quanteda wide format per document
dfm.documents.k100 <-  
  tdf.programs.clean[, 1:3]         %>%
  count(Student, word, sort = TRUE) %>%
  cast_dfm(Student, word, n)


# Take top N words by frequency
tdf.documents.topn <-  
  tdf.programs.clean[, 1:3]         %>%
  # count(Student, word, sort = TRUE) %>%
  top_n(10000)

hist(tdf.documents.topn$n, 
       main = "Frequency distribution of top 10000 words", 
       xlab = "Frequency of occurrence", 
       ylab = "Terms" ,
       freq = TRUE,
       breaks = 250,
       col = "lightblue" )


###############################################################################
# Remove sparse terms
###############################################################################
# We don't need to worry too much about this, as stm automatically caps at 10k terms

# Utility for plotting term frequency thresholds

# plotRemoved(dfm.documents$documents, lower.thresh = seq(from=1, to=200, by = 20))

# Bring down number of terms/features to <10k

# This leaves 3039 documents x 8044 features
dfm.documents <- dfm_trim(dfm.documents, min_termfreq = 120, verbose = TRUE)

# This leaves 3039 documents x 11342 features 
dfm.documents.k100 <- dfm_trim(dfm.documents.k100, min_termfreq = 70, verbose = TRUE)


# dfm.documents2 <- dfm_trim(dfm.documents.raw, min_termfreq = 10, verbose = TRUE)
```

## Find optimal K values: searchK

```{r}
###############################################################################
# Find optimal K values
###############################################################################

#Find values for K
# We noticee later that most topics are defined by <5000 words. Let's reduce
# the matrix more so that our searchK doesn't take forever to run and does 
# not run out of memory. Searching for an optimal value requires many values
# of K to be assayed, which takes significant time and resources. A matrix 
# with <5000 terms and with 5 K-values consumed between 4-9GB of RAM while 
# running, and does NOT terminate with an error when swap / pagination begins,
# like most R operations do. It doesn't seem to terminate when they become 
# full either. This might lead to an unresponsive system for an indefinite
# period of time.

# Trim corpus 
dfm.documents.2 <- dfm_trim(dfm.documents, min_termfreq = 250, verbose = TRUE)

results.searchK.2 <-
  searchK(dfm.documents.2, 
          init.type = "Spectral", 
          K = c(150, 160),       # Values to test seq(100, 200, 10)
          N = floor(0.1 * 3039), # Partial holdout set
          proportion = 0.5,      # Holdout proportion
          heldout.seed = 2019,   # Deterministic with seed
          M = 10,                # M value for exclusivity computation 
          cores = 3)             # Number of cores to use

# Plot some of the results of searchK 
plot(results.searchK.2$results)


```

## SearchK on the full 2015-2018 corpus
```{r}
# RUN ON ALL YEARS
# THIS CAN REQUIRE UP TO 20GB OF RAM TO EXECUTE 

results.searchK.full <-
  searchK(dfm.documents,         # 8044 terms, 3039 documents
          init.type = "Spectral", 
          K = seq(20, 160, 10),  # Values to test seq(100, 200, 10)
          N = floor(0.1 * 3039), # Partial holdout set
          proportion = 0.5,      # Holdout proportion
          heldout.seed = 2019,   # Deterministic with seed
          M = 10,                # M value for exclusivity computation 
          cores = 4)             # Number of cores to use

saveRDS(results.searchK.full, file = "results.searchK.full.RDS")
```

```{r}
# Plotting results

results.searchK.full$results$K 
# We notice that the model ran out of memory and produced errors for some values.

# The data is stored as factors, use as.num(as.char()) to access, only take values
# that didn't throw an error.

# Create plot matrix 
par(mfrow = c(2, 2))
  plot(
    as.numeric(as.character(results.searchK.full$results$K[
      c(1,2,4,5,7,8,10,11)])), 
    as.numeric(as.character(results.searchK.full$results$heldout[
      c(1,2,4,5,7,8,10,11)])),
       main = "Held-Out Likelihood 2015-2018",
       xlab = "Number of Topics(K)",
       ylab = "Held-Out Likelihood",
       type = "b")

  plot(
    as.numeric(as.character(results.searchK.full$results$K[
      c(1,2,4,5,7,8,10,11)])), 
    as.numeric(as.character(results.searchK.full$results$residual[
      c(1,2,4,5,7,8,10,11)])),
       main = "Residuals 2015-2018",
       xlab = "Number of Topics(K)",
       ylab = "Residuals",
       type = "b")

  plot(
    as.numeric(as.character(results.searchK.full$results$K[
      c(1,2,4,5,7,8,10,11)])), 
    as.numeric(as.character(results.searchK.full$results$semcoh[
      c(1,2,4,5,7,8,10,11)])),
       main = "Semantic coherence 2015-2018",
       xlab = "Number of Topics (K)",
       ylab = "Semantic Coherence",
       type = "b")

  plot(
    as.numeric(as.character(results.searchK.full$results$K[
      c(1,2,4,5,7,8,10,11)])), 
    as.numeric(as.character(results.searchK.full$results$lbound[
      c(1,2,4,5,7,8,10,11)])),
       main = "Lower Bound 2015-2018",
       xlab = "Number of Topics(K)",
       ylab = "Lower Bound",
       type = "b")
  

```
```{r}
# Exclusivity plot for the full corpus
  plot(
    as.numeric(as.character(results.searchK.full$results$K[
      c(1,2,4,5,7,8,10,11)])), 
    as.numeric(as.character(results.searchK.full$results$exclus[
      c(1,2,4,5,7,8,10,11)])),
       main = "Exclusivity 2015-2018",
       xlab = "Number of Topics(K)",
       ylab = "Exclusivity",
       type = "b")

```




## Structural topic modeling
```{r}

###############################################################################
# Implement Structural Topic Modeling
###############################################################################
# Run models with K topics. Vocab param is not used with dfm objects.
# stm.documents     <-  stm(dfm.documents,  K = 150, init.type = "Spectral" )

stm.documents.k100 <- stm(dfm.documents.k100,  K = 100, init.type = "Spectral" ) # 11 342 w x 3039 docs

saveRDS(stm.documents.k100, "stm.model.k100.RDS")

# summary(stm.documents.k100)

```

### Correlation networks
```{r}

###############################################################################
# Correlation network plots for 2015-2018
###############################################################################

# Extract topic correlations, plot as network graphs for each corr level 0-0.5
for (i in seq(0, 0.5, 0.05)) {
  topiccorrs <- topicCorr(stm.documents, 
                          method = "simple", 
                          cutoff = i)
  
  plot.topicCorr(topiccorrs, 
                 topics       = c(1:100),
                 vertex.color = NULL, 
                 vertex.size  = 2 , 
                 layout       = layout_with_kk )
}


```

```{r}
# Extract topic correlations, plot as network graphs for each corr level 0-0.5
for (i in seq(0, 0.5, 0.05)) {
  topiccorrs <- topicCorr(stm.documents.2018.k60.trim, 
                          method = "simple", 
                          cutoff = i)
  
  plot.topicCorr(topiccorrs, 
                 topics = c(1:60),
                 vertex.color = NULL, 
                 vertex.size  = 2 , 
                 layout       = layout_with_kk )
}

```

```{r}
###############################################################################
# Top topics by sum gamma
###############################################################################
# Code from https://www.r-bloggers.com/training-evaluating-and-interpreting-topic-models/
# By Julia Silge (2018)

# library(ggthemes)
# library(tidyverse)

# Extract beta matrix
td_beta <- tidy(stm.documents.k100, matrix = "beta")

# Extract gamma matrix
td_gamma <- tidy(stm.documents.k100,
                 matrix = "gamma",
                 document_names = rownames(dfm.documents.k100))

```

```{r}

top_terms <- 
  td_beta                %>%
    arrange(beta)        %>%
    group_by(topic)      %>%
    top_n(7, beta)       %>%
    arrange(-beta)       %>%
    select(topic, term)  %>%
    summarise(terms = list(term)) %>%
    mutate(terms = map(terms, paste, collapse = ", ")) %>% 
    unnest()

gamma_terms <- td_gamma                  %>%
  group_by(topic)                        %>%
  summarise(gamma = mean(gamma))         %>%
  arrange(desc(gamma))                   %>%
  left_join(top_terms, by = "topic")     %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic,   gamma))

gamma_terms                 %>%
  top_n(20, gamma)          %>%
  ggplot(aes(topic, gamma, 
             label = terms, 
             fill  = topic)) +
  
  geom_col(show.legend = FALSE) +
  
  geom_text(hjust   = 0, 
            nudge_y = 0.0005, 
            size    = 3,
            family  = "IBMPlexSans") +
  
  coord_flip() +
  
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = percent_format()) +
  
  theme_tufte(base_family   = "IBMPlexSans", ticks = FALSE) +
  
  theme(plot.title    = element_text(size    = 16,
                                     family  = "IBMPlexSans-Bold"),
        plot.subtitle = element_text(size    = 13)) +
  
  labs(x = NULL, 
       y = expression(gamma),
       title = "Bottom 20 topics by prevalence in the 2018 corpus"
       # ,         subtitle = "With the top words that contribute to each topic"
       )


```

```{r}
###############################################################################
# Top 20 topics
###############################################################################

gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, 
             label = terms, 
             fill  = -gamma)) +
  
  geom_col(show.legend = FALSE) +
  
  geom_text(hjust   = 0, 
            nudge_y = 0.0005, 
            size    = 3,
            family  = "IBMPlexSans") +
  
  coord_flip() +
  
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = percent_format()) +
  
  theme_tufte(base_family   = "IBMPlexSans", ticks = FALSE) +
  
  theme(plot.title = element_text(size    = 16,
                                  family  = "IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  
  labs(x = NULL, 
       y = expression(gamma),
       title = "Top 20 topics by prevalence in the 2015-2018 corpus"
       # ,         subtitle = "With the top words that contribute to each topic"
       )

```

```{r}
###############################################################################
# Bottom 20 topics
###############################################################################

gamma_terms %>%
  top_n(-20, gamma) %>%
  ggplot(aes(topic, gamma, 
             label = terms, 
             fill  = -gamma)) +
  
  geom_col(show.legend = FALSE) +
  
  geom_text(hjust   = 0, 
            nudge_y = 0.0005, 
            size    = 3,
            family  = "IBMPlexSans") +
  
  coord_flip() +
  
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = percent_format()) +
  
  theme_tufte(base_family   = "IBMPlexSans", ticks = FALSE) +
  
  theme(plot.title = element_text(size    = 16,
                                  family  = "IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  
  labs(x = NULL, 
       y = expression(gamma),
       title = "Bottom 20 topics by prevalence in the 2015-2018 corpus"
       # ,         subtitle = "With the top words that contribute to each topic"
       )

```

```{r}
###############################################################################
# Gamma distribution, all topics
###############################################################################

gamma_terms %>%
  top_n(60, gamma) %>%
  ggplot(aes(topic, gamma, 
             label = terms, 
             fill  = -gamma)) +
  
  geom_col(show.legend = FALSE) +
  
  # geom_text(hjust   = 0, 
  #           nudge_y = 0.0005, 
  #           size    = 3,
  #           family  = "IBMPlexSans") +
  # 
  # coord_flip() +
  
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = percent_format()) +
  
  theme_tufte(base_family   = "IBMPlexSans", ticks = FALSE) +
  
  theme(plot.title = element_text(size    = 16,
                                  family  = "IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13),
        axis.text.x   = element_text(angle = 90)) +
  
  
  labs(x = NULL, 
       y = expression(gamma),
       title = "Topic gamma disribution in the 2015-2018 corpus"
       # ,         subtitle = "With the top words that contribute to each topic"
       )

```


# Beta distributions
 The beta values in a topic model describe the the per-term-per-topic association.

```{r}
# 2018 DATASET
###############################################################################
# Extract beta matrix and initialise result df
###############################################################################

# Extract beta matrix from the stm object (beta = default)
tm.beta.documents <- tidy(stm.documents.k100, matrix = "beta")

# Set parameters for analysis (plotting resolution)
# Lowest and highest number of terms included
param.topns <- c( 
  seq(from = 0, to = 10000, by = 100)
  # 10, 50, 100, 200, 500, 1000, 2500
  # 2000, 3000, 4000
  # 5000, 6000, 7500, 10000
  ) 

# df.beta.results <- data.frame(terms = param.topns,
#                            result = 1:length(param.topns))

# Prepare df for results
df.beta.results <- data.frame(terms   = rep(NA, times = 10000),
                              result1 = rep(NA, times = 10000),
                              result2 = rep(NA, times = 10000),
                              result3 = rep(NA, times = 10000),
                              result4 = rep(NA, times = 10000),
                              result5 = rep(NA, times = 10000),
                              result6 = rep(NA, times = 10000))

###############################################################################
# Loop to determine how beta is distributed in the top n terms of the topics
###############################################################################

for (i in param.topns) {

  # Take the top i terms from the beta matrix, sort by beta
  tm.beta.documents.top <-
    tm.beta.documents  %>%
      group_by(topic)  %>% 
      top_n(i, beta)   %>%
      ungroup          %>%
      mutate(term = reorder(term, beta))

  # These are the top n terms for each topic by beta.

  #Identify the most tightly-defined topics (highest sum(beta))
  tm.beta.summary <-
    tm.beta.documents.top[, 1:3]       %>%
      group_by(topic)                  %>%
      summarize(totalbeta = sum(beta)) %>%
      arrange(., desc(totalbeta))
  
  # Store the number of topics above a certain beta score
  df.beta.results$terms[i]  <- i

  df.beta.results$result1[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta == 1   ] )
  df.beta.results$result2[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta >  0.99] )
  df.beta.results$result3[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta >  0.90] )
  df.beta.results$result4[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta >  0.75] )
  df.beta.results$result5[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta >  0.5 ] )
  df.beta.results$result6[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta >  0.25] )

  # summary(tm.beta.summary)
  
  # # Print histogram of beta distribution
  # hist(tm.beta.summary$totalbeta,
  #        main = paste("Beta distribution in top "," terms (k = 60)"),
  #        xlab = "Sum(beta) per topic",
  #        ylab = "No of topics" ,
  #        freq = TRUE,
  #        breaks = 150,
  #        col = "lightblue" )
  
} #end for loop


# Remove NAs, add col names based on beta thresholds
df.beta.results2 <- df.beta.results[complete.cases(df.beta.results) == TRUE, ]
colnames(df.beta.results2) <- c("terms", "1.0", "0.99", "0.90", "0.75", "0.5", "0.25")  



# Create beta threshold plot 

plot(df.beta.results2$terms, df.beta.results2$`1.0`, 
     type = "b",
     main = "2015-2018 Topics above beta threshold (K = 100)",
     xlab = "Terms",
     ylab = "No of topics",
     ylim = c(0, 100),  # No of topics
     xlim = c(0, 2500), # 0-10000 or 0-2500
     cex = 0.8,
     col = "darkblue")
lines(df.beta.results2$terms, df.beta.results2$`0.99`, type = "b", cex = 0.5, col = "red")
lines(df.beta.results2$terms, df.beta.results2$`0.90`, type = "b", cex = 0.5, col = "orange")
lines(df.beta.results2$terms, df.beta.results2$`0.75`, type = "b", cex = 0.5, col = "darkgray")
lines(df.beta.results2$terms, df.beta.results2$`0.5`,  type = "b", cex = 0.5, col = "darkgreen")
lines(df.beta.results2$terms, df.beta.results2$`0.25`, type = "b", cex = 0.5, col = "gray")

legend("topleft",  #"bottomright" for large, "topleft" or "left" for smaller plot
       legend = c("beta = 1", "beta > 0.99", "beta > 0.90", "beta > 0.75", "beta > 0.5", "beta > 0.25"),
       col    = c("darkblue", "red",         "orange",      "darkgray",    "green",      "gray"), 
       lty    = 1:6, 
       cex    = 0.8 )



```

```{r}
###############################################################################
# Plot beta distributions
###############################################################################

# Beta accumulation diagrams for top n words:
i <- 10

 # Take the top i terms from the beta matrix, sort by beta
  tm.beta.documents.top <-
    tm.beta.documents  %>%
      group_by(topic)  %>% 
      top_n(i, beta)   %>%
      ungroup          %>%
      mutate(term = reorder(term, beta))
  
    tm.beta.summary <-
    tm.beta.documents.top[, 1:3]       %>%
      group_by(topic)                  %>%
      summarize(totalbeta = sum(beta)) %>%
      arrange(., desc(totalbeta))
  
  # # Print histogram of beta distribution
  hist(tm.beta.summary$totalbeta,
         main = paste("Beta distribution in top", i, "terms (k = 100)"),
         xlab = "Sum(beta) per topic",
         ylab = "No of topics" ,
         freq = TRUE,
         breaks = 100,
         col = "lightblue" )
  
```

```{r}
# Older version
###############################################################################
# Extract beta matrix and initialise result df
###############################################################################

# Extract beta matrix from the stm object (beta = default)
tm.beta.documents <- tidy(stm.documents, matrix = "beta")

# Set parameters for analysis (plotting resolution)
# Lowest and highest number of terms included
param.topns <- c( 
  seq(from = 0, to = 8500, by = 100)
  # 10, 50, 100, 200, 500, 1000, 2500, 
  # 2000, 3000, 4000
  # 5000, 6000, 7500, 10000
  ) 

# df.beta.results <- data.frame(terms = param.topns,
#                            result = 1:length(param.topns))

# Prepare df for results
df.beta.results <- data.frame(terms   = rep(NA, times = 10000),
                              result1 = rep(NA, times = 10000),
                              result2 = rep(NA, times = 10000),
                              result3 = rep(NA, times = 10000),
                              result4 = rep(NA, times = 10000),
                              result5 = rep(NA, times = 10000))

###############################################################################
# Loop to determine how beta is distributed in the top n terms of the topics
###############################################################################

for (i in param.topns) {

  # Take the top i terms from the beta matrix, sort by beta
  tm.beta.documents.top <-
    tm.beta.documents  %>%
      group_by(topic)  %>% 
      top_n(i, beta)   %>%
      ungroup          %>%
      mutate(term = reorder(term, beta))

  # These are the top n terms for each topic by beta.

  #Identify the most tightly-defined topics (highest sum(beta))
  tm.beta.summary <-
    tm.beta.documents.top[, 1:3]       %>%
      group_by(topic)                  %>%
      summarize(totalbeta = sum(beta)) %>%
      arrange(., desc(totalbeta))
  
  # Store the number of topics above a certain beta score
  df.beta.results$terms[i]  <- i
  
  df.beta.results$result1[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta == 1   ] )
  df.beta.results$result2[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta >  0.99] )
  df.beta.results$result3[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta >  0.75] )
  df.beta.results$result4[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta >  0.5 ] )
  df.beta.results$result5[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta >  0.25] )

  # summary(tm.beta.summary)
  
  # Print histogram of beta distribution
  # hist(tm.beta.summary$totalbeta,
  #        main = paste("Beta distribution in top", i , "terms (k = 150)"), 
  #        xlab = "Sum(beta) per topic", 
  #        ylab = "No of topics" ,
  #        freq = TRUE,
  #        breaks = 150,
  #        col = "lightblue" )
  
} #end for loop


# Remove NAs, add col names based on beta thresholds
df.beta.results2 <- df.beta.results[complete.cases(df.beta.results) == TRUE, ]
colnames(df.beta.results2) <- c("terms", "1.0", "0.99", "0.75", "0.5", "0.25")  



# Create beta threshold plot 

plot(df.beta.results2$terms, df.beta.results2$`1.0`, 
     type = "b",
     main = "Topics above beta threshold",
     xlab = "Terms",
     ylab = "No of topics",
     ylim = c(0, 150),
     xlim = c(0, 8500),
     col = "darkblue")
lines(df.beta.results2$terms, df.beta.results2$`0.99`, type = "b", col = "red")
lines(df.beta.results2$terms, df.beta.results2$`0.75`, type = "b", col = "darkgray")
lines(df.beta.results2$terms, df.beta.results2$`0.5`,  type = "b", col = "darkgreen")
lines(df.beta.results2$terms, df.beta.results2$`0.25`, type = "b", col = "gray")

legend("bottomright", 
       legend = c("beta = 1", "beta > 0.99", "beta > 0.75", "beta > 0.5", "beta > 0.25"),
       col    = c("darkblue", "red",         "darkgray",    "green",      "gray"), 
       lty    = 1:5, 
       cex    = 0.8 )



```


# Gamma distributions
```{r}
###############################################################################
# Extract and summarise gamma matrix
###############################################################################
# Note that this is identical to the "theta" matrix in stm.

# Without defining the document_names, stm replaces them with index numbers
tm.gamma.documents <- tidy(stm.documents.k100, 
                           matrix = "gamma",
                           document_names = rownames(dfm.documents))

# Now we have a tibble with (docs x topics) rows, with gamma values for each

# Add gamma sums for each topic
tm.gamma.summary <-
  tm.gamma.documents[, 2:3]       %>%
    group_by(topic)               %>%
    summarize(total = sum(gamma)) %>%
    arrange(., desc(total))


# TEST: Aggregate by  docs, should add up to 1 for each
# tm.gamma.summary <-
#   tm.gamma.documents[, 1:3]       %>%
#     group_by(document)            %>%
#     summarize(total = sum(gamma)) %>%
#     arrange(., desc(total))


# And now we have the topics sorted by the highest gamma values.
summary(tm.gamma.summary)

```


```{r}
###############################################################################
# Plot gamma distributions
###############################################################################

hist(tm.gamma.summary$total,
     main   = "Gamma sum per topic, k = 100",
     xlab   = "sum(Gamma) per topic",
     ylab   = "Topics",
     breaks =  50,
     col    = "lightyellow")

hist(tm.gamma.documents$gamma,
     main   = "Gamma distribution in doc-topic pairs (0.02 < gamma < 1), k = 100",
     xlab   = "Gamma per interaction",
     ylab   = "Topic-document pairs",
     xlim   = c(0.02, 1),
     ylim   = c(0, 1000),
     breaks = 150, 
     col    = "lightyellow")

hist(tm.gamma.documents$gamma,
     main   = "Gamma distribution in doc-topic pairs (0 < gamma < 1), k = 100",
     xlab   = "Gamma per interaction",
     ylab   = "Topic-document pairs",
     xlim   = c(0, 1),
     # ylim   = c(0, 200),
     breaks = 200, 
     col    = "lightyellow")

options(scipen = 10)
hist(tm.gamma.documents$gamma,
     main   = "Gamma distribution in doc-topic pairs (0 < gamma < 0.001), k = 100",
     xlab   = "Gamma per interaction",
     ylab   = "Topic-document pairs",
     xlim   = c(0, 0.001),
     # ylim   = c(0, 300),
     breaks = 300000, 
     col    = "lightyellow")

hist(tm.gamma.documents$gamma,
     main   = "Gamma distribution in doc-topic pairs (0.9 < gamma < 1), k = 100",
     xlab   = "Gamma per interaction",
     ylab   = "Topic-document pairs",
     xlim   = c(0.9, 1),
     ylim   = c(0, 50),
     breaks = 1000, 
     col    = "lightyellow")
```


```{r}
###############################################################################
# Gamma sums for thresholds
###############################################################################

# Parameters for the loop
seq.gammas      <- seq(from = 1, to = 0, by = -0.001)
seq.gamma.index <- seq(from = 1, to = length(seq.gammas), by = 1)

# Initialise a tdf for the results
tdf.gamma.sums <- tibble(gamma  = rep(0, 1), # seq(from = 0, to = 1, by = 0.05 ),
                              cumsum = rep(0, 1),
                              topics = rep(0, 1),
                              interactions = rep(0, 1))


# The following terrible loop loops the gamma values and sums the gamma below a
# certain threshold int othe cumsum variable. It also counts the number of 
# topics 
for (i in seq.gamma.index) {
  tdf.gamma.sums <-
    rbind(tdf.gamma.sums, 
          tibble(gamma  =  seq.gammas[i],
                 cumsum =  as.numeric(
                            sum(tm.gamma.documents$gamma[
                                tm.gamma.documents$gamma <= as.numeric(seq.gammas[i]) ] )),
                 topics =  as.numeric(
                            count(distinct(
                              tibble::enframe((tm.gamma.documents$topic[
                                tm.gamma.documents$gamma >= as.numeric(seq.gammas[i]) ] ),
                                name = NULL)
                                           )
                                  ) ),
                 interactions = 
                            length(tm.gamma.documents$gamma[
                                   tm.gamma.documents$gamma >= as.numeric(seq.gammas[i]) ] 
                                   )
                )
          )
}
```

```{r}

options(scipen = 10) # suppress scientific notation

###############################################################################
# Cumulative gamma + topic curves, whole range
###############################################################################
plot(tdf.gamma.sums[ 2:length(tdf.gamma.sums$gamma), 1:2], 
     type = "b",
     main = "Cumulative gamma distribution by 0.001 increments, k = 100",
     xlab = "Gamma threshold",
     ylab = "Total gamma above threshold",
     # ylim = c(0, 800),
     cex  = 0.3,
     # ylim = c(0,150),
     col = "darkblue")

  # topic count
  lines(tdf.gamma.sums$gamma[ 2:length(tdf.gamma.sums$gamma)], 
        tdf.gamma.sums$topics[2:length(tdf.gamma.sums$topics)], 
        type = "l", 
        lty  =  1,
        col  = "darkred")
  
  # interaction count
  lines(tdf.gamma.sums$gamma[ 2:length(tdf.gamma.sums$gamma)], 
        tdf.gamma.sums$interactions[2:length(tdf.gamma.sums$interactions)],
        type = "b",
        col  = "darkgreen",
        cex  = 0.3)
  
  legend("right",
         legend = c("Gamma above threshold", "No of topics", "No of interactions" ),
         lty    = c(1,1,1), 
         col    = c("darkblue", "darkred", "darkgreen"))

###############################################################################
# Topic curves, whole range
###############################################################################
plot(tdf.gamma.sums$gamma[ 2:length(tdf.gamma.sums$gamma)], 
     tdf.gamma.sums$topics[2:length(tdf.gamma.sums$topics)], 
     main = "Topics with gamma above threshold in 0.001 increments, k = 100",
     xlab = "Gamma threshold",
     ylab = "Topics with gamma above threshold",
     type = "b", 
     lty  =  1,
     cex  =  0.3,
     col  = "darkred")


###############################################################################
# Interaction curves,  range
###############################################################################
plot(tdf.gamma.sums$gamma[ 2:length(tdf.gamma.sums$gamma)], 
     tdf.gamma.sums$interactions[2:length(tdf.gamma.sums$interactions)], 
     main = "Interactions with gamma above gamma threshold > 0.1 , k = 100",
     xlab = "Gamma threshold",
     ylab = "Interactions with gamma above threshold",
     type = "b", 
     lty  =  1,
     xlim = c(0.1, 1),
     ylim = c(0, 10000),
     cex  =  0.3,
     col  = "darkgreen")

###############################################################################
# Interaction curves, whole range, LOG SCALE
###############################################################################
plot(tdf.gamma.sums$gamma[ 2:length(tdf.gamma.sums$gamma)], 
     tdf.gamma.sums$interactions[2:length(tdf.gamma.sums$interactions)], 
     log  = "y",
     main = "Interactions with gamma above gamma threshold, log scale, k = 100",
     xlab = "Gamma threshold",
     ylab = "Interactions with gamma above threshold",
     type = "b", 
     lty  =  1,
     xlim = c(0, 1),
     # ylim = c(0, 1500),
     cex  =  0.3,
     col  = "darkgreen")  

###############################################################################
# Cumulative gamma + topic curves, top 0.005
###############################################################################

plot(tdf.gamma.sums[ 2:length(tdf.gamma.sums$gamma), 1:2], 
     type = "b",
     main = "Cumulative gamma distribution by 0.001 increments, k = 100",
     xlab = "Gamma threshold",
     ylab =  "Total gamma above threshold",
     xlim = c(0.98, 1),
     # ylim = c(0,150),
     col = "darkblue")
  
  lines(tdf.gamma.sums$gamma[ 2:length(tdf.gamma.sums$gamma)], 
        tdf.gamma.sums$topics[2:length(tdf.gamma.sums$topics)], 
        type = "b", 
        col  = "darkred")
  
  lines(tdf.gamma.sums$gamma[ 2:length(tdf.gamma.sums$gamma)], 
        tdf.gamma.sums$interactions[2:length(tdf.gamma.sums$interactions)],
        type = "b",
        col  = "darkgreen")
  
  legend("right",
         legend = c("Gamma above threshold", "No of topics", "No of interactions" ),
         lty    = c(1,1,1), 
         col    = c("darkblue", "darkred", "darkgreen"))

  
###############################################################################
# Cumulative gamma + topic curves, top 0.005
###############################################################################

plot(tdf.gamma.sums$gamma[ 2:length(tdf.gamma.sums$gamma)], 
     tdf.gamma.sums$topics[2:length(tdf.gamma.sums$topics)],
         type = "b",
         main = "Cumulative gamma distribution by 0.001 increments, k = 100",
         xlab = "Gamma threshold",
         xlim = c(0.98, 1),
         ylim = c(0, 250),
         ylab = "Count",
         col  = "darkred")
  
  
  lines(tdf.gamma.sums$gamma[ 2:length(tdf.gamma.sums$gamma)], 
        tdf.gamma.sums$interactions[2:length(tdf.gamma.sums$interactions)],
        type = "b",
        col  = "darkgreen")
  
  legend("topright",
         legend = c( "No of topics", "No of interactions" ),
         lty    = c(1,1), 
         col    = c("darkred", "darkgreen"))

###############################################################################  
  
```


```{r}
###############################################################################
# Return metadata for year-on-year comparison
###############################################################################
# Prepare the gamma matrix and metadata for joining 
tdf.metadata.arrange <- arrange(tdf.metadata, desc(Year))

# Only take one document (newest) per Student
tdf.metadata.unique <-  tdf.metadata.arrange[!duplicated(tdf.metadata.arrange[, 2]), ]

# tdf.metadata.unique <-  tdf.metadata[!duplicated(tdf.metadata[, 2]), ]
# We notice that this removed 198 entries from the metadata table.
# These are entries for students that submitted a thesis in several years or 
# programs. This introduces other bugs into the analysis. 
# Print omitted student numbers: tdf.metadata$Student[duplicated(tdf.metadata$Student)]


tm.gamma.documents.meta <- tm.gamma.documents
tm.gamma.documents.meta <- as.data.frame(lapply(tm.gamma.documents.meta, as.numeric))
tm.gamma.documents.meta <- as_tibble(tm.gamma.documents.meta)
colnames(tm.gamma.documents.meta) <- c("Student", "topic", "gamma")


length(table(tm.gamma.documents$document))
length(table(tm.gamma.documents$topic))

# Do inner join on the data frames
tm.gamma.meta <- inner_join(tm.gamma.documents.meta, tdf.metadata.unique, by = "Student") 


```

```{r}

# TEST: check gamma sums and compare to document counts 
###############################################################################
# More metadata for year-on-year comparison
###############################################################################
length(table(tm.gamma.documents$document))

int.total.docs <- as.numeric(
  length(table(tm.gamma.meta$Student[tm.gamma.meta$Year == "2018" ])) +
  length(table(tm.gamma.meta$Student[tm.gamma.meta$Year == "2017" ])) +
  length(table(tm.gamma.meta$Student[tm.gamma.meta$Year == "2016" ])) +
  length(table(tm.gamma.meta$Student[tm.gamma.meta$Year == "2015" ])) 
  # + length(table(tm.gamma.meta$Student[tm.gamma.meta$Year == "2014" ])) 
  )
int.total.docs

# int.docs.2018 <-  length(table(tm.gamma.meta$Student[tm.gamma.meta$Year == "2018" ])) 
# int.docs.2017 <-  length(table(tm.gamma.meta$Student[tm.gamma.meta$Year == "2017" ])) 
# int.docs.2016 <-  length(table(tm.gamma.meta$Student[tm.gamma.meta$Year == "2016" ])) 
# int.docs.2015 <-  length(table(tm.gamma.meta$Student[tm.gamma.meta$Year == "2015" ])) 
# # int.docs.2014 <-  length(table(tm.gamma.meta$Student[tm.gamma.meta$Year == "2014" ])) 

# Use sum(gamma):
int.docs.2018 <- sum(tm.gamma.meta$gamma[tm.gamma.meta$Year == "2018" ])
int.docs.2017 <- sum(tm.gamma.meta$gamma[tm.gamma.meta$Year == "2017" ])
int.docs.2016 <- sum(tm.gamma.meta$gamma[tm.gamma.meta$Year == "2016" ])
int.docs.2015 <- sum(tm.gamma.meta$gamma[tm.gamma.meta$Year == "2015" ])

# Sum of exported documents
sum(tdf.yearcounts.15.18$docs) # difference 148

# TEST RESULTS:
#-----------------------------------------
# Documents in tdf.metadata:          3342
# Documents in tdf.metadata.unique:   3144
# Documents in tdf.yearcounts.15.18   3187 no. imported
# Documents in tdf.gamma.documents:   3039
# STM model sum(gamma) ("documents"): 3039         
  
# length(complete.cases(tm.gamma.documents.meta)) / 150  # all complete
```

```{r}
# Add new column with gamma sums
tm.gamma.sums <- tibble(Year     = seq(2018, 2015, by = -1),
                        gammasum = c(int.docs.2018, int.docs.2017,
                                     int.docs.2016, int.docs.2015))

# Add count of years included
int.count.years <- as.numeric(length(table(tm.gamma.sums$Year)))

# Join gamma sums / year as a new column
tm.gamma.meta.sums <- inner_join(tm.gamma.meta, tm.gamma.sums, by = "Year")

# Add relative measure to balance out year-on-year difference 
# Add a new column with gamma / (doc count) * avg(doc count per year)
tm.gamma.meta.sums$relgamma <-( (tm.gamma.meta.sums$gamma / 
                                 tm.gamma.meta.sums$gammasum) 
                                * (int.total.docs/int.count.years))


# Aggregate sum gamma / topic for each year
# Take both, absolute and relative change
# tm.gamma.rel.total <-
#     tm.gamma.meta.sums       %>%
#     group_by(topic)          %>%
#     summarize(relyeargamma = sum(relgamma))
# 
# tm.gamma.abs.total <-
#     tm.gamma.meta.sums       %>%
#     group_by(topic)          %>%
#     summarize(absyeargamma = sum(gamma))

# compute relative topic gamma sums / year
tm.gamma.rel.total <-
    tm.gamma.meta.sums             %>%
      group_by(Year)               %>%
      group_by(topic, add = TRUE)  %>%
      summarize_at(.vars = c("relgamma") ,
                   .funs = funs(relsum = "sum")) # Note deprecation of funs()

# Compute absolute topic gamma sums / year
tm.gamma.abs.total <-
    tm.gamma.meta.sums             %>%
      group_by(Year)               %>%
      group_by(topic, add = TRUE)  %>%
      summarize_at(.vars = c("gamma") ,
                   .funs = funs(abssum = "sum"))


# Spread the gammas per topic per year into a wide format
tm.gamma.deltas <- spread(tm.gamma.rel.total, Year, relsum)

# Add changes between years
tm.gamma.deltas <-
  tm.gamma.deltas %>%
    mutate(delta1815 =  tm.gamma.deltas$`2018` - tm.gamma.deltas$`2015`) %>%
    mutate(delta1817 =  tm.gamma.deltas$`2018` - tm.gamma.deltas$`2017`) %>%
    mutate(delta1716 =  tm.gamma.deltas$`2017` - tm.gamma.deltas$`2016`) %>%
    mutate(delta1615 =  tm.gamma.deltas$`2016` - tm.gamma.deltas$`2015`) %>%
    mutate(reldelta  = (tm.gamma.deltas$`2018` - tm.gamma.deltas$`2015`)
                        / tm.gamma.deltas$`2015`)                       
  # %>%
  # mutate(avgdelta  = (abs(tm.gamma.deltas$delta1817) + 
  #                     abs(tm.gamma.deltas$delta1716) +
  #                     abs(tm.gamma.deltas$delta1615) 
  #                     / 3 ))
```

```{r}

# Get top 10 by relative change
tm.gamma.deltas.top.rel <-
  tm.gamma.deltas         %>%
  top_n(10, reldelta)     %>%
  arrange(desc(reldelta))

# Get top 10 by absolute change
tm.gamma.deltas.top.abs <-
  tm.gamma.deltas         %>%
  top_n(10, delta1815)    %>%
  arrange(desc(delta1815))

# Get bottom 10 by relative change
tm.gamma.deltas.bottom.rel <-
  tm.gamma.deltas        %>%
  top_n(-10, reldelta)   %>%
  arrange(desc(reldelta))

# Get bottom 10 by relative change
tm.gamma.deltas.bottom.abs <-
  tm.gamma.deltas        %>%
  top_n(-10, delta1815)  %>%
  arrange(desc(delta1815))

```

```{r}

# Gamma deltas against years  
# plot(tm.gamma.deltas[, 2:5],
#      col  = "darkblue",
#      main = "Comparison of topic gamma over years",
#      cex  = 0.5) 

hist(tm.gamma.deltas$reldelta * 100, 
     main   = "% Change in relative gamma",
     breaks = 200,
     xlab   = "% Change",
     col    = "lightgray")

hist(tm.gamma.deltas$delta1815,
     main   = "Change in absolute gamma 2015-2018",
     breaks = 50,
     ylab   = "Topcis",
     xlab   = "Gamma change",
     col    = "lightgray")

hist(tm.gamma.deltas$delta1817,
     main   = "Change in absolute gamma 2017-2018",
     breaks = 50,
     xlab   = "Gamma change",
     col    = "lightgray")

hist(tm.gamma.deltas$delta1716,
     main   = "Change in absolute gamma 2016-2017",
     breaks = 50,
     xlab   = "Gamma change",     
     col    = "lightgray")

hist(tm.gamma.deltas$delta1615,     
     main   = "Change in absolute gamma 2015-2016",
     breaks = 50,
     xlab   = "Gamma change",
     col    = "lightgray")



tm.gamma.deltas
tm.gamma.rel.total
```

```{r}
###############################################################################
# Topic Trajectories
###############################################################################

# Plot all topic trajectories
ggplot(     data      = tm.gamma.rel.total,
            aes(x     = Year,
                y     = relsum, 
                group = topic ,
                col   = "topics" )) +
    # Add gray lines for all topics
     geom_line(alpha  = I(4/5),
               size   = 0.5,
               col    = "darkgray") +
  
    # Add coloured line for largest relative or absolute positive change
  
     geom_line(data  = tm.gamma.rel.total[tm.gamma.rel.total$topic %in% 
                                          tm.gamma.deltas.top.abs$topic, ], 
               alpha = I(4/5),
               size  = 0.5,
               col   = "green") +
  
     geom_line(data  = tm.gamma.rel.total[tm.gamma.rel.total$topic %in% 
                                          tm.gamma.deltas.top.abs$topic, ],
               alpha = I(4/5),
               size  = 0.5,
               col   = "black") +
  
  
    # Add coloured line for largest relative or absolute negative change
     geom_line(data  = tm.gamma.rel.total[tm.gamma.rel.total$topic %in%
                                          tm.gamma.deltas.bottom.rel$topic, ],
               alpha = I(4/5),
               size  = 0.5,
               col   = "orange")   +
  
     geom_line(data  = tm.gamma.rel.total[tm.gamma.rel.total$topic %in% 
                                          tm.gamma.deltas.bottom.abs$topic, ],
             alpha = I(4/5),
             size  = 0.5,
             col   = "darkred") +

   ggtitle("Change in gamma per topic") +
   labs(x = "Year", y = "Topic gamma") +
   theme_bw()



```

```{r}
###############################################################################
# FUNCTION: f.topnwords Return Top N words for c(topics)
###############################################################################

# Generic template for top n words for topics

f.topnwords <- function(n, df.include.topics){

  tm.beta.topN.words <-
    tm.beta.documents.top   %>%
      group_by(topic)       %>%
      top_n(n, beta)
  
  return.words <- 
    tm.beta.topN.words[tm.beta.topN.words$topic %in% 
                       df.include.topics, ]
    
  return(return.words)
}

# Get top 100 words for all topics
tm.beta.top100 <- f.topnwords(100, seq(1, length(tm.gamma.summary$topic), 1))
```  

```{r}
# There is also a utility in stm that gets the summary-like word lists for topics

labelTopics(stm.documents.k100, c(3, 7, 20))
```

```{r}
###############################################################################
# Top 20 words for top topics
###############################################################################
# Extract the top 20 words for each top 10 topic

# Get top 20 words by beta for all topics
tm.beta.top20.words <-
  tm.beta.documents.top   %>%
    group_by(topic)       %>%
    top_n(20, beta)

# Get top 20 for top 10 relative pos change
tm.gamma.deltas.top.rel.words <- 
  tm.beta.top20.words[tm.beta.top20.words$topic %in% 
                      tm.gamma.deltas.top.rel$topic, ]
# Matrix plot of it
plot(tm.gamma.deltas.top.rel.words)

# Get top 20 words for top 10 absolute pos change
tm.gamma.deltas.top.abs <-
  tm.gamma.deltas %>%
  top_n(10, delta1814)

tm.gamma.deltas.top.abs.words <- 
  tm.beta.top20.words[tm.beta.top20.words$topic %in% 
                      tm.gamma.deltas.top.abs$topic, ]

plot(tm.gamma.deltas.top.abs.words)

# We notice a pronounced difference here: the betas/topic
# and some betas per term are significantly higher in the 
# relative growers than the absolute growers


# Get top20 words for bottom 10 relative change
tm.gamma.deltas.bottom.rel.words <- 
  tm.beta.top20.words[tm.beta.top20.words$topic %in% 
                      tm.gamma.deltas.bottom.rel$topic, ]

plot(tm.gamma.deltas.bottom.rel.words)

#We immediately notice these are very diffuse groups


# Get top 20 words for bottom 10 absoute change
tm.gamma.deltas.bottom.abs.words <- 
  tm.beta.top20.words[tm.beta.top20.words$topic %in% 
                      tm.gamma.deltas.bottom.abs$topic, ]

plot(tm.gamma.deltas.bottom.abs.words)
# We see a similar pattern here


```


```{r}

###############################################################################
# Plot beta charts for the top results
###############################################################################
# NOTE: Looped manually

# tm.beta.top.documents <- tm.beta.documents[tm.beta.documents$topic %in%
#                                            tm.gamma.deltas.top.abs$topic, ] 

 tm.beta.top.documents <- tm.beta.documents[tm.beta.documents$topic %in%
                                             tm.gamma.deltas.top.rel$topic, ]
# 
# tm.beta.top.documents <- tm.beta.documents[tm.beta.documents$topic %in%
#                                            tm.gamma.deltas.bottom.abs$topic, ] 
# 
# 
# tm.beta.top.documents <- tm.beta.documents[tm.beta.documents$topic %in%
#                                            tm.gamma.deltas.bottom.rel$topic, ] 

# Params & Init
# -------------
# Set parameters for analysis (plotting resolution)
# Lowest and highest number of terms included

param.topns <- c( 
  seq(from = 0, to = 8500, by = 100)
  # 10, 50, 100, 200, 500, 1000, 2500, 
  # 2000, 3000, 4000
  # 5000, 6000, 7500, 10000
  ) 

# df.beta.results <- data.frame(terms = param.topns,
#                              result = rep(NA, times = length(param.topns))

# Prepare df for results
df.beta.results <- data.frame(terms   = rep(NA, times = 10000),
                              result1 = rep(NA, times = 10000),
                              result2 = rep(NA, times = 10000),
                              result3 = rep(NA, times = 10000),
                              result4 = rep(NA, times = 10000),
                              result5 = rep(NA, times = 10000))

###############################################################################
# Loop to determine how beta is distributed in the top n terms of the topics
###############################################################################

for (i in param.topns) {

  # Take the top i terms from the beta matrix, sort by beta
  tm.beta.documents.top <-
    tm.beta.top.documents  %>%
      group_by(topic)  %>% 
      top_n(i, beta)   %>%
      ungroup          %>%
      mutate(term = reorder(term, beta))

  # These are the top n terms for each topic by beta.

  #Identify the most tightly-defined topics (highest sum(beta))
  tm.beta.summary <-
    tm.beta.documents.top[, 1:3]       %>%
      group_by(topic)                  %>%
      summarize(totalbeta = sum(beta)) %>%
      arrange(., desc(totalbeta))
  
  # Store the number of topics above a certain beta score
  df.beta.results$terms[i]  <- i
  
  df.beta.results$result1[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta == 1   ] )
  df.beta.results$result2[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta >  0.99] )
  df.beta.results$result3[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta >  0.75] )
  df.beta.results$result4[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta >  0.5 ] )
  df.beta.results$result5[i] <- length(tm.beta.summary$totalbeta[tm.beta.summary$totalbeta >  0.25] )

  # summary(tm.beta.summary)
  
  # Print histogram of beta distribution
  # hist(tm.beta.summary$totalbeta,
  #        main = paste("Beta distribution in top", i , "terms (k = 150)"), 
  #        xlab = "Sum(beta) per topic", 
  #        ylab = "No of topics" ,
  #        freq = TRUE,
  #        breaks = 150,
  #        col = "lightblue" )
  
} #end for loop


# Remove NAs, add col names based on beta thresholds
df.beta.results2 <- df.beta.results[complete.cases(df.beta.results) == TRUE, ]
colnames(df.beta.results2) <- c("terms", "1.0", "0.99", "0.75", "0.5", "0.25")  


# Plot for beta thresholds
plot(df.beta.results2$terms, df.beta.results2$`1.0`, 
     type = "b",
     main = "Topics above beta threshold",
     xlab = "Terms",
     ylab = "No of topics",
     ylim = c(0, 10),
     xlim = c(0, 8500),
     col = "darkblue")
lines(df.beta.results2$terms, df.beta.results2$`0.99`, type = "b", col = "red")
lines(df.beta.results2$terms, df.beta.results2$`0.75`, type = "b", col = "darkgray")
lines(df.beta.results2$terms, df.beta.results2$`0.5`,  type = "b", col = "darkgreen")
lines(df.beta.results2$terms, df.beta.results2$`0.25`, type = "b", col = "gray")

legend("bottomright", 
       legend = c("beta = 1", "beta > 0.99", "beta > 0.75", "beta > 0.5", "beta > 0.25"),
       col    = c("darkblue", "red",         "darkgray",    "green",      "gray"), 
       lty    = 1:5, 
       cex    = 0.8 )



```





```{r}

###############################################################################
# Gamma histograms
###############################################################################

# Plot histogram of sum(gamma) / topic
hist(tm.gamma.summary$total,
       main = "Gamma distribution (k = 150)", 
       xlab = "Sum(gamma) per topic", 
       ylab = "No of topics" ,
       freq = TRUE,
       breaks = 150,
       col = "lightyellow" )


# No of documents included in the model
length(table(tm.gamma.documents$document))

# We see that this distribution is extremely scewed.
# summary(tm.gamma.documents)

# Histogram of gamma per term distribution:
hist(tm.gamma.documents$gamma[tm.gamma.documents$gamma > 0.001 & tm.gamma.documents$gamma < 0.9], #<0.0001],
       main = "Gamma distribution by doc (k = 150)  gamma > 0.001, < 0.9 ", 
       xlab = "Gamma per topic", 
       ylab = "No of documents" ,
       freq = TRUE,
       breaks = 150,
       col = "lightyellow" )

# This histogram tells us that relatively few topics are super prevalent,
# most are prevalent in a rather small amount of documents.
# Sum(Gamma) = Count(docuements)
# sum(topics) = K
```

```{r}
###############################################################################
# Gamma sums for thresholds
###############################################################################

# Parameters for the loop
seq.gammas      <- seq(from = 1, to = 0, by = -0.001)
seq.gamma.index <- seq(from = 1, to = length(seq.gammas), by = 1)

# Initialise a tdf for the results
tdf.gamma.sums <- tibble(gamma  = rep(0, 1), # seq(from = 0, to = 1, by = 0.05 ),
                         cumsum = rep(0, 1),
                         topics = rep(0, 1))


# The following terrible loop loops the gamma values and sums the gamma below a
# certain threshold int othe cumsum variable. It also counts the number of 
# topics 
for (i in seq.gamma.index) {
  tdf.gamma.sums <-
    rbind(tdf.gamma.sums, 
          tibble(gamma  =  seq.gammas[i],
                 cumsum =  as.numeric(
                            sum(tm.gamma.documents$gamma[
                              tm.gamma.documents$gamma <= as.numeric(seq.gammas[i]) ] )),
                 topics =  as.numeric(
                            count(distinct(
                              tibble::enframe((tm.gamma.documents$topic[
                                tm.gamma.documents$gamma < as.numeric(seq.gammas[i]) ] ),
                                name = NULL)
                                           )
                                    ) )
                )
          )
}

#Identify topics with the highest gammas, obviously.

# test.tibble <- distinct(tibble::enframe((tm.gamma.documents$topic[tm.gamma.documents$gamma >= as.numeric(seq.gammas[5]) ])))
# 
# sum(tm.gamma.documents$gamma[tm.gamma.documents$gamma < 0.1])
# sum(tm.gamma.documents$gamma[tm.gamma.documents$gamma > 0.1])
# sum(tm.gamma.documents$gamma[tm.gamma.documents$gamma > 0.9])
# sum(tm.gamma.documents$gamma[tm.gamma.documents$gamma > 0.99])
# sum(tm.gamma.documents$gamma[tm.gamma.documents$gamma > 0.999])
```

```{r}
###############################################################################
# Gamma plots
###############################################################################
options(scipen = 10) # suppress scientific notation

###############################################################################
# Gamma distribution histograms
###############################################################################
# Full range
hist(tm.gamma.documents$gamma[tm.gamma.documents$gamma > 0 &
                              tm.gamma.documents$gamma <= 1],    #<0.0001],
       main = "Gamma distribution by doc  (k = 150) gamma > 0, <= 1", 
       xlab = "Gamma per topic", 
       ylab = "No of documents" ,
       freq = TRUE,
       breaks = 200,
       col = "lightblue" )

# Non-extreme range
hist(tm.gamma.documents$gamma[tm.gamma.documents$gamma > 0.001 &
                              tm.gamma.documents$gamma < 0.98],    #<0.0001],
       main = "Gamma distribution by doc (k = 150) gamma > 0.001, < 0.98", 
       xlab = "Gamma per topic", 
       ylab = "No of documents" ,
       freq = TRUE,
       breaks = 200,
       col = "lightblue" )

# Non-tiny range
hist(tm.gamma.documents$gamma[tm.gamma.documents$gamma > 0.01 ],    #<0.0001],
       main = "Gamma distribution by doc (k = 150) gamma > 0.01",
       xlab = "Gamma per topic", 
       ylab = "No of documents" ,
       freq = TRUE,
       breaks = 200,
       col = "lightblue" )



###############################################################################
# Cumulative gamma + topic curves, whole range
###############################################################################
plot(tdf.gamma.sums[ 2:length(tdf.gamma.sums$gamma), 1:2], 
     type = "b",
     main = "Cumulative gamma distribution by 0.001 increments",
     xlab = "Gamma threshold",
     ylab = "Total gamma above threshold",
     cex  = 0.3,
     # ylim = c(0,150),
     col = "darkblue")

lines(tdf.gamma.sums$gamma[2:length(tdf.gamma.sums$gamma)], 
      tdf.gamma.sums$topics[2:length(tdf.gamma.sums$topics)], 
      type = "l", 
      lty  =  1,
      col  = "darkred")

legend("topleft",
       legend = c("Gamma above threshold", "No of topics"),
       lty    = c(1,1), 
       col    = c("darkblue", "darkred"))

###############################################################################
# Topic curves, whole range
###############################################################################
plot(tdf.gamma.sums$gamma[ 2:length(tdf.gamma.sums$gamma)], 
     tdf.gamma.sums$topics[2:length(tdf.gamma.sums$topics)], 
     main = "Topics with gamma above threshold in 0.001 increments",
     xlab = "Gamma threshold",
     ylab = "Topics with terms above threshold",
     type = "b", 
     lty  =  1,
     cex  =  0.3,
     col  = "darkred")


###############################################################################
# Cumulative gamma + topic curves, top 0.005
###############################################################################

plot(tdf.gamma.sums[ 2:length(tdf.gamma.sums$gamma), 1:2], 
     type = "b",
     main = "Cumulative gamma distribution by 0.001 increments",
     xlab = "Gamma threshold",
     ylab =  NA, #"Total gamma above threshold",
     xlim = c(0.9, 1),
     # ylim = c(0,150),
     col = "darkblue")

lines(tdf.gamma.sums$gamma[2:length(tdf.gamma.sums$gamma)], 
      tdf.gamma.sums$topics[2:length(tdf.gamma.sums$topics)], 
      type = "b", 
      col = "darkred")

legend("topleft",
       legend = c("Gamma above threshold", "No of topics"),
       lty    = c(1,1), 
       col    = c("darkblue", "darkred"))

###############################################################################
```

### Correlation networks
```{r}

###############################################################################
# Correlation network plots for top/bottom topics 2015-2018
###############################################################################


# List all top and bottom topics
all.top.bottom.topics <-
  c(tm.gamma.deltas.bottom.rel$topic,
  tm.gamma.deltas.bottom.abs$topic,
  tm.gamma.deltas.top.rel$topic ,
  tm.gamma.deltas.top.abs$topic)

# Remove duplicates
all.top.bottom.topics <- all.top.bottom.topics[!duplicated(all.top.bottom.topics)]  

# Extract topic correlations, plot as network graphs for each corr level 0-0.5
for (i in seq(0, 0.3, 0.05)) {
  topiccorrs <- topicCorr(stm.documents.k100, 
                          method = "simple", 
                          cutoff = i)
  
  plot.topicCorr(topiccorrs, 
                 topics = c(1:100),
                 vertex.color = "darkblue", 
                 vertex.size  = 2 , 
                 vlabels      = NA,
                 layout       = layout_with_kk,
                 main   = paste("2015-2018 Topic correlation network at cutoff",
                                i, "(k = 100)")  )
}


for (i in seq(0, 0.5, 0.05)) {
  topiccorrs <- topicCorr(stm.documents.k100, 
                          method = "simple", 
                          cutoff = i)
  
  plot.topicCorr(topiccorrs, 
                 topics = c(all.top.bottom.topics),
                 vertex.color = NULL, 
                 vertex.size  = 2 , 
                 layout       = layout_with_kk ,
                 main   = paste("2015-2018 Topic correlation network at cutoff",
                                i, "(k = 100)") )

}


```

```{r}
###############################################################################
# After all this we could easily do another join / merge to recover metadata.
# This way we could extract the gamma significance of any programs, and 
# associate programs with topics.
###############################################################################
```