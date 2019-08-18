---
title: "Addendum: 2018 analyses"
output: html_notebook
---


```{r}
# Plot the model used
plot.STM(stm.documents.2018.k60.trim)

```

## Partial searchK runs for 2018 data


```{r}
dfm.documents.2018.trim <- dfm_trim(dfm.documents.2018, min_termfreq = 70, verbose = TRUE)
results.searchK.2018 <-
  searchK(dfm.documents.2018.trim, 
          init.type = "Spectral", 
          K = seq(80, 160, 20),       # Values to test seq(100, 200, 10)
          N = floor(0.1 * 3039), # Partial holdout set
          proportion = 0.5,      # Holdout proportion
          heldout.seed = 2019,   # Deterministic with seed
          M = 10,                # M value for exclusivity computation 
          cores = 4)             # Number of cores to use
```
```{r}
plot(results.searchK.2018$results)
```


```{r}
dfm.documents.2018.trim <- dfm_trim(dfm.documents.2018, min_termfreq = 70, verbose = TRUE)

results.searchK.2018.2 <-
  searchK(dfm.documents.2018.trim, 
          init.type = "Spectral", 
          K = seq(40, 80, 20),       # Values to test seq(100, 200, 10)
          N = floor(0.1 * 3039), # Partial holdout set
          proportion = 0.5,      # Holdout proportion
          heldout.seed = 2019,   # Deterministic with seed
          M = 10,                # M value for exclusivity computation 
          cores = 4)             # Number of cores to use
```


```{r}

dfm.documents.2018.trim <- dfm_trim(dfm.documents.2018, min_termfreq = 70, verbose = TRUE)

results.searchK.2018.4 <-
  searchK(dfm.documents.2018.trim, 
          init.type = "Spectral", 
          K = seq(10, 60, 10),       # Values to test seq(100, 200, 10)
          N = floor(0.1 * 3039), # Partial holdout set
          proportion = 0.5,      # Holdout proportion
          heldout.seed = 2019,   # Deterministic with seed
          M = 10,                # M value for exclusivity computation 
          cores = 4)             # Number of cores to use
```


```{r}

# Brutally convert successful 2018 measurements into a data frame for plotting

results.sK.2018 <-
  tibble(K = 0,
         semcoh = 0,
         heldout = 0,
         residual = 0,
         lbound = 0,
         exclus = 0)

results.sK.2018[1:6, 1] <- (as.numeric(as.character(results.searchK.2018.4$results[c(1:6), c(1) ] ))) #,3,4,5,7 ) 
results.sK.2018[1:6, 2] <- (as.numeric(as.character(results.searchK.2018.4$results[c(1:6), c(3) ] )))
results.sK.2018[1:6, 3] <- (as.numeric(as.character(results.searchK.2018.4$results[c(1:6), c(4) ] )))
results.sK.2018[1:6, 4] <- (as.numeric(as.character(results.searchK.2018.4$results[c(1:6), c(5) ] )))
results.sK.2018[1:6, 5] <- (as.numeric(as.character(results.searchK.2018.4$results[c(1:6), c(7) ] )))
results.sK.2018[1:6, 6] <- (as.numeric(as.character(results.searchK.2018.4$results[c(1:6), c(2) ] )))


results.sK.2018[7:9, 1] <- (as.numeric(as.character(results.searchK.2018.2$results[c(1:3), c(1) ] ))) #,3,4,5,7 ) 
results.sK.2018[7:9, 2] <- (as.numeric(as.character(results.searchK.2018.2$results[c(1:3), c(3) ] )))
results.sK.2018[7:9, 3] <- (as.numeric(as.character(results.searchK.2018.2$results[c(1:3), c(4) ] )))
results.sK.2018[7:9, 4] <- (as.numeric(as.character(results.searchK.2018.2$results[c(1:3), c(5) ] )))
results.sK.2018[7:9, 5] <- (as.numeric(as.character(results.searchK.2018.2$results[c(1:3), c(7) ] )))
results.sK.2018[7:9, 6] <- (as.numeric(as.character(results.searchK.2018.2$results[c(1:3), c(2) ] )))


results.sK.2018[10:14, 1] <- (as.numeric(as.character(results.searchK.2018$results[c(1:5), c(1) ] ))) #,3,4,5,7 ) 
results.sK.2018[10:14, 2] <- (as.numeric(as.character(results.searchK.2018$results[c(1:5), c(3) ] )))
results.sK.2018[10:14, 3] <- (as.numeric(as.character(results.searchK.2018$results[c(1:5), c(4) ] )))
results.sK.2018[10:14, 4] <- (as.numeric(as.character(results.searchK.2018$results[c(1:5), c(5) ] )))
results.sK.2018[10:14, 5] <- (as.numeric(as.character(results.searchK.2018$results[c(1:5), c(7) ] )))
results.sK.2018[10:14, 6] <- (as.numeric(as.character(results.searchK.2018$results[c(1:5), c(2) ] )))


# Sort by K 
results.sK.2018 <-  arrange(results.sK.2018, K)

# Plot in a 2x2 matrix
par(mfrow = c(2, 2))
  plot(
    results.sK.2018$K, 
    results.sK.2018$heldout,
       main = "Held-Out Likelihood 2018",
       xlab = "Number of Topics(K)",
       ylab = "Held-Out Likelihood",
       type = "b")

  plot(
    results.sK.2018$K, 
    results.sK.2018$residual,
       main = "Residuals 2018",
       xlab = "Number of Topics(K)",
       ylab = "Residuals",
       type = "b")

  plot(
    results.sK.2018$K, 
    results.sK.2018$semcoh,
       main = "Semantic coherence 2018",
       xlab = "Number of Topics (K)",
       ylab = "Semantic Coherence",
       type = "b")

  plot(results.sK.2018$K, 
    results.sK.2018$lbound,
       main = "Lower Bound 2018",
       xlab = "Number of Topics(K)",
       ylab = "Lower Bound",
       type = "b")
  
      
```

```{r}
# Plot exclusivity plot
plot(
    results.sK.2018$K, 
    results.sK.2018$exclus,
       main = "Exclusivity 2018",
       xlab = "Number of Topics(K)",
       ylab = "Exclusivity",
       type = "b")
```




```{r}

###############################################################################
# Implement Structural Topic Modeling 2018
###############################################################################

# Trim the dmf to top 10209 words
dfm.documents.2018.trim <- dfm_trim(dfm.documents.2018, min_termfreq = 17, verbose = TRUE)

# Run models with K topics. Vocab param is not used with dfm objects.

# Trimmed model
stm.documents.2018.k60.trim <-  stm(dfm.documents.2018.trim,  K = 60, init.type = "Spectral" )

# Untrimmed model
# stm.documents.2018.k60     <-  stm(dfm.documents.2018,  K = 60, init.type = "Spectral" )

saveRDS(stm.documents.2018.k60.trim, "stm.model.2018.k60.trim.RDS")

summary(stm.documents.2018.k60.trim)

```

```{r}
# 2018 top topics by sum gamma

# Code from https://www.r-bloggers.com/training-evaluating-and-interpreting-topic-models/
# By Julia Silge (2018)

# library(ggthemes)
# library(tidyverse)
# library(scales)
# library(purrr)

td_beta <- tidy(stm.documents.2018.k60.trim, 
                matrix = "beta")


td_gamma <- tidy(stm.documents.2018.k60.trim,
                 matrix = "gamma",
                 document_names = rownames(dfm.documents.2018.trim))


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

# Gamma distributions
###############################################################################
# Extract and summarise gamma matrix
###############################################################################
# Note that this is identical to the "theta" matrix in stm.

# Without defining the document_names, stm replaces them with index numbers
tm.gamma.documents.2018 <- tidy(stm.documents.2018.k60.trim, 
                                matrix = "gamma",
                                document_names = 
                                  rownames(stm.documents.2018.k60.trim))

# Now we have a tibble with (docs x topics) rows, with gamma values for each

# Add gamma sums for each topic
tm.gamma.summary.2018 <-
  tm.gamma.documents.2018[, 2:3]       %>%
    group_by(topic)                    %>%
    summarize(total = sum(gamma))      %>%
    arrange(., desc(total))


# TEST: Aggregate by  docs, should add up to 1 for each
# tm.gamma.summary <-
#   tm.gamma.documents[, 1:3]       %>%
#     group_by(document)            %>%
#     summarize(total = sum(gamma)) %>%
#     arrange(., desc(total))


# And now we have the topics sorted by the highest gamma values.
summary(tm.gamma.summary)
summary(tm.gamma.summary.2018)

hist(tm.gamma.summary.2018$total,
     breaks =  35,
     col = "lightblue")

hist(tm.gamma.documents.2018$gamma,
     main   = "Gamma distribution in doc-topic pairs (0.02 < gamma < 1), k = 60",
     xlab   = "Gamma per interaction",
     ylab   = "Topic-document pairs",
     xlim   = c(0.02, 1),
     ylim   = c(0, 300),
     breaks = 150, 
     col    = "lightyellow")

hist(tm.gamma.documents.2018$gamma,
     main   = "Gamma distribution in doc-topic pairs (0 < gamma < 1), k = 60",
     xlab   = "Gamma per interaction",
     ylab   = "Topic-document pairs",
     xlim   = c(0, 1),
     # ylim   = c(0, 200),
     breaks = 200, 
     col    = "lightyellow")

options(scipen = 10)
hist(tm.gamma.documents.2018$gamma,
     main   = "Gamma distribution in doc-topic pairs (0 < gamma < 0.001), k = 60",
     xlab   = "Gamma per interaction",
     ylab   = "Topic-document pairs",
     xlim   = c(0, 0.001),
     # ylim   = c(0, 300),
     breaks = 300000, 
     col    = "lightyellow")

hist(tm.gamma.documents.2018$gamma,
     main   = "Gamma distribution in doc-topic pairs (0.9 < gamma < 1), k = 60",
     xlab   = "Gamma per interaction",
     ylab   = "Topic-document pairs",
     xlim   = c(0.9, 1),
     ylim   = c(0, 300),
     breaks = 3000, 
     col    = "lightyellow")

```




```{r}
# Gamma distributions
###############################################################################
# Extract and summarise gamma matrix
###############################################################################
# Note that this is identical to the "theta" matrix in stm.

# Without defining the document_names, stm replaces them with index numbers
tm.gamma.documents.2018 <- tidy(stm.documents.2018.k60.trim, 
                                matrix = "gamma",
                                document_names = 
                                  rownames(stm.documents.2018.k60.trim))

# Now we have a tibble with (docs x topics) rows, with gamma values for each

# Add gamma sums for each topic
tm.gamma.summary.2018 <-
  tm.gamma.documents.2018[, 2:3]       %>%
    group_by(topic)                    %>%
    summarize(total = sum(gamma))      %>%
    arrange(., desc(total))


# TEST: Aggregate by  docs, should add up to 1 for each
# tm.gamma.summary <-
#   tm.gamma.documents[, 1:3]       %>%
#     group_by(document)            %>%
#     summarize(total = sum(gamma)) %>%
#     arrange(., desc(total))


# And now we have the topics sorted by the highest gamma values.
summary(tm.gamma.summary)
summary(tm.gamma.summary.2018)

hist(tm.gamma.summary.2018$total,
     breaks =  35,
     col = "lightblue")

hist(tm.gamma.documents.2018$gamma,
     main   = "Gamma distribution in doc-topic pairs (0.02 < gamma < 1), k = 60",
     xlab   = "Gamma per interaction",
     ylab   = "Topic-document pairs",
     xlim   = c(0.02, 1),
     ylim   = c(0, 300),
     breaks = 150, 
     col    = "lightblue")

hist(tm.gamma.documents.2018$gamma,
     main   = "Gamma distribution in doc-topic pairs (0 < gamma < 1), k = 60",
     xlab   = "Gamma per interaction",
     ylab   = "Topic-document pairs",
     xlim   = c(0, 1),
     # ylim   = c(0, 200),
     breaks = 200, 
     col    = "lightblue")


```

```{r}
###############################################################################
# Gamma histograms
###############################################################################

# Plot histogram of sum(gamma) / topic
hist(tm.gamma.summary.2018$total,
       main = "Gamma distribution per topic (k = 60)", 
       xlab = "Sum(gamma) per topic", 
       ylab = "No of topics" ,
       freq = TRUE,
       breaks = 70,
       col = "lightyellow" )



```


```{r}
###############################################################################
# Gamma sums for thresholds
###############################################################################

# Parameters for the loop
seq.gammas      <- seq(from = 1, to = 0, by = -0.001)
seq.gamma.index <- seq(from = 1, to = length(seq.gammas), by = 1)

# Initialise a tdf for the results
tdf.gamma.sums.2018 <- tibble(gamma  = rep(0, 1), # seq(from = 0, to = 1, by = 0.05 ),
                              cumsum = rep(0, 1),
                              topics = rep(0, 1),
                              interactions = rep(0, 1))


# The following terrible loop loops the gamma values and sums the gamma below a
# certain threshold int othe cumsum variable. It also counts the number of 
# topics 
for (i in seq.gamma.index) {
  tdf.gamma.sums.2018 <-
    rbind(tdf.gamma.sums.2018, 
          tibble(gamma  =  seq.gammas[i],
                 cumsum =  as.numeric(
                            sum(tm.gamma.documents.2018$gamma[
                                tm.gamma.documents.2018$gamma <= as.numeric(seq.gammas[i]) ] )),
                 topics =  as.numeric(
                            count(distinct(
                              tibble::enframe((tm.gamma.documents.2018$topic[
                                tm.gamma.documents.2018$gamma >= as.numeric(seq.gammas[i]) ] ),
                                name = NULL)
                                           )
                                  ) ),
                 interactions = 
                            length(tm.gamma.documents.2018$gamma[
                                   tm.gamma.documents.2018$gamma >= as.numeric(seq.gammas[i]) ] 
                                   )
                )
          )
}
```

```{r}
###############################################################################
# Cumulative gamma + topic curves, whole range
###############################################################################
plot(tdf.gamma.sums.2018[ 2:length(tdf.gamma.sums.2018$gamma), 1:2], 
     type = "b",
     main = "Cumulative gamma distribution by 0.001 increments, k = 60",
     xlab = "Gamma threshold",
     ylab = "Total gamma above threshold",
     # ylim = c(0, 800),
     cex  = 0.3,
     # ylim = c(0,150),
     col = "darkblue")

  # topic count
  lines(tdf.gamma.sums.2018$gamma[ 2:length(tdf.gamma.sums.2018$gamma)], 
        tdf.gamma.sums.2018$topics[2:length(tdf.gamma.sums.2018$topics)], 
        type = "l", 
        lty  =  1,
        col  = "darkred")
  
  # interaction count
  lines(tdf.gamma.sums.2018$gamma[ 2:length(tdf.gamma.sums.2018$gamma)], 
        tdf.gamma.sums.2018$interactions[2:length(tdf.gamma.sums.2018$interactions)],
        type = "b",
        col  = "darkgreen",
        cex  = 0.3)
  
  legend("left",
         legend = c("Gamma above threshold", "No of topics", "No of interactions" ),
         lty    = c(1,1,1), 
         col    = c("darkblue", "darkred", "darkgreen"))

###############################################################################
# Topic curves, whole range
###############################################################################
plot(tdf.gamma.sums.2018$gamma[ 2:length(tdf.gamma.sums.2018$gamma)], 
     tdf.gamma.sums.2018$topics[2:length(tdf.gamma.sums.2018$topics)], 
     main = "Topics with gamma above threshold in 0.001 increments, k = 60",
     xlab = "Gamma threshold",
     ylab = "Topics with gamma above threshold",
     type = "b", 
     lty  =  1,
     cex  =  0.3,
     col  = "darkred")


###############################################################################
# Interaction curves, whole range
###############################################################################
plot(tdf.gamma.sums.2018$gamma[ 2:length(tdf.gamma.sums.2018$gamma)], 
     tdf.gamma.sums.2018$interactions[2:length(tdf.gamma.sums.2018$interactions)], 
     # log  = "y",
     main = "Interactions with gamma above gamma threshold > 0.1 , k = 60",
     xlab = "Gamma threshold",
     ylab = "Interactions with gamma above threshold",
     type = "b", 
     lty  =  1,
     xlim = c(0.1, 1),
     ylim = c(0, 1500),
     cex  =  0.3,
     col  = "darkgreen")


###############################################################################
# Cumulative gamma + topic curves, top 0.005
###############################################################################

plot(tdf.gamma.sums.2018[ 2:length(tdf.gamma.sums.2018$gamma), 1:2], 
     type = "b",
     main = "Cumulative gamma distribution by 0.001 increments, k = 60",
     xlab = "Gamma threshold",
     ylab =  "Total gamma above threshold",
     xlim = c(0.98, 1),
     # ylim = c(0,150),
     col = "darkblue")
  
  lines(tdf.gamma.sums.2018$gamma[ 2:length(tdf.gamma.sums.2018$gamma)], 
        tdf.gamma.sums.2018$topics[2:length(tdf.gamma.sums.2018$topics)], 
        type = "b", 
        col  = "darkred")
  
  lines(tdf.gamma.sums.2018$gamma[ 2:length(tdf.gamma.sums.2018$gamma)], 
        tdf.gamma.sums.2018$interactions[2:length(tdf.gamma.sums.2018$interactions)],
        type = "b",
        col = "darkgreen")
  
  legend("topleft",
         legend = c("Gamma above threshold", "No of topics", "No of interactions" ),
         lty    = c(1,1,1), 
         col    = c("darkblue", "darkred", "darkgreen"))

###############################################################################
```
```{r}

###############################################################################
# Correlation network plots for 2018
###############################################################################

# These lists of topics are computed from the extracted gamma scores.
# They're out of place here.

# all.top.bottom.topics <-
#   c(tm.gamma.deltas.bottom.rel$topic, 
#   tm.gamma.deltas.bottom.abs$topic, 
#   tm.gamma.deltas.top.rel$topic ,
#   tm.gamma.deltas.top.abs$topic) 
  
# Extract topic correlations, plot as network graphs for each corr level 0-0.5
for (i in seq(0, 0.3, 0.025)) {
  topiccorrs <- topicCorr(stm.documents.2018.k60.trim, 
                          method = "simple", 
                          cutoff = i)
  
  plot.topicCorr(topiccorrs, 
                 topics = c(1:60),
                 vertex.color = NULL, 
                 vertex.size  = 2 , 
                 layout       = layout_with_kk ,
                 main = paste("2018 Topic correlation network at cutoff", i, "(k = 60)") )
}
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
tm.beta.top100 <- f.topnwords(100, seq(1, length(tm.gamma.summary$topic), 1) )
  

###############################################################################
# Top 20 words for top topics
###############################################################################
# Extract the top 20 words for each top 10 topic

# Get top 20 words by beta for all topics
tm.beta.top20.words <-
  tm.beta.documents.top   %>%
    group_by(topic)       %>%
    top_n(20, beta)



```


```{r}



```

