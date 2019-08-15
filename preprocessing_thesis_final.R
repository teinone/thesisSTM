---
title: "Addendum: Preprocessing for Topic Modeling"
output: html_notebook
---

This notebook starts by importing a CSV with the documents split into "sentences."
The notebook ends with exporting the prepared tibbles and metadata into files.
There are multiple switch statements that are controlled with the param.Year parameter.

# Setup and Loading Libraries
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
###############################################################################
# Loading required libraries:
###############################################################################

# Data frame and pipe utilities:
# library(plyr)     #has to be before dplyr
library(dplyr)    #has to be after plyr

# String and text tools:
library(stringr)
library(tidytext)

# Visualisation:
library(ggplot2)

# Topic modeling
library(topicmodels)
library(lsa)

# Text mining libraries
library(quanteda)
library(tm)
# library(SnowballC)
# Required as LDA config and/or dependencies
library(ldatuning)

# Structural Topic Models
library(stm)


```

## Preparing Stop Word Dictionaries
```{r}
###############################################################################
# Stopwords
###############################################################################

# Load stop word dictionary for EN and NL included in the lsa library
data(stop_words)   # EN 1149 words
data(stopwords_nl) # NL  260 words

# Load an additional, larger dictionary for NL by Eikhart (2011) from
# (https://eikhart.com/blog/dutch-stopwords-list)
additional.stopwords.nl <- read.csv("~/TXT/Final/nl_stopwords.csv",
                                    stringsAsFactors = FALSE) #501 words

# Reformat the NL dictionaries into tbl_df (tidy data frame), a tibble
# enframe works like as_tibble, which is likely to be changed in the future
stopwords_nl            <- tibble::enframe(stopwords_nl, name = NULL)  
additional.stopwords.nl <- as_tibble(additional.stopwords.nl)

# Set column name to match data (needed for anti_join)
colnames(stopwords_nl)            <- "word"
colnames(additional.stopwords.nl) <- "word"


# Concatenate the Dutch dictionaries and remove duplicates 
stopwords_nl <- rbind(additional.stopwords.nl, stopwords_nl)
stopwords_nl <- distinct(stopwords_nl)

# We notice that the resulting dictionary has 501 observations, i.e. the smaller
# dictionary from lsa was already contained in the larger dictionary
```

## Preparing tables for metadata: RUN ONLY ONCE IN THE BEGINNING
```{r} 
###############################################################################
# Metadata dtfs created prior to looping years
###############################################################################

# Contains all document metadata
tdf.metadata.all <- tibble(Program = NA,
                           Student = NA,
                           Resit   = NA,
                           Year    = NA )

# Metadata dtf created prior to looping years
# Contains final document counts
tdf.yearcounts <- tibble(Year  = seq(from = 2018, to = 2014, by = -1),
                         docs  = rep(0, 5),
                         vocab = rep(0, 5),
                         words = rep(0, 5),
                         avg   = rep(0, 5)) # currently not used 

```

The following sections are run in an identical fashion for each year separately.

## Data loading

```{r}
###############################################################################
# READ BEFORE RUNNING
###############################################################################
# Please note, EACH year of data can require several GB of RAM. The operations
# run in a reasonable time, unless they require memory to resort to paging or
# swap. That might increase runtime massively.

###############################################################################
# SWITCH SYNTAX AND CONTROL PARAMETER
###############################################################################
# There are quite a few switches in this code. All of them are controlled with
# this parameter. These switches control loading, saving and writing metadata.
# Adding a new year of requires adding a new row to each of the switch statements. 

# PARAM FOR YEAR (used in switch functions and labelling graphs)
param.Year <- "2014"
# Has to be char to work with switch.
###############################################################################

###############################################################################
# Load dataset as a data frame:
###############################################################################
switch(param.Year,
    "2018" = df.corpus.raw <- read.csv("~/TXT/Final/2018.csv", stringsAsFactors = FALSE),
    "2017" = df.corpus.raw <- read.csv("~/TXT/Final/2017.csv", stringsAsFactors = FALSE),
    "2016" = df.corpus.raw <- read.csv("~/TXT/Final/2016.csv", stringsAsFactors = FALSE),
    "2015" = df.corpus.raw <- read.csv("~/TXT/Final/2015.csv", stringsAsFactors = FALSE),
    "2014" = df.corpus.raw <- read.csv("~/TXT/Final/2014.csv", stringsAsFactors = FALSE),
)

# df.corpus.raw  <- read.csv("~/TXT/Final/2016.csv", stringsAsFactors = FALSE)

```

```{r}
###############################################################################
# Clean and extract metadata
###############################################################################
# NOTE: DIFFERENT DATA SCHEMA FOR 2014

# Metadata: Split student numbers, year and programs for later matching
switch(param.Year,
    "2018" = df.metadata  <- distinct(df.corpus.raw[, c(1,2,3,5)]),
    "2017" = df.metadata  <- distinct(df.corpus.raw[, c(1,2,3,5)]),
    "2016" = df.metadata  <- distinct(df.corpus.raw[, c(1,2,3,5)]),
    "2015" = df.metadata  <- distinct(df.corpus.raw[, c(1,2,3,5)]),
    "2014" = df.metadata  <- distinct(df.corpus.raw[, c(3,4,5,6)]) # NOTE!
)

# Reorder 2014 fields 
switch(param.Year,
    "2018" = df.metadata  <- df.metadata,
    "2017" = df.metadata  <- df.metadata,
    "2016" = df.metadata  <- df.metadata,
    "2015" = df.metadata  <- df.metadata,
    "2014" = df.metadata  <- df.metadata[, c(1,3,2,4)]
)

# Append to a tdf for all metadata
tdf.metadata.all <- rbind(tdf.metadata.all, df.metadata)

# Turn the text df into the Tidy format. One word per line. 
tdf.corpus       <- df.corpus.raw %>% unnest_tokens(word, Fulltext)

# Remove stop words from the corpus using anti_join, col "word" as key.
tdf.corpus.stop  <- tdf.corpus      %>% anti_join(stop_words)
tdf.corpus.stop  <- tdf.corpus.stop %>% anti_join(stopwords_nl)

# Clear unnecessary tables from memory.
rm(df.corpus.raw)
```



```{r}
###############################################################################
# Word counts
###############################################################################

# Add word counts for every word in the corpus. Runs in a few seconds for a year.
tdf.count.words.raw <- tdf.corpus      %>% count(word, sort = TRUE)
tdf.count.words     <- tdf.corpus.stop %>% count(word, sort = TRUE)


###############################################################################
# Student level counts
###############################################################################
# Due to a bug in the original extraction script, some FileIDs are missing their 
# last digits. For this reason, the counts are done by student number. If the 
# repository worked as it should, this should lead to a single document per 
# student, and only accepted theses are included. The reality didn't match this
# expectation.

###############################################################################
# Stop words INCLUDED
###############################################################################
# Do document/student level split and aggregation
tdf.doc.words.raw <- tdf.corpus        %>%
    count(Student, word, sort = TRUE)  %>%
    ungroup()

total.words.raw <- 
    tdf.doc.words.raw       %>%
    group_by(Student)       %>%
    summarize(total = sum(n))

#Join total counts and student counts into a single tibble.
tdf.doc.words.raw <- left_join(tdf.doc.words.raw, total.words.raw)

# Histogram of distribution: 
options(scipen = 10) # suppress scientific notation

h.student.words.raw <-
  hist((total.words.raw$total), 
       main = paste("Words per Student", param.Year),
       xlab = "Words (incl. stop words)", 
       xlim = c(1, 200000),
       freq = TRUE,
       breaks = 100,
       col = "lightblue" )

###############################################################################
# Stop words REMOVED
###############################################################################
# Do document/student level split and aggregation
tdf.doc.words <- tdf.corpus.stop       %>%
    count(Student, word, sort = TRUE)  %>%
    ungroup()

total.words <- tdf.doc.words     %>%
    group_by(Student)            %>%
    summarize(total = sum(n))

#Join total counts and student counts into a single tibble.
tdf.doc.words <- left_join(tdf.doc.words, total.words)


# Histogram of distribution: 
h.student.words <-
  hist((total.words$total), 
       main = paste("Non-stop words per Student", param.Year),
       xlab = "Non-stop words", 
       xlim = c(0, 80000),
       freq = TRUE,
       breaks = 100,
       col = "lightblue" )


#TODO: add subtitles with Year info.

```
Using these histograms, we can establish a rough threshold for including or excluding documents based on excessive length or shortness. This enables us to remove theses with large addenda (such as interview transcripts) and non-final documents. 


```{r}
###############################################################################
# Removing too long, short and problematic documents
###############################################################################
# Too short documents are generally drafts that did not pass, but for some reason
# are still included in the repository. Too long documents generally include a 
# significant amount of addenda, primarily interview transcripts. This list also
# includes documents that have been recognised to include a significant amount
# of Dutch text, usually from interviews.

# Manually removed theses
tdf.problematic.theses.2018 <- tibble("Student" = c(341937, 373437, 384041, 
                                                    387882, 385789, 387875, 
                                                    386348 ))

tdf.problematic.theses.2017 <- tibble("Student" = c(369138, 353334))

tdf.problematic.theses.2016 <- tibble("Student" = c(357659, 358896, 370866,
                                                    376442, 370370, 370370,
                                                    366036 ))

tdf.problematic.theses.2015 <- tibble("Student" = c(375819, 348360, 322891,
                                                    374822, 332745, 361749,
                                                    341587, 344760, 357921,
                                                    343824 ))

tdf.problematic.theses.2014 <- tibble("Student" = c(401731, 366958, 376951,
                                                    331658, 387029, 291672,
                                                    375615, 307352, 329201,
                                                    346802, 402727, 404008,
                                                    401154, 320031, 314332,
                                                    370832 ))

# Identify documents that are likely to be incomplete (less than 3500 total words):
exclude.short <- distinct(tdf.doc.words[tdf.doc.words$total < 3500,  c(1, 4)])
exclude.long  <- distinct(tdf.doc.words[tdf.doc.words$total > 50000, c(1, 4)])

# Print removed documents
# print(exclude.short)
print(length(exclude.short$Student))
print(sum(exclude.short$total))

# print(exclude.long)
print(length(exclude.long$Student))
print(sum(exclude.long$total))

# Remove these documents from the corpus
# Note, having a problematic thesis in any year will exclude the student entirely
tdf.doc.words.clean <- 
  tdf.doc.words                          %>% 
  anti_join(exclude.short[, 1])          %>%
  anti_join(exclude.long[ , 1])          %>%
  
  anti_join(tdf.problematic.theses.2018) %>%
  anti_join(tdf.problematic.theses.2017) %>%
  anti_join(tdf.problematic.theses.2016) %>%
  anti_join(tdf.problematic.theses.2015) %>%
  anti_join(tdf.problematic.theses.2014)


h.clean.words <-
  hist((tdf.doc.words.clean$total), 
       main = paste("Non-stop words per Student", param.Year),
       xlab = "Non-stop words", 
       xlim = c(0, 80000),
       ylab = "Frequency",
       # ylim = c(0,100),
       freq = TRUE,
       breaks = 80,
       col = "lightblue" )


```



```{r}  
# Generate statistics on final documents

# No of documents included after cleaning
int.docs.in.year  <- as.numeric(length(table(tdf.doc.words.clean$Student)))
int.vocab.in.year <- as.numeric(length(table(tdf.doc.words.clean$word)))
int.words.in.year <- as.numeric(sum(tdf.doc.words.clean$n))

# # # Metadata dtf created prior to looping years
# tdf.yearcounts <- tibble(Year  = seq(from = 2018, to = 2014, by = -1),
#                          docs  = rep(0, 5),
#                          vocab = rep(0, 5),
#                          words = rep(0, 5),
#                          avg   = rep(0, 5))

# Write final document count into a tdf
switch(param.Year,
    "2018" = tdf.yearcounts[1, 2] <- int.docs.in.year,
    "2017" = tdf.yearcounts[2, 2] <- int.docs.in.year,
    "2016" = tdf.yearcounts[3, 2] <- int.docs.in.year,
    "2015" = tdf.yearcounts[4, 2] <- int.docs.in.year,
    "2014" = tdf.yearcounts[5, 2] <- int.docs.in.year
)

switch(param.Year,
    "2018" = tdf.yearcounts[1, 3] <- int.vocab.in.year,
    "2017" = tdf.yearcounts[2, 3] <- int.vocab.in.year,
    "2016" = tdf.yearcounts[3, 3] <- int.vocab.in.year,
    "2015" = tdf.yearcounts[4, 3] <- int.vocab.in.year,
    "2014" = tdf.yearcounts[5, 3] <- int.vocab.in.year
)

switch(param.Year,
    "2018" = tdf.yearcounts[1, 4] <- int.words.in.year,
    "2017" = tdf.yearcounts[2, 4] <- int.words.in.year,
    "2016" = tdf.yearcounts[3, 4] <- int.words.in.year,
    "2015" = tdf.yearcounts[4, 4] <- int.words.in.year,
    "2014" = tdf.yearcounts[5, 4] <- int.words.in.year
)

rm(int.docs.in.year, int.vocab.in.year, int.words.in.year)


```






```{r}

#Final annual file to export: tdf.doc.words.clean
switch(param.Year,
    "2018" = tdf.export.2018 <- tdf.doc.words.clean,
    "2017" = tdf.export.2017 <- tdf.doc.words.clean,
    "2016" = tdf.export.2016 <- tdf.doc.words.clean,
    "2015" = tdf.export.2015 <- tdf.doc.words.clean,
    "2014" = tdf.export.2014 <- tdf.doc.words.clean
)

#tdf.export <- ("Final file to export here")

# Export results
switch(param.Year,
    "2018" = saveRDS(tdf.export.2018, file = "export2018.RDS"),
    "2017" = saveRDS(tdf.export.2017, file = "export2017.RDS"),
    "2016" = saveRDS(tdf.export.2016, file = "export2016.RDS"),
    "2015" = saveRDS(tdf.export.2015, file = "export2015.RDS"),
    "2014" = saveRDS(tdf.export.2014, file = "export2014.RDS")
)

# Save annual metadata separately
switch(param.Year,
    "2018" = saveRDS(tdf.metadata.all, file = "metadata2018.RDS"),
    "2017" = saveRDS(tdf.metadata.all, file = "metadata2017.RDS"),
    "2016" = saveRDS(tdf.metadata.all, file = "metadata2016.RDS"),
    "2015" = saveRDS(tdf.metadata.all, file = "metadata2015.RDS"),
    "2014" = saveRDS(tdf.metadata.all, file = "metadata2014.RDS")
)

# saveRDS(tdf.metadata.all, file = "metadata2014-2018.RDS")

# saveRDS(tdf.yearcounts,   file = "tdf.yearcounts2014-2018.RDS")

```


```{r}
#######ONLY RUN AT THE END

# The metadata of each year has been appended to tdf.metadata.all
# After each year, take counts and join all metadata:

tdf.full.metadata <- left_join(tdf.metadata.all, 
                               tdf.yearcounts, 
                               by = "Year")

saveRDS(tdf.full.metadata, file = "metadata.full2014-2018.RDS")

```

```{r}

```
