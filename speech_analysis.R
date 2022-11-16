##### Load Packages #####
# Clear Working Directory
rm(list = ls(all.names = TRUE))
# Set Working Directory
setwd("C:/Users/colin/partisan-appeals-to-bipartisanship")
library(stringr)
library(readxl)
library(tidytext)
library(tidyr)
library(textdata)
library(dplyr)
library(corpus)
library(ggplot2)
set.seed(1996)
# Load Data
speeches_bydate <- readRDS("speech_data.rds")

# Average Calculations
speeches_bydate$sentiment_minority_average <- speeches_bydate$sentiment_minority/speeches_bydate$speech_minority_minority
speeches_bydate$sentiment_majority_average <- speeches_bydate$sentiment_majority/speeches_bydate$speech_majority_majority

# Proportion Bipartisanship
speeches_bydate$prop_bipartisan_minority <- speeches_bydate$bipartisan_minority/speeches_bydate$speech_minority_minority
speeches_bydate$prop_bipartisan_majority <- speeches_bydate$bipartisan_majority/speeches_bydate$speech_majority_majority

# Remove Low Speech Dates
speeches_bydate_all <- speeches_bydate
summary(speeches_bydate$speech_majority_majority)
summary(speeches_bydate$speech_minority_minority)
speeches_bydate <- subset(speeches_bydate, speech_majority_majority >=32 & speech_minority_minority >=25)

# Statistics
summary(speeches_bydate$prop_bipartisan_majority)
summary(speeches_bydate$prop_bipartisan_minority)

summary(speeches_bydate$sentiment_minority_average)
summary(speeches_bydate$sentiment_majority_average)

summary(speeches_bydate$sentiment_minority_average_standard)
summary(speeches_bydate$sentiment_majority_average_standard)



# Correlations
# Majority Bipartisan Mentions v. Minority Opinion Sentiment
cor(speeches_bydate$sentiment_minority_average, speeches_bydate$prop_bipartisan_majority)
###cor(speeches_bydate$sentiment_minority_average_standard, speeches_bydate$prop_bipartisan_majority)

# Majority Bipartisan Mentions v. Minority Bipartisan Mentions
cor(speeches_bydate$prop_bipartisan_minority, speeches_bydate$prop_bipartisan_majority)

# Majority Opinion Sentiment v. Minority Opinion Sentiment 
cor(speeches_bydate$sentiment_minority_average, speeches_bydate$sentiment_majority_average)

# Minority Opinion Sentiment v. Minority Bipartisan Mentions
cor(speeches_bydate$prop_bipartisan_minority, speeches_bydate$sentiment_minority_average)

# Majority Opinion Sentiment v. Majority Bipartisan Mentions
cor(speeches_bydate$prop_bipartisan_majority, speeches_bydate$sentiment_majority_average)



####### Figure 1: Bipartisan Messaging v. Opinion Sentiment #######  
ggplot(speeches_bydate, aes(x = prop_bipartisan_majority, y = sentiment_minority_average)) + 
  geom_point()+
  theme_bw()+
  ylab('Minority-Party Speech Opinion Sentiment (Daily Average)')+
  xlab('Proportion of Majority-Party Speeches Mentioning Bipartisanship (Per Day)') +
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))



####### Appendix A/Figure 1: Figure 1 with All Speeches #######
ggplot(speeches_bydate_all, aes(x = prop_bipartisan_majority, y = sentiment_minority_average)) + 
  geom_point()+
  theme_bw()+
  ylab('Minority-Party Speech Opinion Sentiment (Daily Average)')+
  xlab('Proportion of Majority-Party Speeches Mentioning Bipartisanship (Per Day)') +
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))



