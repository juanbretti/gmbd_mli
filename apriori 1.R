# https://www.datacamp.com/community/tutorials/market-basket-analysis-r

library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(arules)
library(arulesViz)
library(plotly)

data <- read.delim("data/grocery_transactional.txt", sep = ',')

# https://stackoverflow.com/questions/41620651/long-dataframe-to-transactions-for-arules-in-r
df <- data %>%
  select(CUSTOMER, PRODUCT) %>%
  filter(!grepl('misc|other|milk', PRODUCT, ignore.case = TRUE)) %>%
  mutate(value = 1) %>%
  spread(PRODUCT, value, fill = 0) 

tr <- as(as.matrix(df[, -1]), 'transactions')

summary(tr)

itemFrequencyPlot(tr, topN=20, type="absolute", main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr, topN=20, type="relative", main="Relative Item Frequency Plot")

# Clean up
rm(association.rules)
association.rules <- apriori(tr, parameter = list(support=0.005, confidence=0.25, minlen=3, maxlen=10))
inspect(association.rules)

# get subset rules in vector
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) == 1)
length(subset.rules)
subset.association.rules. <- association.rules[subset.rules] # remove subset rules.
inspect(subset.association.rules.)

subRules<-association.rules[quality(association.rules)$lift > 1]

#Plot SubRules
# https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf
plot(subRules)
plot(subRules, method = "two-key plot")
plot(subRules, engine = "plotly")
plot(subRules, method = "matrix", measure = "lift")
plot(subRules, method = "grouped")

top10subRules <- head(subRules, n = 15, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

# Filter top 20 rules with highest lift
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")

