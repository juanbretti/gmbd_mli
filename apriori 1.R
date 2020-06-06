# https://www.datacamp.com/community/tutorials/market-basket-analysis-r

library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(arules)
library(arulesViz)
library(plotly)

# Load dats
data <- read.delim("data/grocery_transactional.txt", sep = ',')


# Prepare data
df <- data %>%
  select(CUSTOMER, PRODUCT) %>%
  mutate(value = 1) %>%
  spread(PRODUCT, value, fill = 0) 

tr <- as(as.matrix(df[, -1]), 'transactions')
(summary_ <- summary(tr))

# Items per ticket histogram
tibble(`Items per ticket` = factor(names(summary_@lengths), levels = names(summary_@lengths)), Frequency = as.numeric(summary_@lengths)) %>% 
  ggplot() +
  geom_bar(aes(x = `Items per ticket`, y = Frequency), stat="identity")
  
itemFrequencyPlot(tr, topN=20, type="absolute", main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr, topN=20, type="relative", main="Relative Item Frequency Plot")

# https://stackoverflow.com/questions/41620651/long-dataframe-to-transactions-for-arules-in-r
colnames(df)[grepl('misc|other|milk', colnames(df), ignore.case = TRUE)]

# Remove a few products
df <- df %>% 
  select(-all_of(c('whole milk', 'other vegetables', 'misc. beverages')))
tr <- as(as.matrix(df[, -1]), 'transactions')
summary(tr)

# Clean up
rm(association.rules)
association.rules <- apriori(tr, parameter = list(support=0.005, confidence=0.25, minlen=3, maxlen=10))
inspect(association.rules)

# Get subset rules in vector
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) == 1)
length(subset.rules)
subset.association.rules <- association.rules[subset.rules] # remove subset rules.
subRules<-subset.association.rules[quality(subset.association.rules)$lift > 1]
inspect(subRules)

#Plot SubRules
# https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf
plot(subRules)
plot(subRules, method = "two-key plot")
plot(subRules, engine = "plotly")
plot(subRules, method = "matrix", measure = "lift")
plot(subRules, method = "grouped")
plot(subRules, method = "graph",  engine = "htmlwidget")

# Filter top 20 rules with highest lift
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")