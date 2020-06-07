# https://www.datacamp.com/community/tutorials/market-basket-analysis-r
# https://towardsdatascience.com/association-rule-mining-in-r-ddf2d044ae50
# https://stackoverflow.com/questions/41620651/long-dataframe-to-transactions-for-arules-in-r
# Formulas: http://r-statistics.co/Association-Mining-With-R.html

library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(arules)
library(arulesViz)
library(plotly)

# Load dats
data <- read.delim("data/grocery_transactional.txt", sep = ',', stringsAsFactors = FALSE)

# Prepare data
df <- data %>%
  select(CUSTOMER, PRODUCT) %>%
  mutate(
    PRODUCT = trimws(PRODUCT),
    value = 1) %>%
  spread(PRODUCT, value, fill = 0) 

tr <- as(as.matrix(df[, -1]), 'transactions')
(summary_ <- summary(tr))

# Items per ticket histogram
tibble(`Items per ticket` = factor(names(summary_@lengths), levels = names(summary_@lengths)), Frequency = as.numeric(summary_@lengths)) %>% 
  ggplot() +
  geom_bar(aes(x = `Items per ticket`, y = Frequency), stat="identity")
  
itemFrequencyPlot(tr, topN=20, type="absolute", main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr, topN=20, type="relative", main="Relative Item Frequency Plot")

# Checking particular column names
colnames(df)[grepl('misc|other|milk', colnames(df), ignore.case = TRUE)]

# Remove a few products
df <- df %>% 
  select(-all_of(c('whole milk', 'other vegetables', 'misc. beverages')))
tr <- as(as.matrix(df[, -1]), 'transactions')
summary(tr)

# Clean up
rm(association_rules)
association_rules <- apriori(tr, parameter = list(support=0.002, confidence=0.25, minlen=3, maxlen=5, maxtime = 0))
# inspect(association_rules)

# Additional columns for measurements
bind_cols(
  inspect(association_rules), 
  interestMeasure(association_rules, c("oddsRatio", "leverage"), transactions = tr)
)

# Predict
# https://stackoverflow.com/questions/38394113/deploying-apriori-rulsets-to-the-dataset-in-r
# https://stackoverflow.com/questions/40833925/applying-rules-generated-from-arules-in-r-to-new-transactions

predict_transaction <- function(rules, transaction, sorting = 'confidence') {

  # Find all rules whose lhs matches the training example
  rulesMatch <- is.subset(rules@lhs, transaction, sparse = FALSE)
  
  if(any(rulesMatch)) {
    
    # Subset all applicable rules
    applicable <- rules[rulesMatch==TRUE]
    
    # the first rule has the highest confidence since they are sorted
    rules<-sort(applicable, decreasing=TRUE, by=sorting)
    prediction <- rules[1]
    
    # dump, to remove unwanted outputs
    capture.output({
      rhs_ <- inspect(prediction@rhs)
      inspect_ <- inspect(prediction)
    })
    prediction_ <- prediction
    prediction_ <- prediction
    quality_ <- quality(prediction)
    data_ <- bind_cols(rhs_, quality_)
    
  } else {
    rhs_ <- inspect_ <- quality_ <- prediction_ <- data_ <- NA
    quality_ <- data.frame(support=NA, confidence=NA, coverage=NA, lift=NA, count=NA)
    data_ <- data.frame(items = NA, quality_)
  }
    
  # Output
  return(list(
    rhs = rhs_,
    inspect = inspect_,
    prediction = prediction_,
    quality = quality_,
    data = data_
  ))
}

# Predictions applied
predict_rhs <- bind_cols(
    df,
    map_dfr(1:nrow(df), .f = function(x) predict_transaction(association_rules, tr[x])$data)
  )
skim(predict_rhs)

# Get subset rules in vector
rules_subset <- which(colSums(is.subset(association_rules, association_rules)) > 1)
rules_subset <- association_rules[-rules_subset] # remove subset rules.
# subRules <- head(rules_subset, n = 10, by = "confidence")
rules_subset_filtered<-rules_subset[
  quality(rules_subset)$lift >= 1 & 
  quality(rules_subset)$confidence >= 0.35 &
  quality(rules_subset)$support >= 0.003]

#Plot SubRules
# https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf
plot(rules_subset_filtered)
plot(rules_subset_filtered, method = "two-key plot")
plot(rules_subset_filtered, engine = "plotly")
plot(rules_subset_filtered, method = "matrix", measure = "lift")
plot(rules_subset_filtered, method = "grouped")
plot(rules_subset_filtered, method = "graph",  engine = "htmlwidget")
plot(rules_subset_filtered, method = "paracoord")

# Citation
citation('plyr')
citation('dplyr')
citation('tidyr')
citation('tidyverse')
citation('arules')
citation('arulesViz')
citation('plotly')
