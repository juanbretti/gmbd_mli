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
library(skimr)
library(iplots)

## Functions ----

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

# Additional metrics
rules_metrics <- function(rules, tr) {
  capture.output(
    out <- bind_cols(
      inspect(rules), 
      interestMeasure(rules, c("oddsRatio", "leverage"), transactions = tr),
      tibble(`lhs length` = size(rules@lhs))
    )
  )
  colnames(out)[2] <- '_'
  return(out)
}

## Load data ----
data <- read.delim("data/grocery_transactional.txt", sep = ',', stringsAsFactors = FALSE)

# Prepare data
df1 <- data %>%
  select(CUSTOMER, PRODUCT) %>%
  mutate(
    PRODUCT = trimws(PRODUCT),
    value = 1) %>%
  spread(PRODUCT, value, fill = 0) 

tr1 <- as(as.matrix(df1[, -1]), 'transactions')
(summary_ <- summary(tr1))

## EDA ----

# Items per ticket histogram
tibble(`Items per ticket` = factor(names(summary_@lengths), levels = names(summary_@lengths)), Frequency = as.numeric(summary_@lengths)) %>% 
  ggplot() +
  geom_bar(aes(x = `Items per ticket`, y = Frequency), stat="identity")
  
itemFrequencyPlot(tr1, topN=20, type="absolute", main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr1, topN=20, type="relative", main="Relative Item Frequency Plot")

# Checking particular column names
colnames(df1)[grepl('misc|other|milk', colnames(df1), ignore.case = TRUE)]

# Get 50% of the transactions
set.seed(42)
df2 <- df1 %>% 
  sample_frac(0.5)
tr2 <- as(as.matrix(df2[, -1]), 'transactions')
summary(tr2)

## Apriori ----

association_rules <- apriori(tr2, parameter = list(support=0.005, confidence=0.25, minlen=3, maxtime = 0))
# Clean up subset rules
rules_subset <- which(colSums(is.subset(association_rules, association_rules)) > 1)
rules_subset <- association_rules[-rules_subset] # remove subset rules.
# Get subset rules in vector
rules_subset_top <- head(rules_subset, n = 10, by = "support")

# Whole milk case
association_rules_whole_milk <- apriori(tr1, parameter = list(support=0.05, confidence=0.1, minlen=2, maxtime = 0), appearance = list(lhs="whole milk", default="rhs"))

## Usage of the model ----

# Additional columns for measurements
rules_metrics(association_rules, tr2)
rules_metrics(association_rules_whole_milk, tr2)

# Predictions applied
system.time({
  predict_rhs <- bind_cols(
      df2,
      map_dfr(1:nrow(df2), .f = function(x) predict_transaction(association_rules, tr2[x])$data)
    )
  skim(predict_rhs)
})[3]
# Example of prediction
predict_transaction(association_rules, tr2[10])$data

## Explore results ----
inspect(rules_subset)

# Plot SubRules
# https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf
# All the rules following the previous criteria
plot(rules_subset, method = "scatterplot")
plot(rules_subset, method = "two-key plot")
plot(rules_subset, method = "grouped")
plot(rules_subset, method = "iplots")
plot(rules_subset, engine = "plotly")
# plot(rules_subset, method = "matrix3D")
# Top 10 rules
plot(rules_subset_top, method = "matrix", measure = "lift")
plot(rules_subset_top, method = "graph")
plot(rules_subset_top, method = "graph",  engine = "htmlwidget")
plot(rules_subset_top, method = "paracoord")

## Citation ----
citation('plyr')
citation('dplyr')
citation('tidyr')
citation('tidyverse')
citation('arules')
citation('arulesViz')
citation('plotly')
