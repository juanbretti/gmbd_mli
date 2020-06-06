# https://www.statmethods.net/advstats/cluster.html
# https://uc-r.github.io/kmeans_clustering
# https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/
# https://rpkgs.datanovia.com/factoextra/reference/fviz_silhouette.html

## Libraries ----
# Read SPSS
library(foreign)
#General
library(tidyverse)
# Clustering
library(caret)
library(cluster)
library(factoextra)
library(psych)
library(NbClust)
# Visualization
library(skimr)
library(ggplot2)
library(ggpubr)

## Read data ----
data <- read.spss('data/provinces.sav', to.data.frame=TRUE) %>%
  mutate(provincia = trimws(provincia)) %>% 
  drop_na()
skim(data)

## Functions ----

method_number <- function(data, names, centers = 7) {
  
  set.seed(42)
  data_limited <- data[, names]
  
  # Elbow method
  # https://scikit-learn.org/stable/modules/clustering.html
  p_elbow <- fviz_nbclust(data_limited, kmeans, method = "wss") +
    geom_vline(xintercept = centers, linetype = 2)+
    labs(subtitle = "Elbow method")
  
  # Silhouette method
  p_silhouette <- fviz_nbclust(data_limited, kmeans, method = "silhouette", print.summary = TRUE) +
    labs(subtitle = "Silhouette method")
  
  # Gap statistic
  p_gap_statistic <- fviz_nbclust(data_limited, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
    labs(subtitle = "Gap statistic method")
  
  # NbClust
  res.nbclust <- NbClust(data_limited, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all") 
  # Visualize
  p_NbClust <- fviz_nbclust(res.nbclust, ggtheme = theme_minimal())
  
  return(list(
    p_elbow = p_elbow, 
    p_silhouette = p_silhouette, 
    p_gap_statistic = p_gap_statistic, 
    p_NbClust = p_NbClust,
    p_all = ggarrange(p_elbow, p_silhouette, p_gap_statistic, p_NbClust, nrow = 2, ncol = 2)
  ))
}

# Visualize silhouhette information
method_calculate <- function(data, names, centers = 2, print.summary = FALSE) {
  data_limited <- data[, names]
  
  set.seed(42)
  km.res <- kmeans(data_limited, centers, nstart = 25)
  sil <- silhouette(km.res$cluster, dist(data_limited))
  
  p_s <- fviz_silhouette(sil, print.summary = print.summary)
  p_c <- fviz_cluster(km.res, geom = "point", data = data_limited) + ggtitle(paste('k =', centers))
  
  data_with_clusters <- bind_cols(data_limited, cluster = as.factor(as.numeric(km.res$cluster)))
  
  return(list(
    km = km.res,
    cluster = km.res$cluster,
    data = data_with_clusters,
    p_s = p_s,
    p_c = p_c,
    p_sc = ggarrange(p_s, p_c, ncol = 2)
  ))
}

method_study <- function(filter_data, names, centers = 7) {
  m_num <- method_number(filter_data, names, centers)
  m_calc <- method_calculate(filter_data, names, centers)
  
  return(list(
    plot = ggarrange(m_num$p_elbow, m_num$p_silhouette, m_num$p_gap_statistic, m_num$p_NbClust, 
              m_calc$p_s, m_calc$p_c, 
              nrow = 2, ncol = 3),
    num = m_num,
    calc = m_calc
  ))
}

## Columns names ----
names_gdp <- c('penergy', 'pmining', 'pmetal', 'pmanufac', 'pbuilding', 'pagric', 'ptextile', 'ppharmac', 'pdurab', 'pinterind', 'pinindother', 'pothwhole', 'pretail_food', 'pretail_nonf', 'pretail_other')
names_people <- c('pmale', 'pob', 'pforeign', 'pobgrowth', 'unemprate')
names_ind <- c('ind_wholesale', 'ind_retail', 'ind_rest', 'ind_turis', 'ind_actindex')
names_rich <- c('adsl', 'pcars', 'pbanks')
names_all <- c(names_gdp, names_people, names_ind, names_rich)
names_max_sd <- c('pob', 'ind_actindex', 'pbanks', 'ptextile', 'pothwhole', 'pretail_other')

##Pre process ----
# Barcelona and Madrid are outliers
model_ <- preProcess(data, method = c("center", "scale"))
data_center <- predict(model_, data) %>%
  filter(!provincia %in% c('Madrid', 'Barcelon')) %>% 
  select(-provincia)

# Trial and error
method_study(data_center, names_all, 5)$plot #0.14
method_study(data_center, names_gdp, 3)$plot #0.21
method_study(data_center, names_people, 4)$plot #0.36
method_study(data_center, names_ind, 3)$plot #0.60
method_study(data_center, names_rich, 7)$plot #0.33
method_study(data_center, names_all, 2)$plot #0.27
method_study(data_center, names_max_sd, 6)$plot #0.26

## Using RF for variable importance ----
# Recommendation by Jesús

num_clusters <- 5
t <- method_calculate(data_center, names_all, num_clusters)

# Variable importance to explain the kmeans
set.seed(42)
model <- caret::train(cluster ~ ., data = t$data, method = "rf", tuneLength = 40)
confusionMatrix(data = predict(model, t$data), reference = t$data$cluster)

names_sorted <- varImp(model) %>% 
  .$importance %>% 
  rownames_to_column(var = 'name') %>% 
  arrange(desc(Overall)) %>% 
  top_n(5, Overall)

method_study(data_center, names_sorted$name, 3)$plot #10 0.26
method_study(data_center, names_sorted$name, 3)$plot #15 0.20
method_study(data_center, names_sorted$name, 3)$plot #5 0.27

## Using PCA for variable importance ----

# https://stackoverflow.com/questions/26529659/variable-selection-for-k-means-clustering
#Variable selection
model_ <- preProcess(data_center, method = c("pca"), thresh = 1)
names_sorted <- apply(model_$rotation, 1, function(x) sum(x))
names_sorted <- data.frame(names = names(names_sorted), value = as.numeric(names_sorted)) %>%
  dplyr::arrange(desc(value))

# Explained variance
# https://stats.stackexchange.com/questions/254592/calculating-pca-variance-explained/254598
model_ <- prcomp(data_center[, names_all], center = TRUE, scale. = TRUE)
names_sorted <- model_$rotation[, 'PC1']
names_sorted <- data.frame(names = names(names_sorted), value = as.numeric(abs(names_sorted))) %>%
  dplyr::arrange(desc(value))

plot_cluster_selection_methods(data_center, names_sorted$names[1:13], 9) #0.86
plot_cluster_selection_methods(data_center, names_all, 9) #0.86
plot_sillhouette(data_center, names_sorted$names[1:13], 4)

## Using the centers ----

s <- plot_sillhouette(data_center, names_all, 5)
s1 <- apply(s$km$centers, 2, function(x)sum(abs(x)))
names_sorted <- data.frame(names = names(s1), value = as.numeric(s1)) %>%
  dplyr::arrange(desc(value))
plot_cluster_selection_methods(data_center, names_sorted$names[1:13], 9) #0.86
plot_sillhouette(data_center, names_sorted$names[1:13], 9)