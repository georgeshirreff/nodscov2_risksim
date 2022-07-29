# devtools::install_github("vqv/ggbiplot")
library(ggbiplot)
library(tidyverse)
library(factoextra)
library(cluster)
library(car)
library(nlnet)
library(installr)

dr_mat <- read_csv("output/daily_risk.csv")

tib_forpca <- dr_mat %>% 
  pivot_wider(id_cols = c("newID"), names_from = c("this_status", "that_status"), names_sep = "-"
                    , values_from = c("daily_risk"))
  
cluster3 <- function(this_tib){
  this_mat <- as.matrix(this_tib %>% ungroup %>% select_if(is.numeric) %>% mutate_all(~replace_na(.x, 0)))
  rownames(this_mat) <- this_tib$newID
  
  this_k3 <- kmeans(scale(this_mat), centers = 3, nstart = 25)
  this_p3 <- fviz_cluster(this_k3, data = this_mat, ggtheme = theme_bw(), main = "K-means with 3 clusters"
                          , labelsize = 10, show.clust.cent = F, repel = T)
  this_p3
}

# cluster3(tib_forpca) #linear 
cluster3(tib_forpca %>% mutate_if(is.numeric, function(x) sqrt(x))) #sqrt transformed


ggsave("output/Fig_clustering.pdf", device = "pdf"
       , width = 20, height = 15, units = "cm")


  