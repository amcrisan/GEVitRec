#Loading a storing gevitR information
#gevit_articles<-readxl::read_xlsx(path="../../gevit_gallery_v2/data/MasterDocumentList.xlsx")
#gevit_taxonomy<-readxl::read_xlsx(path="../../gevit_gallery_v2/data/figure_classification_final.xlsx")

library(dplyr)
library(tidyr)

gevit_summary<-gevit_taxonomy %>%
  dplyr::inner_join(gevit_articles[,c("PMID","YearPub")],by = "PMID") %>%
  mutate(chartType = stringr::str_split(chartType,";")) %>%
  tidyr::unnest() %>%
  select(PMID,YearPub,figID_initial,chartType,chartCombinations)

gevit_summary$dataSource<-sapply(gevit_summary$chartType, function(chart_type){
  switch(chart_type,
         "Phylogenetic Tree" = "phyloTree",
         "Table" = "table",
         "Bar Chart" = "table",
         "Genomic Map" = "dna",
         "Scatter Plot" = "table",
         "Line Chart" = "table",
         "Node-link" = "network",
         "Image" = "image",
         "Distribution Chart" = "table",
         "Category Stripe" = "table",
         "Geographic Map" = "spatial",
         "Heatmap" = "table",
         "Dendrogram" = "dendoTree",
         "Alignment" = "dna",
         "Timeline" = "temporal",
         "Pie Chart" = "table",
         "Density Plot" = "table",
         NA)
})

gevit_summary <- dplyr::filter(gevit_summary,!is.na(dataSource))

#recompute the chart scores, penalizing for time
#so that older visualizations take less precendence
weight_score<-gevit_summary %>%
  group_by(YearPub) %>%
  count()%>%
  arrange(desc(YearPub))

weight_score$yearScore = length(unique(gevit_summary$YearPub)):1
weight_score<-mutate(weight_score,yearScore = yearScore/weight_score$yearScore[1]) %>% ungroup()

#Now calculate the final chart weights
chart_scores<- gevit_summary %>%
  group_by(chartType,YearPub) %>% 
  count() %>%
  ungroup()

chart_scores<-lapply(unique(chart_scores$chartType),function(chart_type,weight_score){
  tmp2<-chart_scores %>% filter(chartType == chart_type)
  tmp2<-inner_join(tmp2,weight_score[,c("YearPub","yearScore")],by="YearPub")
  tmp2<-mutate(tmp2,n=n*yearScore) %>% select(-contains("yearScore"))
},weight_score = weight_score) %>% bind_rows()

chart_scores<- chart_scores %>%
  group_by(chartType)%>%
  summarise(score = sum(n)) %>%
  ungroup()%>%
  mutate(freq = score/sum(score)) %>%
  arrange(desc(freq))

#weighted score of chart importances
chart_scores<-chart_scores %>% mutate(rescale = ceiling((freq/chart_scores$freq[1])*10))

#add back the different data sources
datSrc<-gevit_summary %>% 
  select(chartType,dataSource) %>%
  distinct()

chart_scores<-inner_join(chart_scores,datSrc,by="chartType")

save(gevit_articles,gevit_taxonomy,chart_scores,file="R/sysdata.rda")
