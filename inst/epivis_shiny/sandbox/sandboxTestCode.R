devtools::load_all()
library(ggtree)
library(leaflet)

tstTree<-"~/Dropbox/Papers/gevitR/scratch/ana/testdata/tree_data/kleb-pneumo.nwk"
tstTable<-"~/Dropbox/Papers/gevitR/scratch/ana/testdata/table_data/kleb-pneumo-meta-data.csv"
tstTableEBOV<-"~/Dropbox/Papers/gevitR/scratch/ana/testdata/table_data/microreact-project-west-african-ebola-epidemic-data.csv"


#loading the data 
metadata<-input_data(tstTable,"table")
treedata<-input_data(tstTree,"tree")

ebova<-input_data(tstTableEBOV,"table")

inputData<-data.frame(internalID = c("thing1","thing2"),
                      dataType = c("Phylogenetic Tree", "Table"),
                      dataPath = c(treedata,metadata))


ggtree(treedata@data$tree) #how do you summarize this tree?



library(ggtree)
ggtree(treedata@data$tree) + geom_tiplab()
