# Classification of organizational network 


getwd()
setwd("/Users/gianzlupko/Desktop/GitHub/NetworkResearch") 


# load libraries 

library(igraphdata)
library(igraph)
library(tidygraph) 
library(ggraph) 


# import data set 


data(enron)
head(enron)

V(enron)
vertex_attr(enron)
class(enron)
enron 



# convert enron igraph object to data frame 
df <- as_data_frame(enron, what = c("both")) 


# create data frame for nodes
nodes <- df$vertices



# calculate centrality measures on igraph object 
# store as vertex attributes 
# betweennes an indicator of nformational and relationship brokerage in a network 

node_measures <- nodes %>%
  mutate(in_degree = degree(enron, mode = c("in")), 
         out_degree = degree(enron, mode = c("out")), 
         strength = strength(enron), 
         between = betweenness(enron, directed = TRUE)) %>%
  arrange(desc(strength)) 

str(node_measures) 
View(node_measures)




# organize enron data set into JobLevel 


table(node_measures$Note)






# visualizing network centrality characteristics 







