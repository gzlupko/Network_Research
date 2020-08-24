# Classification of organizational network 


rm(list = ls())
getwd()
setwd("/Users/gianzlupko/Desktop/GitHub/NetworkResearch") 


# load libraries 

library(tidyverse)
library(igraphdata)
library(igraph)
library(tidygraph) 
library(ggraph) 
library(gridExtra) 
library(broom)
library(intergraph)



# import data set 


data(enron)
head(enron)

V(enron)
vertex_attr(enron)
class(enron)
enron 



# convert enron igraph object to data frame

df <- asDF(enron)
head(enron)

# create data frame for nodes
nodes <- df$vertexes
head(nodes)

# remove intergraph_id

nodes <- nodes %>%
  select(-intergraph_id) 


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

# example plot by centrality measures for the sake of discussion 

library(threejs) 
data(UKfaculty) 

plot(UKfaculty, layout = layout_nicely(UKfaculty), edge.arrow.size = 0.1, 
     vertex.size = 1, edge.color = "gray77")  

graphjs(UKfaculty, vertex.size = 1) 

UKfaculty_between <- betweenness(UKfaculty, directed  = TRUE) 


faculty_in_degree <- degree(UKfaculty, mode = c("in")) 
plot(UKfaculty, vertex.label = NA, vertex.size = sqrt(faculty_in_degree) + 1, 
     edge.arrow.size = 0.05, layout = layout_nicely(UKfaculty))


# visualize betweenness
# custom function to reduce betweenness to 1/100th of original value

between <- node_measures$between
reduction <- function (x) { 
  x * (1/100)
  }
plot(UKfaculty, vertex.label = NA, vertex.size = reduction(between), 
     edge.arrow.size = 0.05)

  


# visualize constraint
# use Fast-Greedy to incidate network subgroups


data(karate) 
plot(karate) 
karate_constraint <- constraint(karate)
karate_2 <- set_vertex_attr(karate, "constraint", value = constraint(karate))
vertex_attr(karate_2)

# identify subgroups with fast greedy method and plot 
karate_subgroups <- fastgreedy.community(karate)
plot(karate_subgroups, karate) 

# identify individuals who benefit from structural hole network position
# higher constraint scores typically indicate lower brokerage 

V(karate_2)[[constraint <= 0.20]]

V(karate_2)$color <- ifelse(
  
  V(karate_2)$constraint < 0.20, "red", "white" 
)

plot(karate_2) 


  
# organize enron data set into JobLevel 

unique(node_measures$Note)

# create JobLevel column and sort alphabetically


nodes_with_levels <- node_measures %>%
  mutate(JobLevel = Note) %>%
  arrange(JobLevel)

unique(nodes_with_levels$Note) 
View(nodes_with_levels) 

# simplify Job Levels 

nodes_with_levels$JobLevel[1:5] <- "CEO"
nodes_with_levels$JobLevel[6:19] <- "Director"
nodes_with_levels$JobLevel[20:60] <- "Employee"
nodes_with_levels$JobLevel[61] <- "Executive"
nodes_with_levels$JobLevel[62:84] <- "Manager"
nodes_with_levels$JobLevel[85:138] <- "NA" 
nodes_with_levels$JobLevel[139:143] <- "President"
nodes_with_levels$JobLevel[144:154] <- "Trader" 
nodes_with_levels$JobLevel[155:184] <- "Vice President"
  
# 9 job levels on a relatively small data 

# visualize counts by job level 


nodes_with_levels %>%
  group_by(JobLevel) %>%
  count(JobLevel) %>%
  ggplot(aes(x = JobLevel, y = n, fill = JobLevel)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_blank()) + 
  ylab("") 


  

# further clean up. Break into 4 layers - 
# employee, associate, director, executive
# Put lawyer in executive and arbitrarily assign NA and Trader to 'associate'


nodes_with_levels <- nodes_with_levels %>%
  mutate(JobLevel = ifelse (JobLevel == "CEO", "Executive", JobLevel)) %>%
  mutate(JobLevel = ifelse (JobLevel == "Director", "Executive", JobLevel)) %>%
  mutate(JobLevel = ifelse (JobLevel == "Vice President", "Executive", JobLevel)) %>%
  mutate(JobLevel = ifelse (JobLevel == "President", "Executive", JobLevel)) %>%
  mutate(JobLevel = ifelse (JobLevel == "Trader", "Associate", JobLevel)) %>%
  mutate(JobLevel = ifelse (JobLevel == "NA", "Associate", JobLevel)) 
  

table(nodes_with_levels$JobLevel) 


# visualize new job levels to be used for analyses 

nodes_with_levels %>%
  group_by(JobLevel) %>%
  count(JobLevel) %>%
  ggplot(aes(JobLevel, n, fill = JobLevel)) + geom_bar(stat = "identity") +
  ylab("") + theme(legend.position = "none") 
  

# start by breaking into  4 job levels
# arbitrarily assigning Trader and NA to level 2 just above employee

nodes_filtered <- nodes_with_levels %>%
  select(-c(Email, Name, Note)) 



# rearrange order of columns to put target variable in first column
nodes_filtered <- nodes_filtered[, c(5,1,2,3,4)]
head(nodes_filtered)




# EDA of network structure characteristics by job level 

View(nodes_filtered)
table(nodes_filtered$JobLevel)

level_summary <- nodes_filtered %>%
  group_by(JobLevel) %>%
  summarize(avg_in_degree = mean(in_degree), 
            avg_out_degree = mean(out_degree), 
            avg_strength = mean(strength), 
            avg_between = mean(between)) 

head(level_summary)
avg_in_degree <- ggplot(data = level_summary, 
                        aes(x = JobLevel, y = avg_in_degree, fill = JobLevel)) + 
  geom_bar(stat = "identity") + ylab("") + theme(legend.position = "none") + 
  ggtitle("Avg. In-Degree") 

avg_out_degree <- ggplot(data = level_summary, 
                        aes(x = JobLevel, y = avg_out_degree, fill = JobLevel)) + 
  geom_bar(stat = "identity") + ylab("") + theme(legend.position = "none") + 
  ggtitle("Avg. Out-Degree")

avg_strength <- ggplot(data = level_summary, 
                        aes(x = JobLevel, y = avg_strength, fill = JobLevel)) + 
  geom_bar(stat = "identity") + ylab("") + theme(legend.position = "none") + 
  ggtitle("Avg. Strength")

avg_between <- ggplot(data = level_summary, 
                        aes(x = JobLevel, y = avg_between, fill = JobLevel)) + 
  geom_bar(stat = "identity") + ylab("") + theme(legend.position = "none") +
  ggtitle("Avg. Betweenness") 


grid.arrange(avg_in_degree, avg_out_degree, avg_strength, avg_between)

# check for statistical significance

head(nodes_filtered)
lm(between ~ JobLevel, data = nodes_filtered) %>%
  tidy()
lm(in_degree ~ JobLevel, data = nodes_filtered) %>%
  tidy()
lm(out_degree ~ JobLevel, data = nodes_filtered) %>%
  tidy()
lm(strength~ JobLevel, data = nodes_filtered) %>%
  tidy()




# descriptive analyses on the enron network


# network density = proportion of edges that exist in a network out of the total 
# potential number that could exist



# kNN - can model classify job levels by centrality measures in an organization? 



head(nodes_filtered)
str(nodes_filtered)



set.seed(801) 


# randomize rows to mix up Job Level

shuffle <- runif(nrow(nodes_filtered)) 
nodes_filtered<- nodes_filtered[order(shuffle), ]
str(nodes_filtered) 
head(nodes_filtered) 


# normalize functinon to be applied to predictors 
normalize <- function(x) { 
  return( (x-min(x)) / (max(x) - min(x) ) ) } 


# apply normalize to nodes data set 
nodes_norm <- as.data.frame(lapply(nodes_filtered[,c(2,3,4,5)], normalize))

# check that the variable ranges have been normalized 
str(nodes_norm)
summary(nodes_norm)



# determine data splits and create train and test data sets 

184 * 0.8
184 - 147

nodes_train <- nodes_norm[1:147, ]
nodes_test <- nodes_norm[148:184, ]




# create vector of target category, Job Level 
nodes_train_target <- nodes_filtered$JobLevel[1:147]
nodes_test_target <- nodes_filtered$JobLevel[148:184]



# load class to use kNN algorithm and determine appropriate k-value 

library(class) 
sqrt(184) 


# run kNN and generate confusion matrix 

pred <- knn(train = nodes_train, test = nodes_test, cl = nodes_train_target, k = 13) 
tb <- table(nodes_test_target, pred)
tb
library(caret) 
confusionMatrix(tb)





# visualizations 

# ggraph(enron, layout = "with_kk") + geom_edge_link() + geom_node_point()

# ggraph(enron, layout = "with_kk") + geom_edge_link() + geom_node_point() 

# - - - - - - - - - - - 



# Updates to make:

# draw on (Ballinger, Cross, Holtom, 2015) - 
# A) calculate eigenvector scores 
# B) break job levels into: 
# director level and above = 1, below = 0 
# could run kNN again with logistic regression as alternative





# create custom function to add eigen centrality scores in dplyr pipe 

eigen_calc <- function(x) { 
  
  score = eigen_centrality(enron, directed = TRUE)
  as.numeric(score$vector)
  }

knn_two <- nodes_filtered %>%
  mutate(eigen = eigen_calc(enron)) 


# view eigen centrality scores in descending order 

knn_two %>%
  arrange(desc(eigen)) %>%
  head(10)

# run kNN again on Job Level with eigen and in_degree centrality scores 

knn_two <- nodes_filtered %>%
  select(-c(out_degree, strength, between)) 


head(knn_two)

# Pearson correlation: in_degree and eignen vector centrality

cor(knn_two$eigen, knn_two$in_degree)




# run kNN algorithm implementation steps as above but with 
# in_degree and eigen vector as predictors 




set.seed(807) 


# randomize rows to mix up Job Level

shuffle_two <- runif(nrow(knn_two)) 
knn_shuffled <- knn_two[order(shuffle_two), ]

normalize <- function(x) { 
  return( (x-min(x)) / (max(x) - min(x) ) ) } 
norm_two <- as.data.frame(lapply(knn_two[,c(2,3)], normalize))

train_two <- norm_two[1:147, ]
test_two <- norm_two[148:184, ]


train_target_two <- knn_shuffled$JobLevel[1:147]
test_target_two <- knn_shuffled$JobLevel[148:184]

library(class) 


# run kNN and generate confusion matrix 

pred_two <- knn(train = train_two, test = test_two, cl = train_target_two, k = 13) 
tb_two <- table(test_target_two, pred_two)
tb_two
library(caret) 
confusionMatrix(tb_two)



# kNN unable to accurately classify Job Level based on in-degree and eigen scores 
# calculate network constraint and assign scores to nodes 


enron_constraint<- constraint(enron)
head(enron_constraint) 

nodes_filtered <- nodes_filtered %>%
  mutate(constraint = constraint(enron)) 
head(nodes_filtered) 




# re-run kNN with three centrality measures tested in research 
# in-degree, eigenvector, and constraint 

knn_three <- nodes_filtered %>%
  select(-c(strength, out_degree, between)) 
head(knn_three) 

set.seed(8104) 

# randomize rows to mix up Job Level

shuffle_three <- runif(nrow(knn_three)) 
knn_shuffled_three <- knn_three[order(shuffle_three), ]

normalize <- function(x) { 
  return( (x-min(x)) / (max(x) - min(x) ) ) } 
norm_three <- as.data.frame(lapply(knn_three[,c(2,3,4)], normalize))

train_three <- norm_three[1:147, ]
test_three <- norm_three[148:184, ]


train_target_three <- knn_shuffled_three$JobLevel[1:147]
test_target_three <- knn_shuffled_three$JobLevel[148:184]

library(class) 


# run kNN and generate confusion matrix 

pred_three <- knn(train = train_three, test = test_three, cl = train_target_three, k = 13) 
tb_three <- table(test_target_three, pred_three)
tb_three
library(caret) 
confusionMatrix(tb_three)

# update job levels to match research study - directors + are executives 
# and below are non-executives

knn_three %>%
  count(JobLevel) 


head(knn_three) 
knn_four <- knn_three %>%
  mutate(Level = ifelse(JobLevel ==  "Executive", "Executive", "Non-Executive")) %>%
  select(-JobLevel)
head(knn_four) 

# re-order columns 
knn_four <- knn_four[, c(4,1,2,3)]
head(knn_four) 



# run kNN again with only two job levels 

knn_four$Level <- as.factor(knn_four$Level) 
class(knn_four$Level)

set.seed(7003) 

shuffle_four <- runif(nrow(knn_four)) 
knn_shuffled_four <- knn_four[order(shuffle_four), ]

normalize <- function(x) { 
  return( (x-min(x)) / (max(x) - min(x) ) ) } 

norm_four <- as.data.frame(lapply(knn_four[,c(2,3,4)], normalize))

train_four <- norm_four[1:147, ]
test_four <- norm_four[148:184, ]

train_target_four <- knn_shuffled_four$Level[1:147]
test_target_four <- knn_shuffled_four$Level[148:184]

library(class) 

pred_four <- knn(train = train_four, test = test_four, cl = train_target_four, k = 13) 
tb_four <- table(test_target_four, pred_four)
tb_four
library(caret) 
confusionMatrix(tb_four) 




# logistic regression 








