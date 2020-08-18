# Classification of organizational network 


getwd()
setwd("/Users/gianzlupko/Desktop/GitHub/NetworkResearch") 


# load libraries 

library(igraphdata)
library(igraph)
library(tidygraph) 
library(ggraph) 
library(gridExtra) 


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
library(stringr)


unique(node_measures$Note)


# create JobLevel column and sort alphabetically

unique(nodes_with_levels$Note) 

nodes_with_levels <- node_measures %>%
  mutate(JobLevel = Note) %>%
  arrange(JobLevel)

# simplify Job Levels 

nodes_with_levels$JobLevel[1:5] <- "CEO"
nodes_with_levels$JobLevel[6:19] <- "Director"
nodes_with_levels$JobLevel[20:60] <- "Employee"
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

View(nodes_with_levels) 
nodes_with_levels$JobLevel[61] <- "Executive"
nodes_with_levels$JobLevel["Employee"] <- nodes_with_levels$JobLevel["Associate"]
nodes_with_levels$JobLevel["Director"] <- "Executive"

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





# kNN - can model classify job levels by centrality measures in an organization? 









