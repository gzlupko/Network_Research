# Network_Research
In this notebook, I explore the differences in employee social network structures by job level. Specially, I am curious if network centrality measures like connectedness, reputation, and brokerage, are notably different by job level.

This analysis is inspired by recent empirical research showing a strong negative correlation between high network centrality and voluntary turnover (Cross, Ballinger, Holtom, 2015). That is, indvividuals who benefited from more ties in a network, and whose ties were connected with other influential ties in a firm, were less likely to leave the firm. The researchers in this study also found an interaction between job level and employee network structure. In particular, executives were found to occupy ties spanning structural holes more frequently - a position in a social network that affords the advantage of informational brokerage across otherwise disparate sub-groups.

I wanted to explore the interaction between job level and network structure further. I was also curious if a k-Nearest Neighbors algorithm would be an appropriate tool for analysis in a network context. The assumption I wanted to test in ths notebook is that network structure is different by job level. Below is the general outline for this notebook:

Data Cleaning
Visualizations
k-Nearest Neighbors
Logistic Regression
Discussion
References
The Enron email data set was used for this analysis. Due to computational constraints with regards to visualizations in my local environment, I use smaller network data sets to convey examples of centrality measures used. Job level was stored as a vertex attribute,'position', in the original Enron email data set and the edges represent emails sent to and from individuals throughout the firm.

For the analytics and documentation on this project's code, please follow the below link to my accompanying Kaggle notebook:

https://www.kaggle.com/gianzlupko/classifying-job-level-in-organizational-network



![alt text](https://github.com/gzlupko/Network_Research/blob/master/networkclassification.png)
