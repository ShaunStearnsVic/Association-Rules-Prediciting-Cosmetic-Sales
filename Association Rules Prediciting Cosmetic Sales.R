library(arules)

setwd("~/Desktop/CSU Global Data Analytics/MIS510/BookMaterials/DMBA-R-datasets/")
Cosmetics.df <- read.csv("Cosmetics.csv")

#First six rows
head(Cosmetics.df)
#Limit decimals to 4 places
options(digits = 4)
#Dimension of the frame
dim(Cosmetics.df) 
#Print the list in a useful column format
t(t(names(Cosmetics.df)))
#Inspect dataset for missing values
data.frame(miss.val=sapply(Cosmetics.df, function(x) 
  sum(length(which(is.na(x))))))

#Convert First Column and Convert to Matrix
Cosmetics.mat <- as.matrix(Cosmetics.df[, -1])
#Convert Binary into Transactional Database
Cosmetics.trans <- as(Cosmetics.mat, "transactions")
inspect(head(Cosmetics.trans), n = 6)

#Get Rules = 0.1
rules <- apriori(Cosmetics.trans, parameter = list(supp = 0.1, conf = 0.1, target = "rules"))
#Inspect the first 6 rules
inspect(head(sort(rules, by = "lift"), n = 6))

#Get Rules = 0.5
rules <- apriori(Cosmetics.trans, parameter = list(supp = 0.1, conf = 0.5, target = "rules"))
#Inspect the first 6 rules
inspect(head(sort(rules, by = "lift"), n = 6))
