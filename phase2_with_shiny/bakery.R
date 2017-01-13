#market basket analysis
#load library
library("arules")
library("arulesViz")


# 1.data cleaning and maniputations
#read as dataframe
df <- read.csv("files/1000-out1.csv", header = FALSE, sep = ";")
 
#check dataframe structure
str(df)
dim(df)

#check any duplicated or missing value
any(duplicated(df))
sum(is.na(df))
               
#since the dataset already in basket format with first column as a ordered unique 
#identifier for each transaction and 2nd column is the set of  items bought in 
#that transaction all the items bought at the same time in one row which needed 
#in finiteding association rules. we read dataset as trasanction
               
#2.read dataset as transaction
tran1 <- read.transactions("files/1000-out1.csv", format ="basket",  sep = ",", rm.duplicates = TRUE, cols=1)

#create vector for item names according item ID
myLabels <- c("Chocolate Cake","Lemon Cake", "Almond Tart", "Apple Pie","Apple Tart",
              "Apricot Tart", "Berry Tart", "Blackberry Tart", "Blueberry Tart","Chocolate Tart", 
              "Cherry Tart", "Lemon Tart","Casino Cake","Pecan Tart", "Ganache Cookie",
              "Gongolais Cookie", "Raspberry Cookie", "Lemon Cookie","Chocolate Meringue","Vanilla Meringue",
              "Marzipan Cookie", "Tuile Cookie", "Walnut Cookie","Opera Cake","Almond Croissant", 
              "Apple Croissant", "Apricot Croissant", "Cheese Croissant", "Chocolate Croissant","Apricot Danish",
              "Apple Danish","Almond Twist","Almond Bear Claw", "Blueberry Danish",  "Strawberry Cake",
              "Lemon Lemonade", "Raspberry Lemonade","Orange Juice","Green Tea","Bottled Water",
              "Hot Coffee","Chocolate Coffee","Vanilla Frappuccino", "Cherry Soda", "Single Espresso",
              "Truffle Cake", "Chocolate Eclair", "Coffee Eclair", "Vanilla Eclair", "Napoleon Cake")

myLevel1 <- c(0:49)

#label item name to tran1
itemInfo(tran1) <- data.frame(labels = myLabels)

#view transaction data in dataframe
Results = as(tran1, "data.frame")

#inspect the item label correctness
inspect(aggregate(tran1, itemInfo(tran1)[["labels"]])) 
 
#view summary
summary(tran1) 
str(tran1) 

#inspect basket items for all transaction
inspect(tran1) 

#inspect basket items for 3 transaction
inspect(tran1[1:3]) 

#Graph to display top 5 items 
#Frequency of item appeared in each transaction
itemFrequencyPlot(tran1, topN = 5, support = 0) 

#Graph item Frequency with min. support 10% 
#Item that appear more than (1000 * 0.1 = 100) times.
itemFrequencyPlot(tran1, support= 0.1)

#find association rule with default setting
tran1rules0 <- apriori(tran1) 
 
#view total number of rules output from default setting
tran1rules0
               
#run the apriori algorithm on the transactions by modify the default setting to
#support(0.01)
#confidence(0.5)
#min length of rules(2)
tran1rules <- apriori(
                tran1,
                parameter = list(
                  sup = 0.01, 
                  conf = 0.5, 
                  minlen=2,
                  target="rules")
                )

#inspect rules find interesting rule with diff supp and conf until  
inspect(sort(tran1rules, by="lift"))

#after inspect many parameter we found this interesting rule and we decide to choose this parameter
tran1rules <- apriori(
  tran1,
  parameter = list(
    sup = 0.03, 
    conf = 0.9, 
    minlen=2,
    target="rules")
)

#view total number of rules output from modified setting
tran1rules
 
#view summary tran1rules
summary(tran1rules)
               
#Print the association rules 
inspect(tran1rules)
a = as((tran1rules),"data.frame")
View(a["rules"])
#view the sorted high chances rules item purchased by "lift" in order to find the high support & high confidence rule 
inspect(sort(tran1rules, by="lift"))

               
#Plot a few graphs that can help to visualize the rules
plot(tran1rules)
plot(tran1rules,"graph")
plot(tran1rules,"grouped")
plot(tran1rules, method = "grouped", control = list(k = 5))
plot(tran1rules, method="graph", control=list(type="items"))
plot(tran1rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
 

#3.Pruning Redundant Rules
subset.matrix <- is.subset(tran1rules, tran1rules)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >=1

#show redundant rules
which(redundant)

#remove redundant rules
rules.pruned <- tran1rules[!redundant]

#the most likely rules sort by confidence, support , lift  by executing the following code. to show client
conf <-sort(rules.pruned, by="confidence", decreasing=TRUE)
inspect(conf)
supp <-sort(rules.pruned, by="support", decreasing=TRUE)
inspect(supp)
inspect(sort(rules.pruned, by="lift"))

#plot rules with redundant rules removed
plot(rules.pruned) 
plot(rules.pruned,"graph") 
plot(rules.pruned,"grouped") 
plot(rules.pruned, method = "grouped", control = list(k = 5)) 
plot(rules.pruned, method="graph", control=list(type="items")) 
plot(rules.pruned, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
 

