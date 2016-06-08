library(C50)
library(mlbench)
library(class)
library(caret)
library(MASS)

### C5.0
### BANKNOTE AUTHENTICATION DATASET

Banknote = read.csv("C:/dane/banknote.txt", header=TRUE, sep=",")
Banknote <- data.frame(Banknote)
Banknote <- na.omit(Banknote)
Banknote$Class <- as.factor(Banknote$Class)
table(Banknote$Class)

data <- Banknote
set.seed(5)

k = 10
accuracies <- vector('numeric')
data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k

for (i in 1:k)
{
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  ptm <- proc.time()
  tree <- C5.0(Class~.,data=trainingset, trials = 10)
  predtree <- predict(tree,testset)
  fold_accuracy <- mean(predtree == testset$Class)
  proc.time() - ptm
  accuracies <- append(accuracies, fold_accuracy)
}
plot(tree, main = 'C5.0 - Banknote authentication', margin = 0.1)
full <- mean(accuracies)

# ACCURACY
print(full)
# MATRIX
table(testset$Class, Predicted = predtree)
# TIME
proc.time() - ptm

### WALL FOLLOWING ROBOT NAVIGATION DATASET

wfrn = read.csv("C:/dane/wfrn.data.txt", header=TRUE, sep=",")
wfrn <- data.frame(wfrn)
wfrn <- na.omit(wfrn)
wfrn$Class <- as.factor(wfrn$Class)
table(wfrn$Class)
data <- wfrn

k = 10
accuracies <- vector('numeric')
data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k

for (i in 1:k)
{
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  ptm1 <- proc.time()
  tree <- C5.0(Class~.,data=trainingset, trials = 10)
  predtree <- predict(tree,testset)
  fold_accuracy <- mean(predtree == testset$Class)
  proc.time() - ptm1
  accuracies <- append(accuracies, fold_accuracy)
}
plot(tree, main = 'C5.0 - WFRN', margin = 0.1)

# ACCURACY
print(full)
# MATRIX
table(testset$Class, Predicted = predtree)
# TIME
proc.time() - ptm1

### SOYBEAN DATASET

data(Soybean)
Soybean <- data.frame(Soybean)
Soybean <- na.omit(Soybean)
Soybean$Id <- NULL
Soybean$Class = as.factor(Soybean$Class)
x = as.matrix(Soybean[,names(Soybean) != "Class"])
mode(x) = "numeric"
Soybean[,names(Soybean) != "Class"] <-x
data <- Soybean

k = 10
accuracies <- vector('numeric')
data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k

for (i in 1:k)
{
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  ptm2 <- proc.time()
  tree <- C5.0(Class~.,data=trainingset, trials = 10)
  predtree <- predict(tree,testset)
  fold_accuracy <- mean(predtree == testset$Class)
  proc.time() - ptm2
  accuracies <- append(accuracies, fold_accuracy)
}
plot(tree, main = 'C5.0 - Soybean', margin = 0.1)
full <- mean(accuracies)

# ACCURACY
print(full)
# MATRIX
table(testset$Class, Predicted = predtree)
# TIME
proc.time() - ptm2

### CART
library(rpart)
library(plyr)
library(class)
library(caret)
library(MASS)
library(mlbench)

# BANKNOTE AUTHENTICATION DATASET

Banknote = read.csv("C:/dane/banknote.txt", header=TRUE, sep=",")
Banknote <- data.frame(Banknote)
Banknote <- na.omit(Banknote)
Banknote$Class <- as.factor(Banknote$Class)
data <- Banknote

k = 10
agfc <- vector("numeric")
agnpc <- vector("numeric")
aginifc <- vector("numeric")
aginipc <- vector("numeric")

data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k
for (i in 1:k)
{
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  #Gain
  tree <- rpart(Class~., data = trainingset, parms = list(split = "information"))
  if(i == 5)
  {
    plot(tree, uniform=TRUE,
         main="Information Gain for Banknote (full tree)",margin=0.1)
    text(tree, use.n=TRUE, all=TRUE, cex=.8)
  }
  ptm <- proc.time()
  pred.tree <- predict(tree,testset,type = "class")
  fa <- mean(pred.tree == testset$Class)
  proc.time() - ptm
  agfc <- append(agfc, fa)
  #Gini
  tree <- rpart(Class~., data = trainingset, parms = list(split = "gini"))
  if(i == 5)
  {
    plot(tree, uniform=TRUE,
         main="Gini for Banknote(full tree)",margin=0.1)
    text(tree, use.n=TRUE, all=TRUE, cex=.8)
  }
  ptm1 <- proc.time()
  pred.tree <- predict(tree,testset,type = "class")
  fa <- mean(pred.tree == testset$Class)
  proc.time() - ptm1
  aginifc<- append(agfc, fa)
}
gainFullBanknoteAccuracy <- mean(agfc)
giniFullBanknoteAccuracy <- mean(agfc)

data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k
for (i in 1:k)
{
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  #Gain
  tree <- rpart(Class~., data = trainingset, parms = list(split = "information"))

  cp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
  pruned_tree <- prune(tree, cp)
  if(i == 5)
  {
    plot(pruned_tree, uniform=TRUE,
         main="Information Gain for Banknote(pruned tree)",margin=0.1)
    text(pruned_tree, use.n=TRUE, all=TRUE, cex=.8)
  }
  ptm2 <- proc.time()
  pred.tree <- predict(pruned_tree,testset,type = "class")
  fa <- mean(pred.tree == testset$Class)
  proc.time() - ptm2
  agnpc <- append(agnpc, fa)
  #Gini
  tree <- rpart(Class~., data = trainingset, parms = list(split = "gini"))

  cp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
  pruned_tree <- prune(tree, cp)
  if(i == 5)
  {
    plot(pruned_tree, uniform=TRUE,
         main="Gini for Banknote(pruned tree)",margin=0.1)
    text(pruned_tree, use.n=TRUE, all=TRUE, cex=.8)
  }
  ptm3 <- proc.time()
  pred.tree <- predict(pruned_tree,testset,type = "class")
  fa <- mean(pred.tree == testset$Class)
  proc.time() - ptm3
  aginipc <- append(aginipc, fa)
}
gainPrunedBanknoteAccuracy <- mean(agnpc)
giniPrunedBanknoteAccuracy <- mean(aginipc)

### FULL TREE ACCURACY
print(gainFullBanknoteAccuracy)
print(giniFullBanknoteAccuracy)

### PRUNED TREE ACCURACY
print(gainPrunedBanknoteAccuracy)
print(giniPrunedBanknoteAccuracy)

# WALL FOLLOWING ROBOT NAVIGATION DATASET

wfrn = read.csv("C:/dane/wfrn.data.txt", header=TRUE, sep=",")
wfrn <- data.frame(wfrn)
wfrn <- na.omit(wfrn)
wfrn$Class <- as.factor(wfrn$Class)
data <- wfrn

k = 10
agfc <- vector("numeric")
agnpc <- vector("numeric")
aginifc <- vector("numeric")
aginipc <- vector("numeric")

data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k
for (i in 1:k)
{
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  #Gain
  tree <- rpart(Class~., data = trainingset, parms = list(split = "information"))

  if(i == 5)
  {
    plot(tree, uniform=TRUE,
         main="Information Gain for wfrn (full tree)",margin=0.1)
    text(tree, use.n=TRUE, all=TRUE, cex=.8)
  }
  ptm4 <- proc.time()
  pred.tree <- predict(tree,testset,type = "class")
  fa <- mean(pred.tree == testset$Class)
  proc.time() - ptm4
  agfc <- append(agfc, fa)
  #Gini
  tree <- rpart(Class~., data = trainingset, parms = list(split = "gini"))

  if(i == 5)
  {
    plot(tree, uniform=TRUE,
         main="Gini for wfrn(full tree)",margin=0.1)
    text(tree, use.n=TRUE, all=TRUE, cex=.8)
  }
  ptm5 <- proc.time()
  pred.tree <- predict(tree,testset,type = "class")
  fa <- mean(pred.tree == testset$Class)
  proc.time() - ptm5
  aginifc<- append(agfc, fa)
}
gainFullwfrnAccuracy <- mean(agfc)
giniFullwfrnAccuracy <- mean(agfc)

data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k
for (i in 1:k)
{
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  #Gain
  tree <- rpart(Class~., data = trainingset, parms = list(split = "information"))

  cp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
  pruned_tree <- prune(tree, cp)
  if(i == 5)
  {
    plot(pruned_tree, uniform=TRUE,
         main="Information Gain for wfrn(pruned tree)",margin=0.1)
    text(pruned_tree, use.n=TRUE, all=TRUE, cex=.8)
  }
  ptm6 <- proc.time()
  pred.tree <- predict(pruned_tree,testset,type = "class")
  fa <- mean(pred.tree == testset$Class)
  proc.time() - ptm6
  agnpc <- append(agnpc, fa)
  #Gini
  tree <- rpart(Class~., data = trainingset, parms = list(split = "gini"))

  cp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
  pruned_tree <- prune(tree, cp)
  if(i == 5)
  {
    plot(pruned_tree, uniform=TRUE,
         main="Gini for wfrn(pruned tree)",margin=0.1)
    text(pruned_tree, use.n=TRUE, all=TRUE, cex=.8)
  }
  ptm7 <- proc.time()
  pred.tree <- predict(pruned_tree,testset,type = "class")
  fa <- mean(pred.tree == testset$Class)
  proc.time() - ptm7
  aginipc <- append(aginipc, fa)
}
gainPrunedwfrnAccuracy <- mean(agnpc)
giniPrunedwfrnAccuracy <- mean(aginipc)

### FULL TREE ACCURACY
print(gainFullwfrnAccuracy)
print(giniFullwfrnAccuracy)

### PRUNED TREE ACCURACY
print(gainPrunedwfrnAccuracy)
print(giniPrunedwfrnAccuracy)

### SOYBEAN DATASET

data(Soybean)
Soybean <- data.frame(Soybean)
Soybean <- na.omit(Soybean)
Soybean$Id <- NULL
data <- Soybean

k = 10

agfc <- vector("numeric")
agnpc <- vector("numeric")
aginifc <- vector("numeric")
aginipc <- vector("numeric")

data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k

progress.bar <- create_progress_bar("text")
progress.bar$init(k)

for (i in 1:k)
{
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  #Gain
  tree <- rpart(Class~., data = trainingset, parms = list(split = "information"))
  if(i == 5)
  {
    plot(tree, uniform=TRUE,
         main="Information Gain for Soybean (full tree)",margin=0.1)
    text(tree, use.n=TRUE, all=TRUE, cex=.8)
  }
  ptm8 <- proc.time()
  pred.tree <- predict(tree,testset,type = "class")
  fa <- mean(pred.tree == testset$Class)
  proc.time() - ptm8
  agfc <- append(agfc, fa)
  #Gini
  tree <- rpart(Class~., data = trainingset, parms = list(split = "gini"))

  if(i == 5)
  {
    plot(tree, uniform=TRUE,
         main="Gini for Soybean(full tree)",margin=0.1)
    text(tree, use.n=TRUE, all=TRUE, cex=.8)
  }
  ptm9 <- proc.time()
  pred.tree <- predict(tree,testset,type = "class")
  fa <- mean(pred.tree == testset$Class)
  proc.time() - ptm9
  aginifc<- append(agfc, fa)
}
gainFullSoybeanAccuracy <- mean(agfc)
giniFullSoybeanAccuracy <- mean(agfc)

####################

data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k
for (i in 1:k)
{
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  #Gain
  tree <- rpart(Class~., data = trainingset, parms = list(split = "information"))

  cp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
  pruned_tree <- prune(tree, cp)
  if(i == 5)
  {
    plot(pruned_tree, uniform=TRUE,
         main="Information Gain for Soybean(pruned tree)",margin=0.1)
    text(pruned_tree, use.n=TRUE, all=TRUE, cex=.8)
  }
  ptm10 <- proc.time()
  pred.tree <- predict(pruned_tree,testset,type = "class")
  fa <- mean(pred.tree == testset$Class)
  proc.time() - ptm10
  agnpc <- append(agnpc, fa)
  #Gini
  tree <- rpart(Class~., data = trainingset, parms = list(split = "gini"))

  cp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
  pruned_tree <- prune(tree, cp)
  if(i == 5)
  {
    plot(pruned_tree, uniform=TRUE,
         main="Gini for Soybean(pruned tree)",margin=0.1)
    text(pruned_tree, use.n=TRUE, all=TRUE, cex=.8)
  }
  ptm11 <- proc.time()
  pred.tree <- predict(pruned_tree,testset,type = "class")
  fa <- mean(pred.tree == testset$Class)
  proc.time() - ptm11
  aginipc <- append(aginipc, fa)
}
gainPrunedSoybeanAccuracy <- mean(agnpc)
giniPrunedSoybeanAccuracy <- mean(aginipc)

### FULL TREE ACCURACY
print(gainFullSoybeanAccuracy)
print(giniFullSoybeanAccuracy)

### PRUNED TREE ACCURACY
print(gainPrunedSoybeanAccuracy)
print(giniPrunedSoybeanAccuracy)

# TIMES
proc.time() - ptm
proc.time() - ptm1
proc.time() - ptm2
proc.time() - ptm3
proc.time() - ptm4
proc.time() - ptm5
proc.time() - ptm6
proc.time() - ptm7
proc.time() - ptm8
proc.time() - ptm9
proc.time() - ptm10
proc.time() - ptm11
