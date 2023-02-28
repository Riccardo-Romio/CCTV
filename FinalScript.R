###### Setup
library(ctv)
library(tidyverse)
library(RWsearch)
library(tm)
library(cranly)
library(tools)
library(nnet)
library(dplyr)
crandb_down()
#######



####### Gather a vector of Task View Names
taskViews <-  available.views()

taskViewNames <- c()
for(i in seq(length(taskViews))){
  taskViewNames <- append(taskViewNames, unlist(taskViews[i])[1])
}
#######



####### Function for gathering description files of a specific task view 
getDesc <-  function(tv, coreCheck){
  temp <- taskViews[[tv]]
  temp2 <- temp %>% 
    .$packagelist %>% 
    filter(core == coreCheck) %>% 
    .$name
  if(length(temp2) == 0){
    return(data.frame(taskView = c(), packages =  c(), description = c()))
  }
  descArr <- c()
  counter <- 0
  for(i in temp2){
    descTemp <- crandb %>% 
      filter(Package == i) %>% 
      .$Description
    if(length(descTemp) == 0){
      descTemp <- ""
    }
    descArr <- append(descArr, descTemp)
  }
  return(data.frame(taskView = tv, packages = temp2, description = descArr))
}
#######



####### Gather all description files for all packages under a task view
allDescCore <- data.frame(taskView = c(), packages = c(), description = c())
for(i in taskViewNames){
  allDescCore <- rbind(allDescCore, getDesc(i, FALSE))
}
#######



####### Text cleaning and removing sparsity, beginning to create the final dataset (First part of features)
vec <- allDescCore$description
corpus <- Corpus(VectorSource(vec)) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(gsub, pattern = "`r[^`]*`", replacement = " ") %>% 
  tm_map(gsub, pattern = "`[^`]*`", replacement = " ") %>% 
  tm_map(gsub, pattern = "\\(https:[^()]*\\)", replacement = "") %>% 
  tm_map(gsub, pattern = "\\(http:[^()]*\\)", replacement = "") %>% 
  tm_map(gsub, pattern = "\\(http:.*", replacement = "") %>% 
  tm_map(gsub, pattern = "\\(https:.*", replacement = "") %>% 
  tm_map(gsub, pattern = "\\(http:[^()]*\\)", replacement = "") %>% 
  tm_map(gsub, pattern = "\\([^()]*\\)", replacement = "") %>% 
  tm_map(gsub, pattern = "-", replacement = " ") %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace)
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.999)
finalDataSet <- as.data.frame(as.matrix(dtm))
#######



####### Build package network for creating dependence trees
p_db <- tools::CRAN_package_db()
package_db <- clean_CRAN_db()
package_network <- build_network(package_db)
#######



####### Function to compute a package dependence index of a package for a specific task view
dependenceIndex <- function(object, tv) {
  summ <- object$summaries
  dependence_index <- 0
  taskView <- taskViews[[tv]]$packagelist$name
  if (nrow(summ)) {
    tempSummaries <- data.frame(object$summaries)
    tempGeneration <- object$nodes$generation
    tempPackage <- object$nodes$package
    for(i in object$nodes$package){
      temp <- match(i, tempPackage)
      if(!(i %in% taskView)){
        tempSummaries <- tempSummaries[-temp,]
        tempPackage <- tempPackage[-temp]
        tempGeneration <- tempGeneration[-temp]
      }
    }
    w <- 1/colSums(tempSummaries[c("n_imported_by", "n_depended_by", "n_linked_by")])
    g <- tempGeneration
    ind <- names(w) != tempPackage
    dependence_index <- weighted.mean(-g[ind], w[ind]) - 1
    if(is.na(dependence_index)){
      return(0)
    }
    return(dependence_index)
  }
  return(0)
}
#######



####### Create a data frame with all the dependency indexes
options(warn = -1)  
tempdf <- data.frame()
for(i in allDescCore$packages){
  dependenceIndexVec <- c()
  depTree <- build_dependence_tree(package_network, package = i)
  for(j in taskViewNames){
    dependenceIndexVec <- append(dependenceIndexVec, dependenceIndex(depTree, j))
  }
  tempdf <- rbind(tempdf, dependenceIndexVec)
}

options(warn = 0)
#######



####### Compute tf-idf for all the words in finalDataSet
for(i in seq(length(finalDataSet[,1]))){
  for(j in seq(length(finalDataSet[1,]))){
    tf <- finalDataSet[j,i]/length(finalDataSet[1])
    idf <- log2(length(finalDataSet[1])/length(finalDataSet[,i] != 0))
    finalDataSet[j,i] <- tf * idf
  }
}
finalDataSet$taskView <- allDescCore$taskView #inserts class of each observation for the model
#######

####### Alternative method for calculating text similarity

#we want finalDataSet with just term frequency for now and appended taskView 
taskViewWords <- finalDataSet[0,-length(finalDataSet)]

for(i in unique(finalDataSet$taskView)){
  temp <- subset(finalDataSet, taskView == i)
  temp <- temp[, -length(temp)]
  temp <- colSums(temp)
  taskViewWords <- rbind(taskViewWords, temp)
}
taskViewWords$taskView = taskViewNames

taskViewWordsOnly <- taskViewWords[,-length(taskViewWords)]

#now we compute tf-idf of taskViewWords

for(i in seq(length(taskViewWordsOnly[,1]))){
  for(j in seq(length(taskViewWordsOnly[1,]))){
    tf <- taskViewWords[j,i]/length(taskViewWordsOnly[j,] != 0)
    idf <- log2(length(taskViewWordsOnly[1])/length(taskViewWordsOnly[,i] != 0))
    taskViewWordsOnly[j,i] = tf * idf  
  }
}

#calculate cosine similarity of package to taskView

for(i in seq(length(finalDataSet))){
  for(j in seq(length(taskViewWordsOnly))){
    
  }  
}

#######

####### Split data into test and train and create the model
sample_size <- floor(0.7 * nrow(finalDataSet))
set.seed(8821)

# randomly split data in r
picked <- sample(seq_len(nrow(finalDataSet)),size = sample_size)
train <- finalDataSet[picked,]
test <- finalDataSet[-picked,]

train$taskView <- relevel(as.factor(train$taskView), ref = "Agriculture")
test$taskView <- relevel(as.factor(test$taskView), ref = "Agriculture")
modelLambda <- cv.glmnet(as.matrix(train[,-length(train)]), train[,length(train)], family = "multinomial")
model <- glmnet(as.matrix(train[,-length(train)]), train[,length(train)], family = "multinomial")
#######



###### Test the accuracy of the model
glmnetResults <- predict(model, newx = as.matrix(test[,-length(test)]), s = min(modelLambda$lambda))
classifiedList <- colnames(glmnetResults)[apply(exp(glmnetResults),1,which.max)]
finalClassifiedDf <- data.frame(test$taskView, classifiedList)
classificationCounter <- c()
classificationOverview <- unique(finalClassifiedDf$test.taskView)
for(j in classificationOverview){
  counter <- 0
  temp = filter(finalClassifiedDf, finalClassifiedDf$test.taskView == j)
  for(i in seq(length(temp$test.taskView))){
    if(temp$test.taskView[i] == temp$classifiedList[i]){
      counter <- counter + 1 
    }
  }
  classificationCounter <- append(classificationCounter, (counter/length(temp$test.taskView)))
}
data.frame(classificationOverview, classificationCounter)

print(paste("Model Accuracy is:", (counter/length(finalClassifiedDf$test.taskView))))
######