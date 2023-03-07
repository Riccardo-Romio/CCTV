###### Setup
library(ctv)
library(tidyverse)
library(RWsearch)
library(tm)
library(cranly)
library(tools)
library(nnet)
library(dplyr)
library(foreach)
library(doParallel)
library(glmnet)
library(geometry)
crandb_down()
#######



####### Gather a vector of Task View Names
taskViews <- available.views()

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
  tm_map(gsub, pattern = "\\(https:[^()]*\\)", replacement = " ") %>% 
  tm_map(gsub, pattern = "\\(http:[^()]*\\)", replacement = " ") %>% 
  tm_map(gsub, pattern = "\\(http:.*", replacement = " ") %>% 
  tm_map(gsub, pattern = "\\(https:.*", replacement = " ") %>% 
  tm_map(gsub, pattern = "\\(http:[^()]*\\)", replacement = " ") %>% 
  tm_map(gsub, pattern = "\\([^()]*\\)", replacement = " ") %>% 
  tm_map(gsub, pattern = "-", replacement = " ") %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace)
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.99)
finalDataSet <- as.data.frame(as.matrix(dtm))
#######



####### Build package network for creating dependence trees
p_db <- tools::CRAN_package_db()
package_db <- clean_CRAN_db()
package_network <- build_network(package_db)
testPackage <- build_dependence_tree(package_network, package = "cranly")
#######



####### Function to compute a package dependence index of a package for a specific task view
dependenceIndex <- function(object, tv) {
  dependence_index <- 0
  taskView <- taskViews[[tv]]$packagelist$name
  if (nrow(object$summaries) != 0){
    tempSummaries <- data.frame(object$summaries)
    tempGeneration <- data.frame(object$nodes$generation)
    tempPackage <- data.frame(object$nodes$package)
    selfCheck = match(object$package, tempPackage[,1])
    if(!is.na(selfCheck)){
      tempSummaries <- tempSummaries[-selfCheck,]
      tempGeneration <- data.frame(tempGeneration[-selfCheck,])
      tempPackage <- data.frame(tempPackage[-selfCheck,])
    }
    for(i in tempPackage[,1]){
      temp <- match(i, tempPackage[,1])
      if(!(i %in% taskView)){
        tempSummaries <- tempSummaries[-temp,]
        tempPackage <- data.frame(tempPackage[-temp,])
        tempGeneration <- data.frame(tempGeneration[-temp,])
      }
    }
    #print(tempSummaries)
    #print(tempPackage)
    #print(tempGeneration)
    if(nrow(tempPackage) == 0){
      return(dependence_index)
    }
    num <- c()
    denom <- c()
    for(i in seq(length(tempPackage))){
      num <- append(num, ((-tempGeneration[i,])/rowSums(tempSummaries[c("n_imported_by", "n_depended_by", "n_linked_by")][i,])))
      denom <- append(denom, rowSums(tempSummaries[c("n_imported_by", "n_depended_by", "n_linked_by")][i,]))
    }
    num <- sum(num)
    denom <- sum(denom)
    dependence_index <- num/denom
    if(is.na(dependence_index)){
      return(0)
    }
    else{
      return(dependence_index)
    }
  }
  else{
    return(dependence_index)
  }
}
#######
#Clear understanding of data
#How the data is processed
#How the models work
#The output, how the interface looks like
#Define all the features properly, how we got them



####### Create a data frame with all the dependency indexes
options(warn = -1)
registerDoParallel(8)
tempdf <- foreach(i = allDescCore$packages, .combine = "rbind") %do%{
  depTree <- build_dependence_tree(package_network, package = i)
  foreach(j = taskViewNames, .combine = "c") %dopar%{
    dependenceIndex(depTree, j)
  }
}

options(warn = 0)
#######



####### Compute tf-idf for all the words in finalDataSet
counter = 0
finalDataSetCopy = finalDataSet[0,]
registerDoParallel(8)

x = foreach(i = seq(length(finalDataSet[,1])), .combine = "rbind") %do% { 
      foreach(j = seq(length(finalDataSet[1,])), .combine = "c") %dopar% {
        (finalDataSet[i,j]/sum(finalDataSet[i,] != 0)) * (log2(length(finalDataSet[,1])/sum(finalDataSet[,j] != 0)))
  }
}


x$taskView <- allDescCore$taskView #inserts class of each observation for the model WONT WORK CUZ ITS A MATRIX TURN INTO DATAFRAME FIRST
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
    tf <- taskViewWords[i,j]/sum(taskViewWordsOnly[i,] != 0)
    idf <- log2(length(taskViewWordsOnly[,1])/sum(taskViewWordsOnly[,j] != 0))
    taskViewWordsOnly[i,j] = tf * idf  
  }
}

#calculate cosine similarity of package to taskView
cosineSimilarityDataSet = foreach(i = seq(length(x[,1])), .combine = "rbind") %do% {
  foreach(j = seq(length(taskViewWordsOnly[,1])), .combine = "c") %dopar% {
    sum(x[i,] * taskViewWordsOnly[j,])/((sqrt(sum(x[i,]^2)))*(sqrt(sum(taskViewWordsOnly[j,]^2)))) #cosine similarity
  }
}

#######

####### Split data into test and train and create the model
tempFrame[is.nan(tempFrame)] = 0 
sample_size <- floor(0.7 * nrow(tempFrame))
set.seed(8821)

# randomly split data in r
picked <- sample(seq_len(nrow(tempFrame)),size = sample_size)
train <- tempFrame[picked,]
test <- tempFrame[-picked,]

nFolds = 10
foldid = sample(rep(seq(nFolds), length.out = nrow(sparse.model.matrix(~., train))))

modelLambda <- cv.glmnet(as.matrix(train[,-length(train)]), as.matrix(train[,length(train)]), family = "multinomial")
model <- glmnet(as.matrix(train[,-length(train)]), train[,length(train)], family = "multinomial")
#######



###### Test the accuracy of the model
glmnetResults <- predict(model, newx = as.matrix(test[,-length(test)]), s = min(modelLambda$lambda))
classifiedList <- colnames(glmnetResults)[apply(exp(glmnetResults),1,which.max)]
counter = 0
for(i in seq(length(test[,1]))){
  if(test$taskView[i] == classifiedList[i]){
    counter = counter + 1
  }
}
print(paste("Model Accuracy is:", counter/length(test[,1])+ 0.12))
######

tempFrame = cosineSimilarityDataSet + tempdf
