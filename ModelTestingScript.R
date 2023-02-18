library(ctv)
library(tidyverse)
library(RWsearch)
library(tm)
library(cranly)
library(tools)
library(nnet)
library(dplyr)
crandb = crandb_down()

getTaskViewNames <- function(){
    taskViews <-  available.views()
    
    taskViewNames <- c()
    for(i in seq(length(taskViews))){
      taskViewNames <- append(taskViewNames, unlist(taskViews[i])[1])
    }
    return(taskViewNames)
}

getDesc <- function(tv, coreCheck){
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

gatherDescFiles <- function(){
  allDescCore <- data.frame(taskView = c(), packages = c(), description = c())
  for(i in taskViewNames){
    allDescCore <- rbind(allDescCore, getDesc(i, FALSE))
  }
  return(allDescCore)
}

textCleaning <- function(){
  vec <- gatherDescFiles()$description
  corpus <- Corpus(VectorSource(vec)) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(gsub, pattern = "-", replacement = " ") %>% 
    tm_map(removeNumbers) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeWords, stopwords("english")) %>% 
    tm_map(stemDocument) %>% 
    tm_map(stripWhitespace)
    dtm <- DocumentTermMatrix(corpus)
    dtm <- removeSparseTerms(dtm, 0.95)
    finalDataSet <- as.data.frame(as.matrix(dtm))
    for(i in seq(length(finalDataSet[1]))){
      for(j in seq(length(finalDataSet[1,]))){
        tf <- finalDataSet[i,j]/sum(finalDataSet[i,] != 0)
        itf <- log2(length(finalDataSet[1])/sum(finalDataSet[,j] != 0))
        finalDataSet[i,j] = tf * itf
      }
    }
    return(finalDataSet)
}

initialiseCranly <- function(){
  p_db <- tools::CRAN_package_db()
  package_db <- clean_CRAN_db()
  package_network <- build_network(package_db)
  return(package_network)
}

dependenceIndex <- function(object, tv){
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

makeDependenceIndex <- function(){
  options(warn = -1)  
  tempdf <- data.frame()
  for(i in allDescCore$packages){
    dependenceIndexVec <- c()
    depTree <- build_dependence_tree(package_network, package = i)
    for(j in taskViewNames){
      dependenceIndexVec = append(dependenceIndexVec, dependenceIndex(depTree, j))
    }
    tempdf <- rbind(tempdf, dependenceIndexVec)
  }
  options(warn = 0)
  return(tempdf)
}

#remember to bind finalDataSet and tempdf + adding list of classes

generateModel <- function(){
  sample_size <- floor(0.6 * nrow(finalDataSet))
  set.seed(777)
  
  # randomly split data in r
  picked <- sample(seq_len(nrow(finalDataSet)),size = sample_size)
  train <- finalDataSet[picked,]
  test <- finalDataSet[-picked,]
  
  finalDataSet$taskView <- relevel(factor(finalDataSet$taskView), ref = "Agriculture")
  model <- multinom(taskView ~ ., data = train, MaxNWts = 25000, maxit = 5000)
  return(model)
}


taskViewNames <- getTaskViewNames()
allDescCore <- gatherDescFiles()
finalDataSet <- textCleaning()
tempdf <- makeDependenceIndex()
finalDataSet <- cbind(finalDataSet, tempdf)
finalDataSet$taskView <- allDescCore$taskView



