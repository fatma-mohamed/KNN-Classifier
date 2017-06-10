euclideanDistance <- function(instance1, instance2 , cols){
  distance <- 0
  for (c in 1:cols){
    distance <- (distance + (as.double(instance1[c])-as.double(instance2[c]))^2)
  }
  return (sqrt(distance))
}

findNearestNeighbours <- function(trainData, testInstance, k){
  distances <- matrix(ncol = 2)
  trainSize <- nrow(trainData)
  cols <- (length(testInstance)-1)
  classCol <- length(testInstance)
  for (i in 1:trainSize){
    d <- euclideanDistance(trainData[i,], testInstance, cols)
    class <- as.character(trainData[i,classCol])
    if(i==1){
      distances <- c(class,d)}
    else{
      distances <- rbind(distances, c(class,d))}
  }
  distances <- distances[order(as.double(distances[,2]),decreasing = FALSE),]
  neighbours <- c()
  for(l in 1:k){
    neighbours <- rbind(neighbours, distances[l,])
  }
  return (neighbours)
}

getClassLabel <- function(neighbours){
  votes <- matrix(ncol = 2)
  neighboursSize <- nrow(neighbours)
  for(i in 1:neighboursSize){
    index <- match(neighbours[i,1],votes)
    if(i==1)
      votes <- rbind(c(neighbours[1,1],1))
    else{
      if(is.na(index))
        votes <- rbind(votes, c(neighbours[i,1],1))
      else
        votes[index,2] <- strtoi(votes[index,2]) +1
    }
  }
  if(nrow(votes)>1){
    votes <- votes[order(strtoi(votes[,2]),decreasing = TRUE),]
    #check for tie
    if(votes[1,1]==votes[2,1]){
      if(match(votes[1,1],neighbours)<match(votes[2,1],neighbours))
        return (votes[1,1])
      else
        return (votes[2,1])
    }
  }
  else
    return (votes[1,1])
}

getAccuracy <- function(classifiedInstances, total){
  return (classifiedInstances/total)
}

knnAlgorithm <- function() {
  trainData <- as.matrix(read.table("TrainData.txt",sep=',',header=FALSE))
  testData <- as.matrix(read.table("TestData.txt",sep=',',header=FALSE))
  total <- nrow(testData)
  classCol <- ncol(testData)
  for(k in 1:9){
    cat("k value: ", k, "\n")
    correct_count <- 0
    for (i in 1:nrow(testData)){
      actual_class <- as.character(testData[i,classCol])
      predicted_class <- getClassLabel(findNearestNeighbours(trainData,testData[i,],k))
     # cat("Predicted class: ", predicted_class , ", Actual class: ", actual_class, "\n")
      if(identical(predicted_class,actual_class))
        correct_count <- (correct_count + 1)
    }
    cat("Number of correctly classified instances: ", correct_count , "\n")
    cat("Total number of instances: ", total, "\n")
    cat("Accuracy: ", getAccuracy(correct_count, total), "\n")
    cat("-------------------------------------------------\n")
  }
}
