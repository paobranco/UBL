##############################################################################
# Script for conditional density estimation of a given test set
# The main function classPdf provides this estimation by calling other 
# auxiliary functions
# Author: MMRAU
# Changes: P.Branco Dez 2016
##############################################################################


getPDFinRange <- function(y.true, test, train, form){
  # y.true:  all possible values considered for prediction
  # test:    the test set
  # train    the training set
  # form:    the problem formula 
  
  tgt <- which(names(train) == as.character(form[[2]]))
  
  cleanTest <- test[,-tgt]
  weightMat <- classPdf(tgt, train, cleanTest, y.true)
  Ltest <- nrow(test)

  res <- matrix(NA, ncol=length(y.true), nrow=Ltest)
  for(i in 1:nrow(test)){ 
    dd <- approxfun(density(train[,tgt], weights = weightMat[i,])$x,
                    density(train[,tgt], weights = weightMat[i,])$y)
    res[i,] <- dd(y.true)
  }
  res
}



classPdf <- function(tgt, train, test, y.true){
  # input:
  #   tgt      the column where the target variable is
  #   train    the training set
  #   test     the test set without the target variable
  #   y.true   the sequence of equally distant values to consider
  # output: 
  #   a weight matrix with the weights of each example in the test set provided
  
  cv <- test

  # change tgt column to the first column
  if (tgt > 1) {
    orig.order <- colnames(train)
    cols <- 1:ncol(train)
    cols[c(1, tgt)] <- cols[c(tgt, 1)]
    train <- train[, cols]
  }
  
  train_class <- train;
  
  bins_class <- y.true

  #bin the response of train:
  print("Binning train set")
  num_el_bin <- vector(length = length(bins_class) - 1);
  for(i in 1:(length(bins_class) - 1)){
    #print(i);
    index_tr <- train[,1] > bins_class[i] & train[,1] <= bins_class[i + 1]
    num_el_bin[i] <- sum(index_tr);
    train_class[index_tr,1] <- i;
  }
  
  train_class[,1] <- as.factor(train_class[,1]);
  
  print('End binning')
  
  
  cv_class <- generate_bi_probs(bins_class, train, cv);
    weight_matrix <- matrix(nrow = length(cv[,1]), ncol = length(train[,1]));
    
    for(i in 1:length(weight_matrix[,1])){
      weight_matrix[i,] <- calc_weights(cv_class[i,], num_el_bin, train_class[,1])
    }
    
    #eliminate positive elements due to numerical errors by setting them to 0
    weight_matrix[weight_matrix < 0]  <- 0;
    #now normalize this in order to let it sum to one
    
    for(i in 1:length(weight_matrix[,1])){
      weight_matrix[i,] <- weight_matrix[i,]/sum(weight_matrix[i,])
    }

    # reorder data if target was not originally in the first column 
    if (tgt > 1) {
      train <- train[,cols]
    }
    return(weight_matrix)
  
}

