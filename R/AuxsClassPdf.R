# auxiliary functions for conditional density estimation
# Author: MMRAU
# chnages: P.Branco Dez 2016

# assumes the first column in the data set contains the target variable

generate_dataset <- function(thresh, train_data, cv_data){
  
#  require(randomForest);
  #generate a single new training dataset
  #thresh: threshold for lower bound also used
  #        to identify the corresponding model
  #cv_data: cross validation dataset
  #starts at the right border of the first bin

  train_data[train_data[,1]<=thresh,1] <- 0;
  train_data[train_data[,1]>thresh,1] <- 1;

  train_data[,1] <- factor(train_data[,1]);
  model <- randomForest(train_data[,-1], train_data[,1]);
  pred_cv <- predict(model, cv_data, type = 'prob');
  #only return the "0" class because the other one is just 1 - p
  return(pred_cv[,1]);
}


generate_bi_probs <- function(bin_vec, train_data, cv_data){

  #generate all bipoint problems for the cv dataset
  #using the elements of thresh_vec generate all binary problems
  #bin_vec: vector of bin sequences
  #train_data: training data
  #cv_data: crossvalidation data
  #RETURNS:
  #        class conditional probabilities from the cv_data


  binary_probs <- matrix(ncol = length(bin_vec) - 2, nrow = length(cv_data[,1]));
print("Begin models training")
  for(i in 2:(length(bin_vec)-1)){
    if(i==round((length(bin_vec)-1)*0.25))print("25% models trained")
    if(i==round((length(bin_vec)-1)*0.5))print("50% models trained")
    if(i==round((length(bin_vec)-1)*0.75))print("75% models trained")
    binary_probs[,i-1] <- generate_dataset(bin_vec[i], train_data, cv_data);
  }
  print("End models training")

  #rbord is left border from 2 to len-1  so smaller has to increase
  #which leads to isotonic regression or chernochukov rearrangement

  xleft <- 2:(length(bin_vec) - 1);

  for(i in 1:length(binary_probs[,1])){
    mon_fnkt <- isoreg(xleft, binary_probs[i,]);
    binary_probs[i,] <- mon_fnkt$yf;
  }

  #translate into class probabilities easier if smaller than
  #i can do this collectively

  binary_probs <- 1.0 - binary_probs;

  #in principle superflous but consistent with notation paper Frank
  ret_probs <- matrix(ncol = length(bin_vec) - 1, nrow = length(cv_data[,1]));
  ret_probs[,1] <- 1.0 - binary_probs[,1];
  for(i in 2:(length(bin_vec) - 2) ){
      ret_probs[,i] <- binary_probs[,i - 1] - binary_probs[,i];
    }

  ret_probs[,(length(bin_vec) - 1)] <- binary_probs[,(length(bin_vec) - 2)];
  #the last element stays the same
  return(ret_probs);
}



calc_weights <- function(cv_class, numel, train_el_class){
  train_weights <- vector(length = length(train_el_class));
  for(j in 1:length(numel)){
    train_weights[train_el_class == j] <- length(train_el_class) * cv_class[j] / numel[j];
  }
  return(train_weights);
}


