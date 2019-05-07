## ================================================================
## Adasyn over-sampling strategy extended for multi-class problems.
## P.Branco Jan 2017
## ================================================================
# # Examples:
# data(iris)
# dat <- iris[-c(45:75), c(1, 2, 5)]
# # checking the class distribution of this artificial data set
# table(dat$Species)
# newdata <- AdasynClassif(Species~., dat, beta=0.8)
# table(newdata$Species)
# beta <- list("setosa"=1, "versicolor"=0.5)
# newdata <- AdasynClassif(Species~., dat, baseClass="virginica", beta=beta)
# table(newdata$Species)

AdasynClassif <- function(form, dat, baseClass=NULL,
                          beta=1, dth=0.95,
                          k = 5, dist = "Euclidean", p = 2){
  # Args:
  # form      a model formula
  # dat       the original training set (with the unbalanced distribution)
  # baseClass Character specifying the reference class, i.e., the class from 
  #           which all other will be compared to. This is selected by the user,
  #           or estimated from the classes distribution in dat. If not defined,
  #           the default, the majority class is selected.
  # beta      a single numeric or a vector of numerics for defining the desired 
  #           balance level after synthetic examples generation. beta is in [0,1], 
  #           where 1 correspondes to fully balanced classes. If only one beta 
  #           value is provided, this is applied to all classes targeted with
  #           over-sampling. If a vector is supplied, these beta values are 
  #           consecutively applied to the classes with changed distribution. 
  #           Defaults to 1, which represents fully balanced  classes.
  # dth       a threshold for the maximum tolerated degree of class imbalance ratio.
  #           Defaults to 0.95, meaning that the strategy is applied if the 
  #           imbalance ratio is more than 5%.
  # k         is the number of neighbors to consider as the pool from where
  #           the new examples are generated
  # dist      is the distance measure to be used (defaults to "Euclidean")
  # p         is a parameter used when a p-norm is computed
  #
  # Returns: a new data frame modified through adasyn algorithm

  # the column where the target variable is
  tgt <- which(names(dat) == as.character(form[[2]]))
  names <- sort(unique(dat[, tgt]))
  li <- class.freq(dat, tgt)
  
  if(is.null(baseClass)){
      baseClass <- as.character(li[[1]][which.max(li[[2]])])
  }
  LbaseClass <- li[[2]][which(li[[1]] == baseClass)]
  
  d <- li[[2]]/LbaseClass
  dTgt <- which(d<dth)
  
  if(!is.numeric(beta) && !is.list(beta)){
    stop("ADASYN:: beta parameter parovided is not in the correct format.",
         call. = FALSE)
  }
  
  if(length(beta) != 1 && length(beta) != length(dTgt)){
    warning("Adasyn:: Number of values provided in beta parameter (", length(beta), " )
            do not match the number of classes where over-sampling will be
            applied (", length(dTgt), " ). \n
            Using only the first beta value provided.", call. = FALSE)
    beta <- beta[1]
  }
  
  
  if(is.list(beta)){
    tbeta <- c()
    for(nm in li[[1]]){
      if(!is.numeric(beta[[nm]])){
        tbeta <- c(tbeta, 0)
      } else {
      tbeta <- c(tbeta, beta[[nm]])
      }
    }
    li[[3]] <- tbeta
  } else if (is.numeric(beta)){
    li[[3]] <- rep(beta, length(li[[1]])) 
  }
  
  G <- (LbaseClass-li[[2]][dTgt])*li[[3]][dTgt]

  nomatr <- c()
  T <- matrix(nrow = dim(dat)[1], ncol = dim(dat)[2])
  for (col in seq.int(dim(T)[2])) { 
    if (class(dat[, col]) %in% c('factor', 'character')) {
      T[, col] <- as.integer(dat[, col])
      nomatr <- c(nomatr, col)
    } else {
      T[, col] <- dat[, col]
    }
  }
  nC <- dim(T)[2]
  nT <- dim(T)[1]
  
  # check if there is enough data to determine the k neighbors
  if (nT <= k) {
    stop("Trying to determine ",k, " neighbors for a subset with only ",
         nT, " examples")
  }
    
  kNNs <- neighbours(tgt, dat, dist, p, k)
  
  g <- list()
  # for each class in dTgt 
  for(i in 1:length(dTgt)){
    # for each example in class i
    # determine nr of baseClass examples in the KNN
    r <- c()
    for(ex in which(dat[, tgt] == li[[1]][dTgt[i]])){
      r <- c(r, length(which(dat[kNNs[ex,],tgt] == baseClass))/k)
    }
    # normalize
    if(sum(r) == 0){
      new.r <- rep(1/length(r), length(r))
    } else {
      new.r <- r/sum(r)
    }
    # for each ex generate g_i new examples
    g[[i]] <- round(new.r*G[i],0)
  
  }
  
  
  newCases <- matrix(nrow=sum(unlist(g)), ncol=nC) # the new cases generated
  count <- 1
  # generate the synthetic cases
  for(i in 1:length(dTgt)){
    vec <- which(dat[, tgt] == li[[1]][dTgt[i]])
    kNNs <- neighbours(tgt, dat[vec, ], dist, p, k)
    for(ex in vec){
      nr <- g[[i]][which(vec==ex)]
      if(nr>0){
        neig <- sample(1:k, nr, replace=TRUE)
        ex.neig <- vec[kNNs[which(vec==ex),neig]]
        difs <- T[ex.neig,]-T[ex,]
        newCases[count:(count+nr-1),] <- T[ex,]+runif(nr)*difs
        for (a in nomatr) {
          # nominal attributes are randomly selected among the existing values
          # of seed and the selected neighbor 
          newCases[count:(count+nr-1), a] <- c(T[ex.neig, a], 
                                           T[ex, a])[1 + round(runif(1), 0)]
        }
        count <- count+nr
      }
      
    }
    
  }
  
  newM <- data.frame(newCases)
  
  for (a in nomatr){
    newM[, a] <- factor(newM[, a],
                            levels = 1:nlevels(dat[, a]),
                            labels = levels(dat[, a]))
  }
  
  colnames(newM) <- colnames(dat)
  res <- rbind(dat, newM)
  
  res
  
}