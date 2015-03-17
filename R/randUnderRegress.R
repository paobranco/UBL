
# ===================================================
# Learning a model using a random undersampling strategy,
# which basically removes cases of the negative
# "class" until a certain percentage of the positive "class"
# examples is reached
# L. Torgo, Jun 2008
# P. Branco, Mar 2015
# ---------------------------------------------------
RandUnderRegress <- function(form, data, rel="auto", thr.rel=0.5, perc.under=1, repl=FALSE)
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # rel is relevance determined automatically (default) with uba package or provided by the user
  # thr.rel is the relevance threshold above which a case is considered
  #         as belonging to the rare "class"
  # perc.under is the number of normal cases that are randomly
  #                selected for each rare case
  # repl is it allowed to perform sampling with replacement

{
  if(rel="auto"){
  require(uba)
  y <- resp(form,data)
  pc <- phi.control(y, method="extremes")
  both <- all(pc$control.pts[c(2,8)] == c(1,1))
  y.relev <- phi(y,pc)
  } else{ 
    # TODO: handle other relevance functions
    
  }
  # the indexes of the cases with rare target variable values
  rare.cases <- which(y.relev > thr.rel)
  
  
  # start by adding all the cases of the minority class
  newdata <- data[rare.cases,]
  
  # test if it is possible to under-sample with the required conditions
  if(perc.under*length(rare.cases)>nrow(data[,-rare.cases])){
    if(repl){
      warning("Not performing under-sampling!")
    } else{
    stop("The conditions set can not be satisfied!")
    }
  }
  
  # get the undersample of the "majority class" examples
  sel.maj <- sample((1:NROW(data))[-rare.cases],
                    as.integer((perc.under)*nrow(newdata)),
                    replace=repl)
  
  newdata <- rbind(newdata,data[sel.maj,])
  newdata
}


  