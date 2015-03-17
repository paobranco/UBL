
# ===================================================
# Learning a model using a random oversampling strategy,
# which basically adds existing cases (copies) of the positive
# "class" examples until a certain percentage of the negative "class"
# examples is reached
# P. Branco, Mar 2015
# ---------------------------------------------------

RandOverRegress <- function(form, data, rel="auto", thr.rel=0.5, perc.over=1)
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # rel is relevance determined automatically (default) with uba package or provided by the user
  # thr.rel is the relevance threshold above which a case is considered
  #         as belonging to the rare "class"
  # perc.over is the number of "normal" cases that are randomly
  #                selected for each rare case

  
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
  norm.cases <- which(y.relev <= thr.rel)
  
  
  # test if it is possible to over-sample with the required conditions
  if(perc.over*length(norm.cases)-length(rare.cases)<=0){
      stop("The conditions set can not be satisfied!")
  }
  
     
    sel.maj <- sample(rare.cases,
                      as.integer((perc.over)*length(norm.cases)-(length(rare.cases))),
                      replace=TRUE)


  newdata <- rbind(data,data[sel.maj,])
  newdata
  
}