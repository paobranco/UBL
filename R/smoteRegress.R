
# ===================================================
# Learning a regression model using a SMOTE for regression
# resampling strategy.
#
# L. Torgo, Jun 2008
# P. Branco, Mar 2015
# ---------------------------------------------------
smoteRegress <- function(form,
                         data,
                         rel="auto",
                         thr.rel=0.5,
                         perc.over=2,
                         k=5,
                         perc.under=2,
                         repl=FALSE
)
  
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # rel is relevance determined automatically (default) with uba package or provided by the user
  # thr.rel is the relevance threshold above which a case is considered
  #             as belonging to the rare "class"
  # per.over is the number of new cases (smoted cases) generated
  #              for each rare case. If perc.over < 1 a single case
  #              is generated uniquely for a randomly selected perc.over
  #              of the rare cases  
  # k is the number of neighbours to consider as the pool from where
  #             the new generated examples are generated
  # perc.under is the number of "normal" cases that are randomly
  #              selected for each smoted case
  # repl is it allowed to perform sampling with replacement

{
  
  if(rel=="auto"){
    require(uba)
    y <- resp(form,data)
    pc <- phi.control(y, method="extremes")  
    both <- all(pc$control.pts[c(2,8)] == c(1,1))
    y.relev <- phi(y,pc)
  } else{ #TODO: handle other relevance functions
    
  }
  
  # the column where the target variable is
  tgt <- target.col(form,data)
    
  if (both) {  # we have both low and high extrs
    rare.low <- which(y.relev > thr.rel & y < pc$control.pts[4])
    smote.exsL <- smote.exsRegress(data[rare.low,],tgt,perc.over,k)
    rare.high <- which(y.relev > thr.rel & y > pc$control.pts[4])
    smote.exsH <- smote.exsRegress(data[rare.high,],tgt,perc.over,k)
    rare.cases <- c(rare.low,rare.high)
    smoted.exs <- rbind(smote.exsL,smote.exsH)
  } else {
    
    # the indexes of the cases with rare target variable values
#    rare.cases <- if (pc$control.pts[2] == 1)  which(y.relev > thr.rel & y < pc$control.pts[4]) else which(y.relev > thr.rel & y > pc$control.pts[4])
    rare.cases <- which(y.relev >thr.rel)
    # Get the smoted examples associated with these rare cases
    smoted.exs <- smote.exsRegress(data[rare.cases,],tgt,perc.over,k)
  }
  
  # get the undersample of the "majority class" examples
  sel.maj <- sample((1:NROW(data))[-rare.cases],
                    as.integer((perc.under)*(nrow(smoted.exs)+length(rare.cases))),
                    replace=repl)
  
  # the final data set (the undersample+the rare cases+the smoted exs)
  newdataset <- rbind(data[sel.maj,],data[rare.cases,],smoted.exs)
  
}



# ===================================================
# Obtain a set of smoted examples for a set of rare cases.
#
# L. Torgo, Jun 2008
# P.Branco, Mar 2015
# ---------------------------------------------------
smote.exsRegress <- function(T,tgt,N,k)
  # INPUTS:
  # T are the rare cases (the minority "class" cases)
  # tgt the column nr of the target variable
  # N is the percentage of over-sampling to carry out;
  # and k is the number of nearest neighours
  # OUTPUTS:
  # The result of the function is a N*T set of generate
  # examples with rare values on the target
{
  p <- dim(T)[2]
  
  nomattrs <- c()
  lvls <- list()
  for(x in (1:p)[-tgt])
    if (!is.numeric(T[1,x])) {
      nomattrs <- c(nomattrs,x)
      lvls[[as.character(x)]] <- list(l=levels(T[,x]),n=nlevels(T[,x]))
    }
  ranges <- rep(1,p)
  for(x in (1:p)[-c(nomattrs,tgt)]) ranges[x] <- max(T[,x]) - min(T[,x])
  
  if (N < 1) { # only a percentage of the T cases will be SMOTEd
    nT <- NROW(T)
    idx <- sample(1:nT,as.integer(N*nT))
    T <- T[idx,]
    N <- 1
  }
  
  for(x in nomattrs) T[,x] <- as.integer(T[,x])
  T <- as.matrix(T)
  
  nT <- dim(T)[1]
  
  # todo: check if this is what we really want
  nexs <-  as.integer(N) # this is the number of artificial exs generated
  # for each member of T
  
  new <- matrix(nrow=nexs*nT,ncol=p)    # the new cases
  
  for(i in 1:nT) {
    
    # the k NNs of case T[i,]
    xd <- scale(T,T[i,],ranges)
    for(a in nomattrs) xd[,a] <- xd[,a]!=0
    dd <- drop(xd[,-tgt]^2 %*% rep(1, ncol(xd)-1))
    kNNs <- order(dd)[2:(k+1)]
        
    for(n in 1:nexs) {
      # select randomly one of the k NNs
      neig <- sample(1:k,1)
      
      
      # the attribute values of the generated case
      idx <- (i-1)*nexs+n 
      difs <- T[kNNs[neig],-tgt]-T[i,-tgt]
      new[idx,-tgt] <- T[i,-tgt]+runif(1)*difs
      for(a in nomattrs)
        new[idx,a] <- c(T[kNNs[neig],a],T[i,a])[1+round(runif(1),0)]
      
      # now the target value (weighted (by inverse distance) average)
      d1 <- d2 <- 0
      for(x in (1:p)[-c(nomattrs,tgt)]) {
        d1 <- abs(T[i,x] - new[idx,x])/ranges[x]
        d2 <- abs(T[kNNs[neig],x] - new[idx,x])/ranges[x]
      }
      if (length(nomattrs)) {
        d1 <- d1 + sum(T[i,nomattrs] != new[idx,nomattrs])
        d2 <- d2 + sum(T[kNNs[neig],nomattrs] != new[idx,nomattrs])
      }
      # (d2+d1-d1 = d2 and d2+d1-d2 = d1) the more distant the less weight
      new[idx,tgt] <- if (d1 == d2) (T[i,tgt]+T[kNNs[neig],tgt])/2 else (d2*T[i,tgt]+d1*T[kNNs[neig],tgt])/(d1+d2)
      
    }
  }
  
  newCases <- data.frame(new)
  for(a in nomattrs)
    newCases[,a] <- factor(newCases[,a],levels=1:lvls[[as.character(a)]]$n,labels=lvls[[as.character(a)]]$l)
  
  colnames(newCases) <- colnames(T)
  newCases
  
}



