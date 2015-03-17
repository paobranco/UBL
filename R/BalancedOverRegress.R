







if(both){ # we should produce replicas from the two extremes
  rare.casesH <- which(y.relev > thr.rel & y > pc$control.pts[4])
  rare.casesL <- which(y.relev > thr.rel & y < pc$control.pts[4])
  
  # get the replicas of the "minority class" examples
  sel.majH <- sample(rare.casesH,
                     as.integer((perc.over)*length(norm.cases)-(length(rare.cases)/2)),
                     replace=TRUE)
  
  sel.majL <- sample(rare.casesL,
                     as.integer((perc.over)*length(norm.cases)-(length(rare.cases)/2)),
                     replace=TRUE)
  
  sel.maj <- c(sel.majH, sel.majL)
} else{ # only one type of extreme
  
  sel.maj <- sample(rare.cases,
                    as.integer((perc.over)*length(norm.cases)-(length(rare.cases))),
                    replace=TRUE)
  
}
