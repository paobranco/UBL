metacost <- function(preds,cb.matrix,levels.tgt) {
    pars <- list(...)
    if (is.numeric(train[,tgtVar])) stop("MetaCost is only available for classification tasks.",call.=FALSE)
    if (missing(cb.matrix)) stop("MetaCost requires that you specify a cost-benefit matrix.",call.=FALSE)
    if (is.null(dim(preds))) stop("MetaCost requires that the classifier outputs a matrix of probabilities as predictions.",call.=FALSE)
    if (ncol(preds) != ncol(pars$cb.matrix)) stop("Error in MetaCost: predictions do not contain as many class probabilities as there are classes in the cost-benefit matrix.",call.=FALSE)
    ps <- apply(preds,1,function(ps) which.max(apply(pars$cb.matrix,2,function(cs) sum(ps*cs))))
    preds <- levels.tgt[ps]
}
            
 
