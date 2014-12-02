## ===================================================
## Obtaining the predictions of a probabilistic classifier
## taking into account a cost-benefit matrix
## L. Torgo, Nov 2014
## ---------------------------------------------------
metacostClassif <- function(preds, cb.matrix) {
    if (missing(cb.matrix)) stop("MetaCost requires that you specify a cost-benefit matrix.",call.=FALSE)
    if (is.null(dim(preds))) stop("MetaCost requires that the classifier outputs a matrix of probabilities as predictions.",call.=FALSE)
    if (ncol(preds) != ncol(cb.matrix)) stop("Error in MetaCost: predictions do not contain as many class probabilities as there are classes in the cost-benefit matrix.",call.=FALSE)

    ps <- apply(preds,1,
                function(ps) which.max(apply(cb.matrix,2,
                                             function(cs) sum(ps*cs))))
    factor(colnames(preds)[ps],levels=colnames(preds))
}
            
 
