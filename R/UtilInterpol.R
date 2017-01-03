################################################################################
## This script includes a function to obtain an approximation of the utility
## surface using interpolation of points of the form (y, \hat(y), u(y, \hat{y})).
## The function allows to select between different interpolation methods.
## This function uses the control.parms obtained through the function phi to
## estimate the surface diagonal where y==\hat{y}. This means that we use the 
## relevance function for obtaining the points (y, y, U(y,y)). The off-diagonal
## points are provided by the user through a parameter.
## P.Branco Dec 2016
################################################################################

#example with Boston data set
# 
# data(Boston, package = "MASS")
# 
# tgt <- which(colnames(Boston) == "medv")
# sp <- sample(1:nrow(Boston), as.integer(0.7*nrow(Boston)))
# train <- Boston[sp,]
# test <- Boston[-sp,]
# 
# control.parms <- phi.control(Boston[,tgt],method="extremes",extr.type="both")
# # the boundaries of the domain considered
# minds <- min(Boston[,tgt])-5
# maxds <- max(Boston[,tgt])+5
# 
# # build m.pts to include at least the utility of the
# # points (minds, maxds) and (maxds, minds)
# m.pts <- matrix(c(minds, maxds, -1, maxds, minds, 0),
#                 byrow=TRUE, ncol=3)
# 
# trues <- test[,tgt]
# library(randomForest)
# model <- randomForest(medv~., train)
# preds <- predict(model, test)
# 
#  resLIN <- UtilInterpol(trues, preds, type="util", control.parms, minds, maxds, m.pts,
#                          method = "bilinear", visual=TRUE)
#  resIDW <- UtilInterpol(trues, preds, control.parms, minds, maxds, m.pts,
#                         method = "idw", visual=TRUE)
#  resSPL <- UtilInterpol(trues, preds, control.parms, minds, maxds, m.pts,
#                         method = "spl", visual=TRUE)
#  resKRIGE <- UtilInterpol(trues, preds, control.parms, minds, maxds, m.pts,
#                           method = "krige", visual=TRUE)

UtilInterpol <- function(trues, preds, type = c("utility", "cost", "benefit"), 
                         control.parms, minds, maxds, m.pts,
                         method = c("bilinear", "splines", "idw", "krige"),
                         visual = FALSE, eps = 0.1, full.output=FALSE){
# inputs:
  #   trues         a vector of true target variable values
  #   preds         a vector with predicted values for the trues provided
  #   type          character specifying the type of surface that is being interpolated
  #   control.parms are the control.parms also used to obtain function phi(). 
  #                 These parameters define the utility values when y == \hat{y}.
  #                 Allows both: user defined points and to obtain an estimate 
  #                 using the data distribution. The points relevance is in [0,1].
  #   minds         the lower bound of the target variable considered for interpolation
  #   maxds         the upper bound of the target variable considered for interpolation
  #   m.pts         a 3-column matrix with interpolating points for the cases 
  #                 where y != \hat{y}, provided by the user. The first column
  #                 has the y value, the second column the \hat{y} value and the
  #                 third column has the corresponding utility value. The domain
  #                 boundaries of (y, \hat{y}) must be provided.
  #  method         the selected interpolation method.
  #  visual         logical. If TRUE an image is shown with the utility isometrics
  #                 evaluation. If FALSE (the default) no image is plotted.
  #  eps            a value for the precision considered during the interpolation. 
  #                 Only relevant if a plot is displayed or when full.output is TRUE.
  # full.output     Logical. If FALSE (the default) only the results from points
  #                 (trues, preds) are returned. If TRUE a matrix with the utility
  #                 of all points in domain (considering the eps provided) are returned.
  # output:
  #   either a vector with the utility of predicting each point (trues, preds) provided
  #   when full.output is FALSE or a matrix with the utility of all points 
  #   considered on the domain when full.output is TRUE.
  

  type <- match.arg(type)
  method <- match.arg(method)
  
  # control.parms does not need to be specified if surface type is "cost"
  # however, if the user does specifies it, we use the user provided control.parms
  if(type == "cost" && is.null(control.parms)){
    amp <- (maxds-minds)/5
    seqD <- seq(minds, maxds, by = amp)
    c.pts <- c(rbind(seqD,rep(0, length(seqD)),rep(0, length(seqD))))
    control.parms <- list(method="range", npts = length(seqD), control.pts = c.pts)
  }
  
  # modify control.pts to match (y, \hat{y}, u(y, \hat{y}), store utility points in dat
    pts <- matrix(control.parms$control.pts, byrow=TRUE, ncol=3)
    dat <- matrix(NA, ncol=3, nrow=0)
    for (i in 1:control.parms$npts){
      dat <- rbind(dat, c(pts[i, 1], pts[i, 1], pts[i, 2])) 
    }
    
  # for now: limit the possibility of providing any points the user wants.
  # The m.pts points are required to be inside [minds, maxds] interval
  if (any(m.pts[,1:2]>maxds || m.pts[,1:2]<minds)){
    stop("UtilInterpol:: Parameter m.pts must only contain points in [minds, maxds] interval.",
         call. = FALSE)
  }
  
  # add m.pts
  dat <- rbind(dat, m.pts)
  
  # add diagonal extrapolation after checking that points are not in dat already
  ul <- as.numeric(c(minds, minds))
  if (!any(apply(dat, 1, function(x) identical(x[1:2], ul)))){ # ul is not in the matrix
    if(type != "cost"){
      dat <- rbind(dat, c(ul, phi(ul[1], control.parms)))
    } else {
      dat <- rbind(dat, c(ul, 0))
    }
  }

  ul <- as.numeric(c(maxds, maxds))
  if (!any(apply(dat, 1, function(x) identical(x[1:2], ul)))){ # ul is not in the matrix
    if (type != "cost") {
      dat <- rbind(dat, c(ul, phi(ul[1], control.parms)))
    } else {
      dat <- rbind(dat, c(ul, 0))
    }
  }


# global (min, max) point   
  ul1 <- as.numeric(c(minds, maxds))
  
  if(!any(which(apply(dat, 1, function(x) identical(x[1:2], ul1))))){ 
    # mandatory point is missing
    stop("UtilInterpol:: There are points missing in m.pts parameter which are 
         required for the interpolation (minds, maxds).", call. = FALSE)
  }

# global (max, min) point   
  ul2 <- as.numeric(c(maxds, minds))
  
  if(!any(which(apply(dat, 1, function(x) identical(x[1:2], ul2))))){ 
    # mandatory point is missing
    stop("UtilInterpol:: There are points missing in m.pts parameter which are
         required for the interpolation (maxds, minds).", call. = FALSE)
  }

  # when preds and/or trues are outside [minds, maxds] range the utility is 
  # obtained by extrapolation using relevance values and the mandatory m.pts.
  # This extrapolation is obtained by adding new interpolation points with the 
  # same values of the points mentioned: (minds, minds), (maxds, maxds),
  # (minds, maxds) and (maxds, minds)

  # performing extrapolation if needed:
  if(any(preds > maxds) || any(preds < minds) || 
      any(trues > maxds) || any(trues < minds)){
    warning("Predictions and/or true target values provided outside boundaries. \n
            Extrapolating utility values.",
            call. = FALSE)
    # determine farthest distance to original interval [minds, maxds]
    inc.fac <- max(abs(max(preds, trues) - maxds), abs(min(preds, trues) - minds))
    if (type != "cost") {
      # add two diagonal points (using relevance function)
      dat <- rbind(dat, c(minds-inc.fac, minds-inc.fac, phi(minds-inc.fac, control.parms)))
      dat <- rbind(dat, c(maxds+inc.fac, maxds+inc.fac, phi(maxds+inc.fac, control.parms)))
    } else {
      # add two diagonal points with zero relevance
      dat <- rbind(dat, c(minds-inc.fac, minds-inc.fac, 0))
      dat <- rbind(dat, c(maxds+inc.fac, maxds+inc.fac, 0))
    }
    # add two non-diagonal points
    dat <- rbind(dat, 
               c(minds-inc.fac, maxds+inc.fac, 
                 dat[which(apply(dat, 1, function(x) identical(x[1:2], ul1))), 3]))
    dat <- rbind(dat, 
               c(maxds+inc.fac, minds-inc.fac, 
                 dat[which(apply(dat, 1, function(x) identical(x[1:2], ul2))), 3]))
  
    # update minds and maxds:
    minds <- minds-inc.fac
    maxds <- maxds+inc.fac
  }
  
  baseseq <- seq(minds-0.01, maxds+0.01, by=eps)
  if(baseseq[length(baseseq)]!=maxds) baseseq <- c(baseseq, maxds)
  newSet <- c(baseseq, trues, preds)
  
  ord <- order(newSet)
  newSet <- newSet[ord]
  
  
  
  if (method == "bilinear"){ # use loess from stats package

    dat <- as.data.frame(dat)
    colnames(dat) <- c("trues", "preds", "U")
    
    model <- suppressWarnings(loess(U ~., dat, degree = 1,
                      control = loess.control(surface = "direct")))
    res <- matrix(0, ncol= length(newSet), nrow=length(newSet))
    for(i in 1:length(newSet)){
      res[i, ]<- suppressWarnings( predict(model, data.frame( trues = newSet[i], preds = newSet)))
    }
    if (type == "utility"){
      res[which(res > 1)] <- 1
      res[which(res < -1)] <- -1
    } else {
      res[which(res < 0)] <- 0
    }
    
    if(visual){
      tempx <- unique(newSet)
      ifelse(length(tempx) == length(newSet),
             tempz <- res, 
             tempz <- res[-which(duplicated(newSet)), -which(duplicated(newSet))])
      image(tempx, tempx, tempz, xlab=expression(y), ylab=expression(hat(y)))
      contour(tempx, tempx, tempz, add=TRUE, labcex=1)
      points(trues, preds)
    }
  }
  
  if(method == "splines"){ # use package(MBA)
#    require(MBA)
    newdat <- as.matrix(expand.grid(trues=newSet, preds=newSet))
    interp.dat <- suppressWarnings(mba.points(dat, newdat, h=3)$xyz.est)

    if (type == "utility"){
      # removing value above 1 and below -1
      interp.dat[which(interp.dat[,3] > 1), 3] <- 1
      interp.dat[which(interp.dat[,3] < -1), 3] <- -1
    } else {
      interp.dat[which(interp.dat[,3] < 0), 3] <- 0
    }
    
    if(visual){
      temp.dat <- mba.surf(dat, no.X=length(newSet), no.Y=length(newSet))$xyz.est
      image(temp.dat, xlab=expression(y), ylab=expression(hat(y)))
      contour(temp.dat, add=TRUE, labcex=1)
      points(trues, preds)
    }

    res <- matrix(interp.dat[,3], nrow=length(newSet))
  }

  if(method == "idw"){ # use packages sp and gstat
   # require(sp)
   # require(gstat)

   Ndat <- as.data.frame(dat)
   colnames(Ndat) <- c("trues", "preds", "U")
   coordinates(Ndat) <- c("trues", "preds")
    
   # create a grid with points where we want to obtain an estimation of Utility
   dat.grid <- expand.grid(trues = newSet, preds = newSet)
   coordinates(dat.grid) <- c("trues", "preds")
    
   new.grid <- SpatialPoints(dat.grid)
    
   interp.dat <- idw(U~1, Ndat, new.grid)
   res <- matrix(interp.dat@data$var1.pred, nrow=length(newSet))
   if (type == "utility"){
     res[which(res > 1)] <- 1
     res[which(res < -1)] <- -1
   } else {
     res[which(res < 0)] <- 0 
   }
   
   if(visual){
     tempx <- unique(newSet)
     tres <- res[-which(duplicated(interp.dat@coords[,1])), 
                 -which(duplicated(interp.dat@coords[,1]))]
     image(tempx, tempx, tres,xlab=expression(y), ylab=expression(hat(y)))
     contour(tempx, tempx, tres, add=TRUE, labcex=1)
     points(trues, preds) 
   }
   
  }

  if(method == "krige"){ # use packages sp and automap
    # require(sp)
    # require(automap)

    Ndat <- as.data.frame(dat)
    colnames(Ndat) <- c("trues", "preds", "U")
    coordinates(Ndat) <- c("trues", "preds")

    # create a grid with points where we want to obtain an estimation of Utility
    dat.grid <- expand.grid(trues = newSet, preds = newSet)
    coordinates(dat.grid) <- c("trues", "preds")

    new.grid <-SpatialPoints(dat.grid)
    interp.dat <- suppressWarnings(autoKrige(U~1, Ndat, new.grid)) 

    res <- matrix(interp.dat$krige_output@data$var1.pred, nrow=length(newSet))
    
    if (type == "utility"){
      res[which(res > 1)] <- 1
      res[which(res < -1)] <- -1
    } else {
      res[which(res < 0)] <- 0
    }

    if(visual){
      tempx <- unique(newSet)
      tres <- res[-which(duplicated(interp.dat$krige_output@coords[,1])), 
                  -which(duplicated(interp.dat$krige_output@coords[,1]))]
      image(tempx, tempx, tres,xlab=expression(y), ylab=expression(hat(y)))
      contour(tempx, tempx, tres, add=TRUE, labcex=1)
      points(trues, preds)
    }
    
  }
  
  #reorder results and extract the utility of the points provided (trues, preds)
 Util.reorder<- res[order(ord),order(ord)]
 
 if(full.output){
   res <- Util.reorder[1:length(baseseq), 1:length(baseseq)]
  } else {
   res <-diag(Util.reorder[(length(baseseq)+1):(length(baseseq)+length(trues)),
                          (length(baseseq)+length(trues)+1):(length(baseseq)+length(trues)+length(preds))])
 }
 res
}
