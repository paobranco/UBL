
# ======================================================================
# Get the column on which the target variable in a formula object occurs
# in a dataset.
# ----------------------------------------------------------------------
# Author : L. Torgo, May 2003
# ======================================================================
target.col <- function(form,data) {
  mt <- terms(form,data=data)
  if ((yvar <- attr(mt, "response")) <= 0) 
    stop(paste("Incorrect response variable",yvar))
  which(names(data)==as.character(attr(mt, "variables"))[-1][yvar])
}
