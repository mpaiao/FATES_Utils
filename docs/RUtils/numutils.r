#==========================================================================================#
#==========================================================================================#
#     Function that finds the cube root, for both positive and negative numbers.           #
#------------------------------------------------------------------------------------------#
cbrt <<- function(x){
   x333      = sign(x) * abs(x)^onethird
   bad       = is.nan(x333)
   x333[bad] = NA_real_
   return(x333)
}#end function cbrt
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#     Functions that finds round of the log of the number.                                 #
#------------------------------------------------------------------------------------------#
round.log   <<- function(x,base=exp(1),...) base^(round(log(x,base=base),...))
round.log10 <<- function(x,...) 10^(round(log10(x),...))
round.log2  <<- function(x,...) 2^(round(log2(x),...))
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#     Base 10 exponential.                                                                 #
#------------------------------------------------------------------------------------------#
exp10 <<- function(x,...) 10^x
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#     This function finds the binary notation of an integer.                               #
#------------------------------------------------------------------------------------------#
inttobin <<- function(x,reverse=TRUE){

   #----- X must be an integer, check it. -------------------------------------------------#
   if (! is.integer(x)){
      warning ("Function tobin: coercing x to an integer")
      x = as.integer(x)
   }#end if
   #---------------------------------------------------------------------------------------#



   #----- Check whether this is a single element.  If not, use recursive call. ------------#
   if (is.vector(x) && length(x) == 1){
      #----- Paste bits. ------------------------------------------------------------------#
      if (reverse){
        ans = paste(sapply(strsplit(paste(rev(intToBits(x))),""),`[[`,2),collapse="")
      }else{
        ans = paste(sapply(strsplit(paste(intToBits(x)),""),`[[`,2),collapse="")
      }#end if
      #------------------------------------------------------------------------------------#

   }else if (is.matrix(x) || is.array(x)){
      #----- Array or matrix. -------------------------------------------------------------#
      margin = length(dim(x))
      ans    = apply(X=x,MARGIN=sequence(margin),FUN=tobin,reverse=reverse)
      #------------------------------------------------------------------------------------#
   }else if (is.list(x)){
      #----- List or data frame. ----------------------------------------------------------#
      ans = sapply(X=x,FUN=tobin,reverse=reverse,simplify=is.data.frame(x))
      #------------------------------------------------------------------------------------#
   }else if (is.null(dim(x))){
      #----- Vector. ----------------------------------------------------------------------#
      ans = sapply(X=x,FUN=tobin,reverse=reverse,simplify=TRUE)
      #------------------------------------------------------------------------------------#
   }#end if
   #---------------------------------------------------------------------------------------#
   return(ans)
}#end function inttobin
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#     This function finds the binary notation of an integer.                               #
#------------------------------------------------------------------------------------------#
rawtoint <<- function(x){

   #----- Crash in case x is not raw. -----------------------------------------------------#
   dummy = stopifnot (is.raw(x))
   #---------------------------------------------------------------------------------------#



   #----- Pack all elements. --------------------------------------------------------------#
   ans   = as.integer(packBits(x))
   return(ans)
   #---------------------------------------------------------------------------------------#
}#end function rawtoint
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#     This function computes the mean of the elements below (above) a given quantile.      #
#------------------------------------------------------------------------------------------#
qu.mean <<- function(x,p,na.rm=FALSE,lower=TRUE){
   if (na.rm) x = x[! is.na(x)]

   #----- Do the calculation only if there is anything left. ------------------------------#
   if (any(is.finite(x))){
      qu  = quantile(x,probs=p,na.rm=na.rm)
      if (lower){
         ans = mean(x[x <= qu],na.rm=na.rm)
      }else{
         ans = mean(x[x >= qu],na.rm=na.rm)
      }#end if
   }else{
      ans = NA
   }#end if

   return(ans)
}#end function qu.mean
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#     This function is a quick integrator.  For more elegant ways to integrate a function, #
# check function quadrature.                                                               #
#------------------------------------------------------------------------------------------#
weighted.sum <<- function(x,w,na.rm=FALSE) sum(x*w,na.rm=na.rm)
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#     These functions find weighted averages by applying the same weighting factor for     #
# each column (weighted.rowMeans) or rows (weighted.colMeans).                             #
#------------------------------------------------------------------------------------------#
weighted.rowMeans <<- function(X,wc,na.rm=FALSE){
   ans = apply( X      = X
              , MARGIN = 1
              , FUN    = weighted.mean
              , w      = wc
              , na.rm  = na.rm
              )#end apply
   return(ans)
}#end weighted.rowMeans
weighted.colMeans <<- function(X,wr,na.rm=FALSE){
   ans = apply( X      = X
              , MARGIN = 2
              , FUN    = weighted.mean
              , w      = wr
              , na.rm  = na.rm
              )#end apply
   return(ans)
}#end weighted.colMeans
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#     This function finds the weighted fraction for each element of data.frame x, using    #
# weight w.  In case x is not a data frame, we will try to coerce it to a data frame,      #
# if it doesn't work then we crash it.  It will correct the final answer to provide        #
# weights that add up to one, and it will create a vector of equal chances in case weights #
# are all zeroes.                                                                          #
#------------------------------------------------------------------------------------------#
weighted.frac <<- function(x,w,na.rm=TRUE){
   #----- Make sure "x" is a data frame. --------------------------------------------------#
   if (! is.data.frame(x)){
      x = try(as.data.frame(x),silent=TRUE)
      #----- Give the bad news in case it doesn't coerce to a data frame. -----------------#
      if ("try-error" %in% is(x)){
         stop(" 'x' must be an object that can be coerced into a data frame!")
      }#end if ("try-error" %in% is(x))
      #------------------------------------------------------------------------------------#
   }#end if (! is.data.frame(x))
   #---------------------------------------------------------------------------------------#


   #---------------------------------------------------------------------------------------#
   #     Stop in case the dimensions of x and w don't match.                               #
   #---------------------------------------------------------------------------------------#
   w = c(unlist(w))
   if (length(w) != nrow(x)){
      stop(" 'x' and 'w' must have compatible dimensions (length(x) = nrow(x))!")
   }#end if (length(w) != nrow(x))
   #---------------------------------------------------------------------------------------#



   #------ In case all weights are zero, make them equal. ---------------------------------#
   if (na.rm){
      keep = rowSums(! is.finite(as.matrix(x))) == 0 & is.finite(w)
      x    = x[keep,,drop=FALSE]
      w    = w[keep]
   }else if (any(! is.finite(w))){
      #----- Return NA in case w has non-finite elements and na.rm = FALSE. ---------------#
      ans        = rep(NA,times=length(x))
      names(ans) = names(x)
      return(ans)
      #------------------------------------------------------------------------------------#
   }else if (all(w %eq% 0.)){
      #----- Give equal chances in case all weights were zero. ----------------------------#
      w = rep(x=1.,times=nrow(x))
      #------------------------------------------------------------------------------------#
   }#end if
   #---------------------------------------------------------------------------------------#


   #---------------------------------------------------------------------------------------#
   #      Find the weighted mean of each element of x.                                     #
   #---------------------------------------------------------------------------------------#
   ans = sapply(X=x,FUN=weighted.mean,w=w)
   ans = ans / sum(ans)
   names(ans) = names(x)
   return(ans)
   #---------------------------------------------------------------------------------------#
}#end weighted.frac
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#     This function estimates the quantile for a table of observations x, each of which    #
# having a weight w.  This is done by finding the median of a pseudo dataset built using   #
# sample.  If the size of the resampling is not provided, then the number of samples is    #
# dependent on the range of probabilities.  By default we find the 0.50 quantile (median). #
#------------------------------------------------------------------------------------------#
weighted.quantile <<- function(x,w,qu=0.50,size.minp=10,na.rm=FALSE,out.case=FALSE){

   #----- Delete the missing values if the user asked to do it. ---------------------------#
   if (any(w <= 0, na.rm = TRUE) || any(is.infinite(w)) || any(is.na(w))){
      stop(" Weights (w) must be positive and finite, and entirely defined!")
   }else if(qu < 0. || qu > 1.){
      stop(" Quantile (qu) must be between 0 and 1. ")
   }else if(na.rm){
      keep = ! is.na(x)
      x    = x[keep]
      w    = w[keep]
   }#end if
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #      Define the probabilities by normalising the weights.                             #
   #---------------------------------------------------------------------------------------#
   prob = w / sum(w)
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #      Sort the values by the probability.                                              #
   #---------------------------------------------------------------------------------------#
   o     = order(x,decreasing=FALSE)
   x     = x[o]
   w     = w[o]
   prob  = prob[o]
   cum   = cumsum(prob)
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #      Sort the values by the probability.                                              #
   #---------------------------------------------------------------------------------------#
   if (qu <= cum[1]){
      qout = x[1]
      case = "minimum"
   }else if (qu >= cum[length(cum)]){
      qout = x[length(cum)]
      case = "maximum"
   }else if (any(cum == qu)){
      qout = x[which(cum == qu)]
      case = "exact"
   }else{
      below   = qu - cum ; below[below < 0] = Inf 
      above   = cum - qu ; above[above < 0] = Inf
      i.below = which.min(below)
      i.above = which.min(above)
      w.below = 1. / (below[i.below]^2)
      w.above = 1. / (above[i.above]^2)
      qout    = ( x[i.below] * w.below + x[i.above] * w.above ) / (w.below + w.above)
      case    = "interpolated"
   }#end if
   #---------------------------------------------------------------------------------------#


   #---- Decide what to return. -----------------------------------------------------------#
   if (out.case){
      ans = list(q = qout, case = case)
   }else{
      ans = qout
   }#end if (out.case)
   return(ans)
   #---------------------------------------------------------------------------------------#
}#end function weighted.quantile
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#     This function estimates the weighted standard deviation.                             #
#------------------------------------------------------------------------------------------#
weighted.sd <<- function(x,w,M=NULL,na.rm=FALSE){

   #----- Delete the missing values if the user asked to do it. ---------------------------#
   if (any(w < 0, na.rm = TRUE) || any(is.infinite(w)) || any(is.na(w))){
      stop(" Weights (w) must be non-negative and finite, and entirely defined!")
   }else if(na.rm){
      keep = ! is.na(x)
      x    = x[keep]
      w    = w[keep]
   }#end if
   #---------------------------------------------------------------------------------------#


   #----- Assume M to be inversely proportional to the smallest weight. -------------------#
   if (is.null(M)) M = min(w)
   #---------------------------------------------------------------------------------------#

   #---------------------------------------------------------------------------------------#
   #      Check whether at least one weight is non-zero.                                   #
   #---------------------------------------------------------------------------------------#
   if (all(w %eq% 0)){
      ans = NA
   }else{
      xwm    = weighted.mean(x=x,w=w)
      w.sum  = sum(w)
      r2.sum = sum(w*(x-xwm)^2)
      ans    = sqrt(M * r2.sum / (M * w.sum - 1))
   }#end if
   #---------------------------------------------------------------------------------------#

   return(ans)
}#end function weighted.sd
#==========================================================================================#
#==========================================================================================#




#==========================================================================================#
#==========================================================================================#
#     This function computes the standard error.                                           #
#------------------------------------------------------------------------------------------#
se <<- function (x, na.rm = FALSE){
   #---- We follow the same convention as the standard deviation. -------------------------#
   if (is.matrix(x)) {
      msg = "se(<matrix>) is deprecated.\n Use apply(*, 2, se) instead."
      warning(paste(msg, collapse = ""), call. = FALSE, domain = NA)
      ans = apply(X = x, MARGIN = 2, FUN = se, na.rm = na.rm)
   }else if (is.list(x)){
      msg = "se(<list>) is deprecated.\n Use lapply(*, se) instead."
      warning(paste(msg, collapse = ""), call. = FALSE, domain = NA)
      ans = lapply(X = x, FUN = se, na.rm = na.rm)
   }else if (is.data.frame(x)){
      msg = "se(<data.frame>) is deprecated.\n Use sapply(*, se) instead."
      warning(paste(msg, collapse = ""), call. = FALSE, domain = NA)
      ans = sapply(X = x, se, na.rm = na.rm)
   }else{
      #----- Coerce x to a vector. --------------------------------------------------------#
      if (is.vector(x)){
         xx = x
      }else{
         xx = as.vector(x)
      }#end if
      #------------------------------------------------------------------------------------#



      #----- Decide whether to delete NA or not. ------------------------------------------#
      if (na.rm) xx = xx[! is.na(xx)]
      #------------------------------------------------------------------------------------#



      #----- Find the standard error. -----------------------------------------------------#
      nx  = length(xx)
      ans = sd(xx)/sqrt(nx)
      #------------------------------------------------------------------------------------#
   }#end if
   #---------------------------------------------------------------------------------------#

   return(ans)
}#end function se
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#     This function computes the skewness, or the third moment of a distribution.          #
#------------------------------------------------------------------------------------------#
skew <<- function (x, na.rm = FALSE){

   #---- We follow the same convention as the standard deviation. -------------------------#
   if (is.matrix(x)) {
      msg = "skew(<matrix>) is deprecated.\n Use apply(*, 2, skew) instead."
      warning(paste(msg, collapse = ""), call. = FALSE, domain = NA)
      ans = apply(X = x, MARGIN = 2, FUN = skew, na.rm = na.rm)
   }else if (is.list(x)){
      msg = "skew(<list>) is deprecated.\n Use lapply(*, skew) instead."
      warning(paste(msg, collapse = ""), call. = FALSE, domain = NA)
      ans = lapply(X = x, FUN = skew, na.rm = na.rm)
   }else if (is.data.frame(x)){
      msg = "skew(<data.frame>) is deprecated.\n Use sapply(*, skew) instead."
      warning(paste(msg, collapse = ""), call. = FALSE, domain = NA)
      ans = sapply(X = x, skew, na.rm = na.rm)
   }else{
      #----- Coerce x to a vector. --------------------------------------------------------#
      if (is.vector(x)){
         xx = x
      }else{
         xx = as.vector(x)
      }#end if
      #------------------------------------------------------------------------------------#



      #----- Decide whether to delete NA or not. ------------------------------------------#
      if (na.rm) xx = xx[! is.na(xx)]
      #------------------------------------------------------------------------------------#



      #----- Find the skewness. -----------------------------------------------------------#
      nx = length(xx)
      xx.mean = mean(xx)
      xx.sdev = sd(xx)
      ans     = sum((xx-xx.mean)^3) / (nx * xx.sdev^3)
      #------------------------------------------------------------------------------------#
   }#end if
   #---------------------------------------------------------------------------------------#

   return(ans)
}#end function skew
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#     This function computes the kurtosis, or the fourth moment of a distribution.         #
#     NOTE: This calculates the absolute kurtosis, to get the excess kurtosis you must     #
#           subtract 3.                                                                    #
#------------------------------------------------------------------------------------------#
kurt <<- function (x, na.rm = FALSE){

   #---- We follow the same convention as the standard deviation. -------------------------#
   if (is.matrix(x)) {
      msg = "kurt(<matrix>) is deprecated.\n Use apply(*, 2, kurt) instead."
      warning(paste(msg, collapse = ""), call. = FALSE, domain = NA)
      ans = apply(X = x, MARGIN = 2, FUN = kurt, na.rm = na.rm)
   }else if (is.list(x)){
      msg = "kurt(<list>) is deprecated.\n Use lapply(*, kurt) instead."
      warning(paste(msg, collapse = ""), call. = FALSE, domain = NA)
      ans = lapply(X = x, FUN = kurt, na.rm = na.rm)
   }else if (is.data.frame(x)){
      msg = "kurt(<data.frame>) is deprecated.\n Use sapply(*, kurt) instead."
      warning(paste(msg, collapse = ""), call. = FALSE, domain = NA)
      ans = sapply(X = x, kurt, na.rm = na.rm)
   }else{
      #----- Coerce x to a vector. --------------------------------------------------------#
      if (is.vector(x)){
         xx = x
      }else{
         xx = as.vector(x)
      }#end if
      #------------------------------------------------------------------------------------#



      #----- Decide whether to delete NA or not. ------------------------------------------#
      if (na.rm) xx = xx[! is.na(xx)]
      #------------------------------------------------------------------------------------#



      #----- Find the kurtosis. -----------------------------------------------------------#
      nx = length(xx)
      xx.mean = mean(xx)
      xx.sdev = sd(xx)
      ans     = sum((xx-xx.mean)^4) / (nx * xx.sdev^4)
      #------------------------------------------------------------------------------------#
   }#end if
   #---------------------------------------------------------------------------------------#

   return(ans)
}#end function kurt
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#     This function computes the inter-quartile range.                                     #
#------------------------------------------------------------------------------------------#
iqr <<- function (x, na.rm = FALSE){

   #---- We follow the same convention as the standard deviation. -------------------------#
   if (is.matrix(x)) {
      msg = "iqr(<matrix>) is deprecated.\n Use apply(*, 2, kurt) instead."
      warning(paste(msg, collapse = ""), call. = FALSE, domain = NA)
      ans = apply(X = x, MARGIN = 2, FUN = iqr, na.rm = na.rm)
   }else if (is.list(x)){
      msg = "iqr(<list>) is deprecated.\n Use lapply(*, kurt) instead."
      warning(paste(msg, collapse = ""), call. = FALSE, domain = NA)
      ans = lapply(X = x, FUN = iqr, na.rm = na.rm)
   }else if (is.data.frame(x)){
      msg = "iqr(<data.frame>) is deprecated.\n Use sapply(*, kurt) instead."
      warning(paste(msg, collapse = ""), call. = FALSE, domain = NA)
      ans = sapply(X = x, iqr, na.rm = na.rm)
   }else{
      #----- Coerce x to a vector. --------------------------------------------------------#
      if (is.vector(x)){
         xx = x
      }else{
         xx = as.vector(x)
      }#end if
      #------------------------------------------------------------------------------------#



      #----- Decide whether to delete NA or not. ------------------------------------------#
      if (na.rm) xx = xx[! is.na(xx)]
      #------------------------------------------------------------------------------------#



      #----- Find the inter-quartile range. -----------------------------------------------#
      ans        = diff(quantile(xx,probs=c(0.25,0.75)))
      names(ans) = NULL
      #------------------------------------------------------------------------------------#
   }#end if
   #---------------------------------------------------------------------------------------#

   return(ans)
}#end function iqr
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#      This function finds the element index given the array index and the dimensions of   #
# the array.  This is the inverse of function arrayInd in R.                               #
#                                                                                          #
# Original author: Feng Li, Department of Statistics, Stockholm University, Sweden.        #
#                  Based on last version as of Wed Mar 14 18:03:09 CET 2012.               #
#                                                                                          #
# Modified by:     Marcos Longo.  Department of Earth and Planetary Sciences,              #
#                                 Harvard University, Cambridge, MA, USA                   #
#                  Last modified on 25 Oct 2012 - 10:56 EST                                #
#                                                                                          #
#                  The script now recognises whether the arr.ind is a vector, matrix, or   #
#                  list and call it self recursively to return the full list.              #
#------------------------------------------------------------------------------------------#
whichInd <<- function(ai, dims){

   #----- Save the number of dimensions. --------------------------------------------------#
   n.dims  = length(dims)
   #---------------------------------------------------------------------------------------#



   #---- Check the variable type. ---------------------------------------------------------#
   if (is.matrix(ai) || is.data.frame(ai)) {
      #----- Check that the dimensions match. ---------------------------------------------#
      if ( ncol(ai) != n.dims){
         cat(" - Number of columns of ai: ",ncol(ai),"\n")
         cat(" - Length of dims:          ",n.dims  ,"\n")
         stop(" Dimensions between ai and dims don't match!")
      }#end if
      #------------------------------------------------------------------------------------#

      ans = apply (X = ai, FUN = whichInd, MARGIN = 1, dims=dims)

   }else if (is.list(ai)){
      #----- Check that the dimensions match. ---------------------------------------------#
      if ( any(sapply(X=ai,FUN=length) != n.dims)){
         fail = sum(sapply(X=ai,FUN=length) != n.dims)
         cat (" - ",fail," elements of ai don't have correct dimensions","\n")
         cat (" - Length of dims: ",n.dims,"\n")
         stop(" Dimensions between some ai elements and dims don't match!")
      }#end if
      #------------------------------------------------------------------------------------#

      ans = lapply(X = ai, FUN = whichInd, dims=dims)
   }else{

      #----- Coerce the data to be a vector. ----------------------------------------------#
      if (length(ai) != length(dims)){
         cat (" - Length of ai  : ",length(ai),"\n")
         cat (" - Length of dims: ",n.dims    ,"\n")
         stop(" ai must have the same length as dims")
      }#end if
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #  Variable cumdim is the number of elements we jump every time the index increases  #
      # by 1.  Variable shif is just the array     #
      # 
      #------------------------------------------------------------------------------------#
      cumdim = c(1,cumprod(dims[-n.dims]))
      ans    = 1 + sum(cumdim*(ai-1))
      #------------------------------------------------------------------------------------#
   }#end if
   #---------------------------------------------------------------------------------------#

   return(ans)   
}#end whichInd
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#     This function counts how many unique values exist in a vector.                       #
#------------------------------------------------------------------------------------------#
length.unique <<- function(x){
   ans = length(unique(x))
   return(ans)
}#end function lit.sample
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#     This function finds the ratio of consecutive elements.  All arguments used by diff   #
# can be used here too.                                                                    #
#------------------------------------------------------------------------------------------#
ediff <<- function(x,lag=1,differences=1){
   if (differences == 1){
      n   = length(x)
      ans = x[seq(from=1+lag,to=n,by=lag)]/x[seq(from=1,to=n-lag,by=lag)]
      ans[! is.finite(ans)] = NA
   }else{
      ans = ediff(x,lag=lag,differences=differences-1)
   }#end if
   return(ans)
}#end ediff
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#     This function finds the weighted.mean for each col, similar to colMeans.             #
#------------------------------------------------------------------------------------------#
colWgtMeans <<- function(x,w,...,na.rm=FALSE){
   #---------------------------------------------------------------------------------------#
   #     First sanity check.                                                               #
   #---------------------------------------------------------------------------------------#
   mess  = c("x must be a matrix, data.frame, or an array of dimension 2!"
            ,"w must be a matrix, data.frame, or an array of dimension 2!"
            ,"x and w must have identical sizes!"
            )#end mess
   error = c(! (is.matrix(x) || is.data.frame(x) || (is.array(x) && length(dim(x)) == 2))
            ,! (is.matrix(w) || is.data.frame(w) || (is.array(w) && length(dim(w)) == 2))
            ,any(dim(x) != dim(w))
            )#end c
   if (any(error)) stop(paste(mess,collapse="\n"))
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #     Convert x and w into matrices                                                     #
   #---------------------------------------------------------------------------------------#
   x            = as.matrix(x)
   w            = as.matrix(w)
   x[is.nan(x)] = NA
   w[is.nan(w)] = NA
   w[is.na (w)] = 0
   #---------------------------------------------------------------------------------------#




   #---------------------------------------------------------------------------------------#
   #     Second sanity check.                                                              #
   #---------------------------------------------------------------------------------------#
   mess   = c("NA/NaN are not allowed in w (only x may contain NA)!"
             ,"All w must be finite and positive"
             ,"Some w columns have only zeroes"
             )#end c
   error  = c( any(is.na(c(w)))
             , any(! is.finite(c(w)) | (is.finite(c(w)) & c(w) < 0))
             , any(apply(X=w,MARGIN=2,FUN=sum) == 0,na.rm=TRUE)
             )#end error
   if (any(error)) stop(paste(mess,collapse="\n"))
   #---------------------------------------------------------------------------------------#


   #---------------------------------------------------------------------------------------#
   #     Find weighted means.                                                              #
   #---------------------------------------------------------------------------------------#
   ans = apply(X=x*w,MARGIN=2,FUN=sum,na.rm=na.rm) / apply(X=w,MARGIN=2,FUN=sum)
   #---------------------------------------------------------------------------------------#
   return(ans)
}#end colWgtMeans
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#     This function finds the weighted.mean for each row, similar to rowMeans.             #
#------------------------------------------------------------------------------------------#
rowWgtMeans <<- function(x,w,...,na.rm=FALSE){
   #---------------------------------------------------------------------------------------#
   #     First sanity check.                                                               #
   #---------------------------------------------------------------------------------------#
   mess  = c("x must be a matrix, data.frame, or an array of dimension 2!"
            ,"w must be a matrix, data.frame, or an array of dimension 2!"
            ,"x and w must have identical sizes!"
            )#end mess
   error = c(! (is.matrix(x) || is.data.frame(x) || (is.array(x) && length(dim(x)) == 2))
            ,! (is.matrix(w) || is.data.frame(w) || (is.array(w) && length(dim(w)) == 2))
            ,any(dim(x) != dim(w))
            )#end c
   if (any(error)) stop(paste(mess,collapse="\n"))
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #     Convert x and w into matrices                                                     #
   #---------------------------------------------------------------------------------------#
   x            = as.matrix(x)
   w            = as.matrix(w)
   x[is.nan(x)] = NA
   w[is.nan(w)] = NA
   w[is.na (w)] = 0
   #---------------------------------------------------------------------------------------#




   #---------------------------------------------------------------------------------------#
   #     Second sanity check.                                                              #
   #---------------------------------------------------------------------------------------#
   mess   = c("NA/NaN are not allowed in w (only x may contain NA)!"
             ,"All w must be finite and positive"
             ,"Some w columns have only zeroes"
             )#end c
   error  = c( any(is.na(c(w)))
             , any(! is.finite(c(w)) | (is.finite(c(w)) & c(w) < 0))
             , any(apply(X=w,MARGIN=1,FUN=sum) == 0,na.rm=TRUE)
             )#end error
   if (any(error)) stop(paste(mess,collapse="\n"))
   #---------------------------------------------------------------------------------------#


   #---------------------------------------------------------------------------------------#
   #     Find weighted means.                                                              #
   #---------------------------------------------------------------------------------------#
   ans = apply(X=x*w,MARGIN=1,FUN=sum,na.rm=na.rm) / apply(X=w,MARGIN=1,FUN=sum)
   #---------------------------------------------------------------------------------------#
   return(ans)
}#end colWgtMeans
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#     This function converts data into percentil (0-100).                                  #
#------------------------------------------------------------------------------------------#
percentil <<- function(x,trim=0.0){
   qlow    = 0.0+0.5*trim
   qhigh   = 1.0-0.5*trim

   xlow    = quantile(x,probs=qlow ,na.rm=TRUE)
   xhigh   = quantile(x,probs=qhigh,na.rm=TRUE)
   xperc   = 100. * pmax(0.,pmin(1.,(x-xlow)/(xhigh-xlow)))
   xperc   = ifelse(is.finite(xperc),xperc,NA)
   return(xperc)
}#end normalise
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#     A lazy option of square root of sum of squares and root mean sum of squares.         #
#------------------------------------------------------------------------------------------#
sum2           <<- function(x,...)   sqrt(x = sum          (x=x^2               ,...))
mean2          <<- function(x,...)   sqrt(x = mean         (x=x^2               ,...))
meanlog        <<- function(x,...)   exp (x = mean         (x=log(x)            ,...))
weighted.mean2 <<- function(x,w,...) sqrt(x = weighted.mean(x=x^2,w=(w/sum(w))^2,...))
mean.se        <<- function(x,...)   sqrt(x = mean(x=x^2,...) / length(x[is.finite(x)]))
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#     Function to return the mid points between two consecutive points.                    #
#------------------------------------------------------------------------------------------#
mid.points <<- function(x,islog=FALSE,finite=FALSE){
   cx   = if (islog){log(c(x))}else{c(x)}
   cx   = ifelse(is.finite(cx),cx,NA)
   ix   = seq_along(cx)
   last = length(cx)
   if (finite){
      cx    = na.approx(cx,na.rm=FALSE)
      fa    = min(which(is.finite(cx)))
      fz    = max(which(is.finite(cx)))
      delta = max(c(0,mean(diff(cx),na.rm=TRUE)),na.rm=TRUE)
      #----- Append data to the left. -----------------------------------------------------#
      if (fa > 1){
         fill     = seq(from=1,to=fa-1,by=1)
         cx[fill] = cx[fa] + delta * (ix[fill]-ix[fa])
      }#end if
      if (fz < last){
         fill     = seq(from=fz+1,to=last,by=1)
         cx[fill] = cx[fz] + delta * (ix[fill]-ix[fz])
      }#end if
   }#end if

   xmid = rowMeans(cbind(cx[-1],cx[-last]))
   if(islog) xmid = exp(xmid)
   return(xmid)
}#end mid.points
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#     Functions to return the fraction of points that are xxx (xxx = finite, NA, NaN).     #
#------------------------------------------------------------------------------------------#
frac.finite <<- function(x) sum(is.finite(x))/max(1,length(x))
frac.na     <<- function(x) sum(is.na    (x))/max(1,length(x))
frac.nan    <<- function(x) sum(is.nan   (x))/max(1,length(x))
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#     These variables may be used everywhere, they define the order of the six-summary     #
# vector.                                                                                  #
#------------------------------------------------------------------------------------------#
six.summary.names <<- c("expected","variance","skewness","kurtosis","ci.lower","ci.upper")
n.six.summary     <<- length(six.summary.names)
#==========================================================================================#
#==========================================================================================#








#==========================================================================================#
#==========================================================================================#
#     This function calculates the four moments of the distribution plus the 95% C.I. of   #
# the mean using t distributioon.                                                          #
#------------------------------------------------------------------------------------------#
six.summary <<- function(x,conf=0.95,finite=TRUE,cint.type=c("se","sd","quantile"),...){
   #------ Remove non-finite data in case finite=TRUE. ------------------------------------#
   if (finite) x = x[is.finite(x)]
   nx = length(x)
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #       Decide which type of confidence interval to display (CI on the mean, using t    #
   # distribution and standard error, or the quantile distribution.                        #
   #---------------------------------------------------------------------------------------#
   cint.type = match.arg(cint.type)
   #---------------------------------------------------------------------------------------#



   #------ Initialise data with NA in case the function fails. ----------------------------#
   ans = rep(NA,times=n.six.summary)
   names(ans) = six.summary.names
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #      Check that there are sufficient points for at least the mean.                    #
   #---------------------------------------------------------------------------------------#
   if (nx >= 1){
      #----- Find the moments and confidence interval. ------------------------------------#
      expected = mean(x)
      variance = var (x)
      skewness = skew(x)
      kurtosis = kurt(x)
      if (cint.type %in% "se"){
         #----- Find confidence interval of the average. ----------------------------------#
         ci.lower = expected + qt(p=0.5*(1.0-conf),df=nx-1) * se(x)
         ci.upper = expected + qt(p=0.5*(1.0+conf),df=nx-1) * se(x)
         #---------------------------------------------------------------------------------#
      }else if (cint.type %in% "sd"){
         #----- Find range instead of CI, using t distribution. ---------------------------#
         ci.lower = expected + qt(p=0.5*(1.0-conf),df=nx-1) * sd(x)
         ci.upper = expected + qt(p=0.5*(1.0+conf),df=nx-1) * sd(x)
         #---------------------------------------------------------------------------------#
      }else{
         #----- Find the quantiles. -------------------------------------------------------#
         ci.lower = quantile(x=x,probs=0.5*(1.0-conf),names=FALSE)
         ci.upper = quantile(x=x,probs=0.5*(1.0+conf),names=FALSE)
         #---------------------------------------------------------------------------------#
      }#end if 
      #------------------------------------------------------------------------------------#




      #----- Standardise non-finite values to NA. -----------------------------------------#
      expected = ifelse(is.finite(expected),expected,NA)
      variance = ifelse(is.finite(variance),variance,NA)
      skewness = ifelse(is.finite(skewness),skewness,NA)
      kurtosis = ifelse(is.finite(kurtosis),kurtosis,NA)
      ci.lower = ifelse(is.finite(ci.lower),ci.lower,NA)
      ci.upper = ifelse(is.finite(ci.upper),ci.upper,NA)
      #------------------------------------------------------------------------------------#



      #----- Make sure statistics go to the right place. ----------------------------------#
      ans[match("expected",six.summary.names)] = expected
      ans[match("variance",six.summary.names)] = variance
      ans[match("skewness",six.summary.names)] = skewness
      ans[match("kurtosis",six.summary.names)] = kurtosis
      ans[match("ci.lower",six.summary.names)] = ci.lower
      ans[match("ci.upper",six.summary.names)] = ci.upper
      #------------------------------------------------------------------------------------#
   }#end if
   #---------------------------------------------------------------------------------------#

   return(ans)
}#end function six.summary
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      Thirteen-number summary:                                                            #
#                                                                                          #
# qmin - minimum                                                                           #
# q025 - 2.5 percentile                                                                    #
# q250 - first quartile                                                                    #
# q500 - median                                                                            #
# q750 - third quartile                                                                    #
# q975 - 97.5 percentile                                                                   #
# qmax - maximum                                                                           #
# mean - mean                                                                              #
# sdev - standard deviation                                                                #
# skew - skewness                                                                          #
# kurt - kurtosis                                                                          #
# ntot - total number                                                                      #
# nval - total number of valide (i.e. finite) entries)                                     #
#------------------------------------------------------------------------------------------#
thirteen.num <<- function(x){
   #----- Count total number and number of valid entries. ---------------------------------#
   ntot = length(x)
   fine = is.finite(x)
   nval = sum(fine)
   #---------------------------------------------------------------------------------------#


   #----- Discard invalid numbers. --------------------------------------------------------#
   x    = x[fine]
   #---------------------------------------------------------------------------------------#


   #----- Find quantiles. -----------------------------------------------------------------#
   quant = quantile(x=x,probs=c(0,0.025,0.25,0.50,0.75,0.975,1.000))
   names(quant) = c("qmin","q025","q250","q500","q750","q975","q100")
   #---------------------------------------------------------------------------------------#



   #----- Find mean, sd, skewness and kurtosis. -------------------------------------------#
   four  = c(mean=mean(x),sdev=sd(x),skew=skew(x),kurt=kurt(x))
   #---------------------------------------------------------------------------------------#


   #----- Append all results, and standardise weird values to NA. -------------------------#
   ans   = c(quant,four,ntot=ntot,nval=nval)
   ans[! is.finite(ans)] = NA
   #---------------------------------------------------------------------------------------#

   return(ans)
}#end thirteen.num
#==========================================================================================#
#==========================================================================================#




#==========================================================================================#
#==========================================================================================#
#     These functions are the same as cumsum and cumprod, except that it shifts the        #
# cumulative values to the value until right before the point.                             #
#------------------------------------------------------------------------------------------#
left.cumsum  <<- function(x) c(0,cumsum (x)[-length(x)])
left.cumprod <<- function(x) c(0,cumprod(x)[-length(x)])
#==========================================================================================#
#==========================================================================================#




#==========================================================================================#
#==========================================================================================#
#     These functions are similar to cumsum, except that they ignore values above or below #
# a certain threshold (source: (https://stackoverflow.com/questions/63636052/              #
# calculate-cumulative-sum-cumsum-floored-at-zero).                                        #
#------------------------------------------------------------------------------------------#
#----- cumulative sum floored at thresh. --------------------------------------------------#
floor.cumsum <<- function(x,thresh=0){
   cs  = cumsum(x)
   cn  = cummin(cs)
   ans = cs - pmin(cn,thresh)
   return(ans)
}#end floor.cumsum
#----- cumulative sum capped at thresh. ---------------------------------------------------#
ceiling.cumsum <<- function(x,thresh=0){
   cs  = cumsum(x)
   cx  = cummax(cs)
   ans = cs - pmax(cx,thresh)
   return(ans)
}#end ceiling.cumsum
#==========================================================================================#
#==========================================================================================#




#==========================================================================================#
#==========================================================================================#
#      This function calculates cumulative sum for data with gaps.  It will treat NA as    #
# zeroes, but the output for an NA entry will be always zero.  The default is to reset     #
# the sum whenever there is a missing value.                                               #
#------------------------------------------------------------------------------------------#
na.cumsum    <<- function(x,na.reset=TRUE){
   #----- Identify finite entries. --------------------------------------------------------#
   fine  = is.finite(x)
   #---------------------------------------------------------------------------------------#


   #----- Vector length. ------------------------------------------------------------------#
   nx    = length(x)
   #---------------------------------------------------------------------------------------#


   #---------------------------------------------------------------------------------------#
   #     Check whether any special treatment is needed.                                    #
   #---------------------------------------------------------------------------------------#
   if (nx <= 1){
      #----- Empty vector or scalar, return the original data. ----------------------------#
      ans = x
      #------------------------------------------------------------------------------------#
   }else if (all(fine)){
      #----- Input data are complete, use regular cumsum. ---------------------------------#
      ans = cumsum(x)
      #------------------------------------------------------------------------------------#
   }else if (all(! fine)){
      #----- Input data are blank, everything should be NA. -------------------------------#
      ans = rep(NA,times=nx)
      #------------------------------------------------------------------------------------#
   }else{
      #----- Replace NAs with zeroes, then perform cumulative sum. ------------------------#
      x0    = ifelse(test=fine,yes=x,no=0)
      cumx  = cumsum(x0)
      #------------------------------------------------------------------------------------#


      #----- Reset counters at each NA block. ---------------------------------------------#
      if (na.reset){
         #---------------------------------------------------------------------------------#
         #    Variable 'reset' identifies the  first NA entries of each block.  Variable   #
         # 'nreset' extends until the element before the next NA (or the last element).    #
         #---------------------------------------------------------------------------------#
         reset  = which(c(diff(fine) == -1))
         nreset = c(reset[1],diff(c(reset,nx)))
         #---------------------------------------------------------------------------------#



         #------ Find offset. -------------------------------------------------------------#
         off    = c(unlist(mapply(FUN=rep,x=c(0,cumx[reset]),each=nreset)))
         ans    = ifelse(test=fine,yes=cumx - off,no=NA_real_)
         #---------------------------------------------------------------------------------#
      }else{
         #----- No reset sought, use the cumulative sum previously computed. --------------#
         ans    = ifelse(test=fine,yes=cumx,no=NA_real_)
         #---------------------------------------------------------------------------------#
      }#end if (na.reset)
      #------------------------------------------------------------------------------------#
   }#end if
   return(ans)
}#end na.cumsum
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#     This function finds the maximum absolute elementwise difference of two vectors.      #
#------------------------------------------------------------------------------------------#
max.abs.diff <<- function(x,y,na.rm=TRUE) max(abs(x-y),na.rm=na.rm) 
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#     This function counts the number of valid entries.                                    #
#------------------------------------------------------------------------------------------#
count.valid <<- function(x,qq.rm=FALSE){
   type.x = typeof(x)
   if (type.x %in% "logical"){
      ans = sum(! is.na(x))
   }else if (type.x %in% "character"){
      ans = sum(! ( is.na(x) | ((x %in% "") & qq.rm)))
   }else{
      ans = sum(is.finite(x))
   }#end if (type.x %in% c("logical","character"))
   return(ans)
}#end count.finite
#==========================================================================================#
#==========================================================================================#
