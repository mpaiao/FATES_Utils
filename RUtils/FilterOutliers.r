#==========================================================================================
#==========================================================================================
#     Function to remove outliers from the data set using an iterative approach until all
# data are acceptable.
# 
# Input:
# - x        -- The vector with data to be filtered.
# - count    -- Aggregation factor of each value. If not provided, assume equal weights.
# - CntMin   -- Minimum number of valid points in x (regardless of count) to attempt 
#               detecting outliers.
# - zFineMin -- Minimum Z score that will be always considered fine. This is to avoid
#               overly aggressive outlier removal for small data sets.
# - zFineMax -- Maximum Z score that may be considered fine. Values with associated Z 
#               scores outside the [-zFineMax,+zFineMax] range will be always considered
#               outliers. This is to avoid accepting extremely unlikely values when the 
#               sample size is very large. outside the This is to avoid
#               overly aggressive outlier removal for small data sets.
# - ...      -- Additional arguments to be passed to FindBestDistr
#------------------------------------------------------------------------------------------
FilterOutliers <<- function( x
                           , count    = rep(1L,times=length(x))
                           , CntMin   = 100L
                           , zFineMin = 3.0
                           , zFineMax = 5.0
                           , verbose  = FALSE
                           ,...){

   #---~---
   #   Make sure CntMin is not less than 5. Five is the very minimum in which the function
   # can be mathematically solved (even though it is strongly discouraged to use such low
   # number).
   #---~---
   if (CntMin < 5L){
      warning(paste0(" The value of CntMin (",CntMin,") is too low. Coercing it to 5."))
      CntMin = 5L
   }#end if (CntMin < 5L)
   #---~---


   #---~---
   #   Initialise output variable
   #---~---
   xOut = x
   #---~---



   #---~---
   #   We discard data that are considered significant outliers (i.e., normalised variable
   # that is unacceptably far from 0. Keep iterating it until we have no more points to 
   # remove or too few independent data points for fitting distributions.
   #---~---
   Fine      = is.finite(xOut)
   CntFine   = sum(Fine)
   IsIterate = CntFine >= CntMin 
   n         = 0
   while (IsIterate){
      #---~---
      #   Expand data to include multiple instances of the value when it is associated with
      # multiple counts.
      #---~---
      n         = n + 1L
      xExpand   = rep(x=xOut[Fine],times=count[Fine])
      #---~---


      #---~---
      #   Fit the distribution and normalise data.
      #---~---
      xFitted = suppressWarnings(FindBestDistr(x=xExpand,...))
      xDistr  = xFitted$Distr
      xFirst  = xFitted$First
      xSecond = xFitted$Second
      xThird  = xFitted$Third
      xFun    = switch( xDistr
                      , "uniform"       = punif
                      , "normal"        = pnorm
                      , "logistic"      = plogis
                      , "skew-normal"   = sn::psn
                      , "log-normal"    = plnorm
                      , "neglog-normal" = pnlnorm
                      , "weibull"       = pweibull
                      , "gamma"         = pgamma
                      , NA_character_
                      )#end switch
      #---~---


      #---~---
      #   Decide whether the distribution needs two or three parameters, then normalise
      # values using the normal-equivalent quantiles.
      #---~---
      if (xDistr %in% "skew-normal"){
         zScore = qnorm(p=xFun(xOut,xFirst,xSecond,xThird),mean=0.,sd=1.)
      }else if (! is.na(xDistr)){
         zScore = qnorm(p=xFun(xOut,xFirst,xSecond),mean=0.,sd=1.)
      }#end if (xDistr %in% "skew-normal")
      #---~---


      #---~---
      #   Find the maximum acceptable values for Z-scores. For uniform distributions,
      # we always assume the minimum fine Z score. Otherwise, we assign the Z-score 
      # associated with the probability of at least one observation given the number of
      # observations (but with lower and upper bounds to avoid removing too few or too
      # much data).
      #---~---
      if (xDistr %in% "uniform"){
         zUpper = zFineMin
      }else{
         zUpper = min(zFineMax,max(zFineMin,qnorm(p=1.-2./CntFine, mean = 0., sd = 1.)))
      }#end if (xDistr %in% "uniform")
      #---~---


      #---~---
      #   Filter outliers
      #---~---
      isOutlier       = is.infinite(xOut) | ( abs(zScore) %gt% zUpper )
      xOut[isOutlier] = NA_real_
      Fine            = is.finite(xOut)
      CntFine         = sum(Fine)
      CntOutlier      = sum(isOutlier)
      #---~---


      #---~---
      #   Decide whether or not to iterate. If the distribution is uniform, we cannot
      # run iteratively because the probability distribution function does not depend
      # on the quantiles. In this case, we can only trim the extreme values to avoid
      # possible outliers.
      #---~---
      IsIterate = (CntOutlier > 0) & ( CntFine >= CntMin ) & (! xDistr %in% "uniform" )
      #---~---


      #---~---
      #   Report progress.
      #---~---
      if (verbose){
         nLabel       = sprintf(" Iteration = %4i"        ,n)
         OutlierLabel = sprintf(" CntOutlier = %8i"       ,CntOutlier)
         FineLabel    = sprintf(" CntFine = %8i"          ,CntFine)
         zUpperLabel  = sprintf(" zUpperLabel = %.2f (%s)",zUpper,xDistr)
         cat0("       ~ ",nLabel,"; ",OutlierLabel,"; ",FineLabel,"; ",zUpperLabel)
      }#end of
      #---~---
   }#end while (IsIterate)
   #---~---

   #---~---
   #   Return the filtered data set
   #---~---
   return(xOut)
   #---~---

}#end function FilterOutliers
#==========================================================================================
#==========================================================================================
