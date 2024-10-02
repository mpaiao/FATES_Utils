#==========================================================================================#
#==========================================================================================#
#   This function finds first guesses for all parameters of the skew normal distribution.
#------------------------------------------------------------------------------------------#
sn_guess <<- function(x,na.rm=FALSE,method=c("MPLE","MLE"),maxit=9999L){
   #---~---
   #   Stop if package fGarch isn't loaded.
   #---~---
   sn_avail = try(find.package("sn"),silent=TRUE)
   if ("try-error" %in% is(sn_avail)){
      stop("Function sn_guess requires package \"sn\" (version > 1.0)!")
   }#end if ("try-error" %in% is(sn_avail))
   #---~---


   #---~---
   #   Make sure the method is acceptable.
   #---~---
   method = match.arg(method)
   #---~---


   #---~---
   #   If na.rm is TRUE, we select only the data that is finite.
   #---~---
   if (na.rm){
      sel = is.finite(x)
      xsel = x[sel]
      rm(sel)
   }else{
      xsel = x
   }#end if
   #---~---



   #---~---
   #   Uses the skew elliptical error distribution to estimate parameters.
   #---~---
   myfit_mle = try( sn::selm( formula = xsel~1
                            , family  = "SN"
                            , data    = data.frame(xsel=xsel)
                            , method  = method
                            )#end selm
                  , silent = TRUE
                  )#end try
   #---~---


   #---~---
   #   If estimate fails, set parameters to match the normal distribution
   #---~---
   if ("try-error" %in% is(myfit_mle)){
      ans = list(xi=mean(xsel,na.rm=TRUE),omega=sd(xsel,na.rm=TRUE),alpha=0.)
   }else{
      ans = as.list(myfit_mle@param$dp)
   }#end if
   #---~---



   #---~---
   #   Return the answer.
   #---~---
   return(ans)
   #---~---
}#end function sn_guess
#==========================================================================================#
#==========================================================================================#



#==========================================================================================#
#==========================================================================================#
#     Functions for the "negative log-normal" distribution.  This distribution is similar
# to the log-normal distribution, except that it is defined for when all values are
# negative. The density function of this distribution is defined as:
#
# p(x) = - { 1 / [ sqrt(2*pi) * sigma * x ] } * exp{ [-ln(-x)-mu]^2/ (2*sigma^2)}
#
# In practice, the distribution is implemented using the lognormal distribution functions.
#------------------------------------------------------------------------------------------#
   #---~---
   #   Density function
   #---~---
   dnlnorm <<- function(x,meannlog=0,sdnlog=1,log=FALSE){
      ans = dlnorm(x=-x,meanlog=-meannlog,sdlog=sdnlog,log=log)
      return(ans)
   }#end function(dnlnorm)
   #---~---



   #---~---
   #   Cumulative distribution function
   #---~---
   pnlnorm <<- function(q,meannlog=0,sdnlog=1,lower.tail=TRUE,log.p=FALSE){
      ans = 1-plnorm(q=-q,meanlog=-meannlog,sdlog=sdnlog,lower.tail=lower.tail,log.p=log.p)
      return(ans)
   }#end function(pnlnorm)
   #---~---



   #---~---
   #   Quantile estimate
   #---~---
   qnlnorm <<- function(p,meannlog=0,sdnlog=1,lower.tail=TRUE,log.p=FALSE){
      ans = -qlnorm(p=1-p,meanlog=-meannlog,sdlog=sdnlog,lower.tail=lower.tail,log.p=log.p)
      return(ans)
   }#end function(qlnorm)
   #---~---



   #---~---
   #   Random number function
   #---~---
   rnlnorm <<- function(n,meannlog=0,sdnlog=1){
      ans = -1 * rlnorm(n=n,meanlog=-meannlog,sdlog=sdnlog)
      return(ans)
   }#end function(rnlnorm)
   #---~---
#==========================================================================================#
#==========================================================================================#
