#==========================================================================================
#     This function finds the lower and upper limits for the data that will make it 
# look good. It takes four arguments:
#
# x        -- The vector with data.
# ci_level -- The range of data used for setting limits.  This is useful to avoid the 
#             scale to be stretched due to outliers.
# mirror   -- Should the results be mirrored? Set this to `TRUE` when your data can 
#             be positive or negative, and you want zero to be at the centre.
# trans    -- Variable transformation to find breaks. This should be a function from 
#             package `scales`.  Beware that some scales (e.g. log or sqrt) might not 
#             work with negative values or zero (for log-looking scale for negative
#             values, check neglog_trans.
#------------------------------------------------------------------------------------------
find_bounds <<- function(x,ci_level=0.95,mirror=FALSE,trans="identity_trans"){
   # Make sure we have some bounded data
   if (! any(is.finite(x))){
      x_use = c(0.5,1.0,1.5)
   }else{
      x_mabs = mean(abs(x),na.rm=TRUE)
      x_sdev = sd  (x,na.rm=TRUE)
      if (x_sdev > (sqrt(.Machine$double.eps) * x_mabs)){
         x_use = x
      }else if(x_mabs >= sqrt(.Machine$double.eps)){
         x_use = x * runif(n=length(x),min=0.9,max=1.1)
      }else{
         x_use = x + runif(n=length(x),min=-0.5,max=0.5)
      }#end if  (x_sdev >= (sqrt(.Machine$double.eps) * x_mabs))
   }#end if (! any(is.finite(x)))

   # Find first guess for lower and upper bounds.
   if (mirror){
      x_upr = quantile(x=abs(x_use),probs=ci_level,names=FALSE,na.rm=TRUE)
      x_lwr = - x_upr
   }else{
      x_lwr = quantile(x=x_use,probs=0.5*(1-ci_level),names=FALSE,na.rm=TRUE)
      x_upr = quantile(x=x_use,probs=0.5*(1+ci_level),names=FALSE,na.rm=TRUE)
   }#end if (mirror)

   # Find transformation function
   trans = match.fun(trans)
  
   # Find limits and return the range.
   ans      = range(trans()$breaks(c(x_lwr,x_upr)))
   return(ans)
}#end function find_bounds
#==========================================================================================
