#==========================================================================================
#     This function finds the lower and upper limits for the data that will make it 
# look good. It takes four arguments:
#
# x        -- The vector with data.
# ci_level -- The range of data used for setting limits.  This is useful to avoid the 
#             scale to be stretched due to outliers.
# mirror   -- Should the results be mirrored? Set this to `TRUE` when your data can 
#             be positive or negative, and you want zero to be at the centre.
# zero     -- Should the scale include zero? Set this to `TRUE` when you want to assess
#             relative differences, or set it to `FALSE` when you want to assess smaller
#             differences around a mean too far from zero (e.g., pressure or absolute 
#             temperature).
# trans    -- Variable transformation to find breaks. This should be a function from 
#             package `scales`.  Beware that some scales (e.g. log or sqrt) might not 
#             work with negative values or zero (for log-looking scale for negative
#             values, check neglog_trans.
#------------------------------------------------------------------------------------------
find_bounds <<- function(x,ci_level=0.95,mirror=FALSE,zero=FALSE,trans="identity_trans"){
   #---~---
   #   Issue an error in case mirror or zero are TRUE and the variable transformation
   # is logarithmic. Also, if mirror is TRUE, check for exponential or sqrt 
   # transformations.
   #---~---
   if (any(c(mirror,zero)) && grepl(pattern="log",x=trans)){
      cat0("------------------------------------------------------------------------")
      cat0(" Invalid settings."                                                      )
      cat0(" - Requested transformation: ",trans ,"."                                )
      cat0(" - Variable \"mirror\":      ",mirror,"."                                )
      cat0(" - Variable \"zero\":        ",zero  ,"."                                )
      cat0(""                                                                        )
      cat0(" This transformation cannot be used with options \"mirror\" or \"zero\".")
      cat0("------------------------------------------------------------------------")
      stop(" Invalid settings in \"find_bounds\"."                                   )
   }else if (mirror && grepl(pattern="sqrt",x=trans) ){
      cat0("------------------------------------------------------------------------")
      cat0(" Invalid settings."                                                      )
      cat0(" - Requested transformation: ",trans ,"."                                )
      cat0(" - Variable \"mirror\":      ",mirror,"."                                )
      cat0(""                                                                        )
      cat0(" This transformation cannot be used with option \"mirror\"."             )
      cat0("------------------------------------------------------------------------")
      stop(" Invalid settings in \"find_bounds\"."                                   )
   }#end if (any(c(mirror,zero)) && grepl(pattern="log",x=trans))
   #---~---

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

   # If we ought to include zero, we replace either x_lwr or x_upr with zero.
   if (zero && ( (x_lwr*x_upr) > 0.) ){
      x_lwr = min(c(x_lwr,0.))
      x_upr = max(c(x_upr,0.))
   }#end if (zero && ( (x_lwr*x_upr) > 0.) )

   # Find transformation function
   trans = match.fun(trans)
  
   # Find limits and return the range.
   ans      = range(trans()$breaks(c(x_lwr,x_upr)))
   return(ans)
}#end function find_bounds
#==========================================================================================
