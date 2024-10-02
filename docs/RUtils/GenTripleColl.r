#==========================================================================================
#==========================================================================================
#     This function computes the uncertainties using the generalised triple collocation 
# approach, which accounts for auto-correlation of errors/residuals.  Currently the 
# approach works only for data that are complete.
#
# Reference:
# 
# Dong, J., and W. T. Crow, 2017: An improved triple collocation analysis algorithm for 
#    decomposing au- tocorrelated and white soil moisture retrieval errors. J. Geophys. 
#    Res.-Atmos., 122 (24), 13081--13094, doi:10.1002/2017JD027387.
#------------------------------------------------------------------------------------------
GenTripleColl <<- function(x,y,z,silent=FALSE,nmin=5L){
   #---~---
   #   Keep just the times when all data sets have valid points.
   #---~---
   fine   = is.finite(x) & is.finite(y) & is.finite(z)
   xfine = x[fine]
   yfine = y[fine]
   zfine = z[fine]
   n     = sum(fine)
   #---~---


   #---~---
   #   Make sure we have enough data to estimate uncertainties
   #---~---
   if (n %ge% nmin){

      #---~---
      #   Calculate the averages of each time series.
      #---~---
      xbar = mean(xfine)
      ybar = mean(yfine)
      zbar = mean(zfine)
      #---~---


      #---~---
      #   Find co-variances
      #---~---
      sigmaxx = mean(xfine*xfine) - xbar*xbar
      sigmaxy = mean(xfine*yfine) - xbar*ybar
      sigmaxz = mean(xfine*zfine) - xbar*zbar
      sigmayy = mean(yfine*yfine) - ybar*ybar
      sigmayz = mean(yfine*zfine) - ybar*zbar
      sigmazz = mean(zfine*zfine) - zbar*zbar
      #---~---


      #---~---
      #   Find scaling factors and the scaled time series
      #---~---
      bstarx = 1.
      bstary = sigmaxz / sigmayz
      bstarz = sigmaxy / sigmayz
      xscale = xfine
      yscale = bstary * (yfine - ybar) + xbar
      zscale = bstarz * (zfine - zbar) + xbar
      #---~---


      #---~---
      #   Find the serial lag-1 error autocovariance
      #---~---
      lag_x   = 1. / (n - 1) * sum((xscale[-1L]*yscale[-1L])*(xscale[-n]*zscale[-n]))
      lag_y   = 1. / (n - 1) * sum((yscale[-1L]*xscale[-1L])*(yscale[-n]*zscale[-n]))
      lag_z   = 1. / (n - 1) * sum((zscale[-1L]*xscale[-1L])*(zscale[-n]*yscale[-n]))
      #---~---


      #---~---
      #   Find the autocorrelation 
      #---~---
      if (lag_x %ge% 0.){
         auto_x = lag_x / n * sum((xscale[-n]*yscale[-n])*(xscale[-n]*zscale[-n]))
      }else{
         auto_x = 0.
      }#end if (lag_x %ge% 0.)
      if (lag_y %ge% 0.){
         auto_y = lag_y / n * sum((yscale[-n]*xscale[-n])*(yscale[-n]*zscale[-n]))
      }else{
         auto_y = 0.
      }#end if (lag_y %ge% 0.)
      if (lag_z %ge% 0.){
         auto_z = lag_z / n * sum((zscale[-n]*xscale[-n])*(zscale[-n]*yscale[-n]))
      }else{
         auto_z = 0.
      }#end if (lag_y %ge% 0.)
      #---~---


      #---~---
      #   Find the error variance using the difference notation.
      #---~---
      sigma2_x = mean((xscale-yscale)*(xscale-zscale)) / bstarx^2
      sigma2_y = mean((yscale-xscale)*(yscale-zscale)) / bstary^2
      sigma2_z = mean((zscale-xscale)*(zscale-yscale)) / bstarz^2
      #---~---


      #---~---
      #   Return the errors, but check whether they are positive. If they are not, issue
      # warnings.
      #---~---
      if ((! silent) && (! all(c(sigma2_x,sigma2_y,sigma2_z) %ge% 0.))){
         warning(" Negative error variance. Triple collocation assumption likely violated.")
      }#end if (! all(c(sigma2_x,sigma2_y,sigma2_z) %ge% 0.))
      sigma_x = if(sigma2_x %ge% 0.){sqrt(sigma2_x*(1.-auto_x^2))}else{NA_real_}
      sigma_y = if(sigma2_y %ge% 0.){sqrt(sigma2_y*(1.-auto_y^2))}else{NA_real_}
      sigma_z = if(sigma2_z %ge% 0.){sqrt(sigma2_z*(1.-auto_z^2))}else{NA_real_}
      #---~---
   }else{
      #---~---
      #   Insufficient number of points. Return nothing.
      #---~---
      sigma_x = NA_real_
      sigma_y = NA_real_
      sigma_z = NA_real_
      #---~---
   }#end if (n_fine %ge% nmin)
   #---~---


   #---~---
   #   Return answer
   #---~---
   ans     = data.frame(sigma_x=sigma_x,sigma_y=sigma_y,sigma_z=sigma_z)
   return(ans)
   #---~---

}#end function GenTripleColl
#==========================================================================================
#==========================================================================================
