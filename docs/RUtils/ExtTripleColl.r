#==========================================================================================
#==========================================================================================
#     This function computes the uncertainties using the extended triple collocation 
# approach. The references below provide additional information about the assumptions and
# method implementation.
#
# McColl, K. A., J. Vogelzang, A. G. Konings, D. Entekhabi, M. Piles, and A. Stoffelen, 
#    2014: Extended triple collocation: Estimating errors and correlation coefficients
#    with respect to an unknown target. Geophys. Res. Lett., 41 (17), 6229--6236, 
#    doi:10.1002/2014GL061322.
#
# Gruber, A., C.-H. Su, S. Zwieback, W. Crow, W. Dorigo, and W. Wagner, 2016: Recent 
#    advances in (soil moisture) triple collocation analysis. Int. J. Appl. Earth Obs. 
#    Geoinf., 45 (B), 200--211, doi:10.1016/j.jag.2015.09.002.
#------------------------------------------------------------------------------------------
ExtTripleColl <<- function(x,y,z,silent=FALSE,nmin=5L){
   #---~---
   #   Keep just the times when all data sets have valid points.
   #---~---
   fine   = is.finite(x) & is.finite(y) & is.finite(z)
   x_fine = x   [fine]
   y_fine = y   [fine]
   z_fine = z   [fine]
   n_fine = sum(fine)
   #---~---


   #---~---
   #   Make sure we have enough data to estimate uncertainties
   #---~---
   if (n_fine %ge% nmin){

      #---~---
      #   Calculate the averages of each time series.
      #---~---
      x_bar = mean(x_fine,na.rm=TRUE)
      y_bar = mean(y_fine,na.rm=TRUE)
      z_bar = mean(z_fine,na.rm=TRUE)
      #---~---

      #---~---
      #   Find co-variances
      #---~---
      sigma_xx = mean(x_fine*x_fine,na.rm=TRUE) - x_bar*x_bar
      sigma_xy = mean(x_fine*y_fine,na.rm=TRUE) - x_bar*y_bar
      sigma_xz = mean(x_fine*z_fine,na.rm=TRUE) - x_bar*z_bar
      sigma_yy = mean(y_fine*y_fine,na.rm=TRUE) - y_bar*y_bar
      sigma_yz = mean(y_fine*z_fine,na.rm=TRUE) - y_bar*z_bar
      sigma_zz = mean(z_fine*z_fine,na.rm=TRUE) - z_bar*z_bar
      #---~---


      #---~---
      #   Find the error variance using the covariance notation.
      #---~---
      sigma2_x    = sigma_xx - sigma_xy*sigma_xz / sigma_yz
      sigma2_y    = sigma_yy - sigma_xy*sigma_yz / sigma_xz
      sigma2_z    = sigma_zz - sigma_xz*sigma_yz / sigma_xy
      sigma2_fine = all(c(sigma2_x,sigma2_y,sigma2_z) %ge% 0.)
      #---~---

      #---~---
      #   Return the errors, but check whether they are positive. If they are not, issue
      # warnings.
      #---~---
      if (! ( sigma2_fine || silent ) ){
         warning(" Negative error variance. Triple collocation assumption likely violated.")
      }#end if (! all(c(sigma2_x,sigma2_y,sigma2_z) %ge% 0.))
      sigma_x = if(sigma2_x %ge% 0.){sqrt(sigma2_x)}else{NA_real_}
      sigma_y = if(sigma2_y %ge% 0.){sqrt(sigma2_y)}else{NA_real_}
      sigma_z = if(sigma2_z %ge% 0.){sqrt(sigma2_z)}else{NA_real_}
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

}#end function ExtTripleColl
#==========================================================================================
#==========================================================================================
