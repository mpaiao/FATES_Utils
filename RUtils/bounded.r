#==========================================================================================
#    This function applies bounds to data to avoid the scale being too stretched due to 
# outliers.
#------------------------------------------------------------------------------------------
bounded <<- function(x,x_lwr,x_upr){
  # The offset is to ensure the data will not be skipped in case the data are exactly 
  #    at the lower or upper limit
  x_off = sqrt(.Machine$double.eps) * (x_upr - x_lwr)

  # Bounded data
  ans   = 0.*x + pmax(x_lwr+x_off,pmin(x_upr-x_off,x,na.rm=FALSE),na.rm=FALSE)
  return(ans)
}#end bounded
#==========================================================================================
