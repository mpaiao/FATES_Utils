#==========================================================================================
#    This function allows generating logarithmic-looking scales for numbers that are 
# exclusively negative. This helps visualising quantities such as soil matric potential
# and soil depth.
#------------------------------------------------------------------------------------------
neglog_trans <<- function(){
   # Define transformation and its inverse function
   trans = function(x){ - log10(-x)}
   inv   = function(x){ - 10^(-x)}
   
   breaks = function(x,n=5){rev(-scales::log_breaks()(x=-x,n=n))}
   # Define transformation
   ans   = scales::trans_new( name = "neglog"
                            , transform = trans
                            , inverse   = inv
                            , breaks    = breaks
                            , domain    = c(-Inf,-1e-100))
   return(ans)
}#end function neglog_trans
#==========================================================================================
