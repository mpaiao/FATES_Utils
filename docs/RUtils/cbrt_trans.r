#==========================================================================================
#    This function allows generating cube root-looking scales. This helps visualising 
# quantities where relative differences are more important than magnitude but values can
# be positive or negative.
#------------------------------------------------------------------------------------------
cbrt_trans <<- function(){
   # Define transformation and its inverse function
   trans = function(x){ sign(x) * abs(x)^(1/3)}
   inv   = function(x){ x^3}
   
   breaks = function(x,n=5){rev(-scales::log_breaks()(x=-x,n=n))}
   # Define transformation
   ans   = scales::trans_new( name      = "cbrt"
                            , transform = trans
                            , inverse   = inv
                            , domain    = c(-Inf,+Inf))
   return(ans)
}#end function cbrt_trans
#==========================================================================================
