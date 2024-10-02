#==========================================================================================#
#==========================================================================================#
#     This file contains some useful additional operators.                                 #
#------------------------------------------------------------------------------------------#


#----- Safe logical operators.  These will always return FALSE if x or y are not finite. --#
'%eq%' <<- function(x,y){
   if (any(c(FALSE,is.numeric(x) & is.numeric(y)),na.rm=TRUE)){
      ans = is.finite(unlist(x)) & is.finite(unlist(y)) & x == y
   }else{
      ans = ! is.na(x) & ! is.na(y) & ! is.nan(x) & ! is.nan(y) & x == y
   }#end if
   return(ans)
}#end function
'%ne%' <<- function(x,y){
   if (any(c(FALSE,is.numeric(x) & is.numeric(y)),na.rm=TRUE)){
      ans = is.finite(unlist(x)) & is.finite(unlist(y)) & x != y
   }else{
      ans = ! is.na(x) & ! is.na(y) & ! is.nan(x) & ! is.nan(y) & x != y
   }#end if
   return(ans)
}#end function
'%gt%' <<- function(x,y) is.finite(unlist(x)) & is.finite(unlist(y)) & x  > y
'%lt%' <<- function(x,y) is.finite(unlist(x)) & is.finite(unlist(y)) & x  < y
'%ge%' <<- function(x,y) is.finite(unlist(x)) & is.finite(unlist(y)) & x >= y
'%le%' <<- function(x,y) is.finite(unlist(x)) & is.finite(unlist(y)) & x <= y
#------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------#
#     Operators to test whether the values are within range or outside range.              #
#                                                                                          #
#  Operation  | TRUE when...             | If x == min(y) | If x == max(y)                 #
# -------------------------------------------------------------------------                #
#  x %wr% y   | x is within range of y   | TRUE           | TRUE                           #
#  x %wl% y   | x is within range of y   | TRUE           | FALSE                          #
#  x %wu% y   | x is within range of y   | FALSE          | TRUE                           #
#  x %ir% y   | x is inside range of y   | FALSE          | FALSE                          #
#  x %or% y   | x is outside range of y  | FALSE          | FALSE                          #
#  x %ol% y   | x is outside range of y  | FALSE          | TRUE                           #
#  x %ou% y   | x is outside range of y  | TRUE           | FALSE                          #
#  x %nr% y   | x is outside range of y  | TRUE           | TRUE                           #
#------------------------------------------------------------------------------------------#
'%wr%' <<- function(x,y){
   ans = (! (is.na(x) | is.nan(x))) & ( x >= min(y,na.rm=TRUE) & x <= max(y,na.rm=TRUE) )
   return(ans)
}#end function
'%wl%' <<- function(x,y){
   ans = (! (is.na(x) | is.nan(x))) & ( x >= min(y,na.rm=TRUE) & x <  max(y,na.rm=TRUE) )
   return(ans)
}#end function
'%wu%' <<- function(x,y){
   ans = (! (is.na(x) | is.nan(x))) & ( x >  min(y,na.rm=TRUE) & x <= max(y,na.rm=TRUE) )
   return(ans)
}#end function
'%ir%' <<- function(x,y){
   ans = (! (is.na(x) | is.nan(x))) & ( x >  min(y,na.rm=TRUE) & x <  max(y,na.rm=TRUE) )
   return(ans)
}#end function
'%or%' <<- function(x,y){
   ans = (! (is.na(x) | is.nan(x))) & ( x <  min(y,na.rm=TRUE) | x >  max(y,na.rm=TRUE) )
   return(ans)
}#end function
'%ol%' <<- function(x,y){
   ans = (! (is.na(x) | is.nan(x))) & ( x <  min(y,na.rm=TRUE) | x >= max(y,na.rm=TRUE) )
   return(ans)
}#end function
'%ou%' <<- function(x,y){
   ans = (! (is.na(x) | is.nan(x))) & ( x <= min(y,na.rm=TRUE) | x >  max(y,na.rm=TRUE) )
   return(ans)
}#end function
'%nr%' <<- function(x,y){
   ans = (! (is.na(x) | is.nan(x))) & ( x <= min(y,na.rm=TRUE) | x >= max(y,na.rm=TRUE) )
   return(ans)
}#end function
#------------------------------------------------------------------------------------------#
