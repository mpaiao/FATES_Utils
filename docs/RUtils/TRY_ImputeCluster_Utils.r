#==========================================================================================
#   Additional TRY utilities for imputation and cluster analysis
#
#   Author: Marcos Longo 
#   Email:  m l o n g o a@t l b l d.o.t g o v
#
#   Date: 20-Sep-2023
#
#------------------------------------------------------------------------------------------





#---~---
#   Function that determines the fraction of observed (vs. imputed)
#---~---
fracObserved = function(x){
   ans = sum(! ( is.na(x) | is.nan(x) ) ) / length(x)
   return(ans)
}#end function fracObserved
#---~---





#---~---
#   Function that determines the type of variable
#---~---
findType     = function(x){
   if (is.ordered(x) || is.integer(x)){
      ans = "ordered"
   }else if (is.factor(x) || is.character(x)){
      ans = "factor"
   }else if (is.logical(x)){
      ans = "asymm"
   }else{
      ans = "numeric"
   }#end if (is.ordered(x) || is.integer(x))
   return(ans)
}#end function findType
#---~---





#---~---
#   Function to calculate the entropy of the categorical data (Shannon's Index). Sometimes
# the estimated probability of a given level will be zero, which would cause entropy to be
# undefined. In these cases, we take use that lim(p->0) p*ln(p) = 0, and skip them.
#---~---
findEntropy  <<- function(x){
   if (is.factor(x)){
      p   = table(x)/length(x)
      use = p %gt% 0
      ans = -sum(p[use]*log(p[use]))
   }else{
      ans = NA_real_
   }#end if (is.factor(x))
   return(ans)
}#end function findEntropy
#---~---





#---~---
#   Function to calculate the evenness of the categorical data
#---~---
findEvenness <<- function(x){ 
   if (is.factor(x)){
      #---~---
      #   Find evenness. When nlevels(x) = 1, Entropy = 0 and so is the log(nlevels). 
      # For practical purposes, we assume that this converges to 0 and report 0.
      #---~---
      ans = findEntropy(x)/log(max(2L,nlevels(x)))
      #---~---
   }else{
      #---~---
      #   Not a variable for which we can calculate evenness.
      #---~---
      ans = NA_real_
      #---~---
   }#end if (is.factor(x))
   return(ans)
}#end function findEvenness
#---~---





#---~---
#   Function to calculate the variability of the data. For numerical variables, we
# use the true standard deviation divided by the mean of the absolute values (to avoid
# singularities). For categorical variables, we use the evenness
#---~---
findVariability <<- function(x,na.rm=FALSE){
   #---~---
   #   If we are to ignore NA, we delete the values here. If nothing is left, return 
   # NA as the answer
   #---~---
   if (na.rm) x = x[! is.na(x)]
   #---~---



   #---~---
   #   Decide what to do based on variable type.
   #---~---
   if (length(x) == 0L){
      #---~---
      #   Empty variable, return NA.
      #---~---
      ans = NA_real_
      #---~---
   }else if (is.double(x)){
      #---~---
      #   Answer is the standard deviation divided by the mean absolute value of x. When
      # only one valid value exists, set variability to zero.
      #---~---
      nx_fine = sum(! is.na(x))
      if (nx_fine > 1L){
         ans = sd(x,na.rm=TRUE) / mean(abs(x),na.rm=TRUE)
      }else{
         ans = 0.
      }#end if (nx_fine > 1L)
      #---~---
   }else if (is.factor(x)){
      #---~---
      #   Factor (ordered or not). Assume variability to be evenness.
      #---~---
      ans = findEvenness(x)
      #---~---
   }else if (is.character(x) || is.integer(x) || is.logical(x)){
      #---~---
      #   Character or integer. Coerce variable to factor and assume variability to be 
      # evenness.
      #---~---
      xfact = factor(x,levels=sort(unique(x)))
      ans   = findEvenness(xfact)
      #---~---
   }else{
      #---~---
      #   Something else. No idea, assume NA.
      #---~---
      ans   = NA_real_
      #---~---
   }#end if (is.double(x))
   #---~---


   return(ans)
}#end function findVariability
#---~---

