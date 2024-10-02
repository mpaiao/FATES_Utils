#==========================================================================================
# This function creates labels for groups using Tukey's honestly significantly different
# test.
#
# Input:
# x        - a tibble object 
# response - response variable. This should be a character variable (scalar) with a valid
#            variable name in x 
# group    - variable(s) used for grouping. This should be a scalar or a vector, type 
#            character with valid variable names in x.
#
# The output is a vector with characters containing letters for groups.
#------------------------------------------------------------------------------------------
tukey_labels <<- function(x,response,group){
   #---~---
   #   Find number of levels and multiplication factors for data
   #---~---
   g_nlevels = x %>% summarise(across(all_of(c(group)), ~ nlevels(.x)))
   g_mult    = rev(cumprod(rev(c(g_nlevels[-1],1))))
   g_nsize   = prod(g_nlevels)
   #---~---

   #---~---
   #   Make sure levels are sorted
   #---~---
   x     = x %>% mutate(across(all_of(group), ~ nice_factor(.x)))
   #---~---


   #---~---
   #   Initialise answer with dummy characters
   #---~---
   ans       = rep(x="",times=g_nsize)
   #---~---


   #---~---
   #   Run the analysis of variance, and see if it worked before proceeding.
   #---~---
   t_formula = as.formula(paste0(response," ~ ",paste(group,collapse="+")))
   t_aov     = try(aov(t_formula,data=x),silent=TRUE)
   #---~---

   #---~---
   #   AOV may fail when some groups are all NA. In these cases, skip the test
   #---~---
   if (! ("try-error" %in% is(t_aov))){


      #---~---
      #   Run Tukey's HSD test for groups
      #---~---
      t_group   = try(agricolae::HSD.test(t_aov,trt=group,group=TRUE)$groups,silent=TRUE)
      #---~---


      #---~---
      #   Tukey's HSD sometimes fails (mostly when groups are undefined. In these cases,
      # return nothing.
      #---~---
      if (! ("try-error" %in% is(t_group))){
         #---~---
         #   In case there are multiple groups, separate them.
         #---~---
         t_order     = do.call("rbind",strsplit(x=rownames(t_group),split=":"))
         t_order     = apply(X=t_order,MARGIN=2,FUN=as.numeric)
         #---~---

         #---~---
         #   Find index for the vector (first group is the outer group)
         #---~---
         t_mult      = matrix(data=g_mult,nrow=nrow(t_order),ncol=ncol(t_order),byrow=TRUE)
         t_fill      = rowSums(t_order * t_mult) + 1
         ans[t_fill] = t_group$groups
         #---~---
      }#end if ("try-error" %in% is(t_group))
      #---~---
   }#end if (! ("try-error" %in% is(t_aov)))
   #---~---

   return(ans)
}#end function tukey_labels
#==========================================================================================


#==========================================================================================
# Internal function to ensure labels are sorted for output
#------------------------------------------------------------------------------------------
nice_factor <<- function(x){
   x_levels    = levels(x)
   x_nlevels   = length(x_levels)
   x_digits    = ceiling(log10(x_nlevels))
   x_format    = paste0("%",x_digits,".",x_digits,"i")
   x_nouveau   = sprintf(x_format,sequence(x_nlevels)-1)
   ans         = x
   levels(ans) = x_nouveau

   return(ans)
}#end function nice_factor
#==========================================================================================
