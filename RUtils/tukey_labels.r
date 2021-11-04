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
   # Fund number of levels and multiplication factors for data
   g_nlevels = x %>% summarise_at(group,nlevels)
   g_mult    = rev(cumprod(rev(c(g_nlevels[-1],1))))
   g_nsize   = prod(g_nlevels)

   # Make sure levels are sorted
   x     = x %>% mutate_at(group,nice_factor)

   # Initialise answer with dummy characters
   ans       = rep(x="",times=g_nsize)


   # Run Tukey's HSD test for phenological groups
   t_formula = as.formula(paste0(response," ~ ",paste(group,collapse="+")))
   t_aov     = aov(t_formula,data=x)
   t_group   = try(agricolae::HSD.test(t_aov,trt=group,group=TRUE)$groups,silent=TRUE)

   if (! ("try-error" %in% is(t_group))){
      # In case there are multiple groups, separate them.
      t_order     = do.call("rbind",strsplit(x=rownames(t_group),split=":"))
      t_order     = apply(X=t_order,MARGIN=2,FUN=as.numeric)

      # Find index for the vector (first group is the outer group)
      t_mult      = matrix(data=g_mult,nrow=nrow(t_order),ncol=ncol(t_order),byrow=TRUE)
      t_fill      = rowSums(t_order * t_mult) + 1
      ans[t_fill] = t_group$groups
   }#end if ("try-error" %in% is(t_group))

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
