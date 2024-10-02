#===~===
#===~===
#    This function is strongly based on function clusGap (package cluster), which in turn
# is based on Tibshirani et al. (2001), but it has been rewritten to work with mixed data 
# types and weighting factors.  Most arguments were renamed for readability.
#
#   Reference:
#
#   Tibshirani, R., G. Walther, and T. Hastie, 2001: Estimating the number of clusters
#      in a data set via the gap statistic. J. R. Stat. Soc. B, 63 (2), 411-423, 
#      doi:10.1111/1467-9868.00293.
#---~---
clusGapFlex <<- function ( x
                         , fun_cluster = "cluster::pam"
                         , metric      = c("euclidean", "manhattan", "gower")
                         , stand       = FALSE
                         , type        = list()
                         , weights     = NULL
                         , K_max       = 15L
                         , n_mcarlo    = 500L
                         , mc_verb     = 50L
                         , d_power     = 1L
                         , verbose     = interactive()
                         , ...
                         ){


   #---~---
   #   Save call.
   #---~---
   fun_call = match.call()
   #---~---


   #---~---
   #   Match function in case the argument is a character.
   #---~---
   if (! is.function(fun_cluster)) fun_cluster = match.fun(fun_cluster)
   #---~---


   #---~---
   #   Initial check. Input "x" must be a matrix or a data frame.
   #---~---
   if (is.data.frame(x)){
      x = x
   }else{
      x = try(as.data.frame(x),silent=FALSE)
      if ("try-error" %in% is(x)) stop(" \"x\" must be coercible to a data frame.")
   }#end if (is.data.frame(x))
   #---~---


   #---~---
   #   Define even weights in case they are not provided
   #---~---
   if (is.null(weights)) weights=rep.int(x=1.,times=ncol(x))
   #---~---



   #---~---
   #   Make sure n_mcarlo is a positive integer.
   #---~---
   if ( ( ! (n_mcarlo %eq% as.integer(n_mcarlo)) ) || ( n_mcarlo %le% 0. ) ){
      stop("Argument \"n_mcarlo\" must be a positive integer.")
   }else{
      n_mcarlo = as.integer(n_mcarlo)
   }#end if ( ( ! (n_mcarlo %eq% as.integer(B)) ) || ( B %le% 0. ) )
   #---~---


   #---~---
   #   This function calculates the dissimilarity matrix for each group and computes
   # the gap statistic.
   #---~---
   find_GapStat = function(x,metric,type,weights,d_power){
      diss = cluster::daisy(x=x,metric=metric,type=type,weights=weights)
      ans  = sum(c(as.matrix(diss))^d_power/nrow(x))
      return(ans)
   }#end function find_GapStat
   #---~---



   #---~---
   #   Define function that will compute the error measure.
   #---~---
   find_Wk = function(x,diss,metric,type,weights,k,fun,d_power,...){

      #---~---
      #   Retrieve dimensions of dissimilarity matrix
      #---~---
      n_mat   = nrow(x)
      indices = sequence(n_mat)
      #---~---


      #---~---
      #   Find the clusters
      #---~---
      if ( k %eq% 1L){
         cluster_now  = rep.int(x=1L,times=n_mat)
      }else{
         cluster_now  = fun(x=diss,k=k,...)
         cluster_name = c("clustering","cluster")
         cluster_name = cluster_name[cluster_name %in% names(cluster_now)]
         cluster_now  = cluster_now[[cluster_name]]
      }#end if (k %gt% 1)
      #---~---


      #---~---
      #   Find the gap statistic for each cluster.
      #---~---
      x_split = split(x=x,f=cluster_now)
      w_gap   = vapply( X         = x_split
                      , FUN       = find_GapStat
                      , FUN.VALUE = 0.
                      , metric    = metric
                      , type      = type
                      , weights   = weights
                      , d_power   = d_power
                      )#end vapply
      #---~---


      #---~---
      #   Report answer
      #---~---
      ans = 0.5 * sum(w_gap)
      return(ans)
      #---~---
   }#end function find_Wk
   #---~---



   #---~---
   #   This function is a generalisation of the "uniform" reference for when data may 
   # be a mix of numeric (continuous), ordinal and categorical data.
   #---~---
   UnifSampler = function(x){
      #---~---
      #   Sample size
      #---~---
      nx = length(x)
      #---~---

      #---~---
      #   Safe sample function.
      #---~---
      resample = function(x, ...) x[sample.int(n=length(x), ...)]
      #---~---

      #---~---
      #   Sample the closest to a uniform distribution, depending on the variable type.
      #---~---
      if (is.ordered(x)){
         ans = ordered(x=resample(x=levels(x),size=nx,replace=TRUE),levels=levels(x))
      }else if (is.factor(x)){
         ans = factor (x=resample(x=levels(x),size=nx,replace=TRUE),levels=levels(x))
      }else if (is.character(x) | is.logical(x)){
         ans = resample(x=sort(unique(x)),size=nx,replace=TRUE)
      }else if (is.integer(x)){
         xlwr = min(x,na.rm=TRUE)
         xupr = max(x,na.rm=TRUE)
         uniq = seq(from=xlwr,to=xupr,by=1L)
         ans  = resample(x=uniq,size=nx,replace=TRUE)
      }else if (is.numeric(x)){
         xlwr = min(x,na.rm=TRUE)
         xupr = max(x,na.rm=TRUE)
         ans  = runif(n=nx,min=xlwr,max=xupr)
      }#end if (is.ordered(x))
      #---~---

      return(ans)
   }#end UnifSampler
   #---~---


   #---~---
   #   Calculate the dissimilarity matrix for input data
   #---~---
   if (verbose) cat(" + Find dissimilarity matrix.\n")
   diss = cluster::daisy(x=x,metric=metric,type=type,weights=weights)
   #---~---


   #---~---
   #   Find the gap statistic for each number of clusters.
   #---~---
   if (verbose) cat(" + Clustering k = ",sep="")
   logW   = rep(x=NA_real_,times=K_max)
   for (k in sequence(K_max)){
      if (verbose) cat(" ",k,",",sep="")
      W_now   = find_Wk( x       = x
                       , diss    = diss
                       , metric  = metric
                       , type    = type
                       , weights = weights
                       , k       = k
                       , fun     = fun_cluster
                       , d_power = d_power
                       ,...)
      logW[k] = log(W_now)
   }#end for (k in sequence(K_max))
   if (verbose) cat(" Done!\n",sep="")
   #---~---


   #---~---
   #   Find the Monte Carlo samples of reference features uniformly spread across the
   # range of observations. We generalise the sample for mixed data types by randomly 
   # sampling the available levels of factorial and ordered variables. 
   #---~---
   if (verbose) cat("Monte Carlo sample [one \".\" per sample] ; b = ",sep="")
   mcarlo_logW = matrix(data=NA_real_,nrow=n_mcarlo,ncol=K_max)
   for (m in sequence(n_mcarlo)){
      #---~---
      #   Retrieve sample.
      #---~---
      x_mcarlo = data.frame(lapply(X=x,FUN=UnifSampler))
      m_show   = (m %in% c(1L,n_mcarlo)) || ( (m %% mc_verb) %eq% 0. )
      #---~---


      #---~---
      #   Find dissimilarity matrix.
      #---~---
      diss_mcarlo = cluster::daisy(x=x_mcarlo,metric=metric,type=type,weights=weights)
      #---~---


      #---~---
      #   Find the error measure for this sample (all values of k to be tested).
      #---~---
      for (k in sequence(K_max)){
         W_now            = find_Wk( x       = x_mcarlo
                                   , diss    = diss_mcarlo
                                   , metric  = metric
                                   , type    = type
                                   , weights = weights
                                   , k       = k
                                   , fun     = fun_cluster
                                   , d_power = d_power
                                   , ...
                                   )#find_Wk
         mcarlo_logW[m,k] = log(W_now)
      }#end for (k in sequence(K_max))
      #---~---

      #---~---
      #   Report progress (if sought).
      #---~---
      if (verbose) cat(if(m_show){paste0(" ",m," ")}else{"."})
      #---~---
   }#end for (b in sequence(n_mcarlo))
   cat(" Done!\n")
   #---~---


   #---~---
   #   Find the expected error measure and standard error.
   #---~---
   E_logW = colMeans(mcarlo_logW)
   SE_sim = sqrt((1.+1./n_mcarlo) * apply(X=mcarlo_logW,MARGIN=2,FUN=var))
   gap    = E_logW - logW
   #---~---



   #---~---
   #   Save answer in an object of type clusGap
   #---~---
   ans = structure( class = "clusGap"
                  , list( Tab        = cbind( logW   = logW
                                            , E.logW = E_logW
                                            , gap    = gap
                                            , SE.sim = SE_sim
                                            )#end cbind
                        , call       = fun_call
                        , spaceH0    = "original"
                        , n          = nrow(x)
                        , B          = n_mcarlo
                        , K.max      = K_max
                        , FUNcluster = fun_cluster
                        )#end list
                  )#end structure
   return(ans)
   #---~---
}#end function clusGapFlex
#---~---
