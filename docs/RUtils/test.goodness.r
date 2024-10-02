#==========================================================================================#
#==========================================================================================#
#     This function computes several metrics to test the goodness of fit.  The list is     #
# not complete, contributions are welcome.                                                 #
#                                                                                          #
#  INPUT:                                                                                  #
#  ~ x.mod          -- the modelled values of x                                            #
#  ~ x.obs          -- the observed values of x                                            #
#  ~ n.parameters   -- number of parameters (if the number of parameters is unknown, we    #
#                      don't assume any parameters).                                       #
#  ~ out.dfr        -- output as a data frame? This can be either a logical or a character #
#                      variable.                                                           #
#                      Logical: TRUE (FALSE) means output as data frame (list)             #
#                      Character: "list", "data.frame", "vector" for the actual type.      #
#------------------------------------------------------------------------------------------#
test.goodness <<- function(x.mod,x.obs,x.sigma=NULL,n.parameters=NULL,out.dfr=FALSE){

   #----- Make sure out.dfr is properly set. ----------------------------------------------#
   if (is.logical(out.dfr)){
      out.type = if(out.dfr){"data.frame"}else{"list"}
   }else{
      out.type = match.arg(out.dfr,choices=c("list","vector","data.frame"))
   }#end if (is.logical(out.dfr))
   #---------------------------------------------------------------------------------------#


   #---- Crash if the x.mod and x.obs don't have the same size and class. -----------------#
   dlength = (length(x.mod) - length(x.obs)) != 0
   dclass  = typeof(x.mod) != typeof(x.obs)
   if (dlength || dclass){
      stop (" x.mod and x.obs must have the same size and class","\n")
   }#end if
   #---------------------------------------------------------------------------------------#


   #---- Crash if the x.sigma and x.obs don't have the same size and class. ---------------#
   if (! is.null(x.sigma)){
      dlength = (length(x.mod) - length(x.sigma)) != 0
      dclass  = typeof(x.mod) != typeof(x.sigma)
      if (dlength || dclass){
         stop (" x.mod and x.sigma must have the same size and class","\n")
      }#end if
      #------------------------------------------------------------------------------------#
   }#end if (! is.null(x.sigma)
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #     Copy the original observations to temporary vectors, and select only data that    #
   # are pairwise defined.                                                                 #
   #---------------------------------------------------------------------------------------#
   o.mod = x.mod
   o.obs = x.obs
   x.sel = is.finite(x.mod) & is.finite(x.obs)
   x.mod = x.mod[x.sel]
   x.obs = x.obs[x.sel]



   #---------------------------------------------------------------------------------------#
   #     Find the number of observations, and tally the degrees of freedom for both total  #
   # and explained variance.                                                               #
   #---------------------------------------------------------------------------------------#
   n.fine = sum(x.sel)
   df.tot = n.fine - 1L
   if (is.null(n.parameters)){
      df.err = n.fine - 1L
   }else{
      df.err = n.fine - n.parameters
   }#end if
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #     Check whether we have enough valid observations before proceeding.                #
   #---------------------------------------------------------------------------------------#
   if (df.err > 1){
      #------------------------------------------------------------------------------------#
      #      Find the total and model residuals.                                           #
      #------------------------------------------------------------------------------------#
      x.res = x.obs - x.mod
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #      Find the four moments of the distributions.                                   #
      #------------------------------------------------------------------------------------#
      obs.moment = c( mean     = mean(x.obs), variance = var (x.obs)
                    , skewness = skew(x.obs), kurtosis = kurt(x.obs) )
      mod.moment = c( mean     = mean(x.mod), variance = var (x.mod)
                    , skewness = skew(x.mod), kurtosis = kurt(x.mod) )
      res.moment = c( mean     = mean(x.res), variance = var (x.res)
                    , skewness = skew(x.res), kurtosis = kurt(x.res) )
      #------------------------------------------------------------------------------------#




      #------------------------------------------------------------------------------------#
      #     Find the mean bias, standard deviation of the residuals, and the support for   #
      # the errors being normally distributed around the mean.                             #
      #------------------------------------------------------------------------------------#
      bias       = -mean(x.res)
      sigma      =    sd(x.res)
      lsq.lnlike = sum(dnorm(x.res,mean=0,sd=sigma,log=TRUE))
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #     Find the mean square error of the estimator, and the root mean square error.   #
      #------------------------------------------------------------------------------------#
      mae         = mean(abs(x.res))
      mse         = bias^2 + sigma^2
      rmse        = sqrt(mse)
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #     Find the correlation between the model and observations.                       #
      #------------------------------------------------------------------------------------#
      cor.pearson  = cor(x=x.obs,y=x.mod,use="pairwise.complete.obs",method="pearson" )
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #     Find the coefficient of determination (R2).  If the number of parameters is    #
      # given, then we find the adjusted R^2 (the one that penalises due to the number of  #
      # parameters).  Otherwise we can't correct, so we just compare the sum of the        #
      # variances.                                                                         #
      #------------------------------------------------------------------------------------#
      x.tot     = x.obs - mean(x.obs)
      ss.tot    = sum(x.tot^2)
      ss.err    = sum(x.res^2)
      r.squared = 1. - df.tot * ss.err / ( df.err * ss.tot )
      #------------------------------------------------------------------------------------#


      #------------------------------------------------------------------------------------#
      #     Compute the adjusted root mean square error.                                   #
      #------------------------------------------------------------------------------------#
      adj.rmse  = sqrt(ss.err/df.err)
      #------------------------------------------------------------------------------------#


      #------------------------------------------------------------------------------------#
      #     Find the estimator's fraction of variance unexplained (FVU).  Because we use   #
      # MSE instead of ss.err, this is not 1 - R2.                                         #
      #------------------------------------------------------------------------------------#
      fvue        = mse / obs.moment["variance"]
      names(fvue) = NULL
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #     Find relative bias.  Because values may be zero and cause singularity, we      #
      # check whether x switches signs.                                                    #
      #------------------------------------------------------------------------------------#
      x.min   = sqrt(.Machine$double.eps) * mean(abs(x.obs))
      x.scale = pmax(x.min,abs(x.obs)) + 0. * x.obs
      sbias   = mean(x.res/x.scale)
      #------------------------------------------------------------------------------------#


      #------------------------------------------------------------------------------------#
      #     Run the Shapiro-Wilk test to determine whether the residuals are normally      #
      # distributed or not.                                                                #
      #------------------------------------------------------------------------------------#
      sw.test      = sw.test(x.res)
      sw.statistic = sw.test$statistic
      sw.p.value   = sw.test$p.value
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #      Run the Kolmogorov-Smirnov test to compare the distributions.                 #
      #------------------------------------------------------------------------------------#
      this.ks             = ks.test(x=x.obs,y=x.mod)
      ks.statistic        = this.ks$statistic
      ks.p.value          = this.ks$p.value
      names(ks.statistic) = NULL
      #------------------------------------------------------------------------------------#
   }else{
      #------------------------------------------------------------------------------------#
      #    Not enough data points.                                                         #
      #------------------------------------------------------------------------------------#
      obs.moment   = c(mean=NA_real_,variance=NA_real_,skewness=NA_real_,kurtosis=NA_real_)
      mod.moment   = c(mean=NA_real_,variance=NA_real_,skewness=NA_real_,kurtosis=NA_real_)
      res.moment   = c(mean=NA_real_,variance=NA_real_,skewness=NA_real_,kurtosis=NA_real_)
      bias         = NA_real_
      sigma        = NA_real_
      lsq.lnlike   = NA_real_
      mae          = NA_real_
      mse          = NA_real_
      rmse         = NA_real_
      cor.pearson  = NA_real_
      ss.tot       = NA_real_
      ss.err       = NA_real_
      r.squared    = NA_real_
      adj.rmse     = NA_real_
      fvue         = NA_real_
      sbias        = NA_real_
      sw.statistic = NA_real_
      sw.p.value   = NA_real_
      ks.statistic = NA_real_
      ks.p.value   = NA_real_
      #------------------------------------------------------------------------------------#
   }#end if
   #---------------------------------------------------------------------------------------#


   #---- Set weighted observations for finding weighted R2, bias, MAE and RMSE. -----------#
   if (is.null(x.sigma)){
      #----- Assume weighted values to be the same as the original ones. ------------------#
      w.sel = is.finite(x.mod) & is.finite(x.obs)
      w.obs = x.obs[w.sel]
      w.mod = x.mod[w.sel]
      #------------------------------------------------------------------------------------#
   }else{
      #----- Find associated weights, and re-create the vectors x.mod and x.obs. ----------#
      w.sel = ( x.sigma %gt% 0. ) & is.finite(x.mod) & is.finite(x.obs)
      w.obs = x.obs[w.sel] / x.sigma[w.sel]
      w.mod = x.mod[w.sel] / x.sigma[w.sel]
      #------------------------------------------------------------------------------------#
   }#end if (! is.null(x.sigma)
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #     Find the number of observations, and tally the degrees of freedom for both total  #
   # and explained variance.                                                               #
   #---------------------------------------------------------------------------------------#
   w.n.fine = sum(w.sel)
   w.df.tot = w.n.fine - 1L
   if (is.null(n.parameters)){
      w.df.err = w.n.fine - 1L
   }else{
      w.df.err = w.n.fine - n.parameters
   }#end if
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #     Check whether we have enough valid observations before proceeding.                #
   #---------------------------------------------------------------------------------------#
   if (w.df.err > 1){
      #------------------------------------------------------------------------------------#
      #      Find the total and model residuals.                                           #
      #------------------------------------------------------------------------------------#
      w.res = w.obs - w.mod
      #------------------------------------------------------------------------------------#




      #------------------------------------------------------------------------------------#
      #     Find the mean bias, standard deviation of the residuals, and the support for   #
      # the errors being normally distributed around the mean.                             #
      #------------------------------------------------------------------------------------#
      w.bias       = -mean(w.res)
      w.sigma      =    sd(w.res)
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #     Find the mean square error of the estimator, and the root mean square error.   #
      #------------------------------------------------------------------------------------#
      w.mae  = mean(abs(w.res))
      w.rmse = sqrt(w.bias^2 + w.sigma^2)
      #------------------------------------------------------------------------------------#



      #------------------------------------------------------------------------------------#
      #     Find the coefficient of determination (R2).  If the number of parameters is    #
      # given, then we find the adjusted R^2 (the one that penalises due to the number of  #
      # parameters).  Otherwise we can't correct, so we just compare the sum of the        #
      # variances.                                                                         #
      #------------------------------------------------------------------------------------#
      w.tot       = w.obs - mean(w.obs)
      w.ss.tot    = sum(w.tot^2)
      w.ss.err    = sum(w.res^2)
      w.r.squared = 1. - w.df.tot * w.ss.err / ( w.df.err * w.ss.tot )
      #------------------------------------------------------------------------------------#


      #------------------------------------------------------------------------------------#
      #     Compute the adjusted root mean square error.                                   #
      #------------------------------------------------------------------------------------#
      w.adj.rmse  = sqrt(w.ss.err/w.df.err)
      #------------------------------------------------------------------------------------#
   }else{
      #------------------------------------------------------------------------------------#
      #    Not enough data points.                                                         #
      #------------------------------------------------------------------------------------#
      w.bias      = NA_real_
      w.sigma     = NA_real_
      w.mae       = NA_real_
      w.rmse      = NA_real_
      w.r.squared = NA_real_
      w.adj.rmse  = NA_real_
      #------------------------------------------------------------------------------------#
   }#end if
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #     Return everything to the user.                                                    #
   #---------------------------------------------------------------------------------------#
   if (out.type %in% "data.frame"){
      ans = data.frame( n             = n.fine
                      , p             = n.fine - df.err
                      , df.tot        = df.tot
                      , df.err        = df.err
                      , obs.mean      = obs.moment[1]
                      , obs.sdev      = sqrt(obs.moment[2])
                      , obs.skew      = obs.moment[3]
                      , obs.kurt      = obs.moment[4]
                      , mod.mean      = mod.moment[1]
                      , mod.sdev      = sqrt(mod.moment[2])
                      , mod.skew      = mod.moment[3]
                      , mod.kurt      = mod.moment[4]
                      , res.mean      = res.moment[1]
                      , res.sdev      = sqrt(res.moment[2])
                      , res.skew      = res.moment[3]
                      , res.kurt      = res.moment[4]
                      , bias          = bias
                      , sigma         = sigma
                      , lsq.lnlike    = lsq.lnlike
                      , mae           = mae
                      , mse           = mse
                      , rmse          = rmse
                      , cor           = cor.pearson
                      , ss.tot        = ss.tot
                      , ss.err        = ss.err
                      , r.squared     = r.squared
                      , adj.rmse      = adj.rmse
                      , fvue          = fvue
                      , sbias         = sbias
                      , sw.statistic  = sw.statistic
                      , sw.p.value    = sw.p.value
                      , ks.statistic  = ks.statistic
                      , ks.p.value    = ks.p.value
                      , wgt.bias      = w.bias
                      , wgt.sigma     = w.sigma
                      , wgt.mae       = w.mae
                      , wgt.rmse      = w.rmse
                      , wgt.r.squared = w.r.squared
                      , wgt.adj.rmse  = w.adj.rmse
                      )#end list
   }else if (out.type %in% "vector"){
      ans = c( n             = n.fine
             , p             = n.fine - df.err
             , df.tot        = df.tot
             , df.err        = df.err
             , obs.mean      = obs.moment[1]
             , obs.sdev      = sqrt(obs.moment[2])
             , obs.skew      = obs.moment[3]
             , obs.kurt      = obs.moment[4]
             , mod.mean      = mod.moment[1]
             , mod.sdev      = sqrt(mod.moment[2])
             , mod.skew      = mod.moment[3]
             , mod.kurt      = mod.moment[4]
             , res.mean      = res.moment[1]
             , res.sdev      = sqrt(res.moment[2])
             , res.skew      = res.moment[3]
             , res.kurt      = res.moment[4]
             , bias          = bias
             , sigma         = sigma
             , lsq.lnlike    = lsq.lnlike
             , mae           = mae
             , mse           = mse
             , rmse          = rmse
             , cor           = cor.pearson
             , ss.tot        = ss.tot
             , ss.err        = ss.err
             , r.squared     = r.squared
             , adj.rmse      = adj.rmse
             , fvue          = fvue
             , sbias         = sbias
             , sw.statistic  = sw.statistic
             , sw.p.value    = sw.p.value
             , ks.statistic  = ks.statistic
             , ks.p.value    = ks.p.value
             , wgt.bias      = w.bias
             , wgt.sigma     = w.sigma
             , wgt.mae       = w.mae
             , wgt.rmse      = w.rmse
             , wgt.r.squared = w.r.squared
             , wgt.adj.rmse  = w.adj.rmse
             )#end c
   }else{
      ans = list ( n             = n.fine
                 , p             = n.fine - df.err
                 , df.tot        = df.tot
                 , df.err        = df.err
                 , obs.moment    = obs.moment
                 , mod.moment    = mod.moment
                 , res.moment    = res.moment
                 , bias          = bias
                 , sigma         = sigma
                 , lsq.lnlike    = lsq.lnlike
                 , mae           = mae
                 , mse           = mse
                 , rmse          = rmse
                 , cor           = cor.pearson
                 , ss.tot        = ss.tot
                 , ss.err        = ss.err
                 , r.squared     = r.squared
                 , adj.rmse      = adj.rmse
                 , fvue          = fvue
                 , sbias         = sbias
                 , sw.statistic  = sw.statistic
                 , sw.p.value    = sw.p.value
                 , ks.statistic  = ks.statistic
                 , ks.p.value    = ks.p.value
                 , wgt.bias      = w.bias
                 , wgt.sigma     = w.sigma
                 , wgt.mae       = w.mae
                 , wgt.rmse      = w.rmse
                 , wgt.r.squared = w.r.squared
                 , wgt.adj.rmse  = w.adj.rmse
                 )#end list
   }#end if
   return(ans)
   #---------------------------------------------------------------------------------------#
}#end function
#==========================================================================================#
#==========================================================================================#
