#==========================================================================================
#   Additional TRY utilities for fitting allometric models
#
#   Author: Marcos Longo 
#   Email:  m l o n g o a@t l b l d.o.t g o v
#
#   Date: 17-Jan-2023
#
#------------------------------------------------------------------------------------------



#---~---
#     This function fits multiple allometric models based on data, and retrieves the one
# with the most parsimonious yet informative model, using either homoscedastic or 
# heteroscedastic distribution of errors.
#
# Input variables:
# + DataTRY            -- tibble with the data to be used for model fitting.
# + try_trait          -- tibble with the TRY trait look-up table (variables and ID).
# + ModelTRY           -- tibble with list of models that we will seek to fit. It must have
#                         the following elements:
#                         - Model  -- a list of models to try to fit ("character").
#                                     Acceptable options are:
#                                     ~ OneLinear   : y = a0 * x
#                                     ~ OneLogLinear: y = a0 * x^a1
#                                     ~ TwoLinear   : y = a0 * x    * w
#                                     ~ TwoMixLinear: y = a0 * x^a1 * w
#                                     ~ TwoLogLinear: y = a0 * x^a1 * w^a2
#                                     ~ MartinezCano: y = a0 * x^a1 / ( a2 + x^a1 )
#                                     ~ Weibull     : y = a0 * (1 - exp (-a1 * x^a2 ))
#                         - xName  -- Name of the first predictor variable, typically the
#                                     one associated with size (DBH, Height, DBH^2*H).
#                                     It must match a column name in "DataTRY".
#                         - wName  -- Name of the second predictor variable. Used only when
#                                     "Model" requires two predictors. This is typically
#                                     the variable used for scaling (e.g., wood density,
#                                     LMA). When required, it must match a column name in
#                                     "DataTRY"
#                         - yName  -- Name of the response variable. It must match a column
#                                     name in "DataTRY".
# + CategAllom         -- tibble with information on categorical traits
#                         - TraitID    -- TRY trait ID associated with the categorical
#                                         trait.  Two additional special values are 
#                                         possible too:
#                                         ~ NA_integer_  -- special flag for all data
#                                         ~ 0            -- special ID for clusters.
#                         - Name       -- Names of the categories, consistent with the 
#                                         possible values in each categorical trait. For
#                                         the special case of "all data", use "ALL".
#                         - TraitClass -- Name of the column associated with the trait. It
#                                         must match a column in DataTRY. For the special
#                                         case of all data, use "All".
#                         - DescClass  -- Slightly longer description of the column 
#                                         associated with the categorical trait (the trait
#                                         in general, not the specific classes).
# + UseFixedModel      -- If TRUE, we decide which model to use based on the global data.
#                         Otherwise, each Cluster/PFT will be fitted independently.
# + InfoCrit           -- Which information criterion approach to use? Options are 
#                         AIC (Akaike Information Criterion), BIC (Bayesian Information
#                         Criterion), aRMSE (adjusted root mean square error) and wRMSE
#                         (weighted adjusted root mean square error).
# + InfoExtraOffset    -- Extra offset for selecting models with more parameters. We only
#                         take a model with more parameters if there is strong evidence
#                         that it actually improves the model.
# + CntAllomMin        -- Minimum number of points for fitting allometric models.
# + CntAllomMax        -- Maximum number of points to consider for fitting. This is used to
#                         limit the model fitting to a more manageable number of 
#                         observations when allometric data are exceedingly large. If no
#                         cap is sought, set CntAllomMax = +Inf. This setting is 
#                         independent on the data binning sampling defined by UseSizeBins.
# + AllomConfInt       -- Confidence interval for allometric model fittings.
# + AllomMaxItGain     -- Maximum number of iterations for calling the main optimiser for
#                         seeking a better maximum using the previous optimised set of 
#                         parameters. This sometimes helps improving the model, but it
#                         typically stabilises after a handful of iterations.
# + AllomMaxItOptim    -- Maximum number of iterations before giving up optimising the
#                         model. This is actually what controls the search for the optimal
#                         point, and should be relatively high as sometimes the initial
#                         guess is too far from the actual optimal.
# + AllomMaxItRelax    -- Maximum number of iterations to progressively relax tolerance between
#                         the ideal (strictest) and the "best" (most relaxed) tolerances.
# + AllomMinFactor     -- When fitting homoscedastic models with nls, this corresponds to the
#                         minimum factor for step size during the iterative search for solution.
#                         This value is typically defined as 1/2^x, where x is the number of 
#                         steps.
# + AllomTolGain       -- Relative tolerance for successive calls of the main optimiser.
#                         Values of the order of 1e-5 are typically sufficient.
# + AllomTolOptim      -- Relative tolerance for model fitting. This controls the error 
#                         tolerance of the optimiser itself and should be stricter (1.e-7
#                         or less).
# + AllomTolBestMult   -- Multiplication factor to relax tolerance when seeking the global 
#                         best model, for computational efficiency. Typically 100-1000.
# + AllomMaxItBestFrac -- Multiplication factor to relax tolerance for model fitting when
#                        running the bootstrapping for parameter uncertainty. Typically
#                        100-1000.
# + AllomTolMax        -- Maximum (i.e., most relaxed) tolerance acceptable. This can be
#                         used to avoid too relaxed tolerance when finding the best model
#                         or bootstraping.
# + AllomMaxItMin      -- Minimum number of steps per call of the optimiser. This can be
#                         used to go a bit quicker through the multi-model comparison.
# + AllomCntPred       -- Number of points spanning the range of predictor for fitted
#                         curve.
# + AllomQuantPred     -- Span predictor evenly across quantiles (TRUE) or linearly 
#                         (FALSE).
# + AllomCntBoot       -- Number of times for bootstrapping for generating confidence
#                         bands.
# + CntBootFail        -- For each bootstrap iteration, maximum number of attempts before
#                         giving up.
# + UseSizeBins        -- Use binning across size classes to have a more balanced 
#                         distribution across the predictor range? Note that in this case
#                         we cannot use AIC or BIC to define the best model, and thus we
#                         simply use adjusted R2.
# + xSample            -- Which variable to use for binning? This must be a valid column
#                         in DataTRY
# + xLogSmp            -- Should the binning be applied in the linear scale or log scale?
# + MinSmpPerBin       -- Minimum number of observations for each bin. Binning will not 
#                         occur unless there are at least twice as many valid points (and
#                         possibly even more points in case of very imbalanced data sets).
# + MaxCntBins         -- Maximum number of bins to be considered.
# + Verbose            -- Print information?
#---~---
Allom_Fit <<- function( DataTRY
                      , try_trait
                      , ModelTRY
                      , CategAllom
                      , UseFixedModel      = TRUE
                      , InfoCrit           = c("AIC","BIC","aRMSE","wRMSE")
                      , InfoExtraOffset    = 10.
                      , CntAllomMin        = 30L
                      , CntAllomMax        = 10000L
                      , AllomConfInt       = 0.95
                      , AllomMaxItGain     = 5L
                      , AllomMaxItOptim    = 5000L
                      , AllomMaxItRelax    = 10L
                      , AllomMinFactor     = 2.^-16
                      , AllomTolGain       = 1.e-6
                      , AllomTolOptim      = 1.e-8
                      , AllomTolBestMult   = 10.
                      , AllomMaxItBestFrac = 1.
                      , AllomTolMax        = 1.e-5
                      , AllomMaxItMin      = 500L
                      , AllomCntPred       = 101L
                      , AllomQuantPred     = TRUE
                      , AllomCntBoot       = 1000L
                      , AlphaSWTest        = 0.05
                      , UseSizeBins        = FALSE
                      , xSample            = ModelTRY$xName[1L]
                      , xLogSmp            = FALSE
                      , MinSmpPerBin       = 500L
                      , MaxCntBins         = 20L
                      , Verbose            = FALSE
                      ){


   #---~---
   #   Define information criterion to be used.
   #---~---
   InfoCrit = match.arg(InfoCrit)
   if (InfoCrit %in% c("aRMSE","wRMSE")){
      XICFmt          = "%.3f"
      InfoExtraOffset = 0.
   }else{
      XICFmt          = "%.2f"
   }#end if (InfoCrit %in%  c("aRMSE","wRMSE"))
   #---~---


   #---~---
   #   Define function that retrieves the information criterion
   #---~---
   XIC = function(object,InfoCrit){
      ans = switch( InfoCrit
                  , AIC   = object$AIC
                  , BIC   = object$BIC
                  , aRMSE = object$goodness$adj.rmse
                  , wRMSE = object$goodness$wgt.adj.rmse
                  )#end switch
      return(ans)
   }#end function
   #---~---



   #---~---
   #   First, we trim the input data to entries in which all variables considered for the
   # model fitting are valid. This may delete rows that have valid entries for the simplest
   # models being considered, however, this is needed if we want to compare different 
   # models using information criteria.
   #---~---
   if (Verbose) cat0(" + Keep only rows with all data needed.")
   AllomVars = with(ModelTRY,unique(c(xName,wName,yName)))
   AllomVars = AllomVars[! is.na(AllomVars)]
   DataTRY   = DataTRY %>%
      filter_at( all_of(AllomVars), all_vars(! is.na(.)))
   #---~---


   #---~---
   #   Define ancillary tolerance values.
   #---~---
   AllomTolGainBest    = min(AllomTolMax, AllomTolBestMult * AllomTolGain )
   AllomTolOptimBest   = min(AllomTolMax, AllomTolBestMult * AllomTolOptim)
   AllomMaxItGainBest  = 1L
   AllomMaxItOptimBest = max(AllomMaxItMin, ceiling(AllomMaxItOptim * AllomMaxItBestFrac))
   #---~---


   #---~---
   #   Define useful quantities
   #   CntModel         -- Number of models to try.
   #   CntData          -- Number of training data samples.
   #   CntCategAllom    -- Number of category levels
   #   CntPredCateg     -- Number of predictions times number of categories 
   #   CntBootCateg     -- Number of bootstrap realisations times number of categories
   #   CntPredBoot      -- Number of predictions times number of bootstrap realisations
   #   CntBootPredCateg -- Number of bootstrap times predictions times categories
   #---~---
   if (Verbose) cat0(" + Define ancillary variables.")
   CntModel         = nrow(ModelTRY)
   CntData          = nrow(DataTRY )
   CntCategAllom    = nrow(CategAllom)
   CntPredCateg     = AllomCntPred * CntCategAllom
   CntBootCateg     = AllomCntBoot * CntCategAllom
   CntPredBoot      = AllomCntPred * AllomCntBoot
   CntBootPredCateg = AllomCntBoot * AllomCntPred * CntCategAllom
   #---~---




   #---~---
   #   Define some useful vectors for generating predictors for plots.
   #   - IndexX: Vector with the indices for variable X
   #   - AllomBootID: Vector with the bootstrap realisation index.
   #   - AllomBootPredID: Vector with the bootstrap ID for each predictor.
   #   - PredQuant: Vector with the quantiles that may be used for generating predictions.
   #---~---
   IndexX          = sequence(AllomCntPred)
   AllomBootID     = sequence(AllomCntBoot)
   AllomBootPredID = rep( x= AllomBootID, each=AllomCntPred)
   PredQuant       = seq(from=0,to=1,length.out=AllomCntPred)
   #---~---



   #---~---
   #---~---
   #   Initialise objects for allometry plots:
   #---~---
   if (Verbose) cat0(" + Initialise output structures.")
   #---~---
   # SummAllom is the tibble with the coefficients and a few goodness-of-fit metrics.
   #---~---
   SummAllom = tibble( Class        = CategAllom$Name
                     , TraitClass   = CategAllom$TraitClass
                     , DescClass    = CategAllom$DescClass
                     , N            = rep( x = 0L           , times = CntCategAllom )
                     , Function     = rep( x = NA_character_, times = CntCategAllom )
                     , Formula      = rep( x = NA_character_, times = CntCategAllom )
                     , Scedastic    = rep( x = NA_character_, times = CntCategAllom )
                     , xName        = rep( x = NA_character_, times = CntCategAllom )
                     , wName        = rep( x = NA_character_, times = CntCategAllom )
                     , yName        = rep( x = NA_character_, times = CntCategAllom )
                     , a0           = rep( x = NA_real_     , times = CntCategAllom )
                     , SE_a0        = rep( x = NA_real_     , times = CntCategAllom )
                     , a1           = rep( x = NA_real_     , times = CntCategAllom )
                     , SE_a1        = rep( x = NA_real_     , times = CntCategAllom )
                     , a2           = rep( x = NA_real_     , times = CntCategAllom )
                     , SE_a2        = rep( x = NA_real_     , times = CntCategAllom )
                     , s0           = rep( x = NA_real_     , times = CntCategAllom )
                     , SE_s0        = rep( x = NA_real_     , times = CntCategAllom )
                     , s1           = rep( x = NA_real_     , times = CntCategAllom )
                     , SE_s1        = rep( x = NA_real_     , times = CntCategAllom )
                     , LogLik       = rep( x = NA_real_     , times = CntCategAllom )
                     , Bias         = rep( x = NA_real_     , times = CntCategAllom )
                     , SigRes       = rep( x = NA_real_     , times = CntCategAllom )
                     , MAE          = rep( x = NA_real_     , times = CntCategAllom )
                     , aRMSE        = rep( x = NA_real_     , times = CntCategAllom )
                     , R2Adjust     = rep( x = NA_real_     , times = CntCategAllom )
                     , wBias        = rep( x = NA_real_     , times = CntCategAllom )
                     , wSigRes      = rep( x = NA_real_     , times = CntCategAllom )
                     , wMAE         = rep( x = NA_real_     , times = CntCategAllom )
                     , wRMSE        = rep( x = NA_real_     , times = CntCategAllom )
                     , wR2Adjust    = rep( x = NA_real_     , times = CntCategAllom )
                     , AIC          = rep( x = NA_real_     , times = CntCategAllom )
                     , BIC          = rep( x = NA_real_     , times = CntCategAllom )
                     )#end tibble
   #---~---
   #   PredAllom is the tibble with the model predictions along the span of the predictors.
   #---~---
   PredAllom = tibble( Class      = rep( x = CategAllom$Name      , each  = AllomCntPred )
                     , TraitClass = rep( x = CategAllom$TraitClass, each  = AllomCntPred )
                     , DescClass  = rep( x = CategAllom$DescClass , each  = AllomCntPred )
                     , xID        = rep( x = IndexX               , times = CntCategAllom)
                     , xName      = rep( x = NA_character_        , times = CntPredCateg )
                     , wName      = rep( x = NA_character_        , times = CntPredCateg )
                     , yName      = rep( x = NA_character_        , times = CntPredCateg )
                     , x          = rep( x = NA_real_             , times = CntPredCateg )
                     , w          = rep( x = NA_real_             , times = CntPredCateg )
                     , y          = rep( x = NA_real_             , times = CntPredCateg )
                     , sigma      = rep( x = NA_real_             , times = CntPredCateg )
                     , yLwr       = rep( x = NA_real_             , times = CntPredCateg )
                     , yUpr       = rep( x = NA_real_             , times = CntPredCateg )
                     )#end tibble
   #---~---
   #   SummBoot is the tibble with the coefficient estimates from the bootstrapping.
   #---~---
   SummBoot = tibble( Class       = rep( x = CategAllom$Name      , each  = AllomCntBoot  )
                    , TraitClass  = rep( x = CategAllom$TraitClass, each  = AllomCntBoot  )
                    , DescClass   = rep( x = CategAllom$DescClass , each  = AllomCntBoot  )
                    , N           = rep( x = 0L                   , times = CntBootCateg  )
                    , BootID      = rep( x = AllomBootID          , times = CntCategAllom )
                    , Function    = rep( x = NA_character_        , times = CntBootCateg  )
                    , Formula     = rep( x = NA_character_        , times = CntBootCateg  )
                    , Scedastic   = rep( x = NA_character_        , times = CntBootCateg  )
                    , xName       = rep( x = NA_character_        , times = CntBootCateg  )
                    , wName       = rep( x = NA_character_        , times = CntBootCateg  )
                    , yName       = rep( x = NA_character_        , times = CntBootCateg  )
                    , a0          = rep( x = NA_real_             , times = CntBootCateg  )
                    , a1          = rep( x = NA_real_             , times = CntBootCateg  )
                    , a2          = rep( x = NA_real_             , times = CntBootCateg  )
                    , s0          = rep( x = NA_real_             , times = CntBootCateg  )
                    , s1          = rep( x = NA_real_             , times = CntBootCateg  )
                    , Bias        = rep( x = NA_real_             , times = CntBootCateg  )
                    , SigRes      = rep( x = NA_real_             , times = CntBootCateg  )
                    , MAE         = rep( x = NA_real_             , times = CntBootCateg  )
                    , aRMSE       = rep( x = NA_real_             , times = CntBootCateg  )
                    , R2Adjust    = rep( x = NA_real_             , times = CntBootCateg  )
                    , wBias       = rep( x = NA_real_             , times = CntBootCateg  )
                    , wMAE        = rep( x = NA_real_             , times = CntBootCateg  )
                    , wRMSE       = rep( x = NA_real_             , times = CntBootCateg  )
                    , wR2Adjust   = rep( x = NA_real_             , times = CntBootCateg  )
                    , xBias       = rep( x = NA_real_             , times = CntBootCateg  )
                    , xSigRes     = rep( x = NA_real_             , times = CntBootCateg  )
                    , xMAE        = rep( x = NA_real_             , times = CntBootCateg  )
                    , xRMSE       = rep( x = NA_real_             , times = CntBootCateg  )
                    )#end tibble
   #---~---
   #   PredBoot is the tibble with the model predictions along the span of the predictors
   # for each bootstrap realisation.
   #---~---
   PredBoot = tibble( Class      = rep( x = CategAllom$Name      , each  = CntPredBoot   )
                    , TraitClass = rep( x = CategAllom$TraitClass, each  = CntPredBoot   )
                    , DescClass  = rep( x = CategAllom$DescClass , each  = CntPredBoot   )
                    , BootID     = rep( x = AllomBootPredID   , times = CntCategAllom    )
                    , xID        = rep( x = IndexX            , times = CntBootCateg     )
                    , xName      = rep( x = NA_character_     , times = CntBootPredCateg )
                    , wName      = rep( x = NA_character_     , times = CntBootPredCateg )
                    , yName      = rep( x = NA_character_     , times = CntBootPredCateg )
                    , x          = rep( x = NA_real_          , times = CntBootPredCateg )
                    , w          = rep( x = NA_real_          , times = CntBootPredCateg )
                    , y          = rep( x = NA_real_          , times = CntBootPredCateg )
                    , sigma      = rep( x = NA_real_          , times = CntBootPredCateg )
                    )#end tibble
   #---~---



   #---~---
   #   Set variables to help deciding whether to fit the global and specific models only 
   # when needed.
   #---~---
   FitModel   = CntData %ge% CntAllomMin
   LoopGlobal = sequence(CntModel*as.integer(FitModel)*as.integer(UseFixedModel))
   #---~---



   #---~---
   #   If we are to use sampling, create a sample of data points that is evenly
   # distributed across size classes. Otherwise, the data for training is the original
   # data set unless it has so many points that it exceeds the maximum.
   #---~---
   TrainTRY = Allom_SetTrain( OrigTRY      = DataTRY
                            , CntAllomMax  = CntAllomMax
                            , UseSizeBins  = UseSizeBins
                            , xName        = xSample
                            , LogSmp       = xLogSmp
                            , MinSmpPerBin = MinSmpPerBin
                            , MaxCntBins   = MaxCntBins
                            )#end Allom_SetTrain
   CntTrain = nrow(TrainTRY)
   #---~---



   #---~---
   #   If sought, find the best global model.
   #---~---
   gBest       = NA_integer_
   gCntParam   = +Inf
   gXIC        = +Inf
   gScedastic  = NA_character_
   gDesc       = "None. All models failed."
   gExpect     = NULL
   gLnExpect   = NULL
   gFit        = NULL
   gSigma      = NULL
   gStartLSQ   = NA_character_
   gStartSigma = NA_character_
   if (Verbose && ( length(LoopGlobal) > 0)) cat0(" + Fit global model.")
   for (m in LoopGlobal){
      #---~---
      #   Retrieve settings for model fitting.
      #---~---
      mModel  = ModelTRY$Model [m]
      xName   = ModelTRY$xName [m]
      wName   = ModelTRY$wName [m]
      yName   = ModelTRY$yName [m]
      mLogSmp = ModelTRY$LogSmp[m]
      #---~---




      #---~---
      #   Set predictors, number of parameters and first guesses based on the functional 
      # form of the model.
      #---~---
      mSettings   = Allom_PrepOptim( OrigTRY = TrainTRY
                                   , Model   = mModel
                                   , xName   = xName
                                   , yName   = yName
                                   , wName   = wName
                                   )#end Allom_PrepOptim
      mPredictor  = mSettings$Predictor
      mCntParam   = mSettings$CntParam
      mExpect     = mSettings$Expect
      mLnExpect   = mSettings$LnExpect
      mSigma      = mSettings$Sigma
      mStartLSQ   = mSettings$StartLSQ
      mStartSigma = mSettings$StartSigma
      #---~---




      #---~---
      #   Fit homoscedastic model using NLS
      #---~---
      mScedastic = "Homoscedastic"
      mFitHomo   = 
         try( expr = nls( formula = mExpect
                        , data    = TrainTRY
                        , start   = mStartLSQ
                        , control = list( maxiter   = AllomMaxItOptimBest
                                        , tol       = AllomTolOptimBest
                                        , minFactor = AllomMinFactor
                                        )#end list
                        )#end nls
            , silent = TRUE
            )#end try
      #---~---



      #---~---
      #   Retrieve information criterion and update guesses in case fitting was successful.
      #---~---
      if ("try-error" %in% is(mFitHomo)){
         #---~---
         #   Model could not be fitted. Discard model, but still try the heteroscedastic.
         #---~---
         mXICHomo   = +Inf
         if (Verbose){
            cat0("   - ",mModel,",  homoscedastic "," (X = ",xName,"; W = ",wName,")."
                               ,"  Failed fitting.")
         }#end if (Verbose)
         #---~---
      }else{
         #---~---
         #   Append goodness-of-fit to the data set.
         #---~---
         mFitted   = fitted(mFitHomo)
         mObserv   = TrainTRY[[yName]]
         mGoodness = test.goodness( x.mod        = mFitted
                                  , x.obs        = mObserv
                                  , n.parameters = mCntParam
                                  , out.dfr      = TRUE
                                  )#end test.goodness
         mAIC      = Allom_AIC(Object=mFitHomo,Observ=mObserv,Scedastic=mScedastic)
         mBIC      = Allom_BIC(Object=mFitHomo,Observ=mObserv,Scedastic=mScedastic)
         mFitHomo  = modifyList(x=mFitHomo,val=list(goodness=mGoodness,AIC=mAIC,BIC=mBIC))
         #---~---


         #---~---
         #   Use the fitted homoscedastic model parameters as first guess for 
         # the heteroscedastic model.
         #---~---
         mXICHomo         = XIC(mFitHomo,InfoCrit)
         mStartLSQ        = as.list(coef(mFitHomo))
         names(mStartLSQ) = gsub(pattern="^lsq\\.",replacement="",x=names(mStartLSQ))
         if (Verbose){
            cat0("   - ",mModel,",  homoscedastic "," (X = ",xName,"; W = ",wName,")."
                               ,"  ",InfoCrit," = ",sprintf(XICFmt,mXICHomo),".")
         }#end if (Verbose)
         #---~---
      }#end if ("try-error" %in% is(mFitHomo))
      #---~---



      #---~---
      #   Select model if this is the best model so far.
      #---~---
      mXICOff = InfoExtraOffset * as.numeric( mCntParam > gCntParam )
      if ( ( mXICHomo + mXICOff ) < gXIC ){
         gBest       = m
         gCntParam   = mCntParam
         gXIC        = mXICHomo
         gScedastic  = mScedastic
         gDesc       = paste0(mModel,". (",mScedastic,")")
         gExpect     = mExpect
         gLnExpect   = mLnExpect
         gSigma      = NULL
         gFit        = mFitHomo
         gStartLSQ   = mStartLSQ
         gStartSigma = mStartSigma
      }#end if ( ( mXICHomo + mXICOff )< gXIC)
      #---~---



      #---~---
      #   Check whether or not to fit a log-linear model.
      #---~---
      if (is.na(mLnExpect)){
         #---~---
         #   Do not fit log-transformed model.
         #---~---
         mXICLogY = + Inf
         if (Verbose){
            cat0("   - ",mModel,",  log-transformed "," (X = ",xName,"; W = ",wName,")."
                               ,"  Do not attempt fitting.")
         }#end if (Verbose)
         #---~---

      }else{
         #---~---
         #   Fit model.
         #---~---
         mScedastic = "Log-Homoscedastic"
         mFitLogY   = try( expr   = rlm( formula = as.formula(mLnExpect)
                                       , data    = TrainTRY
                                       )#end rlm
                         , silent = TRUE
                         )#end try
         #---~---


         #---~---
         #   Retrieve information criterion and update guesses in case fitting was 
         # successful.
         #---~---
         if ("try-error" %in% is(mFitLogY)){
            #---~---
            #   Model could not be fitted. Discard model.
            #---~---
            mXICLogY = + Inf
            if (Verbose){
               cat0("   - ",mModel,",  log-transformed "," (X = ",xName,"; W = ",wName,")."
                                  ,"  Failed fitting.")
            }#end if (Verbose)
            browser()
            #---~---
         }else{
            #---~---
            #   Find the back-transformation correction factor (smearing estimate factor).
            #---~---
            mBackLog       = Allom_BackLog_Scaler( Model    = mFitLogY
                                                 , Observ   = TrainTRY[[yName]]
                                                 , CntParam = mCntParam
                                                 )#end Allom_BackLog_Scaler
            mMuSmearing    = mBackLog$MuSmearing
            mSigmaSmearing = mBackLog$SigmaSmearing
            #---~---


            #---~---
            #   Append goodness-of-fit to the data set.
            #---~---
            mFitted   = mMuSmearing    * exp(fitted(mFitLogY))
            mSigmaFit = mSigmaSmearing * mFitted
            mGoodness = test.goodness( x.mod        = mFitted
                                     , x.obs        = mObserv
                                     , x.sigma      = mSigmaFit
                                     , n.parameters = mCntParam
                                     , out.dfr      = TRUE
                                     )#end test.goodness
            mJacobian = -sum(log(TrainTRY[[yName]]))
            mAIC      = Allom_AIC(Object=mFitHomo,Observ=mObserv,Scedastic=mScedastic)
            mBIC      = Allom_BIC(Object=mFitHomo,Observ=mObserv,Scedastic=mScedastic)
            mFitLogY  = modifyList(x=mFitLogY,val=list(goodness=mGoodness,AIC=mAIC,BIC=mBIC))
            #---~---


            #---~---
            #   Find information criterion.
            #---~---
            mXICLogY  = XIC(mFitLogY,InfoCrit)
            #---~---


            #---~---
            #   Find coefficients.
            #---~---
            mStartLSQ        = as.list(coef(mFitLogY))
            mLabelLSQ        = names(mStartLSQ)
            mLabelLSQ        = gsub(pattern="log\\("   ,replacement=""  ,x=mLabelLSQ)
            mLabelLSQ        = gsub(pattern="\\("      ,replacement=""  ,x=mLabelLSQ)
            mLabelLSQ        = gsub(pattern="\\)"      ,replacement=""  ,x=mLabelLSQ)
            mLabelLSQ        = gsub(pattern="Intercept",replacement="a0",x=mLabelLSQ)
            mLabelLSQ        = gsub(pattern=xName      ,replacement="a1",x=mLabelLSQ)
            if (! is.na(wName)){
               mLabelLSQ     = gsub(pattern=wName      ,replacement="a2",x=mLabelLSQ)
            }#end if (! is.na(wName))
            names(mStartLSQ) = mLabelLSQ
            #---~---


            #---~---
            #   Apply smearing estimate factor (correction factor).
            #---~---
            mStartLSQ$a0 = mMuSmearing * exp(mStartLSQ$a0)
            #---~---


            #---~---
            #   Report 
            #---~---
            if (Verbose){
               cat0("   - ",mModel,",  log-homoscedastic "," (X = ",xName
                                                          ,"; W = ",wName,")."
                                  ,"  ",InfoCrit," = ",sprintf(XICFmt,mXICLogY),".")
            }#end if (Verbose)
            #---~---
         }#end if ("try-error" %in% is(mFitLogY))
         #---~---
      }#end if (is.na(mLnExpect)){
      #---~---



      #---~---
      #   Test whether the model improved results.
      #---~---
      mXICOff = InfoExtraOffset * as.numeric( mCntParam > gCntParam )
      if ( ( mXICLogY + mXICOff ) < gXIC ){
         gBest       = m
         gCntParam   = mCntParam
         gXIC        = mXICLogY
         gScedastic  = mScedastic
         gDesc       = paste0(mModel,". (",mScedastic,")")
         gExpect     = mExpect
         gLnExpect   = mLnExpect
         gSigma      = NULL
         gFit        = mFitLogY
         gStartLSQ   = mStartLSQ
         gStartSigma = mStartSigma
      }#end if ( ( mXICLogY + mXICOff ) < gXIC)
      #---~---



      #---~---
      #   Fit the heteroscedastic model. Add one parameter as we fit uncertainty too.
      #---~---
      mCntParam  = mCntParam + 1L
      mScedastic = "Heteroscedastic"
      mFitHete   = 
         try( expr = optim.lsq.htscd( lsq.formula = mExpect
                                    , sig.formula = mSigma
                                    , data        = TrainTRY
                                    , lsq.first   = mStartLSQ
                                    , sig.first   = mStartSigma
                                    , tol.gain    = AllomTolGainBest
                                    , tol.optim   = AllomTolOptimBest
                                    , maxit       = AllomMaxItGainBest
                                    , maxit.optim = AllomMaxItOptimBest
                                    , verbose     = FALSE
                                    )#end optim.lsq.htscd
            , silent = TRUE
            )#end try
      #---~---


      #---~---
      #   Retrieve information criterion and update guesses in case fitting was successful.
      #---~---
      if ("try-error" %in% is(mFitHete)){
         #---~---
         #   Model could not be fitted. Discard model.
         #---~---
         mXICHete = + Inf
         if (Verbose){
            cat0("   - ",mModel,",  heteroscedastic "," (X = ",xName,"; W = ",wName,")."
                               ,"  Failed fitting.")
         }#end if (Verbose)
         #---~---
      }else{
         mXICHete         = XIC(mFitHete,InfoCrit)
         mStartLSQ        = as.list(coef(mFitHete))
         mStartLSQ        = mStartLSQ[grepl(pattern="^lsq\\.",x=names(mStartLSQ))]
         names(mStartLSQ) = gsub(pattern="^lsq\\.",replacement="",x=names(mStartLSQ))
         mStartSigma      = list( s1 = coef(mFitHete)["sig.s1"] )
         if (Verbose){
            cat0("   - ",mModel,",  heteroscedastic "," (X = ",xName,"; W = ",wName,")."
                               ,"  ",InfoCrit," = ",sprintf(XICFmt,mXICHete),".")
         }#end if (Verbose)
      }#end if ("try-error" %in% is(mFitHete))
      #---~---


      #---~---
      #   Select model if this is the best model so far.
      #---~---
      mXICOff = InfoExtraOffset * as.numeric( mCntParam > gCntParam )
      if ( ( mXICHete + mXICOff ) < gXIC ){
         gBest       = m
         gCntParam   = mCntParam
         gXIC        = mXICHete
         gScedastic  = mScedastic
         gDesc       = paste0(mModel,". (",mScedastic,")")
         gHete       = TRUE
         gExpect     = mExpect
         gLnExpect   = mLnExpect
         gSigma      = mSigma
         gFit        = mFitHete
         gStartLSQ   = mStartLSQ
         gStartSigma = mStartSigma
      }#end if ( ( mXICHete + mXICOff ) < gXIC )
      #---~---
   }#end for (m in LoopGlobal)
   #---~---



   #---~---
   #   If we sought to fit a global model, we only loop through categories if we were 
   # successful finding a model.
   #---~---
   if (UseFixedModel){
      LoopCateg  = sequence(CntCategAllom*as.integer(FitModel)*is.finite(gXIC))
   }else{
      LoopCateg  = sequence(CntCategAllom*as.integer(FitModel))
   }#end if (UseFixedModel)
   #---~---



   #---~---
   #   Loop through all the categories and models.
   #---~---
   if (Verbose && (length(LoopCateg) > 0)) cat0(" + Fit allometric models by categories:")
   for (z in LoopCateg){
      #---~---
      #   Select category (or everything)
      #---~---
      zName      = SummAllom$TraitClass[z]
      zCateg     = SummAllom$Class     [z]
      zDescClass = SummAllom$DescClass [z]
      if (zName %in% "All"){
         zSel  = rep(TRUE,times=CntData)
      }else{
         zSel  = DataTRY[[zName]] %in% zCateg
      }#end if (zName %in% "All")
      #---~---


      #---~---
      #   Subset data
      #---~---
      SubsetTRY = DataTRY[zSel,,drop=FALSE]
      CntSubset = nrow(SubsetTRY)
      #---~---



      #---~---
      #   If we are to use sampling, create a sample of data points that is evenly
      # distributed across size classes. Otherwise, the data for training is the original
      # data set unless it has so many points that it exceeds the maximum.
      #---~---
      TrainTRY = Allom_SetTrain( OrigTRY      = SubsetTRY
                               , CntAllomMax  = CntAllomMax
                               , UseSizeBins  = UseSizeBins
                               , xName        = xSample
                               , LogSmp       = xLogSmp
                               , MinSmpPerBin = MinSmpPerBin
                               , MaxCntBins   = MaxCntBins
                               )#end Allom_SetTrain
      CntTrain = nrow(TrainTRY)
      #---~---


      #---~---
      #   Decide whether or not to test multiple models for this category. If using the 
      # same functional form for all classes, we skip the test and use the best global
      # model.
      #---~---
      FitModel = CntSubset %ge% CntAllomMin
      if (UseFixedModel && FitModel){
         LoopModel   = integer(0L)
         zBest       = gBest
         zCntParam   = gCntParam
         zXIC        = gXIC
         zScedastic  = gScedastic
         zDesc       = gDesc
         zExpect     = gExpect
         zLnExpect   = gLnExpect
         zFit        = gFit
         zSigma      = gSigma
         zStartLSQ   = gStartLSQ
         zStartSigma = gStartSigma
      }else if (FitModel){
         LoopModel   = LoopGlobal
         zBest       = NA_integer_
         zCntParam   = +Inf
         zXIC        = +Inf
         zScedastic  = NA_character_
         zDesc       = "None. All models failed."
         zExpect     = NULL
         zLnExpect   = NULL
         zFit        = NULL
         zSigma      = NULL
         zStartLSQ   = NA_character_
         zStartSigma = NA_character_
      }else{
         LoopModel   = integer(0L)
         zBest       = NA_integer_
         zCntParam   = +Inf
         zXIC        = +Inf
         zScedastic  = NA_character_
         zDesc       = "None. All models failed."
         zExpect     = NULL
         zLnExpect   = NULL
         zFit        = NULL
         zSigma      = NULL
         zStartLSQ   = NA_character_
         zStartSigma = NA_character_
      }#end if (UseFixedModel && FitModel)
      #---~---


      #---~---
      #   Initialise best model with nothing.
      #---~---
      for (m in LoopModel){
         #---~---
         #   Retrieve settings for model fitting.
         #---~---
         mModel  = ModelTRY$Model [m]
         xName   = ModelTRY$xName [m]
         wName   = ModelTRY$wName [m]
         yName   = ModelTRY$yName [m]
         mLogSmp = ModelTRY$LogSmp[m]
         #---~---



         #---~---
         #   First, we find some information about predictors and predictand that may be
         # useful for defining first guesses.
         #---~---
         qLwrX = quantile(x=TrainTRY[[xName]],probs=0.025,names=FALSE,na.rm=TRUE)
         qMidX = quantile(x=TrainTRY[[xName]],probs=0.500,names=FALSE,na.rm=TRUE)
         qUprX = quantile(x=TrainTRY[[xName]],probs=0.975,names=FALSE,na.rm=TRUE)
         qLwrY = quantile(x=TrainTRY[[yName]],probs=0.025,names=FALSE,na.rm=TRUE)
         qMidY = quantile(x=TrainTRY[[yName]],probs=0.500,names=FALSE,na.rm=TRUE)
         qUprY = quantile(x=TrainTRY[[yName]],probs=0.975,names=FALSE,na.rm=TRUE)
         if (is.na(wName)){
            qMidW = NA_real_
         }else{
            qMidW = quantile(x=TrainTRY[[wName]],probs=0.500,names=FALSE,na.rm=TRUE)
         }#end if (is.na(wName))
         #---~---




         #---~---
         #   Set predictors, number of parameters and first guesses based on the functional 
         # form of the model.
         #---~---
         mSettings   = Allom_PrepOptim( OrigTRY = TrainTRY
                                      , Model   = mModel
                                      , xName   = xName
                                      , yName   = yName
                                      , wName   = wName
                                      )#end Allom_PrepOptim
         mPredictor  = mSettings$Predictor
         mCntParam   = mSettings$CntParam
         mExpect     = mSettings$Expect
         mLnExpect   = mSettings$LnExpect
         mSigma      = mSettings$Sigma
         mStartLSQ   = mSettings$StartLSQ
         mStartSigma = mSettings$StartSigma
         #---~---



         #---~---
         #   Fit homoscedastic model
         #---~---
         mScedastic = "Homoscedastic"
         mFitHomo   = 
            try( expr = nls( formula = mExpect
                           , data    = TrainTRY
                           , start   = mStartLSQ
                           , control = list( maxiter   = AllomMaxItOptimBest
                                           , tol       = AllomTolOptimBest
                                           , minFactor = AllomMinFactor
                                           )#end list
                           )#end nls
               , silent = TRUE
               )#end try
         if ("try-error" %in% is(mFitHomo)){
            #---~---
            #   Model could not be fitted. Discard model and try luck with heteroscedastic.
            #---~---
            mXICHomo  = +Inf
            if (Verbose){
               cat0("   - ",mModel,",  homoscedastic "," (X = ",xName,"; W = ",wName,")."
                                  ,"  Failed fitting.")
            }#end if (Verbose)
            #---~---
         }else{
            #---~---
            #   Append goodness-of-fit to the data set.
            #---~---
            mFitted   = fitted(mFitHomo)
            mObserv   = TrainTRY[[yName]]
            mGoodness = test.goodness( x.mod        = mFitted
                                     , x.obs        = mObserv
                                     , n.parameters = mCntParam
                                     , out.dfr      = TRUE
                                     )#end test.goodness
            mAIC      = Allom_AIC(Object=mFitHomo,Observ=mObserv,Scedastic=mScedastic)
            mBIC      = Allom_BIC(Object=mFitHomo,Observ=mObserv,Scedastic=mScedastic)
            mFitHomo  = modifyList(x=mFitHomo,val=list(goodness=mGoodness,AIC=mAIC,BIC=mBIC))
            #---~---


            #---~---
            #   Use the fitted homoscedastic model parameters as first guess for 
            # the heteroscedastic model.
            #---~---
            mXICHomo         = XIC(mFitHomo,InfoCrit)
            mStartLSQ        = as.list(coef(mFitHomo))
            names(mStartLSQ) = gsub(pattern="^lsq\\.",replacement="",x=names(mStartLSQ))
            if (Verbose){
               cat0("   - ",mModel,",  homoscedastic "," (X = ",xName,"; W = ",wName,")."
                                  ,"  ",InfoCrit," = ",sprintf(XICFmt,mXICHomo),".")
            }#end if (Verbose)
            #---~---
         }#end if ("try-error" %in% is(mFitHomo))
         #---~---


         #---~---
         #   Select model if this is the best model so far.
         #---~---
         mXICOff = InfoExtraOffset * as.numeric( mCntParam > zCntParam )
         if ( ( mXICHomo + mXICOff ) < zXIC ){
            zBest       = m
            zCntParam   = mCntParam
            zXIC        = mXICHomo
            zScedastic  = mScedastic
            zDesc       = paste0(mModel,". (",mScedastic,")")
            zExpect     = mExpect
            zLnExpect   = mLnExpect
            zFit        = mFitHomo
            zSigma      = NULL
            zStartLSQ   = mStartLSQ
            zStartSigma = mStartSigma
         }#end if ( ( mXICHomo + mXICOff ) < zXIC )
         #---~---



         #---~---
         #   Check whether or not to fit a log-linear model.
         #---~---
         if (is.na(mLnExpect)){
            #---~---
            #   Do not fit log-transformed model.
            #---~---
            mXICLogY = + Inf
            if (Verbose){
               cat0("   - ",mModel,",  log-transformed "," (X = ",xName,"; W = ",wName,")."
                                  ,"  Do not attempt fitting.")
            }#end if (Verbose)
            #---~---

         }else{
            #---~---
            #   Fit model.
            #---~---
            mScedastic = "Log-Homoscedastic"
            mFitLogY   = try( expr   = rlm( formula = as.formula(mLnExpect)
                                          , data    = TrainTRY
                                          )#end rlm
                            , silent = TRUE
                            )#end try
            #---~---


            #---~---
            #   Retrieve information criterion and update guesses in case fitting was 
            # successful.
            #---~---
            if ("try-error" %in% is(mFitLogY)){
               #---~---
               #   Model could not be fitted. Discard model.
               #---~---
               mXICLogY = + Inf
               if (Verbose){
                  cat0("   - ",mModel,",  log-transformed "," (X = ",xName
                                                           ,"; W = ",wName,")."
                                     ,"  Failed fitting.")
               }#end if (Verbose)
               browser()
               #---~---
            }else{
               #---~---
               #   Find the back-transformation correction factor (smearing estimate factor).
               #---~---
               mBackLog       = Allom_BackLog_Scaler( Model    = mFitLogY
                                                    , Observ   = TrainTRY[[yName]]
                                                    , CntParam = mCntParam
                                                    )#end Allom_BackLog_Scaler
               mMuSmearing    = mBackLog$MuSmearing
               mSigmaSmearing = mBackLog$SigmaSmearing
               #---~---


               #---~---
               #   Append goodness-of-fit to the data set.
               #---~---
               mObserv   = TrainTRY[[yName]]
               mFitted   = mMuSmearing    * exp(fitted(mFitLogY))
               mSigmaFit = mSigmaSmearing * mFitted
               mGoodness = test.goodness( x.mod        = mFitted
                                        , x.obs        = mObserv
                                        , x.sigma      = mSigmaFit
                                        , n.parameters = mCntParam
                                        , out.dfr      = TRUE
                                        )#end test.goodness
               mAIC      = Allom_AIC(Object=mFitHomo,Observ=mObserv,Scedastic=mScedastic)
               mBIC      = Allom_BIC(Object=mFitHomo,Observ=mObserv,Scedastic=mScedastic)
               mFitLogY  = modifyList(x=mFitLogY,val=list(goodness=mGoodness,AIC=mAIC,BIC=mBIC))
               #---~---


               #---~---
               #   Find information criterion.
               #---~---
               mXICLogY   = XIC(mFitLogY,InfoCrit)
               #---~---


               #---~---
               #   Find coefficients.
               #---~---
               mStartLSQ        = as.list(coef(mFitLogY))
               mLabelLSQ        = names(mStartLSQ)
               mLabelLSQ        = gsub(pattern="log\\("   ,replacement=""  ,x=mLabelLSQ)
               mLabelLSQ        = gsub(pattern="\\("      ,replacement=""  ,x=mLabelLSQ)
               mLabelLSQ        = gsub(pattern="\\)"      ,replacement=""  ,x=mLabelLSQ)
               mLabelLSQ        = gsub(pattern="Intercept",replacement="a0",x=mLabelLSQ)
               mLabelLSQ        = gsub(pattern=xName      ,replacement="a1",x=mLabelLSQ)
               if (! is.na(wName)){
                  mLabelLSQ     = gsub(pattern=wName      ,replacement="a2",x=mLabelLSQ)
               }#end if (! is.na(wName))
               names(mStartLSQ) = mLabelLSQ
               #---~---


               #---~---
               #   Apply smearing estimate factor (correction factor).
               #---~---
               mStartLSQ$a0 = mMuSmearing * exp(mStartLSQ$a0)
               #---~---


               #---~---
               #   Report values.
               #---~---
               if (Verbose){
                  cat0("   - ",mModel,",  log-homoscedastic "," (X = ",xName
                                                             ,"; W = ",wName,")."
                                     ,"  ",InfoCrit," = ",sprintf(XICFmt,mXICLogY),".")
               }#end if (Verbose)
               #---~---
            }#end if ("try-error" %in% is(mFitLogY))
            #---~---
         }#end if (is.na(mLnExpect)){
         #---~---



         #---~---
         #   Test whether the model improved results.
         #---~---
         mXICOff = InfoExtraOffset * as.numeric( mCntParam > gCntParam )
         if ( ( mXICLogY + mXICOff ) < zXIC ){
            zBest       = m
            zCntParam   = mCntParam
            zXIC        = mXICLogY
            zScedastic  = mScedastic
            zDesc       = paste0(mModel,". (",mScedastic,")")
            zExpect     = mExpect
            zLnExpect   = mLnExpect
            zSigma      = NULL
            zFit        = mFitLogY
            zStartLSQ   = mStartLSQ
            zStartSigma = mStartSigma
         }#end if ( ( mXICLogY + mXICOff ) < zXIC)
         #---~---








         #---~---
         #   Fit the heteroscedastic model. Add one parameter as we fit uncertainties too.
         #---~---
         mCntParam  = mCntParam + 1L
         mScedastic = "Heteroscedastic"
         mFitHete   = 
            try( expr = optim.lsq.htscd( lsq.formula = mExpect
                                       , sig.formula = mSigma
                                       , data        = TrainTRY
                                       , lsq.first   = mStartLSQ
                                       , sig.first   = mStartSigma
                                       , tol.gain    = AllomTolGainBest
                                       , tol.optim   = AllomTolOptimBest
                                       , maxit       = AllomMaxItGainBest
                                       , maxit.optim = AllomMaxItOptimBest
                                       , verbose     = FALSE
                                       )#end optim.lsq.htscd
               , silent = TRUE
               )#end try
         #---~---


         #---~---
         #   Retrieve information criterion and update guesses in case fitting was 
         # successful.
         #---~---
         if ("try-error" %in% is(mFitHete)){
            #---~---
            #   Model could not be fitted. Discard model.
            #---~---
            mXICHete = + Inf
            if (Verbose){
               cat0("   - ",mModel,",  heteroscedastic "," (X = ",xName,"; W = ",wName,")."
                                  ,"  ",InfoCrit," = ",sprintf(XICFmt,mXICHomo),".")
            }#end if (Verbose)
            #---~---
         }else{
            mXICHete         = XIC(mFitHete,InfoCrit)
            mStartLSQ        = as.list(coef(mFitHete))
            mStartLSQ        = mStartLSQ[grepl(pattern="^lsq\\.",x=names(mStartLSQ))]
            names(mStartLSQ) = gsub(pattern="^lsq\\.",replacement="",x=names(mStartLSQ))
            mStartSigma      = list( s1 = coef(mFitHete)["sig.s1"] )
            if (Verbose){
               cat0("   - ",mModel,",  heteroscedastic "," (X = ",xName,"; W = ",wName,")."
                                  ,"  ",InfoCrit," = ",sprintf(XICFmt,mXICHete),".")
            }#end if (Verbose)
         }#end if ("try-error" %in% is(mFitHete))
         #---~---


         #---~---
         #   Select model if this is the best model so far.
         #---~---
         mXICOff = InfoExtraOffset * as.numeric( mCntParam > zCntParam )
         if ( ( mXICHete + mXICOff ) < zXIC ){
            zBest       = m
            zCntParam   = mCntParam
            zXIC        = mXICHete
            zScedastic  = mScedastic
            zDesc       = paste0(mModel,". (",mScedastic,")")
            zHete       = TRUE
            zExpect     = mExpect
            zLnExpect   = mLnExpect
            zFit        = mFitHete
            zSigma      = mSigma
            zStartLSQ   = mStartLSQ
            zStartSigma = mStartSigma
         }#end if (mXICHete < zXIC)
         #---~---
      }#end for (m in LoopModel)
      #---~---



      #---~---
      #   Select model if this is the best model so far. If all model fittings failed,
      # cycle to the next class/category.
      #---~---
      if (is.na(zBest)){
         warning("   - Failed fitting models for: ",zDescClass,", category: ",zCateg,".")
         next
      }else{
         zModel  = ModelTRY$Model [zBest]
         xName   = ModelTRY$xName [zBest]
         wName   = ModelTRY$wName [zBest]
         yName   = ModelTRY$yName [zBest]
         zLogSmp = ModelTRY$LogSmp[zBest]
         if (Verbose){
            cat0("   - Selected model: ",zDesc,"; (X = ",xName,"; W = ",wName,")."
                                              ," Find coefficients and uncertainties.")
         }#end if (Verbose){
      }#end if (is.na(zBest))
      #---~---



      #---~---
      #   Set predictors, number of parameters and first guesses based on the functional 
      # form of the model.
      #---~---
      zSettings   = Allom_PrepOptim( OrigTRY = TrainTRY
                                   , Model   = zModel
                                   , xName   = xName
                                   , yName   = yName
                                   , wName   = wName
                                   )#end Allom_PrepOptim
      zPredictor  = zSettings$Predictor
      zCntParam   = zSettings$CntParam
      zStartLSQ   = zSettings$StartLSQ
      zStartSigma = zSettings$StartSigma
      #---~---



      #---~---
      #   Fit the model with stricter tolerance and also generate bootstrapped estimates.
      #---~---
      if ( zScedastic %in% "Homoscedastic" ){
         #---~---
         #   Fit best homoscedastic model We iterate with increasingly more relaxed tolerances
         # because sometimes the strictest tolerances let NLS fail.
         #---~---
         AllomTolOptimList = exp( seq( from       = log(AllomTolOptim)
                                     , to         = log(AllomTolOptimBest)
                                     , length.out = AllomMaxItRelax
                                     )#end seq
                                )#end exp
         r                 = 0
         iterate           = TRUE
         while ( iterate && (r < AllomMaxItRelax) ){
            #---~---
            #   Keep iterating until convergence.
            #---~---
            r                = r + 1L
            AllomTolOptimNow = AllomTolOptimList[r]
            #---~---


            #---~---
            zFit  = 
               try( expr = nls( formula = zExpect
                              , data    = TrainTRY
                              , start   = zStartLSQ
                              , control = list( maxiter   = AllomMaxItOptim
                                              , tol       = AllomTolOptimNow
                                              , minFactor = AllomMinFactor
                                              )#end list
                              )#end nls
                  , silent = TRUE
                  )#end try
            if ("try-error" %in% is(zFit)){
               if (Verbose) cat0(" Iteration ",r,". Best homoscedastic model failed fitting...")
               #---~---
               #   Interrupt fitting if the model is still failing after all the iterations.
               #---~---
               if (r == AllomMaxItRelax){
                  cat0(" Homoscedastic model failed fitting after "
                      ,AllomMaxItRelax," iterations." )
                  browser()
               }#end if (r == AllomMaxItRelax)
               #---~---
            }else{
               #---~---
               #   Fitted model converged, if sought, report values.
               #---~---
               iterate  = FALSE
               zShow    = paste(names(zStartLSQ),sprintf("%.3f",coef(zFit)),sep=" = ")
               zShow    = paste(zShow,collapse=";   ")
               if (Verbose){
                  cat0("     ~ Iteration ",r,"; Optimised values:   ",zShow,";   N = ",CntTrain)
               }#end if (Verbose){
               #---~---
            }#end if ("try-error" %in% is(zFit))
         }#end while ( iterate && (r < AllomMaxItRelax) )
         #---~---





         #---~---
         #   Retrieve useful information.
         #---~---
         zFit  = modifyList(x=zFit,val=list(sigma=sigma(zFit)))
         #---~---


         #---~---
         #   Copy coefficients.
         #---~---
         zParam = coef(zFit)
         if (! "a1" %in% names(zParam)) zParam["a1"] = NA_real_
         if (! "a2" %in% names(zParam)) zParam["a2"] = NA_real_
         if (! "s0" %in% names(zParam)) zParam["s0"] = zFit$sigma
         if (! "s1" %in% names(zParam)) zParam["s1"] = 0.
         zParam = zParam[c("a0","a1","a2","s0","s1")]
         zFit   = modifyList(x=zFit,val=list(coeff.best=zParam))
         #---~---



         #---~---
         #   Find information criteria
         #---~---
         zAIC      = Allom_AIC(Object=zFit,Observ=zObserv,Scedastic=zScedastic)
         zBIC      = Allom_BIC(Object=zFit,Observ=zObserv,Scedastic=zScedastic)
         zFit      = modifyList(x=zFit,val=list(AIC=zAIC,BIC=zBIC))
         #---~---


         #---~---
         #   Prepare bootstrap objects.
         #---~---
         CoeffBoot     = matrix( nrow     = AllomCntBoot
                               , ncol     = 5L
                               , dimnames = list(NULL,c("a0","a1","a2","s0","s1"))
                               )#end matrix
         Sigma0Boot    = rep(NA_real_,times=AllomCntBoot)
         BootSupport   = rep(NA_real_,times=AllomCntBoot)
         BootGoodness  = replicate(n=AllomCntBoot,list())
         CrossGoodness = replicate(n=AllomCntBoot,list())
         #---~---


         #---~---
         #   Bootstrapping loop.
         #---~---
         b = 0L
         while (b < AllomCntBoot){
            #---~---
            #   Bootstrap sample.
            #---~---
            useIdx   = sample.int(n=CntTrain,size=CntTrain,replace=TRUE)
            skipIdx  = which(! (sequence(CntTrain) %in% useIdx))
            BootTRY  = TrainTRY[useIdx ,,drop=FALSE]
            CrossTRY = TrainTRY[skipIdx,,drop=FALSE]
            #---~---



            #---~---
            #   Fit bootstrapped model.
            #---~---
            bFit  = 
               try( expr = nls( formula = zExpect
                              , data    = BootTRY
                              , start   = zStartLSQ
                              , control = list( maxiter   = AllomMaxItOptimBest
                                              , tol       = AllomTolOptimBest
                                              , minFactor = AllomMinFactor
                                              )#end list
                              )#end nls
                  , silent = TRUE
                  )#end try
            if ("try-error" %in% is(bFit)){
               if (Verbose) cat0(" Bootstrap iteration failed fitting...")
            }else{
               #---~---
               #   Accept iteration.
               #---~---
               b      = b + 1L
               bShow  = paste(names(zStartLSQ),sprintf("%.3f",coef(bFit)),sep=" = ")
               bShow  = paste(bShow,collapse=";   ")
               if (Verbose) cat0("     ~ Bootstrap ",b,";   Values:   ",bShow,".")
               #---~---


               #---~---
               #   Retrieve useful information.
               #---~---
               bFit  = modifyList(x=bFit,val=list(sigma=sigma(bFit)))
               #---~---


               #---~---
               #   Copy coefficients.
               #---~---
               bParam = coef(bFit)
               if (! "a1" %in% names(bParam)) bParam["a1"] = NA_real_
               if (! "a2" %in% names(bParam)) bParam["a2"] = NA_real_
               if (! "s0" %in% names(bParam)) bParam["s0"] = bFit$sigma
               if (! "s1" %in% names(bParam)) bParam["s1"] = 0.
               bParam = bParam[c("a0","a1","a2","s0","s1")]
               #---~---


               #---~---
               #   Retried fitted and predicted values.
               #---~---
               bObserv   = BootTRY [[yName]]
               xObserv   = CrossTRY[[yName]]
               bFitted   = predict(object=bFit,newdata=BootTRY )
               xFitted   = predict(object=bFit,newdata=CrossTRY)
               bGoodness = test.goodness( x.mod        = bFitted
                                        , x.obs        = bObserv
                                        , n.parameters = zCntParam
                                        , out.dfr      = "vector"
                                        )#end test.goodness
               xGoodness = test.goodness( x.mod        = xFitted
                                        , x.obs        = xObserv
                                        , n.parameters = zCntParam
                                        , out.dfr      = "vector"
                                        )#end test.goodness
               #---~---



               #---~---
               #   Update the bootstrap table.
               #---~---
               CoeffBoot     [b,] = bParam
               Sigma0Boot   [[b]] = bFit$sigma
               BootSupport  [[b]] = logLik(bFit)
               BootGoodness [[b]] = bGoodness
               CrossGoodness[[b]] = xGoodness
               #---~---
            }#end if ("try-error" %in% is(bFit))
            #---~---
         }#end for (b in sequence(AllomCntBoot))
         #---~---


         #---~---
         #   Convert goodness of fit into data frames
         #---~---
         BootGoodness  = as.data.frame(do.call(what="rbind",args=BootGoodness ))
         CrossGoodness = as.data.frame(do.call(what="rbind",args=CrossGoodness))
         BootTrain     = list( goodness  = BootGoodness )
         #---~---


         #---~---
         #   Append bootstrap coefficients to the fitted object
         #---~---
         zFit = modifyList( x   = zFit
                          , val = list( coeff.boot   = CoeffBoot
                                      , sigma0.boot  = Sigma0Boot
                                      , support.boot = BootSupport
                                      , boot.train   = BootTrain
                                      , cross.val    = CrossGoodness
                                      )#end list
                          )#end modifyList
         #---~---

      }else if (zScedastic %in% "Log-Homoscedastic"){
         #---~---
         #   Fit best homoscedastic model.
         #---~---
         zFit  = 
            try( expr = rlm( formula = as.formula(zLnExpect)
                           , data    = TrainTRY
                           )#end rlm
               , silent = TRUE
               )#end try
         if ("try-error" %in% is(zFit)){
            cat0(" Best log-homoscedastic model failed fitting...")
            browser()
         }else{
            #---~---
            #   Retrieve useful information.
            #---~---
            zFit   = modifyList(x=zFit,val=list(sigma=sigma(zFit)))
            #---~---



            #---~---
            #   Find information criteria
            #---~---
            zAIC = Allom_AIC(Object=zFit,Observ=zObserv,Scedastic=zScedastic)
            zBIC = Allom_BIC(Object=zFit,Observ=zObserv,Scedastic=zScedastic)
            zFit = modifyList(x=zFit,val=list(AIC=zAIC,BIC=zBIC))
            #---~---


            #---~---
            #   Find the back-transformation correction factor (smearing estimate factor).
            #---~---
            zBackLog       = Allom_BackLog_Scaler( Model    = zFit
                                                 , Observ   = TrainTRY[[yName]]
                                                 , CntParam = zCntParam
                                                 )#end Allom_BackLog_Scaler
            zMuSmearing    = zBackLog$MuSmearing
            zSigmaSmearing = zBackLog$SigmaSmearing
            #---~---



            #---~---
            #   Find coefficients.
            #---~---
            zParam        = coef(zFit)
            zLabel        = names(zParam)
            zLabel        = gsub(pattern="log\\("   ,replacement=""  ,x=zLabel)
            zLabel        = gsub(pattern="\\("      ,replacement=""  ,x=zLabel)
            zLabel        = gsub(pattern="\\)"      ,replacement=""  ,x=zLabel)
            zLabel        = gsub(pattern="Intercept",replacement="a0",x=zLabel)
            zLabel        = gsub(pattern=xName      ,replacement="a1",x=zLabel)
            if (! is.na(wName)){
               zLabel     = gsub(pattern=wName      ,replacement="a2",x=zLabel)
            }#end if (! is.na(wName))
            names(zParam) = zLabel
            #---~---


            #---~---
            #   Apply the smearing correction factor to expected value and for sigma, and
            # append coefficients if needed.
            #---~---
            zParam["a0"] = zMuSmearing * exp(zParam["a0"])
            if (! "a1" %in% names(zParam)) zParam["a1"] = NA_real_
            if (! "a2" %in% names(zParam)) zParam["a2"] = NA_real_
            zParam["s0"] = zSigmaSmearing
            zParam["s1"] = 1.
            #---~---

            #---~---
            #   Report value.
            #---~---
            zShow    = paste(names(zParam),sprintf("%.3f",zParam),sep=" = ")
            zShow    = paste(zShow,collapse=";   ")
            if (Verbose) cat0("     ~ Optimised values:   ",zShow,";   N = ",CntTrain)
            zFit   = modifyList(x=zFit,val=list(coeff.best=zParam))
            #---~---
         }#end if ("try-error" %in% is(zFit))
         #---~---


         #---~---
         #   Prepare bootstrap objects.
         #---~---
         CoeffBoot     = matrix( nrow     = AllomCntBoot
                               , ncol     = 5L
                               , dimnames = list(NULL,c("a0","a1","a2","s0","s1"))
                               )#end matrix
         Sigma0Boot    = rep(NA_real_,times=AllomCntBoot)
         BootSupport   = rep(NA_real_,times=AllomCntBoot)
         BootGoodness  = replicate(n=AllomCntBoot,list())
         CrossGoodness = replicate(n=AllomCntBoot,list())
         #---~---


         #---~---
         #   Bootstrapping loop.
         #---~---
         b = 0L
         while (b < AllomCntBoot){
            #---~---
            #   Bootstrap sample.
            #---~---
            useIdx   = sample.int(n=CntTrain,size=CntTrain,replace=TRUE)
            skipIdx  = which(! (sequence(CntTrain) %in% useIdx))
            BootTRY  = TrainTRY[useIdx ,,drop=FALSE]
            CrossTRY = TrainTRY[skipIdx,,drop=FALSE]
            #---~---



            #---~---
            #   Fit bootstrapped model.
            #---~---
            bFit  = 
               try( expr = rlm( formula = as.formula(zLnExpect)
                              , data    = BootTRY
                              )#end rlm
                  , silent = TRUE
                  )#end try
            if ("try-error" %in% is(bFit)){
               if (Verbose) cat0(" Bootstrap iteration failed fitting...")
            }else{
               #---~---
               #   Accept iteration.
               #---~---
               b      = b + 1L
               #---~---


               #---~---
               #   Find the back-transformation correction factor (smearing estimate factor).
               #---~---
               bBackLog       = Allom_BackLog_Scaler( Model    = bFit
                                                    , Observ   = BootTRY[[yName]]
                                                    , CntParam = zCntParam
                                                    )#end Allom_BackLog_Scaler
               bMuSmearing    = bBackLog$MuSmearing
               bSigmaSmearing = bBackLog$SigmaSmearing
               #---~---



               #---~---
               #   Find coefficients.
               #---~---
               bParam        = coef(bFit)
               bLabel        = names(bParam)
               bLabel        = gsub(pattern="log\\("   ,replacement=""  ,x=bLabel)
               bLabel        = gsub(pattern="\\("      ,replacement=""  ,x=bLabel)
               bLabel        = gsub(pattern="\\)"      ,replacement=""  ,x=bLabel)
               bLabel        = gsub(pattern="Intercept",replacement="a0",x=bLabel)
               bLabel        = gsub(pattern=xName      ,replacement="a1",x=bLabel)
               if (! is.na(wName)){
                  bLabel     = gsub(pattern=wName      ,replacement="a2",x=bLabel)
               }#end if (! is.na(wName))
               names(bParam) = bLabel
               #---~---


               #---~---
               #   Apply smearing estimate correction factors to expected value and for sigma, 
               # and append coefficients if needed.
               #---~---
               bParam["a0"] = exp(bParam["a0"]) * bMuSmearing
               if (! "a1" %in% names(zParam)) zParam["a1"] = NA_real_
               if (! "a2" %in% names(zParam)) zParam["a2"] = NA_real_
               bParam["s0"] = bSigmaSmearing
               bParam["s1"] = 1.
               bParam       = bParam[c("a0","a1","a2","s0","s1")]
               #---~---


               #---~---
               #   Report iteration on screen if desired
               #---~---
               bShow  = paste(names(bParam),sprintf("%.3f",coef(bFit)),sep=" = ")
               bShow  = paste(bShow,collapse=";   ")
               if (Verbose) cat0("     ~ Bootstrap ",b,";   Values:   ",bShow,".")
               #---~---


               #---~---
               #   Retried fitted and predicted values.
               #---~---
               bObserv   = BootTRY [[yName]]
               xObserv   = CrossTRY[[yName]]
               bFitted   = bMuSmearing * exp(predict(object=bFit,newdata=BootTRY ))
               xFitted   = bMuSmearing * exp(predict(object=bFit,newdata=CrossTRY))
               bSigmaFit = bSigmaSmearing * bFitted
               xSigmaFit = bSigmaSmearing * xFitted
               bGoodness = test.goodness( x.mod        = bFitted
                                        , x.obs        = bObserv
                                        , x.sigma      = bSigmaFit
                                        , n.parameters = zCntParam
                                        , out.dfr      = "vector"
                                        )#end test.goodness
               xGoodness = test.goodness( x.mod        = xFitted
                                        , x.obs        = xObserv
                                        , x.sigma      = xSigmaFit
                                        , n.parameters = zCntParam
                                        , out.dfr      = "vector"
                                        )#end test.goodness
               #---~---



               #---~---
               #   Update the bootstrap table.
               #---~---
               CoeffBoot     [b,] = bParam
               Sigma0Boot   [[b]] = bSigmaSmearing
               BootSupport  [[b]] = logLik(bFit)
               BootGoodness [[b]] = bGoodness
               CrossGoodness[[b]] = xGoodness
               #---~---
            }#end if ("try-error" %in% is(bFit))
            #---~---
         }#end for (b in sequence(AllomCntBoot))
         #---~---


         #---~---
         #   Convert goodness of fit into data frames
         #---~---
         BootGoodness  = as.data.frame(do.call(what="rbind",args=BootGoodness ))
         CrossGoodness = as.data.frame(do.call(what="rbind",args=CrossGoodness))
         BootTrain     = list( goodness  = BootGoodness )
         #---~---


         #---~---
         #   Append bootstrap coefficients to the fitted object
         #---~---
         zFit = modifyList( x   = zFit
                          , val = list( coeff.boot   = CoeffBoot
                                      , sigma0.boot  = Sigma0Boot
                                      , support.boot = BootSupport
                                      , boot.train   = BootTrain
                                      , cross.val    = CrossGoodness
                                      )#end list
                          )#end modifyList
         #---~---
      }else{
         #---~---
         #   Fit the homoscedastic model first to bring coefficients closer to the answer, 
         # then fit the heteroscedastic model.
         #---~---
         zFitHomo = try( expr   = nls( formula = zExpect
                                     , data    = TrainTRY
                                     , start   = zStartLSQ
                                     , control = list( maxiter   = AllomMaxItOptimBest
                                                     , tol       = AllomTolOptimBest
                                                     , minFactor = AllomMinFactor
                                                     )#end list
                                     )#end nls
                       , silent = TRUE
                       )#end try
         if ("try-error" %in% is(zFit)){
            cat0(" Best homoscedastic model failed fitting...")
            browser()
         }else{
            zStartLSQ        = as.list(coef(zFitHomo))
            names(zStartLSQ) = gsub(pattern="^lsq\\.",replacement="",x=names(zStartLSQ))
         }#end if ("try-error" %in% is(zFit))
         #---~---



         #---~---
         #   Fit the heteroscedastic model.
         #---~---
         zFit = try( expr   = optim.lsq.htscd( lsq.formula = zExpect
                                             , sig.formula = zSigma
                                             , data        = TrainTRY
                                             , lsq.first   = zStartLSQ
                                             , sig.first   = zStartSigma
                                             , err.method  = "bootstrap"
                                             , tol.gain    = AllomTolGain
                                             , tol.optim   = AllomTolOptim
                                             , maxit       = AllomMaxItGain
                                             , maxit.optim = AllomMaxItOptim
                                             , n.boot      = AllomCntBoot
                                             , verbose     = TRUE
                                             )#end optim.lsq.htscd
                   , silent = TRUE
                   )#end try
         if ("try-error" %in% is(zFit)){
            cat0(" Best heteroscedastic model failed fitting...")
            browser()
         }#end if ("try-error" %in% is(zFit))
         #---~---



         #---~---
         #   Set coefficients.
         #---~---
         zParam        = coef(zFit)
         names(zParam) = gsub(pattern="^lsq\\." ,replacement=""  ,x=names(zParam))
         names(zParam) = gsub(pattern="^sig\\." ,replacement=""  ,x=names(zParam))
         names(zParam) = gsub(pattern="sigma0"  ,replacement="s0",x=names(zParam))
         if (! "a1" %in% names(zParam)) zParam["a1"] = NA_real_
         if (! "a2" %in% names(zParam)) zParam["a2"] = NA_real_
         if (! "s0" %in% names(zParam)) zParam["s0"] = zFit$sigma0[1L]
         if (! "s1" %in% names(zParam)) zParam["s1"] = 0.
         zParam        = zParam[c("a0","a1","a2","s0","s1")]
         zFit          = modifyList(x=zFit,val=list(coeff.best=zParam))
         #---~---
      }#end if ( zScedastic %in% "Homoscedastic" )
      #---~---


      #---~---
      #   Report the best model and retrieve its settings
      #---~---
      if (Verbose) cat0("   - Best model (",zDescClass,", category ",zCateg,"): ",zDesc,".")
      zModel     = ModelTRY$Model [zBest]
      zFormula   = as.character(zExpect)
      xName      = ModelTRY$xName [zBest]
      wName      = ModelTRY$wName [zBest]
      yName      = ModelTRY$yName [zBest]
      zLogSmp    = ModelTRY$LogSmp[zBest]
      #---~---




      #---~---
      #---~---
      #   Find the range of the "x" predictor and the average of the "w" predictor (if 
      # needed) for the for fitted model.
      #---~---
      #---~---
      #   Decide approach for x variable.
      #---~---
      if (AllomQuantPred){
         #---~---
         #   Predictors based on equally spaced quantiles (we do not use the subsets).
         #---~---
         PredX   = quantile(x=DataTRY[[xName]],probs=PredQuant,names=FALSE,na.rm=TRUE)
         #---~---
      }else{
         #---~---
         #   Predictors based on equally spaced points across range.
         #---~---
         minX  = min(DataTRY[[xName]],na.rm=TRUE)
         maxX  = max(DataTRY[[xName]],na,rm=TRUE)
         if (zLogSmp){
            PredX = exp(seq(from=log(minX),to=log(maxX),length.out=AllomCntPred))
         }else{
            PredX = seq(from=minX,to=maxX,length.out=AllomCntPred)
         }#end if (zLogSmp)
         #---~---
      }#end if if (AllomQuantPred)
      #---~---
      #   For w variable, we fix it at the mean (or set it to NA when not used).
      #---~---
      if (is.na(wName)){
         PredW = rep(x=NA_real_,times=AllomCntPred)
      }else{
         PredW = rep(x=mean(x=DataTRY[[wName]],na.rm=TRUE),times=AllomCntPred)
      }#end if (is.na(wName))
      #---~---



      #---~---
      #   Retrieve parameters, add dummy parameters for the ones that were not used.
      #---~---
      if (Verbose) cat0("     ~ Retrieve parameters and save goodness-of-fit metrics.")
      zParam        = zFit$coeff.best
      #---~---


      #---~---
      #   Set initial guesses for the bootstrap fitting.
      #---~---
      zStartLSQ   = zParam[(! is.na(zParam)) & (! names(zParam) %in% c("s0","s1") )]
      zStartSigma = if(is.null(zSigma)){NULL}else{zParam["s1"]}
      #---~---



      #---~---
      #   Find the predicted height based on the original data and find goodness-of-fit 
      # metrics.
      #---~---
      yOrig = TrainTRY[[yName]]
      yPred = Allom_Pred( x     = TrainTRY[[xName]]
                        , w     = TrainTRY[[wName]]
                        , param = zParam
                        , fun   = zModel
                        , ans   = "mu"
                        )#end try TRY_AllomPred
      ySigma = Allom_Pred( x     = TrainTRY[[xName]]
                         , w     = TrainTRY[[wName]]
                         , param = zParam
                         , fun   = zModel
                         , ans   = "sigma"
                         )#end try TRY_AllomPred
      #---~---


      #---~---
      #   Copy information to the summary.
      #---~---
      saIdx = with(SummAllom, which(( Class %in% zCateg) & ( TraitClass %in% zName )))
      SummAllom$N        [saIdx] = CntSubset
      SummAllom$Function [saIdx] = zModel
      SummAllom$Formula  [saIdx] = zFormula
      SummAllom$Scedastic[saIdx] = zScedastic
      SummAllom$xName    [saIdx] = xName
      SummAllom$wName    [saIdx] = wName
      SummAllom$yName    [saIdx] = yName
      SummAllom$a0       [saIdx] = zParam["a0"]
      SummAllom$a1       [saIdx] = zParam["a1"]
      SummAllom$a2       [saIdx] = zParam["a2"]
      SummAllom$s0       [saIdx] = zParam["s0"]
      SummAllom$s1       [saIdx] = zParam["s1"]
      SummAllom$LogLik   [saIdx] = logLik(zFit)
      SummAllom$Bias     [saIdx] = Allom_Bias   (yOrig=yOrig,yPred=yPred)
      SummAllom$SigRes   [saIdx] = Allom_SigRes (yOrig=yOrig,yPred=yPred)
      SummAllom$MAE      [saIdx] = Allom_MAE    (yOrig=yOrig,yPred=yPred)
      SummAllom$aRMSE    [saIdx] = Allom_aRMSE  (yOrig=yOrig,yPred=yPred,nParam=zCntParam)
      SummAllom$R2Adjust [saIdx] = Allom_R2Adj  (yOrig=yOrig,yPred=yPred,nParam=zCntParam)
      SummAllom$wBias    [saIdx] = Allom_wBias  (yOrig=yOrig,yPred=yPred,ySigma=ySigma)
      SummAllom$wSigRes  [saIdx] = Allom_wSigRes(yOrig=yOrig,yPred=yPred,ySigma=ySigma)
      SummAllom$wMAE     [saIdx] = Allom_wMAE   (yOrig=yOrig,yPred=yPred,ySigma=ySigma)
      SummAllom$wRMSE    [saIdx] = Allom_wRMSE  ( yOrig  = yOrig
                                                , yPred  = yPred
                                                , ySigma = ySigma
                                                , nParam = zCntParam)
      SummAllom$wR2Adjust[saIdx] = Allom_wR2Adj ( yOrig  = yOrig
                                                , yPred  = yPred
                                                , ySigma = ySigma
                                                , nParam = zCntParam
                                                )#end Allom_R2Adj
      SummAllom$AIC      [saIdx] = zFit$AIC
      SummAllom$BIC      [saIdx] = zFit$BIC
      #---~---


      #---~---
      #   Predict model based on selected function.
      #---~---
      spIdx   = with( PredAllom
                    , which( ( Class %in% zCateg ) & ( TraitClass %in% zName ) )
                    )#end with
      PredAllom$xName [spIdx] = xName
      PredAllom$wName [spIdx] = wName
      PredAllom$yName [spIdx] = yName
      PredAllom$x     [spIdx] = PredX
      PredAllom$w     [spIdx] = PredW
      PredAllom$y     [spIdx] = Allom_Pred( x     = PredX
                                          , w     = PredW
                                          , param = zParam
                                          , fun   = zModel
                                          , ans   = "mu"
                                          )#end Allom_Pred
      PredAllom$sigma [spIdx] = Allom_Pred( x     = PredX
                                          , w     = PredW
                                          , param = zParam
                                          , fun   = zModel
                                          , ans   = "sigma"
                                          )#end Allom_Pred
      #---~---



      #---~---
      #   Iterate over the bootstrap to obtain the parameter distribution.
      #---~---
      BootLoop = sequence(AllomCntBoot*is.finite(zBest))
      if (Verbose && ( length(BootLoop) > 0L )){
         cat0("     ~ Calculate predictions using bootstrapped model: ")
         ProgBar = txtProgressBar(max=AllomCntBoot,char=".",style=3L)
      }#end if (Verbose && ( length(BootLoop) > 0L ))
      for (b in BootLoop){
         #---~---
         #   Report progress.
         #---~---
         if (Verbose) dummy = setTxtProgressBar(pb=ProgBar,value=b)
         #---~---


         #---~---
         #   Retrieve parameters, add dummy parameters for the ones that were not used.
         #---~---
         bParam        = zFit$coeff.boot[b,]
         names(bParam) = gsub(pattern="^lsq\\." ,replacement=""  ,x=names(bParam))
         names(bParam) = gsub(pattern="^sig\\." ,replacement=""  ,x=names(bParam))
         names(bParam) = gsub(pattern="sigma0"  ,replacement="s0",x=names(bParam))
         if (! "a1" %in% names(bParam)) bParam["a1"] = NA_real_
         if (! "a2" %in% names(bParam)) bParam["a2"] = NA_real_
         if (! "s0" %in% names(bParam)) bParam["s0"] = zFit$sigma0.boot[b]
         if (! "s1" %in% names(bParam)) bParam["s1"] = 0.
         bParam        = bParam[c("a0","a1","a2","s0","s1")]
         #---~---


         #---~---
         #   Copy information to the summary.
         #---~---
         bsIdx = with( SummBoot, which( ( Class  %in% zCateg ) & ( TraitClass %in% zName )
                                      & ( BootID %in% b      )
                                      )#end which
                     )#end with
         SummBoot$N        [bsIdx] = CntSubset
         SummBoot$Function [bsIdx] = zModel
         SummBoot$Formula  [bsIdx] = zFormula
         SummBoot$Scedastic[bsIdx] = zScedastic
         SummBoot$xName    [bsIdx] = xName
         SummBoot$wName    [bsIdx] = wName
         SummBoot$yName    [bsIdx] = yName
         SummBoot$a0       [bsIdx] = bParam["a0"]
         SummBoot$a1       [bsIdx] = bParam["a1"]
         SummBoot$a2       [bsIdx] = bParam["a2"]
         SummBoot$s0       [bsIdx] = bParam["s0"]
         SummBoot$s1       [bsIdx] = bParam["s1"]
         SummBoot$LogLik   [bsIdx] = zFit$support.boot                     [b]
         SummBoot$Bias     [bsIdx] = zFit$boot.train$goodness$bias         [b]
         SummBoot$SigRes   [bsIdx] = zFit$boot.train$goodness$sigma        [b]
         SummBoot$MAE      [bsIdx] = zFit$boot.train$goodness$mae          [b]
         SummBoot$aRMSE    [bsIdx] = zFit$boot.train$goodness$adj.rmse     [b]
         SummBoot$R2Adjust [bsIdx] = zFit$boot.train$goodness$r.squared    [b]
         SummBoot$wBias    [bsIdx] = zFit$boot.train$goodness$wgt.bias     [b]
         SummBoot$wSigRes  [bsIdx] = zFit$boot.train$goodness$wgt.sigma    [b]
         SummBoot$wMAE     [bsIdx] = zFit$boot.train$goodness$wgt.mae      [b]
         SummBoot$wRMSE    [bsIdx] = zFit$boot.train$goodness$wgt.adj.rmse [b]
         SummBoot$wR2Adjust[bsIdx] = zFit$boot.train$goodness$wgt.r.squared[b]
         SummBoot$xBias    [bsIdx] = zFit$cross.val$bias                   [b]
         SummBoot$xSigRes  [bsIdx] = zFit$cross.val$sigma                  [b]
         SummBoot$xMAE     [bsIdx] = zFit$cross.val$mae                    [b]
         SummBoot$xRMSE    [bsIdx] = zFit$cross.val$rmse                   [b]
         #---~---


         #---~---
         #   Predict model based on current function
         #---~---
         bpIdx = with( PredBoot, which( ( Class  %in% zCateg ) & ( TraitClass %in% zName )
                                      & ( BootID %in% b      )
                                      )#end which
                     )#end with
         PredBoot$xName [bpIdx] = xName
         PredBoot$wName [bpIdx] = wName
         PredBoot$yName [bpIdx] = yName
         PredBoot$x     [bpIdx] = PredX
         PredBoot$w     [bpIdx] = PredW
         PredBoot$y     [bpIdx] = Allom_Pred( x     = PredX
                                            , w     = PredW
                                            , param = bParam
                                            , fun   = zModel
                                            , ans   = "mu"
                                            )#end Allom_Pred
         PredBoot$sigma [bpIdx] = Allom_Pred( x     = PredX
                                            , w     = PredW
                                            , param = bParam
                                            , fun   = zModel
                                            , ans   = "sigma"
                                            )#end Allom_Pred
         #---~---
      }#end for (b in sequence(AllomCntBoot))
      if (Verbose) dummy = close(ProgBar)
      #---~---

      # Find confidence interval for parameters
      StatSumm = SummBoot %>%
         filter( (Class %in% zCateg) & (TraitClass %in% zName) ) %>%
         summarise( SE_a0 = sd(x=a0,na.rm=TRUE)
                  , SE_a1 = sd(x=a1,na.rm=TRUE)
                  , SE_a2 = sd(x=a2,na.rm=TRUE)
                  , SE_s0 = sd(x=s0,na.rm=TRUE)
                  , SE_s1 = sd(x=s1,na.rm=TRUE) )

      # Find confidence interval for y predictions
      StatPred = PredBoot %>%
         filter( (Class %in% zCateg) & (TraitClass %in% zName) ) %>%
         group_by( xID ) %>%
         summarise( yLwr = quantile(x=y,probs=AllomConfLwr,names=FALSE,na.rm=TRUE)
                  , yUpr = quantile(x=y,probs=AllomConfUpr,names=FALSE,na.rm=TRUE) ) %>%
         ungroup()

      #---~---
      #   Copy data back to the main structure
      #---~---
      SummAllom$SE_a0[saIdx] = StatSumm$SE_a0
      SummAllom$SE_a1[saIdx] = StatSumm$SE_a1
      SummAllom$SE_a2[saIdx] = StatSumm$SE_a2
      SummAllom$SE_s0[saIdx] = StatSumm$SE_s0
      SummAllom$SE_s1[saIdx] = StatSumm$SE_s1
      PredAllom$yLwr [spIdx] = StatPred$yLwr
      PredAllom$yUpr [spIdx] = StatPred$yUpr
      #---~---
   }#end for (z in LoopCateg)
   #---~---

   #---~---
   #   Build list with the elements
   #---~---
   Answer = list( SummAllom = SummAllom, PredAllom = PredAllom
                , SummBoot  = SummBoot , PredBoot  = PredBoot  )
   return(Answer)
   #---~---
}#end function Allom_Fit
#---~---



#---~---
#   Generic function that predicts height from DBH (or the standard deviation of residuals)
#
#   Input variables:
#   x     -- First predictor, usually some measurement of tree size (e.g., dbh, height, 
#             dbh^2*height).
#   w     -- Second predictor, usually some mass-related scale (e.g., wood density, LMA).
#   param -- Set of parameters.  It should be a named vector with the following names
#            + a0 -- Scaling parameter
#            + a1 -- Typically a power parameter, but it depends on the function. Not used
#                    depending on the function.
#            + a2 -- Typically a power parameter, but it depends on the function. Not used
#                    depending on the function.
#            + s0 -- Scale factor for local sigma (when model is heteroscedastic).
#            + s1 -- Power factor for local sigma (when model is heteroscedastic).
#   fun   -- Which functional form to use. Currently the following options are acceptable:
#            + OneLinear   : y = a0 * x
#            + OneLogLinear: y = a0 * x^a1
#            + TwoLinear   : y = a0 * x    * w
#            + TwoMixLinear: y = a0 * x^a1 * w
#            + TwoLogLinear: y = a0 * x^a1 * w^a2
#            + MartinezCano: y = a0 * x^a1 / ( a2 + x^a1 )
#            + Weibull     : y = a0 * (1 - exp (-a1 * x^a2 ))
#   ans   -- What to return. "mu" returns the predicted value, "sigma" returns the local
#            standard deviation of the residuals.
#---~---
Allom_Pred <<- function( x
                       , w     = NULL
                       , param
                       , fun   = c( "OneLinear"   , "OneLogLinear", "TwoLinear"
                                  , "TwoMixLinear", "TwoLogLinear", "MartinezCano"
                                  , "Weibull"     )
                       , ans   = c("mu","sigma")
                       ){

   #---~---
   #   Match the arguments
   #---~---
   fun = match.arg(fun)
   ans = match.arg(ans)
   #---~---



   #---~---
   #   Find the expected value
   #---~---
   if (fun %in% "OneLinear"){
      mu = param["a0"] * x
   }else if (fun %in% "OneLogLinear"){
      mu = param["a0"] * x^param["a1"]
   }else if (fun %in% "TwoLinear"){
      mu = param["a0"] * x * w
   }else if (fun %in% "TwoMixLinear"){
      mu = param["a0"] * x^param["a1"] * w
   }else if (fun %in% "TwoLogLinear"){
      mu = param["a0"] * x^param["a1"] * w^param["a2"]
   }else if (fun %in% "MartinezCano"){
      mu = param["a0"] * x^param["a1"] / ( param["a2"] + x^param["a1"] )
   }else if (fun %in% "Weibull"){
      mu = param["a0"] * ( 1. - exp( - param["a1"] * x^param["a2"] ) )
   }#end if
   #---~---


   #---~---
   #   Decide what to return
   #---~---
   if (ans %in% "mu"){
      #---~---
      #   Expected value
      #---~---
      return(mu)
      #---~---
   }else{
      #---~---
      #   Find the local standard deviation of the parameters
      #---~---
      sigma = param["s0"] * mu^param["s1"]
      return(sigma)
      #---~---
   }#end if (ans %in% "mu")
   #---~---
}#end Allom_Pred
#---~---



#---~---
#     This function computes the adjusted R2 for allometric models. This is intended to be
# used for full-model assessment only, as it inherently assumes that the model is unbiased.
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
# nParam - number of model parameters
#
#---~---
Allom_R2Adj <<- function(yOrig,yPred,nParam){

   #---~---
   #   Keep only the entries in which all yOrig and yPred are valid
   #---~---
   yKeep  = is.finite(yOrig) & is.finite(yPred)
   nData  = sum(yKeep)
   yOrig  = yOrig[yKeep]
   yPred  = yPred[yKeep]
   #---~---


   #---~---
   #   Find the average of the reference data.
   #---~---
   yMean = mean(yOrig)
   #---~---


   #---~---
   #   Find sum of squares and degrees of freedom
   #---~---
   SSRes = sum((yOrig-yPred)^2)
   SSTot = sum((yOrig-yMean)^2)
   DFRes = nData - nParam
   DFTot = nData - 1L
   #---~---


   #---~---
   #   Find the adjusted R2 and report it.
   #---~---
   R2Adj = 1. - SSRes * DFTot / ( SSTot * DFRes )
   return(R2Adj)
   #---~---
}#end Allom_R2Adj
#---~---



#---~---
#     This function computes the adjusted weighted R2 for allometric models. This is intended
# to be used for full-model assessment only, as it inherently assumes that the model is 
# unbiased.  The weighting factor is the inverse of the predicted standard deviation of the
# residuals, following WN88.
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
# ySigma - vector with the predicted values of standard deviation of the residuals.
# nParam - number of model parameters
#
# Reference:
# 
# Willett, J. B., and J. D. Singer, 1988: Another cautionary note about r2: Its use in 
#    weighted least-squares regression analysis. Am. Stat., 42 (3), 236-238,
#    doi:10.1080/00031305.1988.10475573 (WN88).
#---~---
Allom_wR2Adj <<- function(yOrig,yPred,ySigma,nParam){
   #---~---
   #   Keep only the entries in which all yOrig, yPred and ySigma are valid
   #---~---
   yKeep  = is.finite(yOrig) & is.finite(yPred) & (ySigma %gt% 0.)
   nData  = sum(yKeep)
   yOrig  = yOrig[yKeep] / ySigma[yKeep]
   yPred  = yPred[yKeep] / ySigma[yKeep]
   #---~---


   #---~---
   #   Find the average of the reference data.
   #---~---
   yMean = mean(yOrig)
   #---~---


   #---~---
   #   Find sum of squares and degrees of freedom
   #---~---
   SSRes = sum((yOrig-yPred)^2)
   SSTot = sum((yOrig-yMean)^2)
   DFRes = nData - nParam
   DFTot = nData - 1L
   #---~---


   #---~---
   #   Find the adjusted R2 and report it.
   #---~---
   R2Adj = 1. - SSRes * DFTot / ( SSTot * DFRes )
   return(R2Adj)
   #---~---
}#end Allom_wR2Adj
#---~---



#---~---
#     This function computes the adjusted root mean square error of prediction
# for allometric models. This is intended to be used for full-model assessment and not
# cross-validation, as it accounts for number of degrees of freedom.
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
# nParam - number of model parameters
#---~---
Allom_aRMSE <<- function(yOrig,yPred,nParam){
   
   #---~---
   #   Keep only the entries in which both yOrig and yPred are valid
   #---~---
   yKeep = is.finite(yOrig) & is.finite(yPred)
   yOrig = yOrig[yKeep]
   yPred = yPred[yKeep]
   yMean = mean(yOrig)
   nData = length(yOrig)
   #---~---


   #---~---
   #   Find sum of squares and degrees of freedom, then compute the RMSE of predictions
   #---~---
   SSRes = sum((yOrig-yPred)^2)
   DFRes = nData - nParam
   ans   = sqrt(SSRes/DFRes)
   return(ans)
   #---~---
}#end Allom_aRMSE
#---~---



#---~---
#     This function computes the adjusted weighted root mean square error of prediction
# for allometric models. This is intended to be used for full-model assessment and not
# cross-validation, as it accounts for number of degrees of freedom.
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
# ySigma - vector with predicted standard error of variable y
# nParam - number of model parameters
#---~---
Allom_wRMSE <<- function(yOrig,yPred,ySigma,nParam){
   
   #---~---
   #   Keep only the entries in which both yOrig and yPred are valid
   #---~---
   yKeep = is.finite(yOrig) & is.finite(yPred) & ( ySigma %gt% 0. )
   wOrig = yOrig[yKeep] / ySigma[yKeep]
   wPred = yPred[yKeep] / ySigma[yKeep]
   wMean = mean(yOrig)
   nData = length(wOrig)
   #---~---


   #---~---
   #   Find sum of squares and degrees of freedom, then compute the RMSE of predictions
   #---~---
   SSRes = sum((wOrig-wPred)^2)
   DFRes = nData - nParam
   ans   = sqrt(SSRes/DFRes)
   return(ans)
   #---~---
}#end Allom_wRMSE
#---~---



#---~---
#     This function computes the mean absolute error. This can be used for both the full
# model assessment and cross-validation.
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
#---~---
Allom_MAE <<- function(yOrig,yPred){
   #---~---
   #   Keep only the entries in which both yOrig and yPred are valid
   #---~---
   yKeep  = is.finite(yOrig) & is.finite(yPred)
   yOrig  = yOrig[yKeep]
   yPred  = yPred[yKeep]
   yResid = yOrig - yPred
   #---~---


   #---~---
   #   Find mean bias
   #---~---
   ans   = mean(abs(yResid))
   return(ans)
   #---~---
}#end Allom_MAE
#---~---



#---~---
#     This function computes the weighted mean absolute error. This can be used for both the 
# full model assessment and cross-validation.
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
# ySigma - vector with predicted standard error of variable y
#---~---
Allom_wMAE <<- function(yOrig,yPred,ySigma){
   #---~---
   #   Keep only the entries in which both yOrig and yPred are valid
   #---~---
   yKeep  = is.finite(yOrig) & is.finite(yPred) & ( ySigma %gt% 0. )
   wOrig  = yOrig[yKeep] / ySigma[yKeep]
   wPred  = yPred[yKeep] / ySigma[yKeep]
   wResid = wOrig - wPred
   #---~---


   #---~---
   #   Find mean bias
   #---~---
   ans   = mean(abs(wResid))
   return(ans)
   #---~---
}#end Allom_wMAE
#---~---



#---~---
#     This function computes the mean bias (negative average of the residuals).
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
#---~---
Allom_Bias <<- function(yOrig,yPred){
   #---~---
   #   Keep only the entries in which both yOrig and yPred are valid
   #---~---
   yKeep  = is.finite(yOrig) & is.finite(yPred)
   yOrig  = yOrig[yKeep]
   yPred  = yPred[yKeep]
   yResid = yOrig - yPred
   #---~---


   #---~---
   #   Find mean bias
   #---~---
   ans   = mean(-yResid)
   return(ans)
   #---~---
}#end Allom_Bias
#---~---



#---~---
#     This function computes the weighted mean bias (negative average of the weighted 
# residuals).
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
# ySigma - vector with predicted standard error of variable y
#---~---
Allom_wBias <<- function(yOrig,yPred,ySigma){
   #---~---
   #   Keep only the entries in which both yOrig and yPred are valid and ySigma is positive
   #---~---
   yKeep  = is.finite(yOrig) & is.finite(yPred) & (ySigma %gt% 0.)
   wOrig  = yOrig[yKeep] / ySigma[yKeep]
   wPred  = yPred[yKeep] / ySigma[yKeep]
   wResid = wOrig - wPred
   #---~---


   #---~---
   #   Find mean bias
   #---~---
   ans   = mean(-wResid)
   return(ans)
   #---~---
}#end Allom_wBias
#---~---



#---~---
#     This function computes the standard deviation of residuals. This is not the same as 
# the RMSE because it does not account for bias and the number of parameters.
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
#---~---
Allom_SigRes <<- function(yOrig,yPred){
   #---~---
   #   Keep only the entries in which both yOrig and yPred are valid
   #---~---
   yKeep  = is.finite(yOrig) & is.finite(yPred)
   yOrig  = yOrig[yKeep]
   yPred  = yPred[yKeep]
   yResid = yOrig - yPred
   #---~---


   #---~---
   #   Find standard deviation of residuals
   #---~---
   ans   = sd(yResid)
   return(ans)
   #---~---
}#end Allom_SigRes
#---~---



#---~---
#     This function computes the weighted standard deviation of residuals. This is not the same
# as the RMSE because it does not account for bias and the number of parameters.
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
# ySigma - vector with predicted standard error of variable y
#---~---
Allom_wSigRes <<- function(yOrig,yPred,ySigma){
   #---~---
   #   Keep only the entries in which both yOrig and yPred are valid and ySigma is positive
   #---~---
   yKeep  = is.finite(yOrig) & is.finite(yPred) & (ySigma %gt% 0.)
   wOrig  = yOrig[yKeep] / ySigma[yKeep]
   wPred  = yPred[yKeep] / ySigma[yKeep]
   wResid = wOrig - wPred
   #---~---


   #---~---
   #   Find standard deviation of the weighted residuals
   #---~---
   ans   = sd(wResid)
   return(ans)
   #---~---
}#end Allom_wSigRes
#---~---





#---~---
#   This function sets the training data such that it does not exceed the maximum number 
# of training points. Depending on the options, it will also resample the data to ensure 
# the sampling effort is balanced across the sample size.
#
# Inputs
#
# + OrigTRY      -- Original data frame to be used as training data.
# + CntAllomMax  -- Maximum number of points to consider for fitting. This is used to
#                   limit the model fitting to a more manageable number of 
#                   observations when allometric data are exceedingly large. If no
#                   cap is sought, set CntAllomMax = +Inf. This setting is 
#                   independent on the data binning sampling defined by UseSizeBins.
# + UseSizeBins  -- Use binning across size classes to have a more balanced 
#                   distribution across the predictor range? Note that in this case
#                   we cannot use AIC or BIC to define the best model, and thus we
#                   simply use adjusted R2.
# + xName        -- Name of the column with size data.
# + LogSmp       -- Apply log transformation when defining the bin boundaries?
# + MinSmpPerBin -- Minimum number of observations for each bin. Binning will not 
#                   occur unless there are at least twice as many valid points (and
#                   possibly even more points in case of very imbalanced data sets).
# + MaxCntBins   -- Maximum number of bins to be considered.
#---~---
Allom_SetTrain <<- function( OrigTRY
                           , CntAllomMax  = 10000L
                           , UseSizeBins  = FALSE
                           , xName
                           , LogSmp       = FALSE
                           , MinSmpPerBin = 500L
                           , MaxCntBins   = 20L
                           ){

   #---~---
   #   Find the number of valid data points and a few ancillary variables.
   #---~---
   CntOrig   = nrow(OrigTRY)            # Original sample size
   OrigIndex = sequence(CntOrig)        # Vector indices of the original sample
   CntUpper  = min(CntOrig,CntAllomMax) # Upper limit for the sampling effort
   #---~---


   #---~---
   #   If we are to use sampling, create a sample of data points that is evenly
   # distributed across size classes.
   #---~---
   if (UseSizeBins){
      #---~---
      #   Find the bounds for the binning; we use quantiles to avoid outliers stretching
      # the bins.
      #---~---
      qLwrX = quantile(x=OrigTRY[[xName]],probs=0.01,names=FALSE,na.rm=TRUE)
      qUprX = quantile(x=OrigTRY[[xName]],probs=0.99,names=FALSE,na.rm=TRUE)
      #---~---



      #---~---
      #   First guess for number of bins.
      #---~---
      InitCntBins = max(1L, min(MaxCntBins, floor(CntOrig / MinSmpPerBin)))
      #---~---



      #---~---
      #   Iterate until all bins have at least the minimum number of samples. In case
      # the data does not have enough points for binning, we give up on re-sampling.
      #---~---
      CntBins = InitCntBins + 1L
      iterate = TRUE
      while (iterate){
         CntBins     = CntBins - 1L
         CntBrks     = CntBins + 1L


         #---~---
         #   If CntBins is 1, we give up on binning data, as we don't have enough data 
         # points.
         #---~---
         if (CntBins %eq% 1L){
            #---~---
            #   Stop iterating.
            #---~---
            iterate = FALSE
            #---~---

            #---~---
            #   No binning, but we still sample in case the number of data points 
            # exceeds the maximum.
            #---~---
            xIndex   = Allom_Sample(x=OrigIndex,size=CntUpper)
            TrainTRY = OrigTRY[xIndex,,drop=FALSE]
            #---~---
         }else{
            #---~---
            #   Find the number of bins. We don't split in quantiles, but in equal width
            # bins because the idea is to balance the range of samples.
            #---~---
            if (LogSmp){
               qBrksX          = exp(seq(from=log(qLwrX),to=log(qUprX),length.out=CntBrks))
               qBrksX[     1L] = -Inf
               qBrksX[CntBrks] = +Inf
            }else{
               qBrksX          = seq(from=qLwrX,to=qUprX,length.out=CntBrks)
               qBrksX[     1L] = -Inf
               qBrksX[CntBrks] = +Inf
            }#end if (mLogBins)
            #---~---


            #---~---
            #   Split variable X into bins.
            #---~---
            xCut      = cut(OrigTRY[[xName]],breaks=qBrksX,labels=FALSE)
            TallyCut  = table(xCut)
            CntSample = floor(CntUpper / CntBins)
            #---~---


            #---~---
            #   Make sure the sample size is fine. If not, iterate.
            #---~---
            UseBins = all(TallyCut %ge% MinSmpPerBin)
            iterate = ! UseBins
            if (UseBins){
               xIndex   = c( unlist( mapply( FUN      = Allom_Sample
                                           , x        = split(x=seq_along(xCut),f=xCut)
                                           , MoreArgs = list(size=CntSample)
                                           , SIMPLIFY = FALSE
                                           )#end mapply
                                   )#end unlist
                           )#end c
               names(xIndex) = NULL
               TrainTRY = OrigTRY[xIndex,,drop=FALSE]
            }#end if (UseBins)
            #---~---
         }#end if (CntBins %eq% 1L)
         #---~---
      }#end while (iterate)
      #---~---
   }else{
      #---~---
      #   No binning, but we still sample in case the number of data points exceeds the 
      # maximum.
      #---~---
      xIndex   = Allom_Sample(x=OrigIndex,size=CntUpper)
      TrainTRY = OrigTRY[xIndex,,drop=FALSE]
      #---~---
   }#end if (UseSizeBins)
   #---~---


   #---~---
   #   Return training data
   #---~---
   return(TrainTRY)
   #---~---
}#end function Allom_SetTrain
#---~---





#---~---
#   This function sets the first guesses and a few additional parameters for the parameter
# optimisation.
#
# Inputs
#
# + OrigTRY      -- Original data frame to be used as training data.
# + Model        -- Which model to fit? Options are
#                   - OneLinear (y = a0 * x )
#                   - OneLogLinear (y = a0 * x^a1                    )
#                   - TwoLinear    (y = a0 * x    * w                )
#                   - TwoMixLinear (y = a0 * x^a1 * w                )
#                   - TwoLogLinear (y = a0 * x^a1 * w^2              )
#                   - MartinezCano (y = a0 * x^a1 / (a2 + x^a1)      )
#                   - Weibull      (y = a0 * (1 - exp( -a1 * x^a2) ) )
# + xName        -- Name of the column with main predictor (x).
# + yName        -- Name of the column with predictand (y)
# + wName        -- Name of the column with scaling data (only needed for the models
#                   above that have a "w" term).

# + CntAllomMax  -- Maximum number of points to consider for fitting. This is used to
#                   limit the model fitting to a more manageable number of 
#                   observations when allometric data are exceedingly large. If no
#                   cap is sought, set CntAllomMax = +Inf. This setting is 
#                   independent on the data binning sampling defined by UseSizeBins.
# + UseSizeBins  -- Use binning across size classes to have a more balanced 
#                   distribution across the predictor range? Note that in this case
#                   we cannot use AIC or BIC to define the best model, and thus we
#                   simply use adjusted R2.
# + LogSmp       -- Apply log transformation when defining the bin boundaries?
# + MinSmpPerBin -- Minimum number of observations for each bin. Binning will not 
#                   occur unless there are at least twice as many valid points (and
#                   possibly even more points in case of very imbalanced data sets).
# + MaxCntBins   -- Maximum number of bins to be considered.
#---~---
Allom_PrepOptim <<- function( OrigTRY
                            , Model = c("OneLinear","OneLogLinear","TwoLinear"
                                       ,"TwoMixLinear","TwoLogLinear","MartinezCano"
                                       ,"Weibull")
                            , xName
                            , yName
                            , wName
                            ){

   #---~---
   #   Make sure Model is a valid model
   #---~---
   Model = match.arg(Model)
   #---~---


   #---~---
   #   First, we find some information about predictors and predictand that may be
   # useful for defining first guesses.
   #---~---
   qLwrX = quantile(x=OrigTRY[[xName]],probs=0.025,names=FALSE,na.rm=TRUE)
   qMidX = quantile(x=OrigTRY[[xName]],probs=0.500,names=FALSE,na.rm=TRUE)
   qUprX = quantile(x=OrigTRY[[xName]],probs=0.975,names=FALSE,na.rm=TRUE)
   qLwrY = quantile(x=OrigTRY[[yName]],probs=0.025,names=FALSE,na.rm=TRUE)
   qMidY = quantile(x=OrigTRY[[yName]],probs=0.500,names=FALSE,na.rm=TRUE)
   qUprY = quantile(x=OrigTRY[[yName]],probs=0.975,names=FALSE,na.rm=TRUE)
   #--- W is not always available.
   if (is.na(wName)){
      qMidW = NA_real_
   }else{
      qMidW = quantile(x=OrigTRY[[wName]],probs=0.500,names=FALSE,na.rm=TRUE)
   }#end if (is.na(wName))
   #---~---


   #---~---
   #   Set predictors, number of parameters and first guesses based on the functional 
   # form of the model.
   #---~---
   Predictor = 
     switch( EXPR         = Model
           , OneLinear    = paste0("a0 * ",xName                              )
           , OneLogLinear = paste0("a0 * ",xName,"^a1"                        )
           , TwoLinear    = paste0("a0 * ",xName," * ",wName                  )
           , TwoMixLinear = paste0("a0 * ",xName,"^a1 * ",wName               )
           , TwoLogLinear = paste0("a0 * ",xName,"^a1 * ",wName,"^a2"         )
           , MartinezCano = paste0("a0 * ",xName,"^a1 / ( a2 + ",xName,"^a1 )")
           , Weibull      = paste0("a0 * (1 - exp( -a1 * ",xName,"^a2 ) )"    )
           , stop(paste0(" Invalid Model: \"",Model,"\".")                   )
           )#end Predictor
   CntParam = 
     switch( EXPR         = Model
           , OneLinear    = 1L
           , OneLogLinear = 2L
           , TwoLinear    = 1L
           , TwoMixLinear = 2L
           , TwoLogLinear = 3L
           , MartinezCano = 3L
           , Weibull      = 3L
           )#end Predictor
   First_a0  =
     switch( EXPR         = Model
           , OneLinear    = ( qUprY - qLwrY ) / ( qUprX - qLwrX)
           , OneLogLinear = ( qUprY - qLwrY ) / ( qUprX - qLwrX)
           , TwoLinear    = ( qUprY - qLwrY ) / ( qUprX - qLwrX) / qMidW
           , TwoMixLinear = ( qUprY - qLwrY ) / ( qUprX - qLwrX) / qMidW
           , TwoLogLinear = ( qUprY - qLwrY ) / ( qUprX - qLwrX)
           , MartinezCano = qUprY
           , Weibull      = qUprY
           )#end First_a0
   First_a1  =
     switch( EXPR         = Model
           , OneLinear    = NULL
           , OneLogLinear = 1.
           , TwoLinear    = NULL
           , TwoMixLinear = 1.
           , TwoLogLinear = 1.
           , MartinezCano = 1.
           , Weibull      = 1./qUprX
           )#end First_a1
   First_a2  =
     switch( EXPR         = Model
           , OneLinear    = NULL
           , OneLogLinear = NULL
           , TwoLinear    = NULL
           , TwoMixLinear = NULL
           , TwoLogLinear = 0.
           , MartinezCano = qMidX
           , Weibull      = 1.
           )#end First_a2
   #---~---


   #---~---
   #   Define equations for expected model and expected variance.
   #---~---
   Expect     = paste0(yName," ~ I(",Predictor,")")
   Sigma      = paste0(" ~ I(yhat^s1)")
   StartLSQ   = as.list(c( a0 = First_a0, a1 = First_a1, a2 = First_a2 ))
   StartSigma = list(s1=0.)
   #---~---


   #---~---
   #   For some equations, we may want to test log-normal transformations. When a 
   # log-normal transformation is not sought, set it to NA_character_
   #---~---
   LnExpect = 
     switch( EXPR         = Model
           , OneLogLinear = paste0("log(",yName,") ~ log(",xName,")"                  )
           , TwoMixLinear = paste0("log(",yName,")"
                                  ," ~ log(",xName,") + offset(log(",wName,"))"      )
           , TwoLogLinear = paste0("log(",yName,") ~ log(",xName,") + log(",wName,")")
           , NA_character_
           )#end LnExpect
   #---~---


   #---~---
   #   Build a list with all the information
   #---~---
   Answer = list( Predictor  = Predictor
                , CntParam   = CntParam
                , Expect     = Expect
                , LnExpect   = LnExpect
                , Sigma      = Sigma
                , StartLSQ   = StartLSQ
                , StartSigma = StartSigma
                )#end list
   #---~---

   #---~---
   #   Return training data
   #---~---
   return(Answer)
   #---~---
}#end function Allom_PrepOptim
#---~---




#---~---
#   This function finds the back-transformation correction factor when back-transforming 
# estimators of log-transformed fitted models. This is needed because the mean of log-residuals
# is not the same as the log of the mean residuals, so an unbiased log-transformed model can
# become biased after back-transforming estimates.
#
#    We test multiple approaches and select the one that produces the lowest bias after back-
# transforming the data.
#
# 1. Baskerville (1972). This is the most formal approach, which is consistent with the 
#    assumption that after log-transforming the variables, the errors become normally 
#    distributed and homoscesdatic. This correction can produce more biases if the assumptions
#    are not met.
# 2. Duan (1983). This approach does not assume that errors are normally distributed in the 
#    log-transformed space.
# 3. Snowdon (1991). This approach defines the correction term to be the ratio between the 
#    arithmetic sample mean and the mean of the fitted observations after back-transformation 
#    without any correction. This yields to an unbiased estimate of fitted data by default.
# 4. Modified Snowdon (1991). Similar to the original approach, except that we use the slope
#    of a fitted model between observations and the back-transformed values without any 
#    correction, assuming zero intercept, following the thread below:
#    https://tinyurl.com/9xpycvj8
#
# References:
#
# Baskerville, G. L., 1972: Use of logarithmic regression in the estimation of plant biomass. 
#    Can. J. Forest Res., 2 (1), 49-53, doi:10.1139/x72-009 (B72).
# Duan, N., 1983: Smearing estimate: A nonparametric retransformation method. J. Am. Stat. 
#    Assoc., 78 (383), 605-610, doi:10.1080/01621459.1983.10478017 (D83).
# Snowdon, P., 1991: A ratio estimator for bias correction in logarithmic regressions. Can. J. 
#    Forest Res., 21 (5), 720-724, doi:10.1139/x91-101 (S91).
#---~---
Allom_BackLog_Scaler <<- function(Model,Observ,CntParam){

   #---~---
   #   Retrieve the back-transformed fitted values without correction (aka naive 
   # back-transformation).
   #---~---
   Naive = exp(fitted(Model))
   CntFitted = length(Naive)
   #---~---

   #---~---
   #   Retrieve the residuals and sigma of the log-transformed model
   #---~---
   LnError = - residuals(Model)
   LnSigma = sigma(Model)
   #---~---

   #---~---
   #   Fit the slope of the curve between naively back-transformed fitted data and observation.
   #---~---
   FitSmearing = rlm(Observ ~ Naive)
   #---~---


   #---~---
   #   Find the scaling factors of all methods.
   #---~---
   AllMuSmearings   = 
      data.frame( Naive        = 1.0
                , Baskerville = exp(0.5*LnSigma^2)
                , Duan        = mean(exp(LnError),na.rm=TRUE)
                , Snowdon     = mean(Observ,na.rm=TRUE) / mean(Naive,na.rm=TRUE)
                , ModSnowdon  = coef(FitSmearing)[1L]
                )#end data.frame
   #---~---



   #---~---
   #   Apply corrections.
   #---~---
   Baskerville = AllMuSmearings$Baskerville * Naive
   Duan        = AllMuSmearings$Duan        * Naive
   Snowdon     = AllMuSmearings$Snowdon     * Naive
   ModSnowdon  = AllMuSmearings$ModSnowdon  * Naive
   #---~---



   #---~---
   #   Find residuals of each approach.
   #---~---
   rNaive       = Observ - Naive
   rBaskerville = Observ - Baskerville
   rDuan        = Observ - Duan
   rSnowdon     = Observ - Snowdon
   rModSnowdon  = Observ - ModSnowdon
   #---~---


   #---~---
   #   Find the actual standard deviation of residuals.
   #---~---
   AllSigma =
      data.frame( Naive       = sqrt(sum(rNaive^2      ,na.rm=TRUE) / (CntFitted - CntParam))
                , Baskerville = sqrt(sum(rBaskerville^2,na.rm=TRUE) / (CntFitted - CntParam))
                , Duan        = sqrt(sum(rDuan^2       ,na.rm=TRUE) / (CntFitted - CntParam))
                , Snowdon     = sqrt(sum(rSnowdon^2    ,na.rm=TRUE) / (CntFitted - CntParam))
                , ModSnowdon  = sqrt(sum(rSnowdon^2    ,na.rm=TRUE) / (CntFitted - CntParam))
                )#end data.frame
   #---~---


   #---~---
   #   Find the expected standard deviation of residuals by using the assumption that the 
   # local standard deviation of residuals scale linearly with the absolute value of the fitted
   # values, following the expected heteroscedasticity of log-linear transformation.
   #---~---
   sNaive       = sqrt(AllSigma$Naive^2       / mean(Naive^2      ,na.rm=TRUE))
   sBaskerville = sqrt(AllSigma$Baskerville^2 / mean(Baskerville^2,na.rm=TRUE))
   sDuan        = sqrt(AllSigma$Duan^2        / mean(Duan^2       ,na.rm=TRUE))
   sSnowdon     = sqrt(AllSigma$Snowdon^2     / mean(Snowdon^2    ,na.rm=TRUE))
   sModSnowdon  = sqrt(AllSigma$ModSnowdon^2  / mean(ModSnowdon^2 ,na.rm=TRUE))
   AllSigmaSmearings =
      data.frame( Naive       = sNaive
                , Baskerville = sBaskerville
                , Duan        = sDuan
                , Snowdon     = sSnowdon
                , ModSnowdon  = sSnowdon
                )#end data.frame
   #---~---


   #---~---
   #   Find the root mean square error of each approach
   #---~---
   AllRMSEs =
      data.frame( Naive       = Allom_aRMSE(yOrig=Observ,yPred=Naive      ,nParam=CntParam)
                , Baskerville = Allom_aRMSE(yOrig=Observ,yPred=Baskerville,nParam=CntParam)
                , Duan        = Allom_aRMSE(yOrig=Observ,yPred=Duan       ,nParam=CntParam)
                , Snowdon     = Allom_aRMSE(yOrig=Observ,yPred=Snowdon    ,nParam=CntParam)
                , ModSnowdon  = Allom_aRMSE(yOrig=Observ,yPred=ModSnowdon ,nParam=CntParam)
                )#end data.frame
   #---~---


   #---~---
   #   Find the smearing factors for the 
   #---~---
   #---~---


   #---~---
   #   Select method that has the lowest RMSE:
   #---~---
   AllMethods = names(AllRMSEs)
   Best       = which.min(c(unlist(AllRMSEs)))
   Method     = AllMethods[[Best]]
   Answer     = list( Method        = Method
                    , MuSmearing    = AllMuSmearings   [[Method]]
                    , SigmaSmearing = AllSigmaSmearings[[Method]]
                    , RMSE          = AllRMSEs         [[Method]]
                    , AllResults    = data.frame( Method        = AllMethods
                                                , MuSmearing    = AllMuSmearings
                                                , SigmaSmearing = AllSigmaSmearings
                                                , RMSE          = AllRMSEs
                                                )#end list
                    )#end list
   #---~---


   #---~---
   #   Return Answer
   #---~---
   return(Answer)
   #---~---
}#end function Allom_BackLog_Scaler
#---~---



#---~---
#   This function finds the Akaike Information criterion based on A74, with the additional
# correction for small sample size (BA02), and a potential correction for transformed 
# variables (A78).
#
# References:
#
# Akaike, H., 1974: A new look at the statistical model identification. 
#    IEEE T. Automat. Contr., 19 (6), 716-723, doi:10.1109/TAC.1974.1100705 (A74).
#
# Akaike, H., 1978: On the likelihood of a time series model. J. Roy. Stat. Soc. D-Sta., 
#    27 (3-4), 217-235, doi:10.2307/2988185 (A78).
#
# Burnham, K. P., and D. R. Anderson, 2002: Model Selection and Multimodel Inference: A 
#    practical information -- theoretic approach. 2nd ed., Springer New York, New York, NY,
#    U.S.A., doi:10.1007/b97636 (BA02).
#---~---
Allom_AIC <<- function( Object   # Fitted model object
                      , Observ   # Observed values
                      , Scedastic = c("Homoscedastic","Log-Homoscedastic","Heteroscedastic")
                      ){
   #---~---
   #   Make sure the scedasticity is a valid one.
   #---~---
   Scedastic = match.arg(Scedastic)
   #---~---


   #---~---
   #   Find the parameters needed for computing the information criterion.
   #---~---
   N = length(Object)
   P = length(coef(Object))
   #---~---


   #---~---
   #   Find the log-likelihood. For the log-transformed model, we must account for the 
   # log-transformation effect on the log-likelihood of the back-transformed data, following
   # A78 and BA02.
   #---~---
   if ( Scedastic %in% "Log-Homoscedastic" ){
      Support = c(logLik(Object)) - sum(log(Observ))
   }else{
      Support = c(logLik(Object))
   }#end if ( Scedastic %in% "Log-Homoscedastic" )
   #---~---


   #---~---
   #   Compute the second-order AIC
   #---~---
   Ans     = 2. * (N - Support + P*(P+1L)/(N-P-1L))
   return(Ans)
   #---~---
}#end function Allom_AIC
#---~---



#---~---
#   This function finds the Schwarz (Bayesian) Information criterion based on S78, with the 
# additional correction for small sample size, and a potential correction for transformed 
# variables following A78.
#
# References:
#
# Schwarz, G., 1978: Estimating the dimension of a model. Ann. Stat., 6 (2), 461-464, 
#    doi:10.1214/aos/1176344136.
#
# Akaike, H., 1978: On the likelihood of a time series model. J. Roy. Stat. Soc. D-Sta., 
#    27 (3-4), 217-235, doi:10.2307/2988185 (A78).
#
# Burnham, K. P., and D. R. Anderson, 2002: Model Selection and Multimodel Inference: A 
#    practical information -- theoretic approach. 2nd ed., Springer New York, New York, NY,
#    U.S.A., doi:10.1007/b97636 (BA02).
#---~---
Allom_BIC <<- function( Object   # Fitted model object
                      , Observ   # Observed values
                      , Scedastic = c("Homoscedastic","Log-Homoscedastic","Heteroscedastic")
                      ){
   #---~---
   #   Make sure the scedasticity is a valid one.
   #---~---
   Scedastic = match.arg(Scedastic)
   #---~---


   #---~---
   #   Find the parameters needed for computing the information criterion.
   #---~---
   N = length(Object)
   P = length(coef(Object))
   #---~---


   #---~---
   #   Find the log-likelihood. For the log-transformed model, we must account for the 
   # log-transformation effect on the log-likelihood of the back-transformed data, following
   # A78 and BA02.
   #---~---
   if ( Scedastic %in% "Log-Homoscedastic" ){
      Support = c(logLik(Object)) - sum(log(Observ))
   }else{
      Support = c(logLik(Object))
   }#end if ( Scedastic %in% "Log-Homoscedastic" )
   #---~---


   #---~---
   #   Compute BIC
   #---~---
   Ans     = P * ( N - log(2.*pi) ) - 2. * Support
   return(Ans)
   #---~---
}#end function Allom_BIC
#---~---



#---~---
#     This function is a wrapper for sampling that decides whether to run with or without
# replacement depending on the requested size.
#
# Input variables:
# x    - Vector to be sampled
# size - Size of the vector with samples.
#---~---
Allom_Sample <<- function(x,size){
   #---~---
   #   Make sure that x has more than one element (so sample will not assume x is the 
   # number of integer elements to be sampled).
   #---~---
   if (length(x) == 1) x=c(x,x)
   ans       = sort(sample(x=x,size=size,replace=length(x) < size))
   return(ans)
   #---~---
}#end Allom_Sample
#---~---
