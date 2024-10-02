#==========================================================================================
#==========================================================================================
#    This function partitions the incoming solar irradiance by spectral range 
# (visible and near-infrared) and by irradiance distribution properties (direct and
# diffuse).
#
#  Input variables:
#  ~~~~~~~~~~~~~~~~
#
#  - rad_in     -- Reference downward irradiance at surface, in W/m2, regardless of the
#                  irradiance type (see below).
#  - datetime   -- date and time, using lubridate format.
#  - atm_prss   -- Surface atmospheric pressure, in Pa. Either provide actual measurements,
#                  or a typical pressure at the given location of interest.  Pressure will
#                  be used to account for altitude effects on maximum irradiance.
#  - lon        -- Longitude of the point.  Required, single value (scalar) only.
#  - lat        -- Latitude of the point.  Required, single value (scalar) only.
#  - alt        -- Altitude of the point, in m.  Required, single value (scalar) only.
#  - rad_type   -- Which irradiance type is provided. Options are:
#                  * rshort -- Total solar (shortwave) irradiance (default)
#                  * par    -- Visible irradiance (photosynthetic active "radiation")
#                  * nir    -- Near-infrared irradiance
#  - rad_method -- Which method to use for partitioning irradiance?
#                  * wn85     -- Use the method by Weiss and Norman (1985) (default)
#                  * sib      -- Use the SiB method (Sellers et al. 1986)
#                  * clearidx -- Use the clearness index, based on Bendix et al. (2010),
#                                Boland et al. (2008) and Tsubo and Walker (2005).
#  - is_average -- Use average values of zenith angle between times?
#                  * TRUE  -- Use the average over the integration time. The integration
#                             time is defined by the time difference between two 
#                             consecutive times. This works best when the time interval is
#                             constant. If not constant, then the code will use the minimum
#                             difference between two consecutive times, and will issue a
#                             warning.
#                  * FALSE -- This is the default. It will use the instantaneous sun 
#                             position information at the exact time stamps.
#                  Note: If datetime has only one value, the code will not calculate
#                        the average.
#  - imetavg    -- How should the date and time be use for defining the averages of
#                  the cosine of the zenith angle?
#                  * 1 -- integration time ends at the reference time (default);
#                  * 2 -- integration time begins at the reference time;
#                  * 3 -- integration time is centred around the reference time.
#  - cosz_min   -- Minimum cosine of zenith angle to be considered daytime. This is
#                  typically just slightly greater than 0. to avoid numerical
#                  instabilities
#  - n_average  -- Number of points for computing the zenith angle averages over the
#                  integration time.  The higher the number, the more accurate the
#                  average will be, but it will increase the computational burden.
#
#  Output:
#  ~~~~~~~
#
#  The output will be a tibble object with the following variables.
#
#  - datetime     -- The same date and time provided as input                [   --]
#  - zen          -- Sun's zenith angle                                      [  deg]
#  - chapman      -- Chapman function (used for correcting the zenith angle) [   --]
#  - eff_cosz     -- Effective cosine of the zenith angle                    [   --]
#  - par_beam     -- Direct visible irradiance                               [ W/m2]
#  - par_diff     -- Diffuse visible irradiance                              [ W/m2]
#  - par_total    -- Total visible irradiance                                [ W/m2]
#  - par_potl     -- Potential (maximum possible) visible irradiance         [ W/m2]
#  - nir_beam     -- Direct near-infrared irradiance                         [ W/m2]
#  - nir_diff     -- Diffuse near-infrared irradiance                        [ W/m2]
#  - nir_total    -- Total near-infrared irradiance                          [ W/m2]
#  - nir_potl     -- Potential (maximum possible) near-infrared irradiance   [ W/m2]
#  - rshort_beam  -- Direct near-infrared irradiance                         [ W/m2]
#  - rshort_diff  -- Diffuse near-infrared irradiance                        [ W/m2]
#  - rshort_total -- Total near-infrared irradiance                          [ W/m2]
#  - rshort_potl  -- Potential (maximum possible) near-infrared irradiance   [ W/m2]
#
#  References:
#  ~~~~~~~~~~~
#
#  Boland J, Ridley B , Brown B. 2008. Models of diffuse solar radiation.
#     Renew. Energy, 33: 575-584. doi:10.1016/j.renene.2007.04.012 (B08).
#
#  Sellers PJ, Mintz Y, Sud YC , Dalcher A. 1986. A Simple Biosphere model (SiB) for use
#     within general circulation models. J. Atmos. Sci., 43: 505-531.
#     doi:10.1175/1520- 0469(1986)043<0505:ASBMFU>2.0.CO;2 (S86).
#
#  Tsubo M , Walker S. 2005. Relationships between photosynthetically active radiation and
#     clearness index at Bloemfontein, South Africa. Theor. Appl. Climatol., 80: 17-25.
#     doi:10.1007/s00704-004-0080-5 (TW05).
#
#  Weiss A , Norman J. 1985. Partitioning solar-radiation into direct and diffuse, visible
#     and near-infrared components. Agric. For. Meteorol., 34: 205-213.
#     doi:10.1016/0168-1923(85)90020-6 (WN85).
#------------------------------------------------------------------------------------------
rshort_partition <<- function( rad_in
                             , datetime
                             , atm_prss
                             , lon
                             , lat
                             , alt
                             , rad_type   = c("rshort","par","nir")
                             , rad_method = c("wn85","sib","clearidx")
                             , is_average = FALSE
                             , imetavg    = 1L
                             , n_average  = 120L
                             , cosz_min   = 0.0001
                             ){



   #---~---
   #   Make sure imetavg makes sense.
   #---~---
   if (! imetavg %in% c(1L,2L,3L)){
      cat0("---~---")
      cat0("   FATAL ERROR! Variable \"imetavg\" must be 1, 2, or 3")
      cat0("   Variable is currently set as ",imetavg,".")
      cat0("---~---")
      stop(" Invalid imetavg settings.")
   }#end if (! imetavg %in% c(1L,2L,3L))
   #---~---


   #---~---
   #   Make sure the radiation type and radiation method make sense.
   #---~---
   rad_type   = match.arg(rad_type  )
   rad_method = match.arg(rad_method)
   #---~---


   #---~---
   #   Prevent SiB to be used when rad_type is not "rshort".
   #---~---
   if ( (rad_method %in% "sib") && (! (rad_type %in% "rshort")) ){
      cat0(" - Radiation method:     ",rad_method,".")
      cat0(" - Input radiation type: ",rad_type  ,".")
      stop(" Radiation input type must be \"rshort\" if using method \"sib\".)")
   }#end if
   #---~---



   #---~---
   #   Define some constants.
   #---~---
   #   Extinction coefficient. (equations 1 and 4 of WN85).
   par_beam_expext  = -0.185
   nir_beam_expext  = -0.060
   #   Typical conversion of diffuse irradiance in sunny days.
   par2diff_sun     = 0.400
   nir2diff_sun     = 0.600
   #   Default partition amongst irradiance components.
   fvis_beam_def    = 0.43
   fnir_beam_def    = 1.0 - fvis_beam_def
   fvis_diff_def    = 0.52
   fnir_diff_def    = 1.0 - fvis_diff_def
   #   Coefficients for various equations in WN85.
   wn85_06          = c( -1.1950, 0.4459, -0.0345 )
   wn85_11          = c(    0.90, 0.70  )
   wn85_12          = c(    0.88, 0.68  )
   #   Coefficients for Diffuse/total fraction. (equation 32 of B08)
   b08_eqn32        = c(-5.0033, 8.6025)
   #   PAR/SW fraction coefficients (equation 2 of TW05)
   tw05_eqn02       = c( 0.613, -0.334,  0.121)
   #   SiB constants.
   c_sib            = c(580.,464.,499.,963.,1160.)
   d_sib            = c(0.0683, 0.0604, -0.0223)
   #---~---



   #---~---
   #   Find the Sun's zenith angle.
   #---~---
   zen_tibble = ed_zen( datetime   = datetime
                      , lon        = lon
                      , lat        = lat
                      , is_average = is_average
                      , imetavg    = imetavg
                      , n_average  = n_average
                      , cosz_min   = cosz_min
                      )#end ed_zen
   cosz       = zen_tibble$cosz
   zen        = zen_tibble$zen
   period     = zen_tibble$period
   day        = period %in% "day"
   c_twilight = period %in% "civil twilight"
   night      = (! day) & (! c_twilight)
   #---~---


   #---~---
   #   Reference atmospheric pressure. If a single value is provided, we use that value, 
   # otherwise we take the reference pressure to be a value just slightly less than the
   # minimum reported value.
   #---~---
   if (length(atm_prss) == 1L){
      ref_atm_prss = atm_prss
   }else{
      #---~---
      #   Find reference pressure. We use 2.5% and 5.0% quantiles to extrapolate the
      # distribution to slightly less than the minimum, unless the minimum is really
      # an outlier.
      #---~---
      qp050_atm_prss = quantile(x=atm_prss,probs=0.050,na.rm=TRUE,names=FALSE)
      qp025_atm_prss = quantile(x=atm_prss,probs=0.025,na.rm=TRUE,names=FALSE)
      ref_atm_prss   = 3. * qp025_atm_prss - 2. * qp050_atm_prss
      #---~---
   }#end if (length(atm_prss) == 1L)
   #---~---


   #---~---
   #   Save the Chapman function to account for the earth's curvature.
   #---~---
   chapman      = pmin(lnexp.max,huestis_fun(cosz=cosz,alt=alt))
   eff_cosz     = ifelse( test = night, yes = 0., no = 1./chapman)
   log10chapman = log10(chapman)
   #---~---


   #---~---
   #   Initialise the output with missing values.
   #---~---
   empty = rep(x=NA_real_,times=length(rad_in))
   ans   = tibble::tibble( datetime     = datetime
                         , zen          = zen
                         , chapman      = chapman
                         , eff_cosz     = eff_cosz
                         , par_beam     = empty
                         , par_diff     = empty
                         , par_total    = empty
                         , par_potl     = empty
                         , nir_beam     = empty
                         , nir_diff     = empty
                         , nir_total    = empty
                         , nir_potl     = empty
                         , rshort_beam  = empty
                         , rshort_diff  = empty
                         , rshort_total = empty
                         , rshort_potl  = empty
                         )#end tibble::tibble
   #---~---


   #---~---
   #   Total solar irradiance at the top of the atmosphere [W/m2], using ED defaults.
   #---~---
   rshort_beam_toa = solar * eff_cosz
   par_beam_toa    = switch( EXPR     = rad_method
                           , clearidx = tw05_eqn02[1L]  * rshort_beam_toa
                           , fvis_beam_def * rshort_beam_toa
                           )#end switch
   nir_beam_toa    = switch( EXPR     = rad_method
                           , clearidx = (1. - tw05_eqn02[1L]) * rshort_beam_toa
                           , fnir_beam_def * rshort_beam_toa
                           )#end switch
   #---~---


   #---~---
   #    Find the potential PAR components (beam, diffuse, total), using equations 1, 3
   # and 9 of WN85.
   #---~---
   par_beam_potl  = ( par_beam_toa
                    * exp ( par_beam_expext * (ref_atm_prss / prefsea) * chapman) )
   par_diff_potl  = par2diff_sun * (par_beam_toa - par_beam_potl)
   par_total_potl = par_beam_potl + par_diff_potl
   #---~---



   #---~---
   #   Find the NIR absorption of 10 mm of precipitable water, using WN85 equation 6.
   #---~---
   pfac = wn85_06[1L] + log10chapman * ( wn85_06[2L] + wn85_06[3L] * log10chapman )
   w10  = rshort_beam_toa * 10. ^ pfac
   #---~---



   #---~---
   #     Find the potential direct and diffuse near-infrared irradiance, using equations
   # 4, 5, and 10 of WN85 (but using the correction for zenith angle).
   #---~---
   nir_beam_potl  = ( ( nir_beam_toa
                      * exp ( nir_beam_expext * (atm_prss / prefsea) * chapman) - w10 ) )
   nir_diff_potl  = nir2diff_sun * ( nir_beam_toa - nir_beam_potl - w10 )
   nir_total_potl = nir_beam_potl + nir_diff_potl
   #---~---


   #---~---
   #    In case of civil twilight, set beam irradiance to zero and diffuse irradiance to
   # potential.
   #---~---
   par_beam_potl[c_twilight] = 0.0
   par_diff_potl[c_twilight] = par_total_potl[c_twilight]
   nir_beam_potl[c_twilight] = 0.0
   nir_diff_potl[c_twilight] = nir_total_potl[c_twilight]
   #---~---



   #---~---
   #   Potential maximum irradiance.
   #---~---
   rshort_total_potl = par_total_potl + nir_total_potl
   #---~---


   #---~---
   #   Decide on the approach.
   #---~---
   if (rad_method %in% "wn85"){
      #---~---
      #   Weiss and Norman (1985) approach. Find the actual total for PAR and NIR, using
      # equations 7 and 8.
      #---~---
      ratio     =
         switch( EXPR   = rad_type
               , par    = ifelse( test = night, yes = 0., no = rad_in/par_total_potl   )
               , nir    = ifelse( test = night, yes = 0., no = rad_in/nir_total_potl   )
               , rshort = ifelse( test = night, yes = 0., no = rad_in/rshort_total_potl)
               )#end switch
      par_total = ratio * par_total_potl
      nir_total = ratio * nir_total_potl
      #---~---



      #---~---
      #     Find the fraction of PAR and NIR that stays as beam, using equations 11 and 12
      # of WN85.
      #---~---
      aux_par       = pmin(wn85_11[1L],pmax(0.,ratio))
      aux_nir       = pmin(wn85_12[1L],pmax(0.,ratio))
      fvis_beam_act = ( par_beam_potl
                      * (1. - ((wn85_11[1L] - aux_par)/wn85_11[2L]) ^ twothirds)
                      / par_total_potl )
      fvis_beam_act = pmin(1.,pmax(0.,fvis_beam_act))

      fnir_beam_act = ( nir_beam_potl
                      * (1. - ((wn85_12[1L] - aux_nir)/wn85_12[2L]) ^ twothirds)
                      / nir_total_potl )
      fnir_beam_act = pmin(1.,pmax(0.,fvis_beam_act))
      fvis_diff_act = 1. - fvis_beam_act
      fnir_diff_act = 1. - fnir_beam_act
      #---~---



      #---~---
      #   Find the irradiance components.
      #---~---
      par_beam     = fvis_beam_act * par_total
      par_diff     = fvis_diff_act * par_total
      nir_beam     = fnir_beam_act * nir_total
      nir_diff     = fnir_diff_act * nir_total
      rshort_beam  = par_beam      + nir_beam
      rshort_diff  = par_diff      + nir_diff
      rshort_total = rshort_beam   + rshort_diff
      #---~---
   }else if (rad_method %in% "sib"){
      #---~---
      #   SiB approach (Sellers et al. 1986) Find the cloud cover estimate and the 
      # fraction of diffuse irradiance.
      #---~---
      cloud  = pmin(1.,pmax(0.,(c_sib[5L]*eff_cosz-rshort_total)/(c_sib[4L]*eff_cosz)))
      difrat = pmin(1.,pmax(0.,d_sib[2L] / ( eff_cosz + d_sib[3L] ) + d_sib[1L]))
      difrat = difrat + ( 1. - difrat ) * cloud
      vnrat  = ( ( c_sib[1L] - cloud*c_sib[2L] ) 
               / ( ( c_sib[1L] - cloud*c_sib[3L] ) + ( c_sib[1L] - cloud*c_sib[2L] ))
               )#end vnrat
      #---~---


      #---~---
      #   Find the irradiance components.
      #---~---
      rshort_total = rad_in
      rshort_diff  = ifelse(test=day,yes=difrat * vnrat * rshort_total,no=rshort_total)
      rshort_beam  = rshort_total  - rshort_diff
      par_diff     = fvis_diff_def * rshort_diff
      nir_diff     = fnir_diff_def * rshort_diff
      par_beam     = fvis_beam_def * rshort_beam
      nir_beam     = fnir_beam_def * rshort_beam
      par_total    = par_diff + par_beam
      nir_total    = nir_diff + nir_beam
      #---~---
   }else if (rad_method %in% "clearidx"){


      #---~---
      #    Find the clearness index based on total shortwave irradiance, then find the
      # fraction of PAR irradiance as a function of total irradiance and clearness index,
      # following TW05, eqn. 2.
      #---~---
      if (rad_type %in% "par"){
         par_total    = rad_in
         fkt          = ifelse( test = night
                              , yes  = 0.
                              , no   = pmax(0.,pmin(1., par_total / par_total_toa))
                              )#end ifelse
         f_par        = tw05_eqn02[1L] + fkt * ( tw05_eqn02[2L] + fkt * tw05_eqn02[3L] )
         rshort_total = par_total / f_par
         nir_total    = rshort_total - par_total
      }else if (rad.type %in% "nir"){
         nir_total    = rad_in
         fkt          = ifelse( test = night
                              , yes  = 0.
                              , no   = pmax(0.,pmin(1., nir_total / nir_beam_toa))
                              )#end ifelse
         f_par        = tw05_eqn02[1L] + fkt * ( tw05_eqn02[2L] + fkt * tw05_eqn02[3L] )
         rshort_total = nir_total / ( 1. - f_par )
         par.total    = rshort_total - nir_total

      }else{
         rshort_total = rad_in
         fkt          = ifelse( test = night
                              , yes  = 0.
                              , no   = pmax(0.,pmin(1., rshort_total / rshort_beam_toa))
                              )#end ifelse
         f_par        = tw05_eqn02[1L] + fkt * ( tw05_eqn02[2L] + fkt * tw05_eqn02[3L] )
         par_total    = f_par * rshort_total
         nir_total    = rshort_total - par_total
      }#end if (rad_type %in% "par")
      #---~---



      #---~---
      #   Find the diffuse fraction based on B08.
      #---~---
      f_diff_aux = pmax(lnexp.min, pmin(lnexp.max, b08_eqn32[1L] + b08_eqn32[2L] * fkt ) )
      f_diff = 1.0 / (1.0 + exp(fdiff_aux) )
      #---~---



      #---~---
      #   Find the irradiance components.
      #---~---
      par_diff    = f_diff    * par_total
      par_beam    = par_total - par_diff
      nir_diff    = f_diff    * nir_total
      nir_beam    = nir_total - nir_diff
      rshort_diff = par_diff  + nir_diff
      rshort_beam = par_beam  + nir_beam
      #---~---

   }#end if (rad_method %in% "wn85")
   #---~---


   #---~---
   #   Populate output data, ensuring that nighttime fluxes are all zero.
   #---~---
   ans$par_beam     = ifelse( test = night, yes = 0., no = par_beam          )
   ans$par_diff     = ifelse( test = night, yes = 0., no = par_diff          )
   ans$par_total    = ifelse( test = night, yes = 0., no = par_total         )
   ans$par_potl     = ifelse( test = night, yes = 0., no = par_total_potl    )
   ans$nir_beam     = ifelse( test = night, yes = 0., no = nir_beam          )
   ans$nir_diff     = ifelse( test = night, yes = 0., no = nir_diff          )
   ans$nir_total    = ifelse( test = night, yes = 0., no = nir_total         )
   ans$nir_potl     = ifelse( test = night, yes = 0., no = nir_total_potl    )
   ans$rshort_beam  = ifelse( test = night, yes = 0., no = rshort_beam       )
   ans$rshort_diff  = ifelse( test = night, yes = 0., no = rshort_diff       )
   ans$rshort_total = ifelse( test = night, yes = 0., no = rshort_total      )
   ans$rshort_potl  = ifelse( test = night, yes = 0., no = rshort_total_potl )
   #---~---

   #---~---
   #   Return tibble.
   #---~---
   return(ans)
   #---~---

}#end function rshort_partition
#==========================================================================================
#==========================================================================================






#==========================================================================================
#==========================================================================================
#    This function finds the cosine of the zenith angle either for the right instant, or
# to the interval between two consecutive times. This is similar to former function 
# ed.zen.r, but it uses package lubridate instead of chron, and remove deprecated options.
#
#  Input variables:
#  ~~~~~~~~~~~~~~~~
#
#    - datetime   -- date and time, using lubridate format. This is required, and can be
#                    either a single value or a vector.
#    - lon        -- longitude of the point.  Required, single value (scalar) only.
#    - lat        -- latitude of the point.  Required, single value (scalar) only.
#    - is_average -- Return the average values between times?
#                    TRUE  -- Return the average over the integration time. The integration
#                             time is defined by the time difference between two 
#                             consecutive times. This works best when the time interval is
#                             constant. If not constant, then the code will use the minimum
#                             difference between two consecutive times, and will issue a
#                             warning.
#                    FALSE -- This is the default. It will return the instantaneous sun 
#                             position information at the exact time stamps.
#                    Note: If datetime has only one value, the code will not calculate
#                          the average.
#    - imetavg    -- How should the date and time be use for defining the averages?
#                    1 -- integration time ends at the reference time (default);
#                    2 -- integration time begins at the reference time;
#                    3 -- integration time is centred around the reference time.
#    - n_average  -- Number of points for computing the averages over the integration time.
#                    The higher the number, the more accurate the average will be, but it
#                    will increase the computational burden.
#    - cosz_min   -- Minimum cosine of zenith angle to be considered daytime. This is 
#                    typically just slightly greater than 0. to avoid numerical
#                    instabilities
#    - output     -- Which variable to output? Options are:
#                    default -- A tibble onject with all the variables below as columns.
#                    cosz    -- Cosine of zenith angle.
#                    zen     -- The zenith angle in degrees
#                    height  -- The sun height in degrees
#                    declin  -- Declination in degrees
#                    period  -- Period. An ordered categorical variable with the following 
#                               levels:
#                                  night                 -- height <= -18 degrees
#                                  astronomical twilight -- -18 < height <= -12
#                                  nautical twilight     -- -12 < height <= -6
#                                  civil twilight        -- -6  < height <= 0
#                                  day                   -- height > 0 (sun above horizon)
#------------------------------------------------------------------------------------------
ed_zen <<- function( datetime
                   , lon
                   , lat
                   , is_average = FALSE
                   , imetavg    = 1L
                   , n_average  = 120L
                   , cosz_min   = 0.0001
                   , output     = c("default","cosz","zen","height","declin","period")
                   , ... ){


   #---~---
   #   Make sure imetavg makes sense.
   #---~---
   if (! imetavg %in% c(1L,2L,3L)){
      cat0("---~---")
      cat0("   FATAL ERROR! Variable \"imetavg\" must be 1, 2, or 3")
      cat0("   Variable is currently set as ",imetavg,".")
      cat0("---~---")
      stop(" Invalid imetavg settings.")
   }#end if (! imetavg %in% c(1L,2L,3L))
   #---~---


   #---~---
   #   Make sure output makes sense.
   #---~---
   output = match.arg(output)
   #---~---


   #---~---
   #   Make sure time is in UTC.
   #---~---
   orig_datetime = datetime
   datetime      = with_tz(datetime,tzone="UTC")
   n_datetime    = length(datetime)
   #---~---

   #---~---
   #   Find the number of times. If a single time is provided, use instantaneous values.
   #---~---
   if ((! is_average) || ( n_datetime == 1L )) n_average = 1L
   #---~---


   #---~---
   #   Decide whether to use a single time or multiple times.
   #---~---
   if (n_average == 1L){
      #---~---
      #   Single time or instantaneous value.
      #---~---
      DATETIME = matrix( data = datetime, ncol=n_average,nrow=n_datetime)
      #---~---
   }else{
      #---~---
      #   Find the time difference. Make sure we use only one value (the minimum if needed)
      # and that the time step is positive.
      #---~---
      timestep    = unique( as.numeric( difftime( time1 = datetime[-1]
                                                , time2 = datetime[-n_datetime]
                                                , units = "secs"
                                                )#end lubridate::difftime
                                      )#end as.numeric
                          )#end unique
      if ( length(timestep) > 1L ){
         #---~---
         #   Find the minimum interval, and use it as the time step. Issue a warning
         # to let the user know. If time steps are not valid, we will issue an error 
         # instead.
         #---~---
         timestep = min(as.numeric(timestep))
         if (timestep %gt% 0.){
            warning( paste0( " Vector \"datetime\" does not have constant time steps."
                           , " Using the minimum step (",timestep," s)."
                           )#end paste0
                   )#end warning
         }#end if (timestep %gt% 0.)
         #---~---
      }#end if ( length(timestep) > 1L )

      #---~---
      #   If the time step is not valid, stop.
      #---~---
      if (! ( timestep %gt% 0.)){
         stop( paste0(" Invalid time step (",timestep," s). It must be positive."))
      }#end if (! ( timestep %gt% 0.)
      #---~---



      #---~---
      #   Decide the beginning and end of averaging period.
      #---~---
      k_dtz = c(0L,n_average-1L,floor(n_average/2)+0.5*((n_average %%2) - 1.0))[imetavg]
      k_dta = c(1L-n_average,0L,-k_dtz)[imetavg]
      dtime = seq(from=k_dta,to=k_dtz,by=1) / ( k_dtz - k_dta + 1) * timestep
      dtime = lubridate::seconds(dtime)
      #---~---

      #---~---
      #   Matrix of time steps
      #---~---
      DATETIME = ( matrix( data = datetime,ncol=n_average,nrow=n_datetime,byrow=FALSE)
                 + matrix( data = dtime   ,ncol=n_average,nrow=n_datetime,byrow=TRUE )
                 )#end DATETIME
      #---~---
   }#end if (n_average == 1L)
   #---~---


   #---~---
   #   Make sure datetime is in datetime format, and find additional information.
   #---~---
   DATETIME = lubridate::as_datetime(DATETIME[,,drop=FALSE])
   TODAY    = matrix( data = lubridate::floor_date(DATETIME,unit="day")
                    , ncol = n_average
                    , nrow = n_datetime
                    )#end matrix
   TODAY    = lubridate::as_datetime(TODAY[,,drop=FALSE])
   DOY      = matrix(data=lubridate::yday(DATETIME)      ,ncol=n_average,nrow=n_datetime)
   LEAP     = matrix(data=lubridate::leap_year(DATETIME) ,ncol=n_average,nrow=n_datetime)
   FRACDAY  = as.numeric(difftime(time1=DATETIME,time2=TODAY,units="secs")) / day.sec
   SUNHR    = (FRACDAY * day.hr + lon / 15. + day.hr) %% day.hr
   #---~---


   #---~---
   #   Fimd the hour angle and its cosine.
   #---~---
   HRANGLE = 15. * (SUNHR - 12.) * pio180
   CHRA    = cos(HRANGLE)
   #---~---


   #---~---
   #   Find declination
   #---~---
   DOYFUN = ifelse( test = LEAP
                  , yes  = 2 * pi * (DOY - shsummer) / 366.
                  , no   = 2 * pi * (DOY - shsummer) / 365.
                  )#end ifelse
   DECLIN = capri * cos(DOYFUN)
   #---~---


   #---~---
   #   Find the cosine and sine of latitude and declination.
   #---~---
   CLAT = matrix(cos(pio180*lat),ncol=n_average,nrow=n_datetime)
   SLAT = matrix(sin(pio180*lat),ncol=n_average,nrow=n_datetime)
   CDEC = cos(DECLIN)
   SDEC = sin(DECLIN)
   #---~---


   #---~---
   #   Find the cosine of the zenith angle, and the period of the day
   #---~---
   COSZ    = SLAT * SDEC + CLAT * CDEC * CHRA
   ZEN     = acos(COSZ) / pio180
   zen_max = acos(cosz_min) / pio180
   PERIOD  = matrix( data = 6L-as.integer(cut(ZEN,breaks=c(-Inf,zen_max,96,102,108,Inf)))
                   , ncol = n_average
                   , nrow = n_datetime
                   )#end matrix
   period  = apply(X=PERIOD,MARGIN=1,FUN=commonest,na.rm=TRUE)
   #---~---

   #---~---
   #   Find averages at the original time step
   #---~---
   cosz     = rowMeans(COSZ,...)
   zen      = acos(cosz) / pio180
   height   = 90. - zen
   declin   = rowMeans(DECLIN,...) / pio180
   period   = ordered( x      = period
                     , labels = c( "night"
                                 , "astronomical twilight"
                                 , "nautical twilight"
                                 , "civil twilight"
                                 , "day"
                                 )#end levels
                     )#end ordered
   #---~---


   #---~---
   #   Build tibble with all the information
   #---~---
   ans = tibble::tibble( datetime = orig_datetime
                       , cosz     = cosz
                       , zen      = zen
                       , height   = 90. - zen
                       , declin   = declin
                       , period   = period
                       )#end tibble::tibble
   #---~---


   #---~---
   #   Decide what to return
   #---~---
   if (output %in% "default"){
      return(ans)
   }else{
      return(ans[[output]])
   }#end if (output %in% "default")
   #---~---

}#end function(ed_zen)
#==========================================================================================
#==========================================================================================






#==========================================================================================
#==========================================================================================
#    This function calculates the effect of sun angle increasing the optical depth in a
# sphere.  This allows accounting for the diffuse irradiance at twilight, using the
# modified Chapman function following:
#
#    Reference:
#
#    Huestis DL. 2001. Accurate evaluation of the chapman function for atmospheric
#       attenuation. J. Quant. Spectrosc. Radiat. Transf., 69: 709-721.
#       doi:10.1016/S0022-4073(00)00107-2
#
#
#  Input variables:
#  ~~~~~~~~~~~~~~~~
#  - cosz -- Cosine of the zenith angle.
#  - alt  -- Altitude of the point, in m. Required, single value (scalar) only.
#  - dzen -- Resolution for building a look-up table.
#------------------------------------------------------------------------------------------
huestis_fun <<- function(cosz,alt=0.,dzen=0.05){

   #---~---
   #   Dimensionless curvature ratio.
   #---~---
   xcurve  = ( erad + alt ) / ehgt
   #---~---


   #---~---
   #   Create the look-up tables.
   #---~---
   n_zen      = 1L + ceiling(180./dzen)
   zen_ref    = pmin(180.,seq( from = 0., by = dzen, length.out = n_zen ))
   #---~---


   #---~---
   #   Initialise the modified Chapman function for the first element, which should be
   # always 1.
   #---~---
   huestis_ref = c(1,rep(x=NA_real_,times=n_zen-1))
   #---~---





   #---~---
   #   Integrate kernel. We use a staggered approach for computing the kernels and deriving
   # the modified Chapman function evaluation.
   #---~---
   iloop = if(n_zen > 1){seq(from=2L,to=n_zen,by=1L)}else{integer(0L)}
   for (i in iloop){

      #---~---
      #   Find the zenith of the reference angle.
      #---~---
      sin_zen    = sin(zen_ref[i]*pio180)
      #---~---

      #---~---
      #   Find the mid-points for integrand, and the trigonometric functions.
      #---~---
      jloop      = seq(from=1L,to=i,by=1L)
      lambda     = mid.points(zen_ref[jloop])
      dlambda    = diff(zen_ref[jloop])
      sin_lambda = sin( lambda * pio180 )
      cos_lambda = cos( lambda * pio180 )
      #---~---

      #---~---
      #   Find kernel. When the sine of lambda approaches zero, the kernel function
      # becomes undefined, so we use the limit values instead, as our goal is to
      # integrate the function.  Also, we cap the natural logarithm of the kernel to
      # avoid floating point exceptions (though it should be safe with double
      # precision).
      #---~---
      ln_kernel = pmax(lnexp.min,pmin(lnexp.max,xcurve * (1.0 - sin_zen / sin_lambda)))
      kernel    = ifelse( test = abs(sin_lambda) %lt% tiny.num
                        , yes  = 0.0
                        , no   = ifelse( test = abs(1.+cos_lambda) %lt% tiny.num
                                       , yes  = exp(lnexp.max)
                                       , no   = exp(ln_kernel) / (1.0 + cos_lambda)
                                       )#end ifelse
                        )#end ifelse
      #---~---

      #---~---
      #   Find the kernel integral and the modified Chapman function at zen = zen_ref[i].
      #---~---
      integ_kern     = sum(kernel*dlambda*pio180)
      huestis_ref[i] = min( exp(lnexp.max), 1.0 + xcurve * sin_zen * integ_kern)
      #---~---
   }#end for (i in sequence(n_zen)[-1L])
   #---~---


   #---~---
   #   Create matrices with the reference.
   #---~---
   zen_use = dzen * round(acos(cosz)/pio180/dzen)
   idx     = match(zen_use,zen_ref)
   ans     = 0.*cosz + c(huestis_ref[idx])
   return(ans)
   #---~---
}#end function huestis_fun
#==========================================================================================
#==========================================================================================
