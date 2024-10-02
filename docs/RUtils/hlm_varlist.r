#==========================================================================================
#  List: hlm_varlist
#  Author: Marcos Longo
#
#     This list has the registry of variables available at the host land model, along 
# with typical unit conversion factors and the units AFTER unit conversion.
#
# Every row of the tibble ("tribble") must contain the following variables:
#
# - vnam    -- The host land model variable names (prefix only). This is a string and is 
#              case insensitive. IMPORTANT: Variable names should not contain the "+"
#              sign.
# - desc    -- Variable description for plots, to be shown in titles.
#              This is a string.  Spaces are totally fine here.
# - short   -- This is a short, plotmath-friendly label for the variable, used mostly
#              in legends.
# - assess  -- Should this variable be used for model assessment? This is a logical 
#              variable. If TRUE, this variable will be considered for model assessment
#              (typically, these are model outputs).  If FALSE, this variable will not
#              be evaluated even if it is available in the site file (typically, input
#              meteorological variables).
# - add0    -- Value to ADD to the variable. This is also a string, and the script will
#              convert it to numeric.  Check file rconstants.r for a list of common unit 
#              conversion factors. If your unit conversion isn't there, consider adding it
#              there instead of defining magic numbers.  Check ** below for additional 
#              information on time conversion.
# - mult    -- Value to MULTIPLY to the variable. This is also a string, and the script
#              will convert it to numeric.  Check file rconstants.r for a list of common
#              unit conversion factors. If your unit conversion isn't there, consider
#              adding it there instead of defining magic numbers (trivial numbers such 
#              as 0. or 1. are fine).  Check ** below for additional information on time
#              conversion.
# - unit    -- Units for plotting. This is a string.  Check for unit names in unitlist.r.
#              If your unit isn't there, consider adding to that list (note the format
#              there, these are useful for making scientific notation in the R plots).
# - hlm     -- Which host land model has this variable? This is a string and it is case
#              insensitive.  Current options are:
#              clm - Community Land Model
#              elm - E3SM Land Model
#              If a variable exists in multiple land models, concatenate them with some
#              simple symbol (e.g., "clm+elm"). The order does not matter.
# - tsplot  -- Plot simple time series of this variable. This is a logical variable.
#              For many cases it may make more sense to combine multiple variables into a
#              single plot. Check list "tstheme" below.
#
# Important points:
# 1. To keep this slightly more manageable, please try to keep vnam in alphabetical order.
# 2. List all variables needed, including those to be included in "tstheme" plots
#    (see below).
#
# ** Note on time conversion.
#    In addition to the variables defined in rconstants.r, the following variables will
# be defined and updated every month:
#    ~ cmon.day -- Number of days in the current month
#    ~ cmon.hr  -- Number of hours in the current month
#    ~ cmon.min -- Number of minutes in the current month
#    ~ cmon.sec -- Number of seconds in the current month
#------------------------------------------------------------------------------------------


#----- List of HLM variables that are "1D"
hlm1dvar      = tribble( ~vnam                      , ~desc                                , ~short                    , ~assess, ~add0 , ~mult           , ~unit        , ~hlm     , ~tsplot
                       , "agb"                      , "Above-ground biomass"               , "A*G*B"                   , TRUE   , "0."  , "g2kg"          , "kgcom2"     , "clm+elm", FALSE
                       , "ar"                       , "Autotrophic respiration"            , "R[a*u*t*o]"              , TRUE   , "0."  , "day.sec"       , "gcom2oday"  , "clm+elm", FALSE
                       , "btran"                    , "Soil moisture stress factor"        , "beta"                    , FALSE  , "0."  , "1."            , "empty"      , "clm+elm", FALSE
                       , "btranmn"                  , "Minimum soil moisture stress factor", "beta[m*i*n]"             , FALSE  , "0."  , "1."            , "empty"      , "clm+elm", FALSE
                       , "c_lblayer"                , "Leaf boundary-layer conductance"    , "g[b*w]"                  , FALSE  , "0."  , "umols.2.kgWday", "kgwom2oday" , "clm+elm", FALSE
                       , "c_stomata"                , "Stomatal conductance"               , "g[s*w]"                  , FALSE  , "0."  , "umols.2.kgWday", "kgwom2oday" , "clm+elm", FALSE
                       , "disturbance_rate_fire"    , "Fire disturbance rate"              , "lambda[F*i*r*e]"         , FALSE  , "0."  , "frac2pc*yr.day", "pcoyr"      , "clm+elm", FALSE
                       , "disturbance_rate_logging" , "Logging disturbance rate"           , "lambda[L*o*g*g*i*n*g]"   , FALSE  , "0."  , "frac2pc*yr.day", "pcoyr"      , "clm+elm", FALSE
                       , "disturbance_rate_treefall", "Tree fall disturbance rate"         , "lambda[T*r*e*e*f*a*l*l]" , FALSE  , "0."  , "frac2pc*yr.day", "pcoyr"      , "clm+elm", FALSE
                       , "eflx_lh_tot"              , "Total latent heat flux"             , "lambda*E"                , TRUE   , "0."  , "1."            , "wom2"       , "clm+elm", FALSE
                       , "elai"                     , "Exposed one-sided leaf area index"  , "L*A*I[E*x*p]"            , FALSE  , "0."  , "1."            , "m2lom2"     , "clm+elm", FALSE
                       , "er"                       , "Ecosystem respiration"              , "R[e*c*o]"                , TRUE   , "0."  , "day.sec"       , "gcom2oday"  , "clm+elm", FALSE
                       , "esai"                     , "Exposed one-sided stem area index"  , "S*A*I[E*x*p]"            , FALSE  , "0."  , "1."            , "m2lom2"     , "clm+elm", FALSE
                       , "fates_autoresp"           , "Autotrophic respiration"            , "R[a*u*t*o]"              , TRUE   , "0."  , "kg2g*day.sec"  , "gcom2oday"  , "clm+elm", FALSE
                       , "fates_ecoresp"            , "Ecosystem respiration"              , "R[e*c*o]"                , TRUE   , "0."  , "kg2g*day.sec"  , "gcom2oday"  , "clm+elm", FALSE
                       , "fates_gpp"                , "Gross primary productivity"         , "G*P*P"                   , TRUE   , "0."  , "kg2g*day.sec"  , "gcom2oday"  , "clm+elm", FALSE
                       , "fates_het_resp"           , "Heterotrophic respiration"          , "R[H*e*t]"                , TRUE   , "0."  , "kg2g*day.sec"  , "gcom2oday"  , "clm+elm", FALSE
                       , "fates_lai"                , "Leaf area index"                    , "L*A*I"                   , TRUE   , "0."  , "1."            , "m2lom2"     , "clm+elm", FALSE
                       , "fates_nep"                , "Net ecosystem productivity"         , "N*E*P"                   , TRUE   , "0."  , "kg2g*day.sec"  , "gcom2oday"  , "clm+elm", FALSE
                       , "fates_vegc"               , "Total biomass"                      , "B*i*o*m*a*s*s"           , FALSE  , "0."  , "1."            , "kgcom2"     , "clm+elm", FALSE
                       , "fates_vegc_aboveground"   , "Above-ground biomass"               , "A*G*B"                   , TRUE   , "0."  , "1."            , "kgcom2"     , "clm+elm", FALSE
                       , "fgr"                      , "Ground flux"                        , "G"                       , TRUE   , "0."  , "1."            , "wom2"       , "clm+elm", FALSE
                       , "fire"                     , "Upward longwave radiation"          , "Q[L*W]^symbol(\"\\335\")", TRUE   , "0."  , "1."            , "wom2"       , "clm+elm", FALSE
                       , "fire_area"                , "Burnt area"                         , "A[B*u*r*n*t]"            , TRUE   , "0."  , "1."            , "oneoday"    , "clm+elm", TRUE
                       , "fire_fdi"                 , "Fire danger index"                  , "F*D*I"                   , FALSE  , "0."  , "1."            , "empty"      , "clm+elm", TRUE
                       , "fire_ignitions"           , "Fire ignition density"              , "f[I*g*n*i*t*i*o*n]"      , FALSE  , "0."  , "1."            , "oneokm2oday", "clm+elm", TRUE
                       , "fire_ros"                 , "Fire rate of spread"                , "R*O*S[F*i*r*e]"          , FALSE  , "0."  , "1."            , "momin"      , "clm+elm", TRUE
                       , "flds"                     , "Downward longwave radiation"        , "Q[L*W]^symbol(\"\\337\")", FALSE  , "0."  , "1."            , "wom2"       , "clm+elm", FALSE
                       , "fsds"                     , "Downward shortwave radiation"       , "Q[S*W]^symbol(\"\\337\")", FALSE  , "0."  , "1."            , "wom2"       , "clm+elm", FALSE
                       , "fsh"                      , "Sensible heat flux"                 , "H[t*o*t]"                , TRUE   , "0."  , "1."            , "wom2"       , "clm+elm", FALSE
                       , "fsh_g"                    , "Sensible heat flux (ground)"        , "H[g*n*d]"                , FALSE  , "0."  , "1."            , "wom2"       , "clm+elm", FALSE
                       , "fsh_v"                    , "Sensible heat flux (vegetation)"    , "H[v*e*g]"                , FALSE  , "0."  , "1."            , "wom2"       , "clm+elm", FALSE
                       , "fsr"                      , "Upward shortwave radiation"         , "Q[S*W]^symbol(\"\\335\")", TRUE   , "0."  , "1."            , "wom2"       , "clm+elm", FALSE
                       , "gpp"                      , "Gross primary productivity"         , "G*P*P"                   , TRUE   , "0."  , "day.sec"       , "gcom2oday"  , "clm+elm", FALSE
                       , "hr"                       , "Heterotrophic respiration"          , "R[H*e*t]"                , TRUE   , "0."  , "day.sec"       , "gcom2oday"  , "clm+elm", FALSE
                       , "lai"                      , "Leaf area index"                    , "L*A*I"                   , TRUE   , "0."  , "1."            , "m2lom2"     , "clm+elm", FALSE
                       , "nep"                      , "Net ecosystem productivity"         , "N*E*P"                   , TRUE   , "0."  , "day.sec"       , "gcom2oday"  , "clm+elm", FALSE
                       , "pbot"                     , "Atmospheric pressure"               , "p[a*t*m]"                , FALSE  , "0."  , "Pa.2.hPa"      , "hpa"        , "clm+elm", FALSE
                       , "q2m"                      , "2-metre specific humidity"          , "q[2*m]"                  , TRUE   , "0."  , "kg2g"          , "gwokg"      , "clm+elm", FALSE
                       , "qaf"                      , "Canopy air space specific humidity" , "q[c*a*s]"                , TRUE   , "0."  , "kg2g"          , "gwokg"      , "clm"    , FALSE
                       , "qbot"                     , "Atmospheric specific humidity"      , "q[a*t*m]"                , FALSE  , "0."  , "kg2g"          , "gwokg"      , "clm+elm", FALSE
                       , "qdirect_throughfall"      , "Throughfall + irrigation"           , "W[t*h*r*f]"              , FALSE  , "0."  , "cmon.sec"      , "mmomo"      , "clm"    , FALSE
                       , "qdrai"                    , "Sub-surface drainage"               , "W[d*r*a*i*n]"            , FALSE  , "0."  , "cmon.sec"      , "mmomo"      , "clm+elm", FALSE
                       , "qdrip"                    , "Canopy dripping"                    , "W[d*r*i*p]"              , FALSE  , "0."  , "cmon.sec"      , "mmomo"      , "clm+elm", FALSE
                       , "qevtr"                    , "Total evaporation"                  , "E[t*o*t]"                , TRUE   , "0."  , "cmon.sec"      , "mmomo"      , "clm+elm", FALSE
                       , "qintr"                    , "Canopy interception"                , "W[c*i*n*t]"              , FALSE  , "0."  , "cmon.sec"      , "mmomo"      , "clm+elm", FALSE
                       , "qover"                    , "Runoff"                             , "W[r*o*f*f]"              , TRUE   , "0."  , "cmon.sec"      , "mmomo"      , "clm+elm", FALSE
                       , "qsoil"                    , "Ground evaporation"                 , "E[g*n*d]"                , FALSE  , "0."  , "cmon.sec"      , "mmomo"      , "clm+elm", FALSE
                       , "qvege"                    , "Canopy evaporation"                 , "E[c*a*n]"                , FALSE  , "0."  , "cmon.sec"      , "mmomo"      , "clm+elm", FALSE
                       , "qvegt"                    , "Canopy transpiration"               , "E[t*r*p]"                , FALSE  , "0."  , "cmon.sec"      , "mmomo"      , "clm+elm", FALSE
                       , "rain"                     , "Rainfall"                           , "W[r*a*i*n]"              , FALSE  , "0."  , "cmon.sec"      , "mmomo"      , "clm+elm", FALSE
                       , "taf"                      , "Canopy air temperature"             , "T[c*a*s]"                , TRUE   , "-t00", "1."            , "degC"       , "clm"    , FALSE
                       , "tbot"                     , "Atmospheric temperature"            , "T[a*t*m]"                , FALSE  , "-t00", "1."            , "degC"       , "clm+elm", FALSE
                       , "tg"                       , "Ground temperature"                 , "T[g*n*d]"                , TRUE   , "-t00", "1."            , "degC"       , "clm+elm", FALSE
                       , "tlai"                     , "Total projected leaf area index"    , "L*A*I[T*o*t]"            , FALSE  , "0."  , "1."            , "m2lom2"     , "clm+elm", FALSE
                       , "trefmnav"                 , "2-metre minimum temperature"        , "T[2*m]^{(m*i*n)}"        , FALSE  , "-t00", "1."            , "degC"       , "clm+elm", FALSE
                       , "trefmxav"                 , "2-metre maximum temperature"        , "T[2*m]^{(m*a*x)}"        , FALSE  , "-t00", "1."            , "degC"       , "clm+elm", FALSE
                       , "tsa"                      , "2-metre temperature"                , "T[2*m]"                  , TRUE   , "-t00", "1."            , "degC"       , "clm+elm", FALSE
                       , "tsai"                     , "Total projected stem area index"    , "S*A*I[T*o*t]"            , FALSE  , "0."  , "1."            , "m2lom2"     , "clm+elm", FALSE
                       , "tv"                       , "Vegetation temperature"             , "T[v*e*g]"                , FALSE  , "-t00", "1."            , "degC"       , "clm+elm", FALSE
                       , "u10"                      , "10-metre wind speed"                , "u[10*m]"                 , TRUE   , "0."  , "1."            , "mos"        , "clm"    , FALSE
                       , "uaf"                      , "Canopy air space wind"              , "u[c*a*s]"                , TRUE   , "0."  , "1."            , "mos"        , "clm"    , FALSE
                       , "ustar"                    , "Friction velocity"                  , "u^symbol(\"\\052\")"     , TRUE   , "0."  , "1."            , "mos"        , "clm"    , FALSE
                       , "zwt"                      , "Water table depth"                  , "z[W*T]"                  , TRUE   , "0."  , "1."            , "m"          , "clm+elm", FALSE
                       , "zwt_perch"                , "Water table depth (perched)"        , "z[W*T*p]"                , TRUE   , "0."  , "1."            , "m"          , "clm+elm", FALSE
                       )#end tribble
#---~---



#--- Tally the variables
nhlm1dvar = nrow(hlm1dvar)
#---~---


#==========================================================================================
#
#     This list has the registry of soil variables available at the host land model, along 
# with typical unit conversion factors and the units AFTER unit conversion.
#
# Every row of the tibble ("tribble") must contain the following variables:
#
# - vnam    -- The host land model variable names (prefix only). This is a string and is 
#              case insensitive. IMPORTANT: Variable names should not contain the "+"
#              sign.
# - desc    -- Variable description for plots, to be shown in titles.
#              This is a string.  Spaces are totally fine here.
# - short   -- This is a short, plotmath-friendly label for the variable, used mostly
#              in legends.
# - assess  -- Should this variable be used for model assessment? This is a logical 
#              variable. If TRUE, this variable will be considered for model assessment
#              (typically, these are model outputs).  If FALSE, this variable will not
#              be evaluated even if it is available in the site file (typically, input
#              meteorological variables).
# - add0    -- Value to ADD to the variable. This is also a string, and the script will
#              convert it to numeric.  Check file rconstants.r for a list of common unit 
#              conversion factors. If your unit conversion isn't there, consider adding it
#              there instead of defining magic numbers.  Check ** below for additional 
#              information on time conversion.
# - mult    -- Value to MULTIPLY to the variable. This is also a string, and the script
#              will convert it to numeric.  Check file rconstants.r for a list of common
#              unit conversion factors. If your unit conversion isn't there, consider
#              adding it there instead of defining magic numbers (trivial numbers such 
#              as 0. or 1. are fine).  Check ** below for additional information on time
#              conversion.
# - unit    -- Units for plotting. This is a string.  Check for unit names in unitlist.r.
#              If your unit isn't there, consider adding to that list (note the format
#              there, these are useful for making scientific notation in the R plots).
# - hlm     -- Which host land model has this variable? This is a string and it is case
#              insensitive.  Current options are:
#              clm - Community Land Model
#              elm - E3SM Land Model
#              If a variable exists in multiple land models, concatenate them with some
#              simple symbol (e.g., "clm+elm"). The order does not matter.
# - csch    -- Which colour palette to use.  This should be a character variable, and
#              should be either a standard colour palette from packages "RColorBrewer" or 
#              "viridis", or the name of a user-defined function.  If 
#              the goal is to use the reverse colour palette, add a prefix i_ to the
#              name (e.g., if planning to use "magma" in the reverse order, set this to
#              "i_magma".
# - mirror -- Should variables be mirrored (i.e., range should be centred in zero with
#             same limit for negative and positive side)? This should be FALSE for 
#             most variables, with exception of net fluxes.
# - trans  -- Which transformation to use to data. This is a character variable, and 
#              currently accepts any transformation from package scales (and you can
#              define new ones based on function scales::trans_new()
# Important points:
# 1. To keep this slightly more manageable, please try to keep vnam in alphabetical order.
# 2. List all variables needed, including those to be included in "tstheme" plots
#    (see below).
#
# ** Note on time conversion.
#    In addition to the variables defined in rconstants.r, the following variables will
# be defined and updated every month:
#    ~ cmon.day -- Number of days in the current month
#    ~ cmon.hr  -- Number of hours in the current month
#    ~ cmon.min -- Number of minutes in the current month
#    ~ cmon.sec -- Number of seconds in the current month
#------------------------------------------------------------------------------------------


#----- List of HLM variables that are "1D"
hlm2dsoi      = tribble( ~vnam            , ~desc                     , ~short          , ~assess, ~add0 , ~mult       , ~unit    , ~hlm     , ~csch   , ~mirror,     ~trans
                       , "smp"            , "Soil matric potential"   , "Psi[S*o*i*l]"  , FALSE  , "0."  , "mm.2.mpa"  , "mpa"    , "clm+elm", "YlGnBu",   FALSE, "neglog10"
                       , "tsoi"           , "Soil temperature"        , "T[S*o*i*l]"    , FALSE  , "-t00",       "1."  , "degC"   , "clm+elm",  "magma",   FALSE, "identity"
                       )#end tribble
#---~---



#--- Tally the variables
nhlm2dsoi = nrow(hlm2dsoi)
#---~---



#------------------------------------------------------------------------------------------
#   List: tstheme
#   Author: Marcos Longo
#
#      These are lists of time series grouped by theme.  These are useful for displaying
# multiple variables that can be interpreted together.
# 
# Every row of the tibble ("tribble") must contain the following variables:
# 
# thnam   -- "Theme" key.  This is used for generating file names.  This is a string.
#            Because this is used for file names, use only letters, numbers, and limit 
#            symbols to the following: _.-
# thdesc  -- "Theme" description. This is used for titles.  This is a string.
#            Here you can use any symbol you want, as well as spaces.
# thunit  -- Units for plotting. This is a string.  Check for unit names in unitlist.r. If
#            your unit isn't there, consider adding to that list (note the format there,
#            these are useful for making scientific notation in the R plots).
# thstack -- Should variables be stacked? This is a logical variable.
# vnames  -- A string with the variables names to be plot together, separated by 
#            the "+" sign.  Variables included here MUST be registered in the hlm1dvar 
#            list above.
# vcols   -- Either a string that matches a colour palette function, or a string that
#            has as many colours as variables, separated by "+".
#------------------------------------------------------------------------------------------
tstheme = tribble( ~thnam        , ~thdesc                         , ~thunit    , ~thstack,     ~trans
                      , ~vnames
                      , ~vcols
                 , "btran"       , "Water stress factor"           , "empty"    , FALSE   , "identity"
                      , paste( "btran"  , "btranmn", sep="+")
                      , paste( "#1F78B4", "#9A78B8", sep="+")
                 , "carbon_flux" , "Carbon fluxes"                 , "gcom2oday", FALSE   , "identity"
                      , paste( "nep"    , "gpp"    , "ar"     , "hr"     , sep="+")
                      , paste( "#CAB2D6", "#33A02C", "#A6CEE3", "#FB9A99", sep="+")
                 , "conductance" , "Conductances"                  ,"kgwom2oday", FALSE   , "log10"
                      , paste( "c_stomata", "c_lblayer", sep="+")
                      , paste(   "#A6CEE3", "#33A02C"  , sep="+")
                 , "disturbance" , "Disturbance rates"             , "pcoyr"    , TRUE    , "sqrt"
                      , paste( "disturbance_rate_treefall", "disturbance_rate_logging", "disturbance_rate_fire", sep="+")
                      , paste( "#B2DF8A"                  , "#CAB2D6"                 , "#E31A1C"              , sep="+")
                 , "energy_flux" , "Energy fluxes"                 , "wom2"     , FALSE   , "identity"
                      , paste( "fsds"   , "fsr"    , "flds"   , "fire"   , "fsh"    , "eflx_lh_tot", "fgr"    , sep="+")
                      , paste( "#FF7F00", "#FDBF6F", "#FB9A99", "#E31A1C", "#B15928", "#1F78B4"    , "#6A3D9A", sep="+")
                 , "evaporation" , "Evaporation"                   , "mmomo"    , TRUE    , "identity"
                      , paste( "qvegt"  , "qvege"  , "qsoil"  , sep="+")
                      , paste( "#B2DF8A", "#1F78B4", "#6A3D9A", sep="+")
                 , "exposed_ai"  , "Exposed one-sided area indices", "m2om2"    , TRUE    , "identity"
                      , paste( "elai"   , "esai"   , sep="+")
                      , paste( "#33A02C", "#FB9A99", sep="+")
                 , "fates_c_flux" , "Carbon fluxes"                 , "gcom2oday", FALSE   , "identity"
                      , paste( "fates_nep", "fates_gpp", "fates_autoresp", "fates_het_resp", sep="+")
                      , paste( "#CAB2D6"  , "#33A02C"  , "#A6CEE3"       , "#FB9A99"       , sep="+")
                 , "humidity"    , "Specific humidity"             , "gwokg"    , FALSE   , "identity"
                      , paste( "qbot"   , "qaf"    , "q2m"    , sep="+")
                      , paste( "#1F78B4", "#A6CEE3", "#9A78B8", sep="+")
                 , "liquid_water", "Liquid water flux"             , "mmomo"    , FALSE   , "identity"
                      , paste( "rain"   , "qintr"  , "qdrip"  , "qdirect_throughfall", "qover"  , "qdrai"  , sep="+")
                      , paste( "#1F78B4", "#B2DF8A", "#6A3D9A", "#A6CEE3"            , "#FF7F00", "#E31A1C", sep="+")
                 , "momentum"    , "Velocity"                      , "mos"      , FALSE   , "sqrt"
                      , paste( "uaf"    , "u10"    , "ustar"  , sep="+")
                      , paste( "#A6CEE3", "#9A78B8", "#E31A1C", sep="+")
                 , "sensible"    , "Sensible heat flux"            , "wom2"     , FALSE   , "identity"
                      , paste( "fsh"    , "fsh_v"  , "fsh_g"  , sep="+")
                      , paste( "#FF7F00", "#B2DF8A", "#6A3D9A", sep="+")
                 , "temperature" , "Temperature"                   , "degC"     , FALSE   , "identity"
                      , paste( "tbot"   , "taf"    , "trefmnav", "tsa"    , "trefmxav", "tg"     , "tv"     , sep="+")
                      , paste( "#1F78B4", "#A6CEE3", "#CAB2D6" , "#9A78B8", "#6A3D9A" , "#E31A1C", "#B2DF8A", sep="+")
                 , "total_ai"    , "Total projected area indices"  , "m2om2"    , TRUE    , "identity"
                      , paste( "tlai"   , "tsai"   , sep="+")
                      , paste( "#33A02C", "#FB9A99", sep="+")
                 )#end tribble
#---~---



#--- Tally the theme entries.
ntstheme = nrow(tstheme)
#---~---
