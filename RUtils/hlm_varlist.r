#==========================================================================================
#  List: hlm_varlist
#  Author: Marcos Longo
#
#     This list has the registry of variables available at the host land model, along 
# with typical unit conversion factors and the units AFTER unit conversion.
#
# Every element of the list must contain the following variables:
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
n             = 0
hlm1dvar      = list()
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "agb"
                    , desc   = "Above-ground biomass"
                    , short  = "A*G*B"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "g2kg"
                    , unit   = "kgcom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "ar"
                    , desc   = "Autotrophic respiration"
                    , short  = "R[a*u*t*o]"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "day.sec"
                    , unit   = "gcom2oday"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "btran"
                    , desc   = "Soil moisture stress factor."
                    , short  = "beta"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "empty"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "disturbance_rate_fire"
                    , desc   = "Fire disturbance rate"
                    , short  = "lambda[F*i*r*e]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "frac2pc*yr.day"
                    , unit   = "pcoyr"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "disturbance_rate_logging"
                    , desc   = "Logging disturbance rate"
                    , short  = "lambda[L*o*g*g*i*n*g]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "frac2pc*yr.day"
                    , unit   = "pcoyr"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "disturbance_rate_treefall"
                    , desc   = "Tree fall disturbance rate"
                    , short  = "lambda[T*r*e*e*f*a*l*l]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "frac2pc*yr.day"
                    , unit   = "pcoyr"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "ed_biomass"
                    , desc   = "Total biomass"
                    , short  = "B*i*o*m*a*s*s"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "g2kg"
                    , unit   = "kgcom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "eflx_lh_tot"
                    , desc   = "Total latent heat flux"
                    , short  = "lambda*E"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "wom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "elai"
                    , desc   = "Exposed one-sided leaf area index"
                    , short  = "L*A*I[E*x*p]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "m2lom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "er"
                    , desc   = "Ecosystem respiration"
                    , short  = "R[e*c*o]"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "day.sec"
                    , unit   = "gcom2oday"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "esai"
                    , desc   = "Exposed one-sided stem area index"
                    , short  = "S*A*I[E*x*p]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "m2lom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "fgr"
                    , desc   = "Ground flux"
                    , short  = "G"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "wom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "fire"
                    , desc   = "Upward longwave radiation"
                    , short  = "Q[L*W]^symbol(\"\\335\")"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "wom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "fire_area"
                    , desc   = "Burnt area"
                    , short  = "A[B*u*r*n*t]"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "oneoday"
                    , hlm    = "clm+elm"
                    , tsplot = TRUE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "fire_fdi"
                    , desc   = "Fire danger index"
                    , short  = "F*D*I"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "empty"
                    , hlm    = "clm+elm"
                    , tsplot = TRUE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "fire_ignitions"
                    , desc   = "Fire ignition density"
                    , short  = "f[I*g*n*i*t*i*o*n]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "oneokm2oday"
                    , hlm    = "clm+elm"
                    , tsplot = TRUE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "fire_ros"
                    , desc   = "Fire rate of spread"
                    , short  = "R*O*S[F*i*r*e]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "momin"
                    , hlm    = "clm+elm"
                    , tsplot = TRUE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "flds"
                    , desc   = "Downward longwave radiation"
                    , short  = "Q[L*W]^symbol(\"\\337\")"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "wom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "fsds"
                    , desc   = "Downward shortwave radiation"
                    , short  = "Q[S*W]^symbol(\"\\337\")"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "wom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "fsh"
                    , desc   = "Sensible heat flux"
                    , short  = "H[t*o*t]"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "wom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "fsh_g"
                    , desc   = "Sensible heat flux (ground)"
                    , short  = "H[g*n*d]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "wom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "fsh_v"
                    , desc   = "Sensible heat flux (vegetation)"
                    , short  = "H[v*e*g]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "wom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "fsr"
                    , desc   = "Upward shortwave radiation"
                    , short  = "Q[S*W]^symbol(\"\\335\")"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "wom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "gpp"
                    , desc   = "Gross primary productivity"
                    , short  = "G*P*P"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "day.sec"
                    , unit   = "gcom2oday"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "hr"
                    , desc   = "Heterotrophic respiration"
                    , short  = "R[H*e*t]"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "day.sec"
                    , unit   = "gcom2oday"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "nep"
                    , desc   = "Net ecosystem productivity"
                    , short  = "N*E*P"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "day.sec"
                    , unit   = "gcom2oday"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "pbot"
                    , desc   = "Atmospheric pressure"
                    , short  = "p[a*t*m]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "Pa.2.hPa"
                    , unit   = "hpa"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "q2m"
                    , desc   = "2-metre specific humidity"
                    , short  = "q[2*m]"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "kg2g"
                    , unit   = "gwokg"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "qaf"
                    , desc   = "Canopy air space specific humidity"
                    , short  = "q[c*a*s]"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "kg2g"
                    , unit   = "gwokg"
                    , hlm    = "clm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "qbot"
                    , desc   = "Atmospheric specific humidity"
                    , short  = "q[a*t*m]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "kg2g"
                    , unit   = "gwokg"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "qdirect_throughfall"
                    , desc   = "Throughfall + irrigation"
                    , short  = "W[t*h*r*f]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "cmon.sec"
                    , unit   = "mmomo"
                    , hlm    = "clm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "qdrai"
                    , desc   = "Sub-surface drainage"
                    , short  = "W[d*r*a*i*n]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "cmon.sec"
                    , unit   = "mmomo"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "qdrip"
                    , desc   = "Canopy dripping"
                    , short  = "W[d*r*i*p]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "cmon.sec"
                    , unit   = "mmomo"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "qevtr"
                    , desc   = "Total evaporation"
                    , short  = "E[t*o*t]"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "cmon.sec"
                    , unit   = "mmomo"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "qintr"
                    , desc   = "Canopy interception"
                    , short  = "W[c*i*n*t]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "cmon.sec"
                    , unit   = "mmomo"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "qover"
                    , desc   = "Runoff"
                    , short  = "W[r*o*f*f]"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "cmon.sec"
                    , unit   = "mmomo"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "qsoil"
                    , desc   = "Ground evaporation"
                    , short  = "E[g*n*d]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "cmon.sec"
                    , unit   = "mmomo"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "qvege"
                    , desc   = "Canopy evaporation"
                    , short  = "E[c*a*n]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "cmon.sec"
                    , unit   = "mmomo"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "qvegt"
                    , desc   = "Canopy transpiration"
                    , short  = "E[t*r*p]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "cmon.sec"
                    , unit   = "mmomo"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "rain"
                    , desc   = "Rainfall"
                    , short  = "W[r*a*i*n]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "cmon.sec"
                    , unit   = "mmomo"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "taf"
                    , desc   = "Canopy air temperature"
                    , short  = "T[c*a*s]"
                    , assess = TRUE
                    , add0   = "-t00"
                    , mult   = "1."
                    , unit   = "degC"
                    , hlm    = "clm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "tbot"
                    , desc   = "Atmospheric temperature"
                    , short  = "T[a*t*m]"
                    , assess = FALSE
                    , add0   = "-t00"
                    , mult   = "1."
                    , unit   = "degC"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "tg"
                    , desc   = "Ground temperature"
                    , short  = "T[g*n*d]"
                    , assess = TRUE
                    , add0   = "-t00"
                    , mult   = "1."
                    , unit   = "degC"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "tlai"
                    , desc   = "Total projected leaf area index"
                    , short  = "L*A*I[T*o*t]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "m2lom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "trefmnav"
                    , desc   = "2-metre minimum temperature"
                    , short  = "T[2*m]^{(m*i*n)}"
                    , assess = FALSE
                    , add0   = "-t00"
                    , mult   = "1."
                    , unit   = "degC"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "trefmxav"
                    , desc   = "2-metre maximum temperature"
                    , short  = "T[2*m]^{(m*a*x)}"
                    , assess = FALSE
                    , add0   = "-t00"
                    , mult   = "1."
                    , unit   = "degC"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "tsa"
                    , desc   = "2-metre temperature"
                    , short  = "T[2*m]"
                    , assess = TRUE
                    , add0   = "-t00"
                    , mult   = "1."
                    , unit   = "degC"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "tsai"
                    , desc   = "Total projected stem area index"
                    , short  = "S*A*I[T*o*t]"
                    , assess = FALSE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "m2lom2"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "tv"
                    , desc   = "Vegetation temperature"
                    , short  = "T[v*e*g]"
                    , assess = FALSE
                    , add0   = "-t00"
                    , mult   = "1."
                    , unit   = "degC"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "u10"
                    , desc   = "10-metre wind speed"
                    , short  = "u[10*m]"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "mos"
                    , hlm    = "clm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "uaf"
                    , desc   = "Canopy air space wind"
                    , short  = "u[c*a*s]"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "mos"
                    , hlm    = "clm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "ustar"
                    , desc   = "Friction velocity"
                    , short  = "u^symbol(\"\\052\")"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "mos"
                    , hlm    = "clm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "zwt"
                    , desc   = "Water table depth"
                    , short  = "z[W*T]"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "m"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
n             = n + 1
hlm1dvar[[n]] = list( vnam   = "zwt_perch"
                    , desc   = "Water table depth (perched)"
                    , short  = "z[W*T*p]"
                    , assess = TRUE
                    , add0   = "0."
                    , mult   = "1."
                    , unit   = "m"
                    , hlm    = "clm+elm"
                    , tsplot = FALSE
                    )#end list
#---~---



#--- Convert the `hlmvar` list to a data.table
hlm1dvar  = do.call( what = rbind
                  , args = lapply( X                = hlm1dvar
                                 , FUN              = as_tibble
                                 , stringsAsFactors = FALSE
                                 )#end lapply
                  )#end do.call
nhlm1dvar = nrow(hlm1dvar)
#---~---



#n             = n + 1
#hlm1dvar[[n]] = list( vnam   = "smp"
#                    , desc   = "Soil matric potential"
#                    , short  = "psi[W]"
#                    , add0   = "0."
#                    , mult   = "mm.2.mpa"
#                    , unit   = "mpa"
#                    , hlm    = "clm+elm"
#                    , tsplot = FALSE
#                    )#end list



#------------------------------------------------------------------------------------------
#   List: tstheme
#   Author: Marcos Longo
#
#      These are lists of time series grouped by theme.  These are useful for displaying
# multiple variables that can be interpreted together.
# 
# Every element of the list must contain the following variables:
# 
# thnam   -- "Theme" key.  This is used for generating file names.  This is a string.
#            Because this is used for file names, use only letters, numbers, and limit 
#            symbols to the following: _.-
# thdesc  -- "Theme" description. This is used for titles.  This is a string.
#            Here you can use any symbol you want, as well as spaces.
# thunit  -- Units for the axis.  Mind to group variables with the same units.
# vnames  -- A string with the variables names to be plot together, separated by 
#            the "+" sign.  Variables included here MUST be registered in the hlm1dvar 
#            list above.
# vcols   -- Either a string that matches a colour palette function, or a string that
#            has as many colours as variables, separated by "+".
# thstack -- Should variables be stacked? This is a logical variable.
#------------------------------------------------------------------------------------------
n            = 0
tstheme      = list()
n            = n + 1
tstheme[[n]] = list( thnam   = "disturbance"
                   , thdesc  = "Disturbance rates"
                   , thunit  = "pcoyr"
                   , vnames  = paste( "disturbance_rate_treefall"
                                    , "disturbance_rate_logging"
                                    , "disturbance_rate_fire"
                                    , sep="+"
                                    )#end paste
                   , vcols   = paste( "#B2DF8A"
                                    , "#CAB2D6"
                                    , "#E31A1C"
                                    , sep="+"
                                    )#end paste
                   , tsstack = TRUE
                   )#end list
n            = n + 1
tstheme[[n]] = list( thnam   = "energy_flux"
                   , thdesc  = "Energy fluxes"
                   , thunit  = "wom2"
                   , vnames  = paste( "fsds"
                                    , "fsr"
                                    , "flds"
                                    , "fire"
                                    , "fsh"
                                    , "eflx_lh_tot"
                                    , "fgr"
                                   , sep="+"
                                    )#end paste
                   , vcols   = paste( "#FF7F00"
                                    , "#FDBF6F"
                                    , "#FB9A99"
                                    , "#E31A1C"
                                    , "#B15928"
                                    , "#1F78B4"
                                    , "#6A3D9A"
                                    , sep="+"
                                    )#end paste
                   , tsstack = FALSE
                   )#end list
n            = n + 1
tstheme[[n]] = list( thnam   = "carbon_flux"
                   , thdesc  = "Carbon fluxes"
                   , thunit  = "gcom2oday"
                   , vnames  = paste( "nep"
                                    , "gpp"
                                    , "ar"
                                    , "hr"
                                    , sep="+"
                                    )#end paste
                   , vcols   = paste( "#CAB2D6"
                                    , "#33A02C"
                                    , "#A6CEE3"
                                    , "#FB9A99"
                                    , sep="+"
                                    )#end paste
                   , tsstack = FALSE
                   )#end list
n            = n + 1
tstheme[[n]] = list( thnam   = "total_ai"
                   , thdesc  = "Total projected area indices"
                   , thunit  = "m2om2"
                   , vnames  = paste( "tlai"
                                    , "tsai"
                                    , sep="+"
                                    )#end paste
                   , vcols   = paste( "#33A02C"
                                    , "#FB9A99"
                                    , sep="+"
                                    )#end paste
                   , tsstack = TRUE
                   )#end list
n            = n + 1
tstheme[[n]] = list( thnam   = "exposed_ai"
                   , thdesc  = "Exposed one-sided area indices"
                   , thunit  = "m2om2"
                   , vnames  = paste( "elai"
                                    , "esai"
                                    , sep="+"
                                    )#end paste
                   , vcols   = paste( "#33A02C"
                                    , "#FB9A99"
                                    , sep="+"
                                    )#end paste
                   , tsstack = TRUE
                   )#end list
n            = n + 1
tstheme[[n]] = list( thnam   = "temperature"
                   , thdesc  = "Temperature"
                   , thunit  = "degC"
                   , vnames  = paste( "tbot"
                                    , "taf"
                                    , "trefmnav"
                                    , "tsa"
                                    , "trefmxav"
                                    , "tg"
                                    , "tv"
                                    , sep="+"
                                    )#end paste
                   , vcols   = paste( "#1F78B4"
                                    , "#A6CEE3"
                                    , "#CAB2D6"
                                    , "#9A78B8"
                                    , "#6A3D9A"
                                    , "#E31A1C"
                                    , "#B2DF8A"
                                    , sep="+"
                                    )#end paste
                   , tsstack = FALSE
                   )#end list
n            = n + 1
tstheme[[n]] = list( thnam   = "humidity"
                   , thdesc  = "Specific humidity"
                   , thunit  = "gwokg"
                   , vnames  = paste( "qbot"
                                    , "qaf"
                                    , "q2m"
                                    , sep="+"
                                    )#end paste
                   , vcols   = paste( "#1F78B4"
                                    , "#A6CEE3"
                                    , "#9A78B8"
                                    , sep="+"
                                    )#end paste
                   , tsstack = FALSE
                   )#end list
n            = n + 1
tstheme[[n]] = list( thnam   = "momentum"
                   , thdesc  = "Velocity"
                   , thunit  = "mos"
                   , vnames  = paste( "uaf"
                                    , "u10"
                                    , "ustar"
                                    , sep="+"
                                    )#end paste
                   , vcols   = paste( "#A6CEE3"
                                    , "#9A78B8"
                                    , "#E31A1C"
                                    , sep="+"
                                    )#end paste
                   , tsstack = FALSE
                   )#end list
n            = n + 1
tstheme[[n]] = list( thnam   = "evaporation"
                   , thdesc  = "Evaporation"
                   , thunit  = "mmomo"
                   , vnames  = paste( "qvegt"
                                    , "qvege"
                                    , "qsoil"
                                    , sep="+"
                                    )#end paste
                   , vcols   = paste( "#B2DF8A"
                                    , "#1F78B4"
                                    , "#6A3D9A"
                                    , sep="+"
                                    )#end paste
                   , tsstack = TRUE
                   )#end list
n            = n + 1
tstheme[[n]] = list( thnam   = "sensible"
                   , thdesc  = "Sensible heat flux"
                   , thunit  = "mmomo"
                   , vnames  = paste( "fsh"
                                    , "fsh_v"
                                    , "fsh_g"
                                    , sep="+"
                                    )#end paste
                   , vcols   = paste( "#FF7F00"
                                    , "#B2DF8A"
                                    , "#6A3D9A"
                                    , sep="+"
                                    )#end paste
                   , tsstack = FALSE
                   )#end list
n            = n + 1
tstheme[[n]] = list( thnam   = "liquid_water"
                   , thdesc  = "Liquid water flux"
                   , thunit  = "mmomo"
                   , vnames  = paste( "rain"
                                    , "qintr"
                                    , "qdrip"
                                    , "qdirect_throughfall"
                                    , "qover"
                                    , "qdrai"
                                    , sep="+"
                                    )#end paste
                   , vcols   = paste( "#1F78B4"
                                    , "#B2DF8A"
                                    , "#6A3D9A"
                                    , "#A6CEE3"
                                    , "#FF7F00"
                                    , "#E31A1C"
                                    , sep="+"
                                    )#end paste
                   , tsstack = FALSE
                   )#end list
#---~---



#--- Convert the `tstheme` list to a data.table
tstheme  = do.call( what = rbind
                  , args = lapply( X                = tstheme
                                 , FUN              = as_tibble
                                 , stringsAsFactors = FALSE
                                 )#end lapply
                  )#end do.call
ntstheme = nrow(tstheme)
#---~---
