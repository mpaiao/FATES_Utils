#==========================================================================================
#  List: fates_varlist
#  Author: Marcos Longo
#
#     This list has the registry of FATES variables, along with typical unit conversion
# factors and the units AFTER unit conversion.
#
# Every element of the list must contain the following variables:
#
# - vnam  -- The FATES variable names (prefix only). This is a string and is case 
#            insensitive.
# - desc  -- Variable description for plots. This is a string.
# - add0  -- Value to ADD to the variable. This is also a string, and the script will
#            convert it to numeric.  Check file rconstants.r for a list of common unit 
#            conversion factors. If your unit conversion isn't there, consider adding it
#            there instead of defining magic numbers.
# - mult  -- Value to MULTIPLY to the variable. This is also a string, and the script will
#            convert it to numeric.  Check file rconstants.r for a list of common unit 
#            conversion factors. If your unit conversion isn't there, consider adding it
#            there instead of defining magic numbers.
# - unit  -- Units for plotting. This is a string.  Check for unit names in unitlist.r. If
#            your unit isn't there, consider adding to that list (note the format there,
#            these are useful for making scientific notation in the R plots).
# - aggr  -- How to aggregate variables (e.g., if using "SCPF" variables to obtain
#            SCLS or PFT variables).  This is also a string, and should match a function
#            name that takes the na.rm argument. If the function doesn't have, you may 
#            trick R by creating a new function that takes the ellipsis argument, e.g.
#            length_na <- function(x,...) length(x)
#            Then set aggr = "length_na"
# - stack -- Plot variables in "stacks"? This is a logical variable.
# - dbh01 -- Use the first dbh class for plots? Sometimes it is useful to exclude the
#            class, when the contribution from seedlings is disproportionally large.
#            This is a logical variable.
#
# To keep this slightly more manageable, please try to keep vnam in alphabetical order.
#------------------------------------------------------------------------------------------



#---- List of FATES variables
n             = 0
fatesvar      = list()
n             = n + 1
fatesvar[[n]] = list( vnam  = "agb"
                    , desc  = "Above-ground biomass"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "kgcom2"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = TRUE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "ba"
                    , desc  = "Basal area"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "m2oha"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = TRUE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "canopy_area"
                    , desc  = "Canopy area"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "m2om2"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = TRUE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "carbon_balance_canopy"
                    , desc  = "Carbon balance (Canopy)"
                    , add0  = "0."
                    , mult  = "m2.2.ha"
                    , unit  = "kgcom2oyr"
                    , aggr  = "sum"
                    , stack = FALSE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "carbon_balance_understory"
                    , desc  = "Carbon balance (Understory)"
                    , add0  = "0."
                    , mult  = "m2.2.ha"
                    , unit  = "kgcom2oyr"
                    , aggr  = "sum"
                    , stack = FALSE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "ddbh_canopy"
                    , desc  = "Growth rate (Canopy)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "cmohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "ddbh_understory"
                    , desc  = "Growth rate (Understory)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "cmohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "demotion_rate"
                    , desc  = "Understory demotion rate"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "plohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "gpp"
                    , desc  = "Gross Primary Productivity"
                    , add0  = "0."
                    , mult  = "day.sec"
                    , unit  = "gcom2oday"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = TRUE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "gpp_canopy"
                    , desc  = "Gross Primary Productivity (Canopy)"
                    , add0  = "0."
                    , mult  = "day.sec"
                    , unit  = "gcom2oday"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = TRUE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "gpp_understory"
                    , desc  = "Gross Primary Productivity (Understory)"
                    , add0  = "0."
                    , mult  = "day.sec"
                    , unit  = "gcom2oday"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = TRUE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "mortality_canopy"
                    , desc  = "Mortality rate (Canopy)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "plohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "mortality_understory"
                    , desc  = "Mortality rate (Understory)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "plohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "m1"
                    , desc  = "Mortality rate (Background)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "plohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "m2"
                    , desc  = "Mortality rate (Hydraulic)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "plohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "m3"
                    , desc  = "Mortality rate (C Starvation)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "plohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "m4"
                    , desc  = "Mortality rate (Impact)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "plohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "m5"
                    , desc  = "Mortality rate (Fire)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "plohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "m6"
                    , desc  = "Mortality rate (Termination)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "plohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "m7"
                    , desc  = "Mortality rate (Logging)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "plohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "m8"
                    , desc  = "Mortality rate (Freezing)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "plohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "m9"
                    , desc  = "Mortality rate (Senescence)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "plohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "m10"
                    , desc  = "Mortality rate (Age senescence)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "plohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "lai"
                    , desc  = "Leaf area index"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "m2lom2"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = TRUE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "lai_canopy"
                    , desc  = "Leaf area index (Canopy)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "m2lom2"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = TRUE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "lai_understory"
                    , desc  = "Leaf area index (Understory)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "m2lom2"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = TRUE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "nplant"
                    , desc  = "Number density"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "ploha"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "nplant_canopy"
                    , desc  = "Number density (Canopy)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "ploha"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "nplant_understory"
                    , desc  = "Number density (Understory)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "ploha"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "patch_area"
                    , desc  = "Patch area"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "m2om2"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "promotion_rate"
                    , desc  = "Canopy promotion rate"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "plohaoyr"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "trimming_canopy"
                    , desc  = "Trimming (Canopy)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "ploha"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
n             = n + 1
fatesvar[[n]] = list( vnam  = "trimming_understory"
                    , desc  = "Trimming (Understory)"
                    , add0  = "0."
                    , mult  = "1."
                    , unit  = "ploha"
                    , aggr  = "sum"
                    , stack = TRUE
                    , dbh01 = FALSE
                    )#end fates
#---~---



#--- Convert the `fates` list to a data.table
fatesvar  = list.2.data.table(fatesvar)
nfatesvar = nrow(fatesvar)
#---~---


