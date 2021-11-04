#==========================================================================================
#  List: fates_varlist
#  Author: Marcos Longo
#
#     This list has the registry of FATES variables, along with typical unit conversion
# factors and the units AFTER unit conversion.
#
# Every row of the tibble ("tribble") must contain the following variables:
#
# - vnam      -- The FATES variable names (prefix only). This is a string and is case 
#                insensitive.
# - desc      -- Variable description for plots. This is a string.
# - add0_sp   -- Value to ADD to the "SCPF" and "SCLS" variables. This is also a string, 
#                and the script will convert it to numeric.  Check file rconstants.r for a
#                list of common unit conversion factors. If your unit conversion isn't 
#                there, consider adding  it there instead of defining magic numbers.
# - mult_sp   -- Value to MULTIPLY to the "SCPF" and "SCLS" variable. This is also a 
#                string, and the script will convert it to numeric.  Check file 
#                rconstants.r for a list of common unit  conversion factors. If your unit
#                conversion isn't there, consider adding it there instead of defining magic
#                numbers.
# - add0_ag   -- Value to ADD to the "BY_AGE" variables. This is also a string, and the 
#                script will convert it to numeric.  Check file rconstants.r for a list of 
#                common unit conversion factors. If your unit conversion isn't there, 
#                consider adding  it there instead of defining magic numbers.
# - mult_ag   -- Value to MULTIPLY to the "BY_AGE" variable. This is also a string, and the
#                script will convert it to numeric.  Check file rconstants.r for a list of 
#                common unit  conversion factors. If your unit conversion isn't there, 
#                consider adding it there instead of defining magic numbers.
# - mult_dp   -- Value to MULTIPLY variables when plotting heat maps by PFT and DBH. This 
#                is also a string, and the script will convert it to numeric.  Check file 
#                rconstants.r for a list of common unit  conversion factors. If your unit
#                conversion isn't there, consider adding it there instead of defining magic
#                numbers.
# - unit      -- Units for plotting. This is a string.  Check for unit names in unitlist.r. If
#                your unit isn't there, consider adding to that list (note the format there,
#                these are useful for making scientific notation in the R plots).
# - dp_unit   -- Units for plotting the heat maps by size and PFT. This is a string.  
#                Check for unit names in unitlist.r. If your unit isn't there, consider
#                adding to that list (note the format there, these are useful for making 
#                scientific notation in the R plots).
# - dp_indvar -- Which variable to use to convert matrices by DBH and size? The following
#                options are valid:
#                   "understory"  - use nplant_understory
#                   "canopy"      - use nplant_canopy
#                   "total"       - use nplant
#                   NA_character_ - do not change units keep it in per unit area
#
#                If you set anything to other than NA_character_, then ensure the 
#                corresponding nplant variable is also included in fatesvar. And do
#                not set dp_indvar to anything other than NA_character_ for the nplant
#                variables, otherwise nasty things may occur for some variables.
# - aggr      -- How to aggregate variables (e.g., if using "SCPF" variables to obtain
#                SCLS or PFT variables).  This is also a string, and should match a function
#                name that takes the na.rm argument. If the function doesn't have, you may 
#                trick R by creating a new function that takes the ellipsis argument, e.g.
#                length_na <- function(x,...) length(x)
#                Then set aggr = "length_na"
# - is_upc    -- Is this a derived variable to be found from Understory Plus Canopy (UPC)?
#                Logical variable.  These are useful for defining total quantities not 
#                directly available in FATES output (e.g., LAI, storage biomass, etc.).
# - vtype     -- Is this a special type of variable? This is a character variable. Acceptable
#                values are:
#                - "mort". Mortality variable (used for stacking mortality types by size
#                  or PFT).
#                - "nppo". Organ-specific NPP variable (used for stacking NPP components
#                  by size or PFT). IMPORTANT: Do not set npp to nppo, as it is the sum
#                  of individual organs.
#                - NA_character_ General variable.
#                This variable is used for stacking mortality types by size or PFT.
# - order     -- Order to plot, in case the vtype is not NA_character_.  This will sort
#                the variables in a logical way if sought. In case of ties, the original
#                order will be used. This should be an integer
# - colour    -- Colour used in multiple variable plots (only those in which vtype is
#                not NA_character_).
# - stack     -- Plot variables in "stacks"? This is a logical variable.
# - dbh01     -- Use the first dbh class for plots? Sometimes it is useful to exclude the
#                class, when the contribution from seedlings is disproportionally large.
#                This is a logical variable.
# - csch      -- Which colour palette to use in heat maps by size and PFT.  This should 
#                be a character variable, and should be either a standard colour palette 
#                from packages "RColorBrewer" or "viridis", or the name of a 
#                user-defined function.  If  the goal is to use the reverse colour 
#                palette, add a prefix i_ to the name (e.g., if planning to use "magma" 
#                in the reverse order, set this to "i_magma".
# - mirror    -- Should variables be mirrored (i.e., range should be centred in zero with
#                same limit for negative and positive side)? This should be FALSE for 
#                most variables, with exception of net fluxes.
# - trans     -- Which transformation to use to data. This is a character variable, and 
#                currently accepts any transformation from package scales (and you can
#                define new ones based on function scales::trans_new()
#
# To keep this slightly more manageable, please try to keep vnam in alphabetical order.
#------------------------------------------------------------------------------------------



#---- List of FATES variables
fatesvar = tribble(
     ~vnam                      , ~desc                                    , ~add0_sp,        ~mult_sp, ~add0_ag,        ~mult_ag,   ~mult_dp,       ~unit,    ~dp_unit,    ~dp_indvar, ~aggr, ~is_upc,        ~vtype, ~order,       ~colour, ~stack, ~dbh01,     ~cschm, ~mirror,     ~trans
   , "agb"                      , "Above-ground biomass"                   ,     "0.",            "1.",     "0.",            "1.",  "ha.2.m2",    "kgcom2",       "kgc",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,   "PuBuGn",   FALSE,    "log10"
   , "ar"                       , "Autotrophic respiration"                ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "OrRd",   FALSE, "identity"
   , "ar_grow"                  , "Autotrophic respiration (Growth)"       ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,          "ar",     3L,     "#B2DF8A",   TRUE,   TRUE,     "OrRd",   FALSE, "identity"
   , "ar_darkm"                 , "Autotrophic respiration (Leaf)"         ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,          "ar",     5L,     "#33A02C",   TRUE,   TRUE,     "OrRd",   FALSE, "identity"
   , "ar_agsapm"                , "Autotrophic respiration (AG Sapwood)"   ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,          "ar",     4L,     "#FDBF6F",   TRUE,   TRUE,     "OrRd",   FALSE, "identity"
   , "ar_crootm"                , "Autotrophic respiration (BG Sapwood)"   ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,          "ar",     1L,     "#FB9A99",   TRUE,   TRUE,     "OrRd",   FALSE, "identity"
   , "ar_frootm"                , "Autotrophic respiration (Fine root)"    ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,          "ar",     2L,     "#1F78B4",   TRUE,   TRUE,     "OrRd",   FALSE, "identity"
   , "ba"                       , "Basal area"                             ,     "0.",            "1.",     "0.",            "1.", "m2.2.cm2",     "m2oha",       "cm2",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,  "cividis",   FALSE,    "log10"
   , "bleaf"                    , "Leaf biomass"                           ,     "0.",       "m2.2.ha",     "0.",       "m2.2.ha",  "ha.2.m2",    "kgcom2",       "kgc",       "total", "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity"
   , "bleaf_canopy"             , "Leaf biomass (Canopy)"                  ,     "0.",       "m2.2.ha",     "0.",       "m2.2.ha",  "ha.2.m2",    "kgcom2",       "kgc",      "canopy", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity"
   , "bleaf_understory"         , "Leaf biomass (Understory)"              ,     "0.",       "m2.2.ha",     "0.",       "m2.2.ha",  "ha.2.m2",    "kgcom2",       "kgc",  "understory", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity"
   , "bstor"                    , "Storage biomass"                        ,     "0.",       "m2.2.ha",     "0.",       "m2.2.ha",  "ha.2.m2",    "kgcom2",       "kgc",       "total", "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "mako",   FALSE, "identity"
   , "bstor_canopy"             , "Storage biomass (Canopy)"               ,     "0.",       "m2.2.ha",     "0.",       "m2.2.ha",  "ha.2.m2",    "kgcom2",       "kgc",      "canopy", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "mako",   FALSE, "identity"
   , "bstor_understory"         , "Storage biomass (Understory)"           ,     "0.",       "m2.2.ha",     "0.",       "m2.2.ha",  "ha.2.m2",    "kgcom2",       "kgc",  "understory", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "mako",   FALSE, "identity"
   , "canopy_area"              , "Canopy area"                            ,     "0.",            "1.",     "0.",            "1.",       "1.",     "m2om2",     "m2om2", NA_character_, "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,  "viridis",   FALSE, "identity"
   , "carbon_balance"           , "Carbon balance"                         ,     "0.",       "m2.2.ha",     "0.",       "m2.2.ha",  "ha.2.m2", "kgcom2oyr",    "kgcoyr",       "total", "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,     "PRGn",    TRUE, "identity"
   , "carbon_balance_canopy"    , "Carbon balance (Canopy)"                ,     "0.",       "m2.2.ha",     "0.",       "m2.2.ha",  "ha.2.m2", "kgcom2oyr",    "kgcoyr",      "canopy", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,     "PRGn",    TRUE, "identity"
   , "carbon_balance_understory", "Carbon balance (Understory)"            ,     "0.",       "m2.2.ha",     "0.",       "m2.2.ha",  "ha.2.m2", "kgcom2oyr",    "kgcoyr",  "understory", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,     "PRGn",    TRUE, "identity"
   , "c_lblayer"                , "Leaf boundary-layer conductance"        ,     "0.","umols.2.kgWday",     "0.","umols.2.kgWday",  "ha.2.m2", "kgom2oday",    "kgoday",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,  FALSE,  FALSE,  "cividis",   FALSE, "identity"
   , "c_stomata"                , "Stomatal conductance"                   ,     "0.","umols.2.kgWday",     "0.","umols.2.kgWday",  "ha.2.m2", "kgom2oday",    "kgoday",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,  FALSE,  FALSE,  "cividis",   FALSE, "identity"
   , "ddbh_canopy"              , "Growth rate (Canopy)"                   ,     "0.",            "1.",     "0.",            "1.",  "ha.2.m2",  "cmohaoyr",     "cmoyr",      "canopy", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,  "cividis",   FALSE, "identity"
   , "ddbh_understory"          , "Growth rate (Understory)"               ,     "0.",            "1.",     "0.",            "1.",  "ha.2.m2",  "cmohaoyr",     "cmoyr",  "understory", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,  "cividis",   FALSE, "identity"
   , "demotion_rate"            , "Understory demotion rate"               ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,  "cividis",   FALSE, "identity"
   , "gpp"                      , "Gross Primary Productivity"             ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,   "PuBuGn",   FALSE, "identity"
   , "gpp_canopy"               , "Gross Primary Productivity (Canopy)"    ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",      "canopy", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,   "PuBuGn",   FALSE, "identity"
   , "gpp_understory"           , "Gross Primary Productivity (Understory)",     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",  "understory", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,   "PuBuGn",   FALSE, "identity"
   , "mortality"                , "Mortality rate"                         ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,     "BuPu",   FALSE,    "log10"
   , "mortality_canopy"         , "Mortality rate (Canopy)"                ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",      "canopy", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,     "BuPu",   FALSE,    "log10"
   , "mortality_understory"     , "Mortality rate (Understory)"            ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",  "understory", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,     "BuPu",   FALSE,    "log10"
   , "m1"                       , "Mortality rate (Background)"            ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     3L,     "#B2DF8A",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10"
   , "m2"                       , "Mortality rate (Hydraulic)"             ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     2L,     "#1F78B4",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10"
   , "m3"                       , "Mortality rate (C Starvation)"          ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     5L,     "#FB9A99",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10"
   , "m4"                       , "Mortality rate (Impact)"                ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     7L,     "#FDBF6F",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10"
   , "m5"                       , "Mortality rate (Fire)"                  ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     6L,     "#E31A1C",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10"
   , "m6"                       , "Mortality rate (Termination)"           ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",    10L,     "#6A3D9A",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10"
   , "m7"                       , "Mortality rate (Logging)"               ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     8L,     "#FF7F00",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10"
   , "m8"                       , "Mortality rate (Freezing)"              ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     1L,     "#A6CEE3",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10"
   , "m9"                       , "Mortality rate (Senescence)"            ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     9L,     "#CAB2D6",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10"
   , "m10"                      , "Mortality rate (Age senescence)"        ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     4L,     "#33A02C",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10"
   , "lai"                      , "Leaf area index"                        ,     "0.",            "1.",     "0.",            "1.",       "1.",    "m2lom2",    "m2lom2", NA_character_, "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity"
   , "lai_canopy"               , "Leaf area index (Canopy)"               ,     "0.",            "1.",     "0.",            "1.",       "1.",    "m2lom2",       "m2l", NA_character_, "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity"
   , "lai_understory"           , "Leaf area index (Understory)"           ,     "0.",            "1.",     "0.",            "1.",       "1.",    "m2lom2",       "m2l", NA_character_, "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity"
   , "nplant"                   , "Number density"                         ,     "0.",            "1.",     "0.",            "1.",       "1.",     "ploha",     "ploha", NA_character_, "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,   "YlGnBu",   FALSE,    "log10"
   , "nplant_canopy"            , "Number density (Canopy)"                ,     "0.",            "1.",     "0.",            "1.",       "1.",     "ploha",     "ploha", NA_character_, "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,   "YlGnBu",   FALSE,    "log10"
   , "nplant_understory"        , "Number density (Understory)"            ,     "0.",            "1.",     "0.",            "1.",       "1.",     "ploha",     "ploha", NA_character_, "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,   "YlGnBu",   FALSE,    "log10"
   , "npp"                      , "Net Primary Productivity"               ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BrBG",    TRUE, "identity"
   , "npp_agdw"                 , "NPP flux (AG Heartwood)"                ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,        "nppo",     5L,     "#FF7F00",   TRUE,   TRUE,     "BrBG",    TRUE, "identity"
   , "npp_agsw"                 , "NPP flux (AG Sapwood)"                  ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,        "nppo",     6L,     "#FDBF6F",   TRUE,   TRUE,     "BrBG",    TRUE, "identity"
   , "npp_bgdw"                 , "NPP flux (BG Heartwood)"                ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,        "nppo",     2L,     "#E31A1C",   TRUE,   TRUE,     "BrBG",    TRUE, "identity"
   , "npp_bgsw"                 , "NPP flux (BG Sapwood)"                  ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,        "nppo",     3L,     "#FB9A99",   TRUE,   TRUE,     "BrBG",    TRUE, "identity"
   , "npp_fnrt"                 , "NPP flux (Fine root)"                   ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,        "nppo",     4L,     "#1F78B4",   TRUE,   TRUE,     "BrBG",    TRUE, "identity"
   , "npp_leaf"                 , "NPP flux (Leaf)"                        ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,        "nppo",     7L,     "#33A02C",   TRUE,   TRUE,     "BrBG",    TRUE, "identity"
   , "npp_seed"                 , "NPP flux (Reproduction)"                ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,        "nppo",     8L,     "#A6CEE3",   TRUE,   TRUE,     "BrBG",    TRUE, "identity"
   , "npp_stor"                 , "NPP flux (Storage)"                     ,     "0.",   "kg2g/yr.day",     "0.",       "day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,        "nppo",     1L,     "#B2DF8A",   TRUE,   TRUE,     "BrBG",    TRUE, "identity"
   , "patch_area"               , "Patch area"                             ,     "0.",            "1.",     "0.",            "1.",       "1.",     "m2om2",     "m2om2", NA_character_, "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,   "rocket",   FALSE, "identity"
   , "promotion_rate"           , "Canopy promotion rate"                  ,     "0.",            "1.",     "0.",            "1.",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,"i_viridis",   FALSE, "identity"
   , "trimming"                 , "Trimming"                               ,     "0.",            "1.",     "0.",            "1.",     "100.",     "ploha",        "pc",       "total", "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,   "rocket",   FALSE, "identity"
   , "trimming_canopy"          , "Trimming (Canopy)"                      ,     "0.",            "1.",     "0.",            "1.",     "100.",     "ploha",        "pc",      "canopy", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,   "rocket",   FALSE, "identity"
   , "trimming_understory"      , "Trimming (Understory)"                  ,     "0.",            "1.",     "0.",            "1.",     "100.",     "ploha",        "pc",  "understory", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,   "rocket",   FALSE, "identity"
)#end tribble
#---~---



#--- Tally the variables.
nfatesvar = nrow(fatesvar)
#---~---


#==========================================================================================
#     The following list mediates drought-deciduous variables that should be loaded.
# The phenology diagram requires all these data layers, so ensure to include all of them
# in the output, otherwise they will not be generated.  
#
# Every row of the tibble ("tribble") must contain the following variables:
# - vorig  -- The FATES variable names. This is a string and is case insensitive.
#             Do not change these names unless you are also editing the scripts and know 
#             what you are doing.
# - vnam   -- Shorter name to make the code a bit simpler to read. Do not change these
#             names unless you are also editing the scripts and know what you are doing.
# - desc   -- Variable description for plots. This is a string.
# - add0   -- Value to ADD to the variables. This is also a string, and the script will
#             convert it to numeric.  Check file rconstants.r for a list of common unit
#             conversion factors. If your unit conversion isn't there, consider adding
#             it there instead of defining magic numbers.
# - mult   -- Value to MULTIPLY to the variables. This is also a string, and the script
#             will convert it to numeric.  Check file rconstants.r for a list of common
#             unit  conversion factors. If your unit conversion isn't there, consider 
#             adding it there instead of defining magicnumbers.
# - unit   -- Units for plotting. This is a string.  If your unit isn't there, consider 
#             adding to that list (note the format there, these are useful for making 
#             scientific notation in the R plots).
# - trans  -- Which transformation to use to data. This is a character variable, and 
#             currently accepts any transformation from package scales (and you can
#             define new ones based on function scales::trans_new()
# - colour -- Colour used in multiple variable plots.
#------------------------------------------------------------------------------------------



#---- List of FATES variables
dphenvar = tribble(
       ~vorig                         , ~vnam      , ~desc                           ,     ~add0,      ~mult, ~unit   , ~trans    , ~colour
     , "site_drought_status"          , "dstatus"  , "Drought phenology status"      ,      "0.",       "1.", "empty" , "identity", "#6A3D9A"
     , "site_daysince_droughtleafoff" , "ndays_off", "Days since last shedding event",      "0.",       "1.", "day"   , "identity", "#E31A1C"
     , "site_daysince_droughtleafon"  , "ndays_on" , "Days since last flushing event",      "0.",       "1.", "day"   , "identity", "#33A02C"
     , "site_meanliqvol_droughtphen"  , "mean_swc" , "Soil moisture"                 ,      "0.",       "1.", "m3wom3", "identity", "#1F78B4"
     , "site_meansmp_droughtphen"     , "mean_smp" , "Soil matric potential"         ,      "0.", "mm.2.mpa", "mpa"   , "neglog"  , "#B15928"
)#end tribble
#---~---

#--- Tally the variables.
ndphenvar = nrow(dphenvar)
#---~---



#--- Set phenology types
dphinfo = tribble( ~id, ~desc                   , ~colour
                 ,   0, "Leaves off (time)"     ,"#FDBF6F"
                 ,   1, "Leaves off (threshold)","#FB9A99"
                 ,   2, "Leaves on (threshold)" ,"#B2DF8A"
                 ,   3, "Leaves on (time)"      ,"#A6CEE3"
                 )#end tribble
#---~---

#--- Tally the variables.
ndphs   = nrow(dphinfo) 
#---~---
