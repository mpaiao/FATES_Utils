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
# - add0      -- Value to ADD. This is also a string, and the script will convert it to
#                numeric.  Check file rconstants.r for a list of common unit conversion
#                factors. If your unit conversion isn't there, consider adding  it there
#                instead of defining magic numbers.
# - mult      -- Value to MULTIPLY . This is also a string, and the script will convert it
#                to numeric.  Check file rconstants.r for a list of common unit
#                conversion factors. If your unit conversion isn't there, consider adding
#                it there instead of defining magic numbers.
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
# - aggr      -- How to aggregate variables (e.g., if using "SZPF" variables to obtain
#                SZ or PFT variables).  This is also a string, and should match a function
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
# - ylower    -- Minimum value for the y axis for time series by DBH and PFT, if known.
#                This allows plotting multiple simulations with a shared y axis. To use
#                the default limits, set this to NA_real_
# - yupper    -- Maximum value for the y axis for time series by DBH and PFT, if known.
#                This allows plotting multiple simulations with a shared y axis. To use
#                the default limits, set this to NA_real_
# To keep this slightly more manageable, please try to keep vnam in alphabetical order.
#------------------------------------------------------------------------------------------


#---- List of FATES variables
fatesvar = tribble(
     ~vnam                        , ~desc                                    ,    ~add0,           ~mult,   ~mult_dp,       ~unit,    ~dp_unit,    ~dp_indvar, ~aggr, ~is_upc,        ~vtype, ~order,       ~colour, ~stack, ~dbh01,     ~cschm, ~mirror,     ~trans,     ~ylower,     ~yupper
   , "fates_agsapmaintar"         , "Autotrophic respiration (AG Sapwood)"   ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,          "ar",     4L,     "#FDBF6F",   TRUE,   TRUE,     "OrRd",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_agsapwood_alloc"      , "Allocation flux (AG Sapwood)"           ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,       "alloc",     6L,     "#FDBF6F",   TRUE,   TRUE,     "BrBG",    TRUE, "identity",    NA_real_,    NA_real_
   , "fates_agstruct_alloc"       , "Allocation flux (AG Heartwood)"         ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,       "alloc",     5L,     "#FF7F00",   TRUE,   TRUE,     "BrBG",    TRUE, "identity",    NA_real_,    NA_real_
   , "fates_autoresp"             , "Autotrophic respiration"                ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "OrRd",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_basalarea"            , "Basal area"                             ,     "0.",       "ha.2.m2", "m2.2.cm2",     "m2oha",       "cm2",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,  "cividis",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_bgsapmaintar"         , "Autotrophic respiration (BG Sapwood)"   ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,          "ar",     1L,     "#FB9A99",   TRUE,   TRUE,     "OrRd",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_bgsapwood_alloc"      , "Allocation flux (BG Sapwood)"           ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,       "alloc",     3L,     "#FB9A99",   TRUE,   TRUE,     "BrBG",    TRUE, "identity",    NA_real_,    NA_real_
   , "fates_bgstruct_alloc"       , "Allocation flux (BG Heartwood)"         ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,       "alloc",     2L,     "#E31A1C",   TRUE,   TRUE,     "BrBG",    TRUE, "identity",    NA_real_,    NA_real_
   , "fates_canopy_area"          , "Canopy area"                            ,     "0.",            "1.",       "1.",     "m2om2",     "m2om2", NA_character_, "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,  "viridis",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_crownarea"            , "Crown area index"                       ,     "0.",            "1.",       "1.",     "m2om2",    "m2lom2", NA_character_, "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_crownarea_canopy"     , "Crown area index (Canopy)"              ,     "0.",            "1.",       "1.",     "m2om2",        "m2", NA_character_, "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_crownarea_ustory"     , "Crown area index (Understory)"          ,     "0.",            "1.",       "1.",     "m2om2",        "m2", NA_character_, "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_ddbh_canopy"          , "Growth rate (Canopy)"                   ,     "0.",        "m.2.cm",  "ha.2.m2",  "cmohaoyr",     "cmoyr",      "canopy", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,  "cividis",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_ddbh_ustory"          , "Growth rate (Understory)"               ,     "0.",        "m.2.cm",  "ha.2.m2",  "cmohaoyr",     "cmoyr",  "understory", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,  "cividis",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_demotion_rate"        , "Understory demotion rate"               ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,  "cividis",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_froot_alloc"          , "Allocation flux (Fine root)"            ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,       "alloc",     4L,     "#1F78B4",   TRUE,   TRUE,     "BrBG",    TRUE, "identity",    NA_real_,    NA_real_
   , "fates_frootmaintar"         , "Autotrophic respiration (Fine root)"    ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,          "ar",     2L,     "#1F78B4",   TRUE,   TRUE,     "OrRd",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_gpp"                  , "Gross Primary Productivity"             ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,   "PuBuGn",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_gpp_canopy"           , "Gross Primary Productivity (Canopy)"    ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",      "canopy", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,   "PuBuGn",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_gpp_ustory"           , "Gross Primary Productivity (Understory)",     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",  "understory", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,   "PuBuGn",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_growar"               , "Autotrophic respiration (Growth)"       ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,          "ar",     3L,     "#B2DF8A",   TRUE,   TRUE,     "OrRd",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_lblayer_cond"         , "Leaf boundary-layer conductance"        ,     "0.", "mols.2.kgWday",  "ha.2.m2", "kgom2oday",    "kgoday",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,  FALSE,  FALSE,  "cividis",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_lai"                  , "Leaf area index"                        ,     "0.",            "1.",       "1.",    "m2lom2",    "m2lom2", NA_character_, "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_lai_canopy"           , "Leaf area index (Canopy)"               ,     "0.",            "1.",       "1.",    "m2lom2",       "m2l", NA_character_, "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_lai_ustory"           , "Leaf area index (Understory)"           ,     "0.",            "1.",       "1.",    "m2lom2",       "m2l", NA_character_, "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_leaf_alloc"           , "Allocation flux (Leaf)"                 ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,       "alloc",     7L,     "#33A02C",   TRUE,   TRUE,     "BrBG",    TRUE, "identity",    NA_real_,    NA_real_
   , "fates_leafc"                , "Leaf biomass"                           ,     "0.",            "1.",  "ha.2.m2",    "kgcom2",       "kgc",       "total", "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_leafc_canopy"         , "Leaf biomass (Canopy)"                  ,     "0.",            "1.",  "ha.2.m2",    "kgcom2",       "kgc",      "canopy", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_leafc_ustory"         , "Leaf biomass (Understory)"              ,     "0.",            "1.",  "ha.2.m2",    "kgcom2",       "kgc",  "understory", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "BuGn",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_mortality"            , "Mortality rate"                         ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,     "BuPu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_mortality_canopy"     , "Mortality rate (Canopy)"                ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",      "canopy", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,     "BuPu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_mortality_ustory"     , "Mortality rate (Understory)"            ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",  "understory", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,     "BuPu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_mortality_agescen"    , "Mortality rate (Age senescence)"        ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     4L,     "#33A02C",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_mortality_background" , "Mortality rate (Background)"            ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     3L,     "#B2DF8A",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_mortality_cstarv"     , "Mortality rate (C Starvation)"          ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     5L,     "#FB9A99",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_mortality_freezing"   , "Mortality rate (Freezing)"              ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     1L,     "#A6CEE3",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_mortality_fire"       , "Mortality rate (Fire)"                  ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     6L,     "#E31A1C",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_mortality_hydraulic"  , "Mortality rate (Hydraulic)"             ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     2L,     "#1F78B4",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_mortality_impact"     , "Mortality rate (Impact)"                ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     7L,     "#FDBF6F",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_mortality_logging"    , "Mortality rate (Logging)"               ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     8L,     "#FF7F00",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_mortality_senescence" , "Mortality rate (Senescence)"            ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",     9L,     "#CAB2D6",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_mortality_termination", "Mortality rate (Termination)"           ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE,        "mort",    10L,     "#6A3D9A",   TRUE,  FALSE,     "BuPu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_nplant"               , "Stem Number density"                    ,     "0.",       "ha.2.m2",       "1.",     "ploha",     "ploha", NA_character_, "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,   "YlGnBu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_nplant_canopy"        , "Stem Number density (Canopy)"           ,     "0.",       "ha.2.m2",       "1.",     "ploha",     "ploha", NA_character_, "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,   "YlGnBu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_nplant_ustory"        , "Stem Number density (Understory)"       ,     "0.",       "ha.2.m2",       "1.",     "ploha",     "ploha", NA_character_, "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,   "YlGnBu",   FALSE,    "log10",    NA_real_,    NA_real_
   , "fates_npp"                  , "Net primary productivity"               ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "kgcom2oyr",    "kgcoyr",       "total", "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,     "PRGn",    TRUE, "identity",    NA_real_,    NA_real_
   , "fates_npp_canopy"           , "Net primary productivity (Canopy)"      ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "kgcom2oyr",    "kgcoyr",      "canopy", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,     "PRGn",    TRUE, "identity",    NA_real_,    NA_real_
   , "fates_npp_ustory"           , "Net primary productivity (Understory)"  ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "kgcom2oyr",    "kgcoyr",  "understory", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,     "PRGn",    TRUE, "identity",    NA_real_,    NA_real_
   , "fates_patcharea"            , "Patch area"                             ,     "0.",            "1.",       "1.",     "m2om2",     "m2om2", NA_character_, "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,   "rocket",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_promotion_rate"       , "Canopy promotion rate"                  ,     "0.",       "ha.2.m2",     "100.",  "plohaoyr",     "pcoyr",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,"i_viridis",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_rdark"                , "Autotrophic respiration (Leaf)"         ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,          "ar",     5L,     "#33A02C",   TRUE,   TRUE,     "OrRd",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_seed_alloc"           , "Allocation flux (Reproduction)"         ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,       "alloc",     8L,     "#A6CEE3",   TRUE,   TRUE,     "BrBG",    TRUE, "identity",    NA_real_,    NA_real_
   , "fates_stomatal_cond"        , "Stomatal conductance"                   ,     "0.", "mols.2.kgWday",  "ha.2.m2", "kgom2oday",    "kgoday",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,  FALSE,  FALSE,  "cividis",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_storec"               , "Storage biomass"                        ,     "0.",            "1.",  "ha.2.m2",    "kgcom2",       "kgc",       "total", "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "mako",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_storec_canopy"        , "Storage biomass (Canopy)"               ,     "0.",            "1.",  "ha.2.m2",    "kgcom2",       "kgc",      "canopy", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "mako",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_storec_ustory"        , "Storage biomass (Understory)"           ,     "0.",            "1.",  "ha.2.m2",    "kgcom2",       "kgc",  "understory", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,     "mako",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_store_alloc"          , "Allocation flux (Storage)"              ,     "0.",  "kg2g*day.sec",  "ha.2.m2", "gcom2oday",    "gcoday",       "total", "sum",   FALSE,       "alloc",     1L,     "#B2DF8A",   TRUE,   TRUE,     "BrBG",    TRUE, "identity",    NA_real_,    NA_real_
   , "fates_trimming"             , "Trimming"                               ,     "0.",       "ha.2.m2",     "100.",     "ploha",        "pc",       "total", "sum",    TRUE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,   "rocket",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_trimming_canopy"      , "Trimming (Canopy)"                      ,     "0.",       "ha.2.m2",     "100.",     "ploha",        "pc",      "canopy", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,   "rocket",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_trimming_ustory"      , "Trimming (Understory)"                  ,     "0.",       "ha.2.m2",     "100.",     "ploha",        "pc",  "understory", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,  FALSE,   "rocket",   FALSE, "identity",    NA_real_,    NA_real_
   , "fates_vegc_aboveground"     , "Above-ground biomass carbon density"    ,     "0.",            "1.",  "ha.2.m2",    "kgcom2",       "kgc",       "total", "sum",   FALSE, NA_character_,     0L, NA_character_,   TRUE,   TRUE,   "PuBuGn",   FALSE,    "log10",    NA_real_,    NA_real_
)#end tribble
#---~---



#---- Helpful variables for standardising some plots
if ("fates_ts_ystd" %in% ls()){
   new_ystd                  = fates_ts_ystd %>% filter(vnam %in% fatesvar$vnam)
   idx_ystd                  = match(new_ystd$vnam,fatesvar$vnam)
   fatesvar$ylower[idx_ystd] = new_ystd$ylower
   fatesvar$yupper[idx_ystd] = new_ystd$yupper
}#end if ("fates_ts_ystd" %in% ls())
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
       ~vorig                            , ~vnam          , ~desc                           ,     ~add0,      ~mult, ~unit   , ~trans    , ~colour
     , "fates_drought_status_pf"         , "dstatus"      , "Drought phenology status"      ,      "0.",       "1.", "empty" , "identity", "#6A3D9A"
     , "fates_daysince_droughtleafoff_pf", "ndays_off"    , "Days since last shedding event",      "0.",       "1.", "day"   , "identity", "#E31A1C"
     , "fates_daysince_droughtleafon_pf" , "ndays_on"     , "Days since last flushing event",      "0.",       "1.", "day"   , "identity", "#33A02C"
     , "fates_elong_factor_pf"           , "elong_factor" , "Elongation factor"             ,      "0.",       "1.", "empty" , "identity", "#CAB2D6"
     , "fates_meanliqvol_droughtphen_pf" , "mean_swc"     , "Soil moisture"                 ,      "0.",       "1.", "m3wom3", "identity", "#1F78B4"
     , "fates_meansmp_droughtphen_pf"    , "mean_smp"     , "Soil matric potential"         ,      "0.", "mm.2.mpa", "mpa"   , "neglog10", "#B15928"
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
                 ,   4, "Leaves abscissing"     ,"#CAB2D6"
                 )#end tribble
#---~---

#--- Tally the variables.
ndphs   = nrow(dphinfo) 
#---~---
