#!/bin/bash

#==========================================================================================
#
#    This script generates a case for a single-point simulation for a user-defined site. 
# It assumes all the site-specific data files (SITE_NAME) are located in the 
# SITE_BASE_PATH folder.
#
# Developed by: Marcos Longo < m l o n g o -at- l b l -dot- g o v "
#               18 Jun 2021 09:45 PDT
# Based on scripts developed by Ryan Knox and Shawn Serbin.
#
#     This script can take a few arguments for quick set up. Otherwise, the beginning
# of the script has many variables to control the simulation settings. Either way, it is
# better to go through the beginning of the script to familiarise with the options, and to
# make sure that the argument settings are what you really think they are.
#
#    List of optional keys (which must be followed by the actual arguments):
# 
#    -s, --site     -- Index of known site, from the list defined in variable SITE_INFO
#    -c, --config   -- Index of known configuration, from the list defined in variable 
#                      CONFIG_INFO
#    -r, --runtime  -- Run time for jobs (Format "[DD-]HH:MM:SS", DD is optional)
#==========================================================================================


#---~---
#    Variables that can be passed as arguments (their description is further down).
#---~---
SITE_USE=""
CONFIG_USE=""
RUN_TIME=""
#--- Parse arguments
while [[ ${#} -gt 0 ]]
do
   ARG="${1}"
   case ${ARG} in
   -s|--site)
      export SITE_USE="${2}"
      shift 2
      ;;
   -c|--config)
      export CONFIG_USE="${2}"
      shift 2
      ;;
   -r|--runtime)
      export RUN_TIME="${2}"
      shift 2
      ;;
   *)
      #---~---
      #   Unknown option, stop the script.
      #---~---
      echo " Unknown option ${1}. Check script for instructions."
      exit 99
      #---~---
      ;;
   esac
done
#---~---


#--- Main settings.
export HESM="CTSM"         # Host "Earth System Model". (E3SM, CESM, CTSM)
export MACH="eschweilera"  # Machine used for preparing the case
#---~---


#---~---
#   Job submission settings. These settings depend on the machine select, and may not
# be used.
#
# ---------------------------------------------------------
#  Relevant for all submissions
# ---------------------------------------------------------
#
# - AUTO_SUBMIT -- Submit the job upon successful creation? (true or false)
#
# ---------------------------------------------------------
#  Relevant for SLURM-based HPC clusters
# ---------------------------------------------------------
#
# - IS_INTERACTIVE  -- Should the job be submitted interactively (as opposed to submitted
#                      as a batch job)? (true or false)
# - PROJECT         -- Project account to be used for this submission
#                      Set it to empty (PROJECT="") in case this is not applicable.
# - PARTITION       -- Specify the partition for the job to be run.
#                      Set it to empty (PARTITION="") to use the default.
# - RUN_TIME        -- Run time for job, in HH:MM:SS format. Make sure this
#                      does not exceed maximum allowed.
# - CPUS_PER_TASK   -- Number of CPUS requested for each task. Set it to 1 unless
#                      using multi-threading (shared memory parallelisation, OpenMP).
#---~---
export AUTO_SUBMIT=true
export IS_INTERACTIVE=false
export PROJECT="m4460"
export PARTITION="regular"
export RUN_TIME="23:59:59"
export CPUS_PER_TASK=1
#---~---


#---~---
#    Debugging settings
#
# - DEBUG_LEVEL. The higher the number, the more information will be provided (at the 
#   expense of running slower).  0 means no debugging (typical setting for scientific 
#   analysis), 6 means very strict debugging (useful when developing the code).
# - USE_FATES. Logical flag (true or false).  This option allows running the native 
#   host land model (CLM or ELM) without FATES, which may be useful for some debugging.
#---~---
export DEBUG_LEVEL=0
export USE_FATES=true
#---~---



#---~---
#    Path settings:
#
# WORK_PATH: The main working path (typically <host_model>/cime/scripts)
# BASE_PATH: The main path where cases and simulations are written.
# CASE_ROOT: The main path where to create the directory for this case.
# SIMUL_ROOT: The main path where to create the directory for the simulation output.
#
# In all cases, use XXXX in the part you want to be replaced with either E3SM or CTSM.
#---~---
case "${MACH}" in
eschweilera)
   export WORK_PATH="${HOME}/Models/XXXX/cime/scripts"
   export BASE_PATH="${HOME}/Documents/LocalData/FATES"
   export CASE_ROOT="${BASE_PATH}/SingleRuns/Cases"
   export SIMUL_ROOT="${BASE_PATH}/SingleRuns/Simulations"
   ;;
*)
   export WORK_PATH="${HOME}/Models/XXXX/cime/scripts"
   export CASE_ROOT="${HOME}/SingleRuns/Cases"
   export SIMUL_ROOT="${SCRATCH}/SingleRuns/Simulations"
   ;;
esac
#---~---



#---~---
#   Define a base directory using SITE_BASE_PATH.  If using a standard data file
# structure, this is the directory where all site data are located, one sub-directory per
# site. The names of these sub-directories match site_name. Each site-specific path (i.e.
# `<SITE_BASE_PATH>/<SITE_INFO%site_name>` should contain:
#
# 1. A sub-directory `CLM1PT` containing the meteorological drivers.  Check documentation
#    for script [make_fates_met_driver.Rmd](make_fates_met_driver.html) for more details.
# 2. The domain and surface data specific for this site. Check documentation for script
#    [make_fates_domain+surface.Rmd](make_fates_domain+surface.html) for further
#    information.
# 3. _Optional_. A FATES parameter file, which is defined by variable `fates_param_base`,
#    defined a bit below in the chunk. This file is assumed to be in the `<site_name>`
#    sub-directory.  In case none is provided (i.e., `<fates_param_base>=""`), the case
#    will use the default parameter file. Beware that the default is not optimised and
#    may yield bad results.
# 4. _Optional_. The forest structure control data, which should contain the full paths
#    to ED2-style  pss (patch) and css (cohort) files. This file base name should be
#    `<site_name>_<inv_suffix>.txt`, or blank in case inventories should not be used.
#     Check the ED2 Wiki (https://github.com/EDmodel/ED2/wiki/Initial-conditions)
#    for details on how to generate the files.
#
#    The actual scripts are available on GitHub:
#    https://github.com/mpaiao/ED2_Support_Files/tree/master/pss%2Bcss_processing.
#---~---
export SITE_BASE_PATH="${HOME}/Data/FATES_DataSets"
#---~---


#---~---
#   Main case settings.  These variables will control compilation settings and the
# case name for this simulation.  It is fine to leave these blank, in which case the
# script will use default settings.
#---~---
export COMP=""
export CASE_PREFIX=""
#---~---


#---~---
#   Append git commit to the case name?
#---~---
export APPEND_GIT_HASH=false
#---~---



#---~---
#   Grid resolution.  If this is a standard ELM/CLM grid resolution, variables defined
# in the site information block below will be ignored.  In case you want to use the
# site information, set RESOL to YYY_USRDAT. (The host model will be replaced later
# in the script).
#---~---
export RESOL="YYY_USRDAT" # Grid resolution
#---~---


#---~---
#   Site information (SITE_INFO). In case RESOL is set to YYY_USRDAT, this allows the user
# to pick one of their favourite sites, and load the pre-defined settings for each of
# them. This should be an array with each line containing the following elements:
#
#
# site_id    -- A unique site id.  Typically a sequential order (but the code checks it).
#               Do not use zero or negative numbers, though.
# site_desc  -- A descriptive but short name for site (no spaces, please).
# site_name  -- The full site name, typically the sub-directory where all the site-specific
#               data are stored.
# datm_first -- First year with meteorological driver. This is normally fixed unless new
#               versions of the data become available.
# datm_last  -- Last year with meteorological driver. This is normally fixed unless new
#               versions of the data become available.
# calendar   -- Which calendar type should we use? This must be consistent with the
#               meteorological drivers. Options are `"NO_LEAP"`, for non-leap years and
#               `"GREGORIAN"`, for Gregorian calendar. Note that `"GREGORIAN"` calendar
#               requires meteorological drivers that extend to entire simulation time span
#               (i.e., no recycling), otherwise, the simulation will likely crash in one of
#               the Februaries outside the meteorological driver range.
#---~---
#                  site_id  site_desc            site_name                               datm_first  datm_last  calendar
export SITE_INFO=("      1  BarroColorado        1x1pt-bciPAN_v5.0_c20240616                   2003       2016   NO_LEAP"
                  "      2  Paracou              1x1pt-paracouGUF_v1.8_c20220114               2004       2019   NO_LEAP"
                  "      3  Tapajos              1x1pt-tapajosPABR_v1.0_c20231201              1999       2020   NO_LEAP"
                  "      4  Tanguro              1x1pt-tanguroMTBR_v1.2_c20210913              2008       2018   NO_LEAP"
                  "      5  SerraTalhada         1x1pt-serratalhadaPEBR_v1.0_c20220114         2008       2021   NO_LEAP"
                  "      6  ESECSerido           1x1pt-esecseridoRNBR_v1.0_c20220119           2008       2021   NO_LEAP"
                  "      7  Petrolina            1x1pt-petrolinaPEBR_v1.2_c20210913            2004       2012   NO_LEAP")
#---~---


#---~---
#    Variable SITE_USE lets you pick which site to use. This is an integer variable that
# must correspond to one of the site_id listed in SITE_INFO.
#---~---
if [[ "${SITE_USE}" == "" ]]
then
   export SITE_USE=1
fi
#---~---


#---~---
#   Set the base parameter path and file, in case a specific file exists. If the file is
# a site-specific one, set FATES_PARAMS_FILE= without directories. This will mean that
# the path is the SITE_PATH. If a specific parameter file is to be used across sites, set
# FATES_PARAMS_FILE with full path.  Leaving the variable empty (i.e., FATES_PARAMS_FILE="")
# means that we should be using the default parameter file.
#---~---
#export FATES_PARAMS_FILE="${SITE_BASE_PATH}/ParameterFiles/fates_params_1trop_ChonggangXu_c20220603.cdl"
#export FATES_PARAMS_FILE="${SITE_BASE_PATH}/ParameterFiles/fates_params_1trop_CXu+bciopt224.c201022.693.cdl"
export FATES_PARAMS_FILE="${SITE_BASE_PATH}/ParameterFiles/fates_params_4pfts_opt224_api33.cdl"
#---~---



#---~---
#   Configuration information.  This allows setting multiple FATES configurations. Note
# that this is different from changing parameter values or running parameter sensitivity
# experiments. Variable CONFIG_INFO in this current setting contains the following
# elements, but this may need to be edited (along with the places in the script where 
# CONFIG_INFO is used) depending on the simulation tests.
#
# config_id       -- A unique configuration ID. Typically a sequential order (but the code
#                    checks it).  Do not use zero or negative numbers, though.
# config_desc     -- Suffix to append to site name that summarises configuration.
# inv_suffix      -- Suffix for the forest inventory plot initialisation instructions.
#                    The base file name with instructions should be like:
#                    `<site_name>_<inv_suffix>.txt`.  In case you do not want to use
#                    inventory initialisation, set this to NA.
# fates_hydro     -- Use plant hydrodynamics? .true. turns it on, .false. turns it off.
# fates_st3       -- Used Static Stand Structure (ST3)? If TRUE, FATES will disable 
#                    changes in vegetation structure and leaf area index (effectively
#                    halting competition amongst PFTs), but it will still compute 
#                    gross primary productivity, evapotranspiration, etc.
# yeara           -- First year of the simulation. The actual meaning depends on the sign.
#                    Positive      -- Calendar year (e.g., 2022)
#                    Zero/Negative -- Year relative to the first year of the meteorological
#                                     cycle. The absolute value of yeara will determine how
#                                     many years BEFORE the first met driver year the model
#                                     run. (e.g., -5 means start five years before the
#                                     first met driver year).
# yearz           -- Last year of the simulation. The actual meaning depends on the sign.
#                    Positive      -- Calendar year (e.g., 2022)
#                    Zero/Negative -- Year relative to the first year of the meteorological
#                                     cycle. The absolute value of yeara will determine how
#                                     many years AFTER the first met driver year the model
#                                     run. (e.g., -5 means stop five years after the
#                                     last met driver year).
# stress_decid   -- Which drought deciduous approach should be used for those PFTs that are
#                   drought deciduous: 1 - Obligate ("hard") deciduous; 2 - Semi-deciduous
# drought_thresh -- Drought threshold for full abscission. If positive, this represents the
#                   soil moisture [m3/m3]. If negative, it represents the soil matric 
#                   potential [mm]. This is applied only for drought deciduous PFTs.
# moist_thresh   -- Drought threshold for full flushing. If positive, this represents the
#                   soil moisture [m3/m3]. If negative, it represents the soil matric 
#                   potential [mm]. Relevant only when running semi-deciduous.
#---~---
#                     config_id  config_desc                   inv_suffix  fates_hydro  fates_st3  yeara    yearz  stress_decid  drought_thresh  moist_thresh
export CONFIG_INFO=("         1  InitBare_CompeteON_HydroOFF   NA              .false.    .false.   1700     2025             1       -152957.4     -122365.9"
                    "         2  InitPlot_CompeteON_HydroOFF   nopft_info      .false.    .false.     -5        0             1       -152957.4     -122365.9"
                    "         3  InitPlot_CompeteOFF_HydroOFF  nopft_info      .false.     .true.     -5        0             1       -152957.4     -122365.9"
                    "         4  InitBare_CompeteON_HydroON    NA               .true.    .false.   1700     2025             1       -152957.4     -122365.9"
                    "         5  InitPlot_CompeteON_HydroON    nopft_info       .true.    .false.     -5        0             1       -152957.4     -122365.9"
                    "         6  InitPlot_CompeteOFF_HydroON   nopft_info       .true.     .true.     -5        0             1       -152957.4     -122365.9")
#---~---


#---~---
#    Variable CONFIG_USE lets you pick which configuration setting to use. This is an
# integer variable that must correspond to one of the config_id listed in CONFIG_INFO.
#---~---
if [[ "${CONFIG_USE}" == "" ]]
then
   export CONFIG_USE=5
fi
#---~---



#---~---
#    XML settings to change.  In case you don't want to change any settings, leave this
# part blank.  Otherwise, the first argument is the variable name, the second argument is
# the value.  Make sure to add single quotes when needed.  If you want, you can use the
# generic XXX for variables that may be either CLM or ELM (lower case xxx will replace
# with the lower-case model name).  The code will interpret it accordingly.
#
#    If the variable is site-specific, it may be easier to develop the code by adding
# new columns to SITE_INFO, and using a similar approach as the one used to set variable
# RUN STARTDATE (look for it in the script).
#
# Example:
#
# No change in xml settings:
# xml_settings=()
#
# Changes in xml settings
# xml_settings=("DOUT_S_SAVE_INTERIM_RESTART_FILES TRUE"
#               "DOUT_S                            TRUE"
#               "STOP_N                            10"
#               "XXX_FORCE_COLDSTART               on"
#               "RUN_STARTDATE                     '2001-01-01'")
#---~---
xml_settings=("DEBUG                             FALSE"
              "STOP_OPTION                       nyears"
              "REST_N                            1"
              "YYY_FORCE_COLDSTART               on")
#---~---



#---~---
#    List of parameters to be updated. In case you do not want to change any parameter,
# leave this part blank. Otherwise, the first argument is the variable name, the second 
# argument is the PFT number (or zero for global parameter), and the third argument is 
# the value. For parameters that are linked to multiple organs, list the values for all
# organs separated by commas and with NO SPACES in between the values.
#
# Example:
#
# No change in parameter settings:
# prm_settings=()
#
# Changes in xml settings
# prm_settings=("fates_wood_density 1      0.65"
#               "fates_wood_density 2      0.75"
#               "fates_smpsc        1 -200000.0"
#               "fates_smpsc        2 -300000.0")
#---~---
prm_settings=("fates_pftname                             1 DroughtDeciduousStrr"
              "fates_pftname                             2 EarlyEvergreenThsg"
              "fates_pftname                             3 MidEvergreenPlnv"
              "fates_pftname                             4 LateEvergreenCryp"

              "fates_wood_density                        1       0.598"
              "fates_wood_density                        2       0.503"
              "fates_wood_density                        3       0.561"
              "fates_wood_density                        4       0.692"

              "fates_leaf_slatop                         1      0.0285"
              "fates_leaf_slatop                         2      0.0336"
              "fates_leaf_slatop                         3      0.0211"
              "fates_leaf_slatop                         4      0.0176"

              "fates_leaf_slamax                         1      0.0360"
              "fates_leaf_slamax                         2      0.0384"
              "fates_leaf_slamax                         3      0.0321"
              "fates_leaf_slamax                         4      0.0300"

              "fates_leaf_vcmax25top                     1       38.05"
              "fates_leaf_vcmax25top                     2       52.96"
              "fates_leaf_vcmax25top                     3       43.28"
              "fates_leaf_vcmax25top                     4       27.44"

              "fates_leaf_jmax25top_scale                1       1.958"
              "fates_leaf_jmax25top_scale                2       1.769"
              "fates_leaf_jmax25top_scale                3       1.882"
              "fates_leaf_jmax25top_scale                4       2.165"

              "fates_turnover_leaf                       1      0.7545"
              "fates_turnover_leaf                       2      0.6571"
              "fates_turnover_leaf                       3      0.8129"
              "fates_turnover_leaf                       4      1.3147"

              "fates_turnover_fnrt                       1      0.7545"
              "fates_turnover_fnrt                       2      0.6571"
              "fates_turnover_fnrt                       3      0.8129"
              "fates_turnover_fnrt                       4      1.3147"

              "fates_mort_bmort                          1      0.0305"
              "fates_mort_bmort                          2      0.0510"
              "fates_mort_bmort                          3      0.0384"
              "fates_mort_bmort                          4      0.0100"

              "fates_mort_scalar_cstarvation             1       5.000"
              "fates_mort_scalar_cstarvation             2       5.000"
              "fates_mort_scalar_cstarvation             3       5.000"
              "fates_mort_scalar_cstarvation             4       5.000"

              "fates_mort_upthresh_cstarvation           1       0.060"
              "fates_mort_upthresh_cstarvation           2       0.060"
              "fates_mort_upthresh_cstarvation           3       0.060"
              "fates_mort_upthresh_cstarvation           4       0.060"

              "fates_mort_hf_sm_threshold                1       0.200"
              "fates_mort_hf_sm_threshold                2       0.200"
              "fates_mort_hf_sm_threshold                3       0.200"
              "fates_mort_hf_sm_threshold                4       0.200"

              "fates_allom_la_per_sa_int                 1       0.410"
              "fates_allom_la_per_sa_int                 2       0.410"
              "fates_allom_la_per_sa_int                 3       0.410"
              "fates_allom_la_per_sa_int                 4       0.410"

              "fates_allom_hmode                         1           2"
              "fates_allom_hmode                         2           2"
              "fates_allom_hmode                         3           2"
              "fates_allom_hmode                         4           2"

              "fates_allom_d2h1                          1       25.38"
              "fates_allom_d2h1                          2       25.75"
              "fates_allom_d2h1                          3       39.16"
              "fates_allom_d2h1                          4       48.53"

              "fates_allom_d2h2                          1    -0.03880"
              "fates_allom_d2h2                          2    -0.05108"
              "fates_allom_d2h2                          3    -0.04333"
              "fates_allom_d2h2                          4    -0.04507"

              "fates_allom_d2h3                          1      1.0873"
              "fates_allom_d2h3                          2      1.0325"
              "fates_allom_d2h3                          3      0.8836"
              "fates_allom_d2h3                          4      0.7969"

              "fates_allom_dbh_maxheight                 1        69.6"
              "fates_allom_dbh_maxheight                 2        66.8"
              "fates_allom_dbh_maxheight                 3       163.4"
              "fates_allom_dbh_maxheight                 4       270.8"

#              "fates_allom_amode                         1           3"
#              "fates_allom_amode                         2           3"
#              "fates_allom_amode                         3           3"
#              "fates_allom_amode                         4           3"

#              "fates_allom_agb1                          1       0.198"
#              "fates_allom_agb1                          2       0.198"
#              "fates_allom_agb1                          3       0.198"
#              "fates_allom_agb1                          4       0.208"

#              "fates_allom_agb2                          1       0.886"
#              "fates_allom_agb2                          2       0.886"
#              "fates_allom_agb2                          3       0.886"
#              "fates_allom_agb2                          4       0.931"

#              "fates_allom_agb3                          1          -9"
#              "fates_allom_agb3                          2          -9"
#              "fates_allom_agb3                          3          -9"
#              "fates_allom_agb3                          4          -9"

#              "fates_allom_agb4                          1          -9"
#              "fates_allom_agb4                          2          -9"
#              "fates_allom_agb4                          3          -9"
#              "fates_allom_agb4                          4          -9"

              "fates_allom_lmode                         1           4"
              "fates_allom_lmode                         2           4"
              "fates_allom_lmode                         3           4"
              "fates_allom_lmode                         4           4"

              "fates_allom_d2ca_coefficient_min          1      0.4145"
              "fates_allom_d2ca_coefficient_min          2      0.4333"
              "fates_allom_d2ca_coefficient_min          3      0.4333"
              "fates_allom_d2ca_coefficient_min          4      0.4333"

              "fates_allom_d2ca_coefficient_max          1      0.4145"
              "fates_allom_d2ca_coefficient_max          2      0.4333"
              "fates_allom_d2ca_coefficient_max          3      0.4333"
              "fates_allom_d2ca_coefficient_max          4      0.4333"

              "fates_allom_d2bl1                         1     0.00075"
              "fates_allom_d2bl1                         2     0.00084"
              "fates_allom_d2bl1                         3     0.00090"
              "fates_allom_d2bl1                         4     0.00097"

              "fates_allom_d2bl2                         1       0.632"
              "fates_allom_d2bl2                         2       0.565"
              "fates_allom_d2bl2                         3       0.582"
              "fates_allom_d2bl2                         4       0.580"

              "fates_allom_d2bl3                         1      -1.000"
              "fates_allom_d2bl3                         2      -1.000"
              "fates_allom_d2bl3                         3      -1.000"
              "fates_allom_d2bl3                         4      -1.000"

              "fates_allom_blca_expnt_diff               1      -0.127"
              "fates_allom_blca_expnt_diff               2      -0.097"
              "fates_allom_blca_expnt_diff               3      -0.114"
              "fates_allom_blca_expnt_diff               4      -0.111"

              "fates_allom_dmode                         1           2"
              "fates_allom_dmode                         2           2"
              "fates_allom_dmode                         3           2"
              "fates_allom_dmode                         4           2"

              "fates_allom_h2cd1                         1      0.4023"
              "fates_allom_h2cd1                         2      0.5605"
              "fates_allom_h2cd1                         3      0.4236"
              "fates_allom_h2cd1                         4      0.3686"

              "fates_allom_h2cd2                         1      0.9444"
              "fates_allom_h2cd2                         2      0.8539"
              "fates_allom_h2cd2                         3      0.9287"
              "fates_allom_h2cd2                         4      0.9779"

              "fates_allom_l2fr                          1         1.0"
              "fates_allom_l2fr                          2         1.0"
              "fates_allom_l2fr                          3         1.0"
              "fates_allom_l2fr                          4         1.0"

              "fates_allom_fmode                         1           2"
              "fates_allom_fmode                         2           2"
              "fates_allom_fmode                         3           2"
              "fates_allom_fmode                         4           2"

              "fates_allom_fnrt_prof_mode                1           3"
              "fates_allom_fnrt_prof_mode                2           3"
              "fates_allom_fnrt_prof_mode                3           3"
              "fates_allom_fnrt_prof_mode                4           3"

              "fates_allom_fnrt_prof_a                   1         5.9"
              "fates_allom_fnrt_prof_a                   2         7.3"
              "fates_allom_fnrt_prof_a                   3         7.3"
              "fates_allom_fnrt_prof_a                   4         7.3"

              "fates_allom_fnrt_prof_b                   1         2.3"
              "fates_allom_fnrt_prof_b                   2         0.8"
              "fates_allom_fnrt_prof_b                   3         0.8"
              "fates_allom_fnrt_prof_b                   4         0.8"

              "fates_allom_stmode                        1           2"
              "fates_allom_stmode                        2           2"
              "fates_allom_stmode                        3           2"
              "fates_allom_stmode                        4           2"

              "fates_alloc_storage_cushion               1         3.6"
              "fates_alloc_storage_cushion               2         1.2"
              "fates_alloc_storage_cushion               3         1.2"
              "fates_alloc_storage_cushion               4         1.2"

              "fates_recruit_init_density                1        0.01"
              "fates_recruit_init_density                2        0.01"
              "fates_recruit_init_density                3        0.01"
              "fates_recruit_init_density                4        0.01"

              "fates_recruit_seed_alloc                  1        0.99"
              "fates_recruit_seed_alloc                  2        0.99"
              "fates_recruit_seed_alloc                  3        0.99"
              "fates_recruit_seed_alloc                  4        0.99"

              "fates_recruit_seed_alloc_mature           1        0.00"
              "fates_recruit_seed_alloc_mature           2        0.00"
              "fates_recruit_seed_alloc_mature           3        0.00"
              "fates_recruit_seed_alloc_mature           4        0.00"

              "fates_trs_repro_alloc_a                   1     0.00324"
              "fates_trs_repro_alloc_a                   2     0.00406"
              "fates_trs_repro_alloc_a                   3     0.00096"
              "fates_trs_repro_alloc_a                   4     0.00034"

              "fates_trs_repro_alloc_b                   1      -2.647"
              "fates_trs_repro_alloc_b                   2      -2.694"
              "fates_trs_repro_alloc_b                   3      -2.409"
              "fates_trs_repro_alloc_b                   4      -2.291"

              "fates_recruit_seed_dbh_repro_threshold    1        14.2"
              "fates_recruit_seed_dbh_repro_threshold    2        12.5"
              "fates_recruit_seed_dbh_repro_threshold    3        23.1"
              "fates_recruit_seed_dbh_repro_threshold    4        30.9"

              "fates_recruit_seed_germination_rate       1        0.05"
              "fates_recruit_seed_germination_rate       2        0.05"
              "fates_recruit_seed_germination_rate       3        0.05"
              "fates_recruit_seed_germination_rate       4        0.05"

              "fates_allom_sai_scaler                    1        0.11"
              "fates_allom_sai_scaler                    2        0.11"
              "fates_allom_sai_scaler                    3        0.11"
              "fates_allom_sai_scaler                    4        0.11"

              "fates_maintresp_leaf_ryan1991_baserate    1   2.659E-06"
              "fates_maintresp_leaf_ryan1991_baserate    2   3.861E-06"
              "fates_maintresp_leaf_ryan1991_baserate    3   3.020E-06"
              "fates_maintresp_leaf_ryan1991_baserate    4   1.933E-06"

              "fates_maintresp_leaf_atkin2017_baserate   1       1.184"
              "fates_maintresp_leaf_atkin2017_baserate   2       1.166"
              "fates_maintresp_leaf_atkin2017_baserate   3       1.185"
              "fates_maintresp_leaf_atkin2017_baserate   4       1.181"

              "fates_maintresp_reduction_curvature       1      1.e-09"
              "fates_maintresp_reduction_curvature       2      1.e-16"
              "fates_maintresp_reduction_curvature       3      1.e-04"
              "fates_maintresp_reduction_curvature       4      1.e-01"

              "fates_maintresp_reduction_intercept       1         1.0"
              "fates_maintresp_reduction_intercept       2         1.0"
              "fates_maintresp_reduction_intercept       3         1.0"
              "fates_maintresp_reduction_intercept       4         1.0"

              "fates_cnp_prescribed_nuptake              1           0"
              "fates_cnp_prescribed_nuptake              2           0"
              "fates_cnp_prescribed_nuptake              3           0"
              "fates_cnp_prescribed_nuptake              4           0"

              "fates_cnp_prescribed_puptake              1           0"
              "fates_cnp_prescribed_puptake              2           0"
              "fates_cnp_prescribed_puptake              3           0"
              "fates_cnp_prescribed_puptake              4           0"

              "fates_nonhydro_smpsc                      1   -175901.0"
              "fates_nonhydro_smpsc                      2   -175901.0"
              "fates_nonhydro_smpsc                      3   -175901.0"
              "fates_nonhydro_smpsc                      4   -175901.0"

              "fates_nonhydro_smpso                      1   -61182.97"
              "fates_nonhydro_smpso                      2   -61182.97"
              "fates_nonhydro_smpso                      3   -61182.97"
              "fates_nonhydro_smpso                      4   -61182.97"

              "fates_phen_fnrt_drop_fraction             1         1.0"

              "fates_cnp_nfix1                           1         0.0"
              "fates_cnp_nfix1                           2         0.0"
              "fates_cnp_nfix1                           3         0.0"
              "fates_cnp_nfix1                           4         0.0"

              "fates_cnp_store_ovrflw_frac               1         1.0"
              "fates_cnp_store_ovrflw_frac               2         1.0"
              "fates_cnp_store_ovrflw_frac               3         1.0"
              "fates_cnp_store_ovrflw_frac               4         1.0"

              "fates_cnp_vmax_nh4                        1     5.0e-09"
              "fates_cnp_vmax_nh4                        2     5.0e-09"
              "fates_cnp_vmax_nh4                        3     5.0e-09"
              "fates_cnp_vmax_nh4                        4     5.0e-09"

              "fates_cnp_vmax_no3                        1         0.0"
              "fates_cnp_vmax_no3                        2         0.0"
              "fates_cnp_vmax_no3                        3         0.0"
              "fates_cnp_vmax_no3                        4         0.0"

              "fates_cnp_vmax_p                          1     5.0e-10"
              "fates_cnp_vmax_p                          2     5.0e-10"
              "fates_cnp_vmax_p                          3     5.0e-10"
              "fates_cnp_vmax_p                          4     5.0e-10"

              "fates_cnp_eca_vmax_ptase                  1     3.0e-08"
              "fates_cnp_eca_vmax_ptase                  2     3.0e-08"
              "fates_cnp_eca_vmax_ptase                  3     3.0e-08"
              "fates_cnp_eca_vmax_ptase                  4     3.0e-08"

              "fates_cnp_nitr_store_ratio                1         3.0"
              "fates_cnp_nitr_store_ratio                2         3.0"
              "fates_cnp_nitr_store_ratio                3         3.0"
              "fates_cnp_nitr_store_ratio                4         3.0"

              "fates_cnp_phos_store_ratio                1         3.0"
              "fates_cnp_phos_store_ratio                2         3.0"
              "fates_cnp_phos_store_ratio                3         3.0"
              "fates_cnp_phos_store_ratio                4         3.0"

              "fates_recruit_seed_supplement             1      0.0000"
              "fates_recruit_seed_supplement             2      0.0000"
              "fates_recruit_seed_supplement             3      0.0000"
              "fates_recruit_seed_supplement             4      0.0000"

              "fates_grperc                              1       0.167"
              "fates_grperc                              2       0.167"
              "fates_grperc                              3       0.167"
              "fates_grperc                              4       0.167"

              "fates_phen_evergreen                      1           0"
              "fates_phen_evergreen                      2           1"
              "fates_phen_evergreen                      3           1"
              "fates_phen_evergreen                      4           1"

              "fates_phen_season_decid                   1           0"
              "fates_phen_season_decid                   2           0"
              "fates_phen_season_decid                   3           0"
              "fates_phen_season_decid                   4           0"

              "fates_phen_stress_decid                   1           1"
              "fates_phen_stress_decid                   2           0"
              "fates_phen_stress_decid                   3           0"
              "fates_phen_stress_decid                   4           0"

              "fates_phen_flush_fraction                 1         0.5"

              "fates_stoich_nitr                         1 0.0599,0.0430,1.791E-08,0.00842"
              "fates_stoich_nitr                         2 0.0677,0.0486,2.024E-08,0.00951"
              "fates_stoich_nitr                         3 0.0444,0.0319,1.328E-08,0.00624"
              "fates_stoich_nitr                         4 0.0367,0.0263,1.097E-08,0.00516"

              "fates_stoich_phos                         1 0.00289,0.00210,8.757E-10,0.000412"
              "fates_stoich_phos                         2 0.00367,0.00267,1.113E-09,0.000523"
              "fates_stoich_phos                         3 0.00208,0.00151,6.304E-10,0.000296"
              "fates_stoich_phos                         4 0.00129,0.00094,3.899E-10,0.000183"

              "fates_hydro_epsil_node                    1 15.990,13.330,13.330,10.660"
              "fates_hydro_epsil_node                    2 14.970,12.480,12.480,09.980"
              "fates_hydro_epsil_node                    3 10.850,09.040,09.040,07.230"
              "fates_hydro_epsil_node                    4 11.070,09.230,09.230,07.380"

              "fates_hydro_pinot_node                    1 -1.812,-1.518,-1.518,-1.290"
              "fates_hydro_pinot_node                    2 -1.382,-1.158,-1.158,-0.984"
              "fates_hydro_pinot_node                    3 -1.454,-1.218,-1.218,-1.035"
              "fates_hydro_pinot_node                    4 -1.736,-1.454,-1.454,-1.236"

              "fates_hydro_pitlp_node                    1 -2.134,-1.789,-1.789,-1.533"
              "fates_hydro_pitlp_node                    2 -1.760,-1.475,-1.475,-1.265"
              "fates_hydro_pitlp_node                    3 -1.820,-1.526,-1.526,-1.308"
              "fates_hydro_pitlp_node                    4 -2.020,-1.693,-1.693,-1.451"

              "fates_hydro_kmax_node                     1 -999.,2.829,-999.,-999."
              "fates_hydro_kmax_node                     2 -999.,7.700,-999.,-999."
              "fates_hydro_kmax_node                     3 -999.,3.193,-999.,-999."
              "fates_hydro_kmax_node                     4 -999.,2.106,-999.,-999."

              "fates_hydro_p50_node                      1 -1.995,-1.995,-1.995,-1.995"
              "fates_hydro_p50_node                      2 -1.333,-1.333,-1.333,-1.333"
              "fates_hydro_p50_node                      3 -1.705,-1.705,-1.705,-1.705"
              "fates_hydro_p50_node                      4 -2.972,-2.972,-2.972,-2.972"

              "fates_cnp_turnover_nitr_retrans           1 0.45,0.25,0.00,0.00"
              "fates_cnp_turnover_nitr_retrans           2 0.45,0.25,0.00,0.00"
              "fates_cnp_turnover_nitr_retrans           3 0.45,0.25,0.00,0.00"
              "fates_cnp_turnover_nitr_retrans           4 0.45,0.25,0.00,0.00"

              "fates_cnp_turnover_phos_retrans           1 0.65,0.25,0.00,0.00"
              "fates_cnp_turnover_phos_retrans           2 0.65,0.25,0.00,0.00"
              "fates_cnp_turnover_phos_retrans           3 0.65,0.25,0.00,0.00"
              "fates_cnp_turnover_phos_retrans           4 0.65,0.25,0.00,0.00"

              "fates_hlm_pft_map                         1 0,0,0,0,1,0,0,0,0,0,0,0,0,0"
              "fates_hlm_pft_map                         2 0,0,0,1,0,0,0,0,0,0,0,0,0,0"
              "fates_hlm_pft_map                         3 0,0,0,1,0,0,0,0,0,0,0,0,0,0"
              "fates_hlm_pft_map                         4 0,0,0,1,0,0,0,0,0,0,0,0,0,0"

              "fates_alloc_organ_priority                1 1,1,3,4"
              "fates_alloc_organ_priority                2 1,1,3,4"
              "fates_alloc_organ_priority                3 1,1,3,4"
              "fates_alloc_organ_priority                4 1,1,3,4"

              "fates_mort_disturb_frac                   0 0.5"
              "fates_comp_excln                          0  -1"
              "fates_mort_cstarvation_model              0   2"
              "fates_mort_understorey_death              0   1"
              "fates_regeneration_model                  0   3"
              "fates_rad_model                           0   2"
              "fates_maintresp_leaf_model                0   1")
#---~---



#---~---
#    Additional settings for the host land model namelist.  This is done in two steps:
#
# --------
#  Step 1
# --------
#    Define the output variables by setting variable "hlm_variables", which 
# should have six entries:
# * variable  -- The variable name (case sensitive)
# * add_fates -- A flag to decide whether or not to include the variable based on whether or
#                not this is a FATES run. This is case insensitive. Possible options:
#                * yes  -- Add variable only when running FATES
#                * no   -- Add variable only when NOT running FATES
#                * both -- Add variable regardless of whether or not running FATES
# * add_hlm   -- List of the host land models (case insensitive) that can use the 
#                variable. To list more than one model, use "+" as a separator.
# * monthly   -- Add the variable to monthly output? Use yes/no (case insensitive)
# * daily     -- Add the variable to daily output? Use yes/no (case insensitive)
# * hourly    -- Add the variable to hourly output? Use yes/no (case insenstive)
#
#    Alternatively, one can define the default list of variables by not changing the
# default variables. In this case, the code will only produce monthly output.  In this
# case, set hist_empty_tapes to .false. in hlm_settings (Step 2), otherwise the model 
# will not write any output.
#
# Examples:
# 
# No change in variable settings:
# hlm_variables=()
#
# List variables to be included:
# hlm_variables=("AR                          no    clm+elm yes yes yes"
#                "ELAI                      both    clm+elm yes yes  no"
#                "FATES_NPLANT_CANOPY_SZPF   yes    clm+elm yes  no  no")
#
# --------
#  Step 2
# --------
#   Define other namelist settings. This normally contains variable hist_empty_htapes,
# which decides whether or not to include the default variables.
# 
# Example:
#
# No change in namelist settings:
# hlm_settings=()
#
# Changes in xml settings
# hlm_settings=("hist_empty_htapes  .true."
#               "fates_parteh_mode       1")
#---~---
#--- Step 1: Define output variables.
#               variable                               add_fates    add_hlm  monthly    daily   hourly"
hlm_variables=("AR                                            no    clm+elm      yes       no       no"
               "BTRAN                                       both    clm+elm      yes       no       no"
               "BTRANMN                                     both        clm      yes       no       no"
               "EFLX_LH_TOT                                 both    clm+elm      yes       no       no"
               "ELAI                                        both    clm+elm      yes       no       no"
               "ESAI                                        both    clm+elm      yes       no       no"
               "FATES_AGSAPMAINTAR_SZPF                      yes    clm+elm      yes       no       no"
               "FATES_AGSAPWOOD_ALLOC_SZPF                   yes    clm+elm      yes       no       no"
               "FATES_AGSTRUCT_ALLOC_SZPF                    yes    clm+elm      yes       no       no"
               "FATES_AUTORESP                               yes    clm+elm      yes       no       no"
               "FATES_AUTORESP_SZPF                          yes    clm+elm      yes       no       no"
               "FATES_BASALAREA_SZPF                         yes    clm+elm      yes       no       no"
               "FATES_BGSAPMAINTAR_SZPF                      yes    clm+elm      yes       no       no"
               "FATES_BGSAPWOOD_ALLOC_SZPF                   yes    clm+elm      yes       no       no"
               "FATES_BGSTRUCT_ALLOC_SZPF                    yes    clm+elm      yes       no       no"
               "FATES_CANOPYAREA_AP                          yes    clm+elm      yes       no       no"
               "FATES_CROWNAREA_CANOPY_SZPF                  yes    clm+elm      yes       no       no"
               "FATES_CROWNAREA_USTORY_SZPF                  yes    clm+elm      yes       no       no"
               "FATES_DAYSINCE_DROUGHTLEAFOFF_PF             yes    clm+elm      yes       no       no"
               "FATES_DAYSINCE_DROUGHTLEAFON_PF              yes    clm+elm      yes       no       no"
               "FATES_DDBH_CANOPY_SZPF                       yes    clm+elm      yes       no       no"
               "FATES_DDBH_USTORY_SZPF                       yes    clm+elm      yes       no       no"
               "FATES_DEMOTION_RATE_SZ                       yes    clm+elm      yes       no       no"
               "FATES_DROUGHT_STATUS_PF                      yes    clm+elm      yes       no       no"
               "FATES_ELONG_FACTOR_PF                        yes    clm+elm      yes       no       no"
               "FATES_FROOT_ALLOC_SZPF                       yes    clm+elm      yes       no       no"
               "FATES_FROOTMAINTAR_SZPF                      yes    clm+elm      yes       no       no"
               "FATES_GPP                                    yes    clm+elm      yes       no       no"
               "FATES_GPP_AP                                 yes    clm+elm      yes       no       no"
               "FATES_GPP_SZPF                               yes    clm+elm      yes       no       no"
               "FATES_GROWAR_SZPF                            yes    clm+elm      yes       no       no"
               "FATES_HET_RESP                               yes    clm+elm      yes       no       no"
               "FATES_LAI_AP                                 yes    clm+elm      yes       no       no"
               "FATES_LAI_CANOPY_SZPF                        yes    clm+elm      yes       no       no"
               "FATES_LAI_USTORY_SZPF                        yes    clm+elm      yes       no       no"
               "FATES_LBLAYER_COND                           yes    clm+elm      yes       no       no"
               "FATES_LBLAYER_COND_AP                        yes    clm+elm      yes       no       no"
               "FATES_LEAF_ALLOC_SZPF                        yes    clm+elm      yes       no       no"
               "FATES_LEAFC_CANOPY_SZPF                      yes    clm+elm      yes       no       no"
               "FATES_LEAFC_USTORY_SZPF                      yes    clm+elm      yes       no       no"
               "FATES_MEANLIQVOL_DROUGHTPHEN_PF              yes    clm+elm      yes       no       no"
               "FATES_MEANSMP_DROUGHTPHEN_PF                 yes    clm+elm      yes       no       no"
               "FATES_MORTALITY_AGESCEN_SZPF                 yes    clm+elm      yes       no       no"
               "FATES_MORTALITY_BACKGROUND_SZPF              yes    clm+elm      yes       no       no"
               "FATES_MORTALITY_CANOPY_SZPF                  yes    clm+elm      yes       no       no"
               "FATES_MORTALITY_CSTARV_SZPF                  yes    clm+elm      yes       no       no"
               "FATES_MORTALITY_FREEZING_SZPF                yes    clm+elm      yes       no       no"
               "FATES_MORTALITY_FIRE_SZPF                    yes    clm+elm      yes       no       no"
               "FATES_MORTALITY_HYDRAULIC_SZPF               yes    clm+elm      yes       no       no"
               "FATES_MORTALITY_IMPACT_SZPF                  yes    clm+elm      yes       no       no"
               "FATES_MORTALITY_LOGGING_SZPF                 yes    clm+elm      yes       no       no"
               "FATES_MORTALITY_SENESCENCE_SZPF              yes    clm+elm      yes       no       no"
               "FATES_MORTALITY_TERMINATION_SZPF             yes    clm+elm      yes       no       no"
               "FATES_MORTALITY_USTORY_SZPF                  yes    clm+elm      yes       no       no"
               "FATES_NEP                                    yes    clm+elm      yes       no       no"
               "FATES_NPLANT_CANOPY_SZPF                     yes    clm+elm      yes       no       no"
               "FATES_NPLANT_USTORY_SZPF                     yes    clm+elm      yes       no       no"
               "FATES_NPP_SZPF                               yes    clm+elm      yes       no       no"
               "FATES_NPP_CANOPY_SZ                          yes    clm+elm      yes       no       no"
               "FATES_NPP_USTORY_SZ                          yes    clm+elm      yes       no       no"
               "FATES_PATCHAREA_AP                           yes    clm+elm      yes       no       no"
               "FATES_PROMOTION_RATE_SZ                      yes    clm+elm      yes       no       no"
               "FATES_RDARK_SZPF                             yes    clm+elm      yes       no       no"
               "FATES_SEED_ALLOC_SZPF                        yes    clm+elm      yes       no       no"
               "FATES_STOMATAL_COND                          yes    clm+elm      yes       no       no"
               "FATES_STOMATAL_COND_AP                       yes    clm+elm      yes       no       no"
               "FATES_STORE_ALLOC_SZPF                       yes    clm+elm      yes       no       no"
               "FATES_STOREC_CANOPY_SZPF                     yes    clm+elm      yes       no       no"
               "FATES_STOREC_USTORY_SZPF                     yes    clm+elm      yes       no       no"
               "FATES_TRIMMING_CANOPY_SZ                     yes    clm+elm      yes       no       no"
               "FATES_TRIMMING_USTORY_SZ                     yes    clm+elm      yes       no       no"
               "FATES_VEGC_ABOVEGROUND                       yes    clm+elm      yes       no       no"
               "FATES_VEGC_ABOVEGROUND_SZPF                  yes    clm+elm      yes       no       no"
               "FIRE                                        both    clm+elm      yes       no       no"
               "FGR                                         both    clm+elm      yes       no       no"
               "FLDS                                        both    clm+elm      yes       no       no"
               "FSH                                         both    clm+elm      yes       no       no"
               "FSH_V                                       both    clm+elm      yes       no       no"
               "FSH_G                                       both    clm+elm      yes       no       no"
               "FSDS                                        both    clm+elm      yes       no       no"
               "FSR                                         both    clm+elm      yes       no       no"
               "GPP                                           no    clm+elm      yes       no       no"
               "HR                                            no    clm+elm      yes       no       no"
               "NEP                                           no    clm+elm      yes       no       no"
               "PBOT                                        both    clm+elm      yes       no       no"
               "Q2M                                         both    clm+elm      yes       no       no"
               "QAF                                         both        clm      yes       no       no"
               "QBOT                                        both    clm+elm      yes       no       no"
               "QDIRECT_THROUGHFALL                         both        clm      yes       no       no"
               "QDRAI                                       both    clm+elm      yes       no       no"
               "QDRIP                                       both    clm+elm      yes       no       no"
               "QINTR                                       both    clm+elm      yes       no       no"
               "QOVER                                       both    clm+elm      yes       no       no"
               "QSOIL                                       both    clm+elm      yes       no       no"
               "Qtau                                        both        clm      yes       no       no"
               "QVEGE                                       both    clm+elm      yes       no       no"
               "QVEGT                                       both    clm+elm      yes       no       no"
               "RAIN                                        both    clm+elm      yes       no       no"
               "SMP                                         both    clm+elm      yes       no       no"
               "TAF                                         both        clm      yes       no       no"
               "TBOT                                        both    clm+elm      yes       no       no"
               "TG                                          both    clm+elm      yes       no       no"
               "TLAI                                        both    clm+elm      yes       no       no"
               "TREFMNAV                                    both    clm+elm      yes       no       no"
               "TREFMXAV                                    both    clm+elm      yes       no       no"
               "TSA                                         both    clm+elm      yes       no       no"
               "TSAI                                        both    clm+elm      yes       no       no"
               "TSOI                                        both    clm+elm      yes       no       no"
               "TV                                          both    clm+elm      yes       no       no"
               "U10                                         both    clm+elm      yes       no       no"
               "UAF                                         both        clm      yes       no       no"
               "USTAR                                       both        clm      yes       no       no"
               "ZWT                                         both    clm+elm      yes       no       no"
               "ZWT_PERCH                                   both    clm+elm      yes       no       no")
#--- Step 2: Additional namelist settings.
hlm_settings=("hist_empty_htapes    .true.")
#---~---

#---~---
#    Additional settings for output files.
#---~---
month_mfilt=1   # 1   # How many steps to put in each monthly file?
day_mfilt=365   # 30  # How many steps to put in each daily file?
hour_mfilt=8760 # 720 # How many steps to put in each hour file?
#---~---


#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#                  CHANGES BEYOND THIS POINT ARE FOR SCRIPT DEVELOPMENT ONLY!
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------



#---~---
#  Define the host Earth System Model from upper-case CIME_MODEL.
#---~---
export HESM=$(echo ${HESM} | tr '[:lower:]' '[:upper:]')
#---~---

#--- Update cade and simulation paths, in case a generic name was provided.
export WORK_PATH=$(echo ${WORK_PATH}           | sed s@"XXXX"@"${HESM}"@g)
export CASE_ROOT=$(echo ${CASE_ROOT}           | sed s@"XXXX"@"${HESM}"@g)
export SIMUL_ROOT=$(echo ${SIMUL_ROOT}         | sed s@"XXXX"@"${HESM}"@g)
#---~---

#--- Current date.
export TODAY=$(date +"%Y-%m-%d")
#---~---

#--- Path of the main script
export SOURCE_PATH=$(dirname ${BASH_SOURCE})
export SOURCE_PATH=$(cd ${SOURCE_PATH}; pwd)
export SOURCE_BASE="$(basename ${BASH_SOURCE})"
#---~---

#--- Site path.
export SITE_PATH="${SITE_BASE_PATH}/${SITE_NAME}"
#---~---


#---~---
#   Check if cluster settings make sense (only if running in a HPC environment.
#---~---
case "${MACH}" in
pm-*)
   #---~---
   #   pm-cpu (NERSC), in the future, possibly pm-gpu. Set up some global SLURM information.
   #---~---
   case "${MACH}" in
   pm-cpu)
      export N_NODES_MAX=300
      export MAX_CPUS_PER_TASK=128
      export MAX_TASKS_PER_NODE=256
      export RUN_TIME_MAX="24:00:00"
      export CONSTRAINT="cpu"
      ;;
   esac
   #---~---


   #---~---
   #   Set maximum time allowed for interactive nodes
   #---~---
   export RUN_TIME_INT="04:00:00"
   #---~---


   #---~---
   #   Set the default partition in case it is not provided.
   #---~---
   if [[ "${PARTITION}" == "" ]]
   then
      export PARTITION="regular"
   fi
   #---~---


   #---~---
   #   Do not let account to be undefined
   #---~---
   if [[ "${PROJECT}" == "" ]]
   then
      echo " Variable \"PROJECT\" ought to be defined if using perlmutter (pm-cpu)!"
      exit 59
   fi
   #---~---


   #---~---
   #   Check that CPU requests are reasonable
   #---~---
   if [[ ${CPUS_PER_TASK} -gt ${MAX_CPUS_PER_TASK} ]]
   then
      echo " Too many CPUs per task requested:"
      echo " Machine                 = ${MACH}"
      echo " Maximum CPUs per task   = ${MAX_CPUS_PER_TASK}"
      echo " Requested CPUs per task = ${CPUS_PER_TASK}"
      exit 99
   else
      #--- Find maximum number of tasks allowed
      (( N_TASKS_MAX = N_NODES_MAX * CPUS_PER_NODE ))
      (( N_TASKS_MAX = N_TASKS_MAX / CPUS_PER_TASK ))
      #---~---
   fi
   #---~---


   #---~---
   #   Check time requests
   #---~---
   export RUN_TIME=$(echo     ${RUN_TIME}     | tr '[:upper:]' '[:lower:]')
   export RUN_TIME_MAX=$(echo ${RUN_TIME_MAX} | tr '[:upper:]' '[:lower:]')
   case "${RUN_TIME}" in
   infinite)
      #---~---
      #   Infinite run time.  Make sure the queue supports this type of submission.
      #---~---
      case "${RUN_TIME_MAX}" in
      infinite)
         echo "" > /dev/null
         ;;
      *)
         echo " Machine:                    ${MACH}"
         echo " Maximum run time permitted: ${RUN_TIME_MAX}"
         echo " Requested run time:         ${RUN_TIME}"
         echo " This cluster does not support infinite time."
         exit 91
         ;;
      esac
      #---~---
      ;;
   *)
      #---~---
      #   Find out the format provided.
      #---~---
      case "${RUN_TIME}" in
      *-*:*:*|*-*:*)
         #--- dd-hh:mm:ss.
         ndays=$(echo ${RUN_TIME}  | sed s@"-.*$"@@g)
         nhours=$(echo ${RUN_TIME} | sed s@"^[0-9]\+-"@@g | sed s@":.*"@@g)
         nminutes=$(echo ${RUN_TIME} | sed s@"^[0-9]\+-[0-9]\+:"@@g | sed s@":.*$"@@g)
         #---~---
         ;;
      *:*:*)
         #--- hh:mm:ss.
         ndays=0
         nhours=$(echo ${RUN_TIME} | sed s@":.*"@@g)
         nminutes=$(echo ${RUN_TIME} | sed s@"^[0-9]\+:"@@g | sed s@":.*"@@g)
         #---~---
         ;;
      *:*)
         #--- hh:mm:ss.
         ndays=0
         nhours=0
         nminutes=$(echo ${RUN_TIME} | sed s@":.*$"@@g)
         #---~---
         ;;
      *)
         #--- Hours.
         (( ndays  = RUN_TIME / 24 ))
         (( nhours = RUN_TIME % 24 ))
         nminutes=0
         #---~---
         ;;
      esac
      #---~---


      #---~---
      #   Find the walltime in hours, and the run time in nice format.
      #---~---
      (( wall     = nminutes + 60 * nhours + 1440 * ndays ))
      (( nhours   = wall / 60 ))
      (( nminutes = wall % 60 ))
      fmthr=$(printf '%.2i' ${nhours})
      fmtmn=$(printf '%2.2i' ${nminutes})
      RUN_TIME="${fmthr}:${fmtmn}:00"
      #---~---



      #---~---
      #   Find the maximum number of hours allowed in the partition.
      #---~---
      case "${RUN_TIME_MAX}" in
      infinite)
         (( ndays_max    = ndays + 1 ))
         (( nhours_max   = nhours    ))
         (( nminutes_max = nminutes  ))
         #---~---
         ;;
      *-*:*:*|*-*:*)
         #--- dd-hh:mm:ss.
         ndays_max=$(echo ${RUN_TIME_MAX}  | sed s@"-.*"@@g)
         nhours_max=$(echo ${RUN_TIME_MAX} | sed s@"^[0-9]\+-"@@g | sed s@":.*"@@g)
         nminutes_max=$(echo ${RUN_TIME_MAX} | sed s@"^[0-9]\+-[0-9]\+:"@@g | sed s@":.*$"@@g)
         #---~---
         ;;
      *:*:*)
         #--- hh:mm:ss.
         ndays_max=0
         nhours_max=$(echo ${RUN_TIME_MAX}   | sed s@":.*"@@g)
         nminutes_max=$(echo ${RUN_TIME_MAX} | sed s@"^[0-9]\+:"@@g | sed s@":.*"@@g)
         #---~---
         ;;
      *:*)
         #--- hh:mm:ss.
         ndays_max=0
         nhours_max=0
         nminutes_max=$(echo ${RUN_TIME_MAX} | sed s@":.*$"@@g)
         #---~---
         ;;
      *)
         #--- Hours.
         (( ndays_max  = RUN_TIME_MAX / 24 ))
         (( nhours_max = RUN_TIME_MAX % 24 ))
         nminutes_max=0
         #---~---
         ;;
      esac
      (( wall_max = nminutes_max + 60 * nhours_max + 1440 * ndays_max ))
      #---~---


      #---~---
      #   Check requested walltime and the availability.
      #---~---
      if [[ ${wall} -gt ${wall_max} ]]
      then
         echo " Machine:                    ${MACH}"
         echo " Maximum run time permitted: ${RUN_TIME_MAX}"
         echo " Requested run time:         ${RUN_TIME}"
         echo " - Requested time exceeds limits."
         exit 92
      fi
      #---~---
      ;;
   esac
   #---~---

   ;;
esac
#---~---


#---~---
#   Make changes to some of the settings based on the host model.
#---~---
case "${HESM}" in
E3SM)
   #---~---
   # E3SM-FATES
   #---~---

   #--- Set CIME model.
   export CIME_MODEL="e3sm"
   #---~---

   #--- Set host land model. Set both upper case and lower case for when needed.
   export hlm="elm"
   export HLM="ELM"
   #---~---

   #--- Main path for host model
   export HOSTMODEL_PATH=$(dirname $(dirname ${WORK_PATH}))
   #---~---

   #--- Additional options for "create_newcase"
   export NEWCASE_OPTS=""
   #---~---

   #--- Main source path for FATES.
   export FATES_SRC_PATH="${HOSTMODEL_PATH}/components/elm/src/external_models/fates"
   #---~---

   #--- In case compilates settings is not defined, use the default settings.
   if ${USE_FATES} && [[ "${COMP}" == "" ]]
   then
      export COMP="IELMFATES"
   elif [[ "${COMP}" == "" ]]
   then
      export COMP="IELMBGC"
   fi
   #---~---

   ;;
CTSM|CESM)
   #---~---
   # CESM-FATES or CTSM-FATES
   #---~---

   #--- Set CIME model.
   export CIME_MODEL="cesm"
   #---~---

   #--- Set host land model. Set both upper case and lower case for when needed.
   export hlm="clm"
   export HLM="CLM"
   #---~---


   #--- Main path for host model
   export HOSTMODEL_PATH=$(dirname $(dirname ${WORK_PATH}))
   #---~---

   #--- Additional options for "create_newcase"
   export NEWCASE_OPTS="--run-unsupported --driver=mct"
   #---~---

   #--- Main source path for FATES.
   export FATES_SRC_PATH="${HOSTMODEL_PATH}/src/fates"
   #---~---


   #--- In case compilation settings are not defined, use the default settings.
   if ${USE_FATES} && [[ "${COMP}" == "" ]]
   then
      export COMP="I2000Clm51FatesRs"
   elif [[ "${COMP}" == "" ]]
   then
      export COMP="I2000Clm51Bgc"
   fi
   #---~---

   ;;
esac
#---~---


#--- Define version of host model and FATES
export HLM_HASH="${HLM}-$(cd ${HOSTMODEL_PATH};   git log -n 1 --pretty=%h)"
export FATES_HASH="FATES-$(cd ${FATES_SRC_PATH}; git log -n 1 --pretty=%h)"
#---~---


#--- Define setting for single-point
export V_HLM_USRDAT_NAME="${HLM}_USRDAT_NAME"
export V_HLM_NAMELIST_OPTS="${HLM}_NAMELIST_OPTS"
#---~---


#--- Substitute wildcards in the resolution with the actual model
export RESOL=$(echo "${RESOL}" | sed s@"YYY"@"${HLM}"@g | sed s@"yyy"@"${hlm}"@g)
#---~---




#---~---
#   Retrieve site information.
#---~---
site_success=false
for i in ${!SITE_INFO[*]}
do
   site_now=${SITE_INFO[i]}
   export SITE_ID=$(echo ${site_now}   | awk '{print $1}')
   
   if [[ ${SITE_ID} -eq ${SITE_USE} ]]
   then
      # Load data and flag that we found the site information.
      export SITE_DESC=$(echo ${site_now}       | awk '{print $2}')
      export SITE_NAME=$(echo ${site_now}       | awk '{print $3}')
      export SITE_DATM_FIRST=$(echo ${site_now} | awk '{print $4}')
      export SITE_DATM_LAST=$(echo ${site_now}  | awk '{print $5}')
      export SITE_CALENDAR=$(echo ${site_now}   | awk '{print $6}')

      #--- Append site settings to xml_settings.
      xml_settings+=("DATM_CLMNCEP_YR_START ${SITE_DATM_FIRST}"
                     "DATM_CLMNCEP_YR_END   ${SITE_DATM_LAST} ")
      #---~---

      #---~---
      #   Set derived quantities.
      #---~---
      # Site path.
      export SITE_PATH="${SITE_BASE_PATH}/${SITE_NAME}"
      # Domain file (it must be in the SITE_NAME sub-directory).
      export HLM_USRDAT_DOMAIN="domain.lnd.${SITE_NAME}_navy.nc"
      # Surface data file (it must be in the SITE_NAME sub-directory).
      export HLM_USRDAT_SURDAT="surfdata_${SITE_NAME}.nc"
      #---~---


      #--- Update status and exit loop.
      site_success=true
      break
      #---~---
   fi
done
#---~---




#---~---
#   Retrieve configuration information.
#---~---
config_success=false
export CONFIG_ID_MAX=0
for i in ${!CONFIG_INFO[*]}
do
   config_now=${CONFIG_INFO[i]}
   export CONFIG_ID=$(echo ${config_now}   | awk '{print $1}')

   if [[ ${CONFIG_ID} -eq ${CONFIG_USE} ]]
   then
      # Load data and flag that we found the configuration information.
      export CONFIG_DESC=$(echo ${config_now}    | awk '{print  $2}')
      export INV_SUFFIX=$(echo ${config_now}     | awk '{print  $3}')
      export FATES_HYDRO=$(echo ${config_now}    | awk '{print  $4}')
      export FATES_ST3=$(echo ${config_now}      | awk '{print  $5}')
      export SIMUL_YEARA=$(echo ${config_now}    | awk '{print  $6}')
      export SIMUL_YEARZ=$(echo ${config_now}    | awk '{print  $7}')
      export STRESS_DECID=$(echo ${config_now}   | awk '{print  $8}')
      export DROUGHT_THRESH=$(echo ${config_now} | awk '{print  $9}')
      export MOIST_THRESH=$(echo ${config_now}   | awk '{print $10}')
      #---~---


      #---~---
      # In case SIMUL_YEARA is non-positive, convert first year to calendar year.
      #---~---
      if [[ ${SIMUL_YEARA} -le 0 ]]
      then
        let SIMUL_YEARA=${SITE_DATM_FIRST}+${SIMUL_YEARA}
      fi
      #---~---


      #---~---
      # In case SIMUL_YEARZ is non-positive, convert last year to calendar year.
      # The minus sign ensures that the last year offset will be added to the
      # last year of the met driver.
      #---~---
      if [[ ${SIMUL_YEARZ} -le 0 ]]
      then
        let SIMUL_YEARZ=${SITE_DATM_LAST}-${SIMUL_YEARZ}
      fi
      #---~---

      #--- Define the first and last year of simulations
      export SITE_START_DATE="${SIMUL_YEARA}-01-01"
      let SITE_STOP_N=${SIMUL_YEARZ}-${SIMUL_YEARA}+1
      export SITE_STOP_N=${SITE_STOP_N}
      #---~---


      #--- Update settings as needed.
      hlm_settings+=("use_fates_planthydro                     ${FATES_HYDRO}"    )
      hlm_settings+=("use_fates_ed_st3                         ${FATES_ST3}"      )
      prm_settings+=("fates_phen_drought_threshold           1 ${DROUGHT_THRESH}" )
      prm_settings+=("fates_phen_moist_threshold             1 ${MOIST_THRESH}"   )
      prm_settings+=("fates_phen_stress_decid                1 ${STRESS_DECID}"   )
      xml_settings+=("RUN_STARTDATE                            ${SITE_START_DATE}")
      xml_settings+=("STOP_N                                   ${SITE_STOP_N}    ")
      #---~---



      #--- Configuration was successful but keep looping to find the maximum ID.
      config_success=true
      #---~---
   fi


   #--- Update maximum ID
   if [[ ${CONFIG_ID} -gt ${CONFIG_ID_MAX} ]]
   then
      export CONFIG_ID_MAX=${CONFIG_ID}
   fi
   #---~---

done
#---~---



#---~---
#   Check for success. In case so, set additional variables that may depend on both 
# site and configuration information. Otherwise, stop the shell.
#---~---
if ${site_success} && ${config_success}
then
   #---~---
   #   Set default case prefix
   #---~---
   (( SIMUL_ID = CONFIG_ID_MAX * SITE_USE - CONFIG_ID_MAX + CONFIG_USE ))
   export SIMUL_LABEL="$(printf '%4.4i' ${SIMUL_ID})"
   if [[ "${CASE_PREFIX}" == "" ]]
   then
      export CASE_PREFIX="E${SIMUL_LABEL}_${SITE_DESC}_${CONFIG_DESC}"
   fi
   #---~---



   #---~---
   #    In case the inventory/lidar initialisation is sought, provide the file name of the
   # control file specification (the control file should be in the 
   # SITE_NAME sub-directory). Otherwise, do not set this variable (INVENTORY_BASE="")
   #
   #  For additional information, check
   # https://github.com/NGEET/fates/wiki/Model-Initialization-Modes#Inventory_Format_Type_1
   #---~---
   case "${INV_SUFFIX}" in
   NA|Na|na|NA)
      export INVENTORY_BASE=""
      ;;
   *)
      export INVENTORY_BASE="${SITE_NAME}_${INV_SUFFIX}.txt"
      ;;
   esac
   #---~---
else
   #---~---
   #   Settings were incorrect, stop the run.
   #---~---
   echo " "
   echo "---~---"
   echo " Invalid site and/or configuration settings:"
   echo ""
   echo " + SITE_USE : ${SITE_USE}"
   echo " + CONFIG_USE : ${CONFIG_USE}"
   echo ""
   echo " + Site settings failed : ${site_failed}"
   echo " + Model configuration settings failed : ${config_failed}"
   echo ""
   echo " Make sure that variables \"SITE_USE\" and \"CONFIG_USE\" are set to a value."
   echo "    listed in columns \"site_id\" of array \"SITE_INFO\", and \"config_id\" of"
   echo "    array \"CONFIG_INFO\", respectively."
   echo "---~---"
   exit 91
   #---~---
fi
#---~---


#---~---
#   Append github commit hash, or a simple host-model / FATES tag.
#---~---
if ${USE_FATES} && ${APPEND_GIT_HASH}
then
   export CASE_NAME="${CASE_PREFIX}_${HLM_HASH}_${FATES_HASH}"
elif ${APPEND_GIT_HASH}
then
   export CASE_NAME="${CASE_PREFIX}_${HLM_HASH}_BigLeaf"
elif ${USE_FATES}
then
   export CASE_NAME="${CASE_PREFIX}_${HLM}_FATES"
else
   export CASE_NAME="${CASE_PREFIX}_${HLM}_BigLeaf"
fi
#---~---


#---~---
#    Set paths for case and simulation.
#---~---
export CASE_PATH="${CASE_ROOT}/${CASE_NAME}"
export SIMUL_PATH="${SIMUL_ROOT}/${CASE_NAME}"
export FIGURE_PATH="${SIMUL_ROOT}/Figures/${CASE_NAME}"
export RDATA_FILE="${SIMUL_ROOT}/RData/Monthly_${CASE_PREFIX}.RData"
#---~---



#--- Namelist for the host land model.
export USER_NL_HLM="${CASE_PATH}/user_nl_${hlm}"
#---~---


#--- Set additional namelists
export USER_NL_DATM="${CASE_PATH}/user_nl_datm"
export USER_NL_MOSART="${CASE_PATH}/user_nl_mosart"
#---~---




#---~---
#  In case the case exists, warn user before assuming it's fine to delete files.
#---~---
if [[ -s ${CASE_PATH}   ]] || [[ -s ${SIMUL_PATH} ]] || 
   [[ -s ${FIGURE_PATH} ]] || [[ -s ${RDATA_FILE} ]]
then
   #---~---
   #    Check with user if it's fine to delete existing case.
   #---~---
   echo    " Case directories (${CASE_NAME}) already exist, proceeding will delete them."
   echo -n " Proceed (y|N)?   "
   read proceed
   proceed=$(echo ${proceed} | tr '[:upper:]' '[:lower:]')
   #---~---


   #---~---
   #    We give one last chance for users to cancel before deleting the files.
   #---~---
   case "${proceed}" in
   y|yes)
      echo "---------------------------------------------------------------------"
      echo " FINAL WARNING!"
      echo " I will start deleting files in 5 seconds."
      echo " In case you change your mind, press Ctrl+C before the time is over."
      echo "---------------------------------------------------------------------"
      when=6
      echo -n " - "
      while [[ ${when} -gt 1 ]]
      do
         let when=${when}-1
         echo -n " ${when}..."
         sleep 1
      done
      echo " Time is over!"



      #--- Delete files.
      /bin/rm -rvf ${CASE_PATH} ${SIMUL_PATH} ${FIGURE_PATH} ${RDATA_FILE}
      #---~---

      ;;
   *)
      echo " - Script interrupted, files were kept."
      exit 0
      ;;
   esac
   #---~---
fi
#---~---



#---~---
#    Move to the main cime path then create the new case
#---~---
echo "   - Initialise case for this single-node job."
cd ${WORK_PATH}
./create_newcase --case=${CASE_PATH} --res=${RESOL} --compset=${COMP} --mach=${MACH}       \
   --project=${PROJECT} ${NEWCASE_OPTS}
#---~---


#---~---
#     Set the CIME output to the main CIME path.
#---~---
echo "   - Update output paths and job run time."
cd ${CASE_PATH}
./xmlchange CIME_OUTPUT_ROOT="${SIMUL_ROOT}"
./xmlchange DOUT_S_ROOT="${SIMUL_PATH}"
./xmlchange PIO_DEBUG_LEVEL="${DEBUG_LEVEL}"
./xmlchange JOB_WALLCLOCK_TIME="${RUN_TIME}" --subgroup case.run
./xmlchange JOB_QUEUE="${PARTITION}"
#---~---


#---~---
#     In case this is a user-defined site simulation, set the user-specified paths.
# DATM_MODE must be set to CLM1PT, even when running E3SM-FATES.
#---~---
cd ${CASE_PATH}
case "${RESOL}" in
?LM_USRDAT)
   echo "   - Update paths for surface and domain files."

   # Append site-specific surface data information to the namelist.
   HLM_SURDAT_FILE="${SITE_PATH}/${HLM_USRDAT_SURDAT}"


   ./xmlchange DATM_MODE="CLM1PT"
   ./xmlchange CALENDAR="${SITE_CALENDAR}"
   ./xmlchange ${V_HLM_USRDAT_NAME}="${SITE_NAME}"
   ./xmlchange ${V_HLM_NAMELIST_OPTS}="fsurdat = '${HLM_SURDAT_FILE}'"
   ./xmlchange ATM_DOMAIN_PATH="${SITE_PATH}"
   ./xmlchange LND_DOMAIN_PATH="${SITE_PATH}"
   ./xmlchange ATM_DOMAIN_FILE="${HLM_USRDAT_DOMAIN}"
   ./xmlchange LND_DOMAIN_FILE="${HLM_USRDAT_DOMAIN}"
   ./xmlchange DIN_LOC_ROOT_CLMFORC="${SITE_BASE_PATH}"

   ;;
esac
#---~---


#---~---
#     Set the PE layout for a single-site run (unlikely that users would change this).
#---~---
echo "   - Configure Processing elements (PES)."
cd ${CASE_PATH}
./xmlchange NTASKS_ATM=1
./xmlchange NTASKS_LND=1
./xmlchange NTASKS_ROF=1
./xmlchange NTASKS_ICE=1
./xmlchange NTASKS_OCN=1
./xmlchange NTASKS_CPL=1
./xmlchange NTASKS_GLC=1
./xmlchange NTASKS_WAV=1
./xmlchange NTASKS_ESP=1

./xmlchange NTHRDS_ATM=1
./xmlchange NTHRDS_LND=1
./xmlchange NTHRDS_ROF=1
./xmlchange NTHRDS_ICE=1
./xmlchange NTHRDS_OCN=1
./xmlchange NTHRDS_CPL=1
./xmlchange NTHRDS_GLC=1
./xmlchange NTHRDS_WAV=1
./xmlchange NTHRDS_ESP=1

./xmlchange ROOTPE_ATM=0
./xmlchange ROOTPE_LND=0
./xmlchange ROOTPE_ROF=0
./xmlchange ROOTPE_ICE=0
./xmlchange ROOTPE_OCN=0
./xmlchange ROOTPE_CPL=0
./xmlchange ROOTPE_GLC=0
./xmlchange ROOTPE_WAV=0
./xmlchange ROOTPE_ESP=0
#---~---



#---~---
#     Change XML configurations if needed.
#---~---
cd ${CASE_PATH}
if [[ ${#xml_settings[*]} -gt 0 ]]
then
   #--- Loop through the options to update.
   echo " + Update XML settings."
   for x in ${!xml_settings[*]}
   do
      #--- Retrieve settings.
      xml_id=$(echo ${xml_settings[x]}  | awk '{print $1}')
      xml_id=$(echo ${xml_id}           | sed s@"YYY"@${HLM}@g | sed s@"yyy"@${hlm}@g)
      xml_val=$(echo ${xml_settings[x]} | awk '{print $2}')
      echo " ID = ${xml_id}; VAL = ${xml_val}"
      #---~---

      #--- Update settings.
      ./xmlchange ${xml_id}="${xml_val}"
      #---~---

   done
   #---~---
else
   #--- No changes needed.
   echo " + No XML changes required."
   #---~---
fi
#---~---


#--- Start setting up the case
echo "   - Set up the case."
cd ${CASE_PATH}
./case.setup --silent
#---~---



#---~---
#    Make sure that the axis mode is configured to cycle, so the meteorological 
# drivers are correctly recycled over time. 
#---~---
case "${RESOL}" in
?LM_USRDAT)
   # Append time axis mode to the user namelist
   echo "taxmode = 'cycle','cycle','cycle'" >> ${USER_NL_DATM}
   # Append MOSART input file to the user namelist
   # echo "frivinp_rtm = ' '" >> ${USER_NL_MOSART}
   ;;
esac
#---~---



#---~---
#     Include settings for the inventory initialisation.
#---~---
if ${USE_FATES} && [[ "${INVENTORY_BASE}" != "" ]]
then

   #--- Set inventory file with full path.
   INVENTORY_FILE="${SITE_PATH}/${INVENTORY_BASE}"
   #---~---


   #--- Instruct the host land model to use the modified parameter set. 
   touch ${USER_NL_HLM}
   echo "use_fates_inventory_init = .true."                   >> ${USER_NL_HLM}
   echo "fates_inventory_ctrl_filename = '${INVENTORY_FILE}'" >> ${USER_NL_HLM}
   #---~---

fi
#---~---



#---~---
#
# WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! 
# WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! 
# WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! 
# WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! 
#
#     This part modifies the stream file, in case incoming long wave radiation is not
# available.  You should not need to change anything in here.
#---~---
# Find the first met driver.
case "${RESOL}" in
?LM_USRDAT)
   #--- Define files with meteorological driver settings.
   HLM_USRDAT_ORIG="${SIMUL_PATH}/run/datm.streams.txt.CLM1PT.${RESOL}"
   HLM_USRDAT_USER="${CASE_PATH}/user_datm.streams.txt.CLM1PT.${RESOL}"
   #---~---

   #--- Define meteorological driver path
   DATM_PATH="${SITE_PATH}/CLM1PT_data"
   #---~---


   ANY_METD_NC=$(/bin/ls -1 ${DATM_PATH}/????-??.nc 2> /dev/null | wc -l)
   if [[ ${ANY_METD_NC} -gt 0 ]]
   then
      #--- Load one netCDF file.
      METD_NC_1ST=$(/bin/ls -1 ${DATM_PATH}/????-??.nc 2> /dev/null | head -1)
      ANY_FLDS=$(ncdump -h ${METD_NC_1ST} 2> /dev/null | grep FLDS | wc -l)
      if [[ ${ANY_FLDS} -eq 0 ]]
      then
         #--- Incoming long wave radiation is missing, change the stream file
         echo " + Remove incoming longwave radiation from met driver."
         /bin/cp -f ${HLM_USRDAT_ORIG} ${HLM_USRDAT_USER}
         sed -i".bck" '/FLDS/d' ${HLM_USRDAT_USER}
         /bin/rm -f "${HLM_USRDAT_USER}.bck"
         #---~---
      else
         echo " + Incoming longwave radiation available from met driver."
      fi
      #---~---
   else
      #--- Report error.
      echo " FATAL ERROR!"
      echo " ANY_METD_NC = ${ANY_METD_NC}"
      echo " Meteorological drivers not found in ${DATM_PATH}".
      echo " Make sure all the met driver files are named as yyyy-mm.nc"
      exit 91
      #---~---
   fi
   #---~---
   ;;
esac
#---~---


#---~---
#     Change FATES parameters if needed.
#---~---
if ${USE_FATES}
then

   #--- Identify original parameter file
   if [[ "${FATES_PARAMS_FILE}" == "" ]]
   then
      FATES_PARAMS_ORIG="${FATES_SRC_PATH}/parameter_files/fates_params_default.cdl"
   else
      FATES_PARAMS_ORIG="${FATES_PARAMS_FILE}"
   fi
   #---~---


   #--- Create a local parameter file.
   echo " + Create local parameter file from $(basename ${FATES_PARAMS_ORIG})."
   FATES_PARAMS_CASE="${CASE_PATH}/fates_params_${CASE_NAME}.nc"
   ncgen -o ${FATES_PARAMS_CASE} ${FATES_PARAMS_ORIG}
   #---~---


   #---~---
   #   Check whether or not to edit parameters
   #---~---
   if [[ ${#prm_settings[*]} -gt 0 ]]
   then

      #--- Set python script for updating parameters.
      MODIFY_PARAMS_PY="${FATES_SRC_PATH}/tools/modify_fates_paramfile.py"
      #---~---


      #--- Loop through the parameters to update.
      echo " + Create local parameter file."
      for p in ${!prm_settings[*]}
      do
         #--- Retrieve settings.
         prm_var=$(echo ${prm_settings[p]} | awk '{print $1}')
         prm_num=$(echo ${prm_settings[p]} | awk '{print $2}')
         prm_val=$(echo ${prm_settings[p]} | awk '{print $3}')
         #---~---

         #--- Update parameters, skipping the PFT setting in case it is zero (global).
         case ${prm_num} in
         0)
            ${MODIFY_PARAMS_PY} --var=${prm_var} --val=${prm_val}                          \
               --fin=${FATES_PARAMS_CASE} --fout=${FATES_PARAMS_CASE} --O
            ;;
         *)
            ${MODIFY_PARAMS_PY} --var=${prm_var} --pft=${prm_num} --val=${prm_val}         \
               --fin=${FATES_PARAMS_CASE} --fout=${FATES_PARAMS_CASE} --O
            ;;
         esac
         #---~---
      done
      #---~---
   fi
   #---~---


   #--- Instruct the host land model to use the modified parameter set. 
   touch ${USER_NL_HLM}
   echo "fates_paramfile = '${FATES_PARAMS_CASE}'" >> ${USER_NL_HLM}
   #---~---
else
   #--- No changes needed.
   echo " + No change to parameter settings required."
   #---~---
fi
#---~---


#---~---
# Add other variables to the namelist of the host land model.
#---~---
if  [[ ${#hlm_settings[*]} -gt 0 ]]
then
   #--- Loop through the options to update.
   echo " + Update host land model settings."
   for h in ${!hlm_settings[*]}
   do
      #--- Retrieve settings.
      hlm_id=$(echo ${hlm_settings[h]}  | awk '{print $1}')
      hlm_id=$(echo ${hlm_id}           | sed s@"YYY"@${HLM}@g | sed s@"yyy"@${hlm}@g)
      hlm_val=$(echo ${hlm_settings[h]} | awk '{for(i=2;i<=NF;++i)printf $i""FS ; print ""}')
      #---~---

      #--- Check whether or not this is a FATES variable.
      is_fates_var=$(echo ${hml_id} | grep -i fates | wc -l)
      #---~---


      #---~---
      #   Check whether this is a FATES variable.  In case it is and USE_FATES is false,
      # we ignore the variable.
      #---~---
      if ${USE_FATES} || [[ ${is_fates_var} -eq 0 ]]
      then
         #--- Update namelist
         echo " + Append variable ${hlm_id} = ${hlm_val} to $(basename ${USER_NL_HLM})."
         touch ${USER_NL_HLM}
         echo "${hlm_id} = ${hlm_val}" >> ${USER_NL_HLM}
         #---~---
      else
         #--- Do not update.  Instead, warn the user.
         echo " + Ignoring ${hlm_id} as this a FATES variable, and USE_FATES=false."
         #---~---
      fi
      #---~---
   done
   #---~---
else
   #--- No changes needed.
   echo " + No general HLM namelist settings required."
   #---~---
fi
#---~---


#---~---
# Add the variable list to the namelist of the host land model.
#---~---
if  [[ ${#hlm_variables[*]} -gt 0 ]]
then
   #---~---
   #   Initialise variables assuming no output, then update them.
   #---~---
   hlm_mlist=""
   hlm_dlist=""
   hlm_hlist=""
   #---~---



   #--- Loop through the options to update.
   echo " + Check whether to append list of variables to $(basename ${USER_NL_HLM})."
   n_month_add=0
   n_day_add=0
   n_hour_add=0
   for h in ${!hlm_variables[*]}
   do
      #--- Retrieve settings.
      hlm_now=${hlm_variables[h]}
      hlm_var=$(echo ${hlm_now}   | awk '{print $1}'                             )
      add_fates=$(echo ${hlm_now} | awk '{print $2}' | tr '[:upper:]' '[:lower:]')
      add_hlm=$(echo ${hlm_now}   | awk '{print $3}' | grep -i ${hlm} | wc -l    )
      add_month=$(echo ${hlm_now} | awk '{print $4}' | tr '[:upper:]' '[:lower:]')
      add_day=$(echo ${hlm_now}   | awk '{print $5}' | tr '[:upper:]' '[:lower:]')
      add_hour=$(echo ${hlm_now}  | awk '{print $6}' | tr '[:upper:]' '[:lower:]')
      #---~---


      #---~---
      #   We only add the variable if the variable is compatible with the host land model
      # and if we will use FATES or it is not a FATES variable
      #---~---
      if [[ ${add_hlm} -gt 0 ]]
      then
         if ${USE_FATES}
         then
            case "${add_fates}" in
               yes|both) add_var=true  ;;
               *)        add_var=false ;;
            esac
         else
            case "${add_fates}" in
               no|both) add_var=true  ;;
               *)       add_var=false ;;
            esac
         fi
      else
         add_var=false
      fi
      #---~---


      #---~---
      #    Append variable to the list in case the variable is fine for this settings.
      # Then check which time scales to include.
      #---~---
      if ${add_var}
      then
         echo "   - Append variable \"${hlm_var}\" to the output variable list."


         #---~---
         #   Monthly
         #---~---
         if [[ "${add_month}" == "yes" ]]
         then
            #--- Increment variable counter
            (( n_month_add = n_month_add + 1 ))
            #---~---


            #---~---
            #    Append variable to the list. Make sure commas are added for all but
            # the first.
            #---~---
            case ${n_month_add} in
               1) hlm_mlist="${hlm_mlist} '${hlm_var}'" ;;
               *) hlm_mlist="${hlm_mlist},'${hlm_var}'" ;;
            esac
            #---~---
         fi
         #---~---

         #---~---
         #   Daily
         #---~---
         if [[ "${add_day}" == "yes" ]]
         then
            #--- Increment variable counter
            (( n_day_add = n_day_add + 1 ))
            #---~---


            #---~---
            #    Append variable to the list. Make sure commas are added for all but
            # the first.
            #---~---
            case ${n_day_add} in
               1) hlm_dlist="${hlm_dlist} '${hlm_var}'" ;;
               *) hlm_dlist="${hlm_dlist},'${hlm_var}'" ;;
            esac
            #---~---
         fi
         #---~---

         #---~---
         #   Hourly
         #---~---
         if [[ "${add_hour}" == "yes" ]]
         then
            #--- Increment variable counter
            (( n_hour_add = n_hour_add + 1 ))
            #---~---


            #---~---
            #    Append variable to the list. Make sure commas are added for all
            # but the first.
            #---~---
            case ${n_hour_add} in
               1) hlm_hlist="${hlm_hlist} '${hlm_var}'" ;;
               *) hlm_hlist="${hlm_hlist},'${hlm_var}'" ;;
            esac
            #---~---
         fi
         #---~---
      else
         #--- Do not update.  Instead, warn the user.
         echo "   - Ignoring \"${hlm_var}\" because it is incompatible with the settings."
         #---~---
      fi
      #---~---
   done
   #---~---


   #---~---
   #    We only append the variable lists if at least one variable was included and the
   # output variable is to be included. Regardless, we always set monthly outputs as the
   # primary, to avoid generating large data sets for daily or hourly.
   #---~---
   hlm_nhtfrq="hist_nhtfrq = 0"
   hlm_mfilt="hist_mfilt  = ${month_mfilt}"
   hlm_incl=1
   #---~---


   #---~---
   #   Check monthly. The settings will be always included, so the default output
   # is always monthly, even if nothing is written.
   #---~---
   if [[ ${n_month_add} -gt 0 ]]
   then
      #--- Add list of monthly variables to the output.
      hlm_mlist="hist_fincl1 = ${hlm_mlist}"
      touch ${USER_NL_HLM}
      echo ${hlm_mlist} >> ${USER_NL_HLM}
      #---~---
   fi
   #---~---


   #---~---
   #   Check daily, and add if any variable is to be in the output.
   #---~---
   if [[ ${n_day_add} -gt 0 ]]
   then
      #--- Add list of daily variables to the output.
      (( hlm_incl = hlm_incl + 1 ))
      hlm_dlist="hist_fincl${hlm_incl} = ${hlm_dlist}"
      touch ${USER_NL_HLM}
      echo ${hlm_dlist} >> ${USER_NL_HLM}
      #---~---

      #---~---
      #   Update frequency and steps.
      #---~---
      hlm_nhtfrq="${hlm_nhtfrq}, -24"
      hlm_mfilt="${hlm_mfilt}, ${day_mfilt}"
      #---~---
   fi
   #---~---


   #---~---
   #   Check hourly, and add if any variable is to be in the output.
   #---~---
   if [[ ${n_hour_add} -gt 0 ]]
   then
      #--- Add list of daily variables to the output.
      (( hlm_incl = hlm_incl + 1 ))
      hlm_hlist="hist_fincl${hlm_incl} = ${hlm_hlist}"
      touch ${USER_NL_HLM}
      echo ${hlm_hlist} >> ${USER_NL_HLM}
      #---~---

      #---~---
      #   Update frequency and steps.
      #---~---
      hlm_nhtfrq="${hlm_nhtfrq}, -1"
      hlm_mfilt="${hlm_mfilt}, ${hour_mfilt}"
      #---~---
   fi
   #---~---


   #--- Append time table instructions
   touch ${USER_NL_HLM}
   echo ${hlm_nhtfrq} >> ${USER_NL_HLM}
   echo ${hlm_mfilt}  >> ${USER_NL_HLM}
   #---~---

else
   #--- Append time table instructions
   touch ${USER_NL_HLM}
   echo "hist_nhtfrq = 0"               >> ${USER_NL_HLM}
   echo "hist_mfilt  = ${month_mfilt}"  >> ${USER_NL_HLM}
   #---~---

   #--- Other than the time table, no further changes needed.
   echo " + No additional HLM variables required."
   #---~---
fi
#---~---


#---~---
#   Make sure namelists are set as expected.
#---~---
echo "   - Preview namelists."
cd ${CASE_PATH}
./preview_namelists --silent
#---~---



#--- Build case.
echo "   - Build case."
BUILD_LOG="${CASE_PATH}/status_case.build_$(date '+%y%m%d-%H%M%S')"
cd ${CASE_PATH}
./case.build --silent --clean
./case.build 1> ${BUILD_LOG} 2>& 1
#---~---



#--- Go to the source path.
cd ${SOURCE_PATH}
#---~---


#--- Print instructions on how to run the model
IS_SUCCESS=$(grep "MODEL BUILD HAS FINISHED SUCCESSFULLY" ${BUILD_LOG} | wc -l | xargs)
#---~---


#---~---
#   Decide whether or not to launch the case
#---~---
if [[ ${IS_SUCCESS} -gt 0 ]] && ${AUTO_SUBMIT}
then
   # Case was successfully built. Submitting the case.
   echo ""
   echo ""
   echo ""
   echo "---------------------------------------------------------------------"
   echo " CASE WAS SUCCESSFULLY BUILT, CONGRATULATIONS!"
   echo " I will start running the case in 10 seconds."
   echo " In case you change your mind, press Ctrl+C before the time is over."
   echo "---------------------------------------------------------------------"
   when=11
   echo -n " - "
   while [[ ${when} -gt 1 ]]
   do
      let when=${when}-1
      echo -n " ${when}..."
      sleep 1
   done
   echo " Time is over!"

   #  Proceed with submission, but check whether this is a local machine or a cluster.
   case "${MACH}" in
   eschweilera)
      #  Local machine, we save the standard output to a file.
      SUBMIT_LOG="${SOURCE_PATH}/$(echo ${SOURCE_BASE} | sed s@"\\.sh$"@".log"@g)"
      cd ${CASE_PATH}
      ./case.submit | tee ${SUBMIT_LOG}
      cd ${SOURCE_PATH}
      ;;
   *)
      #  Cluster, we save the standard output will be saved in files specified by job scheduler.
      cd ${CASE_PATH}
      ./case.submit
      cd ${SOURCE_PATH}
      ;;
   esac


elif [[ ${IS_SUCCESS} -gt 0 ]]
then
   # Case was successfully built. Give instructions on how to submit the job.
   echo ""
   echo ""
   echo ""
   echo "---------------------------------------------------------------------"
   echo " CASE WAS SUCCESSFULLY BUILT, CONGRATULATIONS!"
   echo " To submit the case, use the following commands"
   if ${IS_INTERACTIVE}
   then
      case "${MACH}" in
      pm-*)
         echo "salloc --qos=interactive --constraint=${CONSTRAINT} --time=${RUN_TIME_INT}"
         echo "cd ${CASE_PATH}"
         echo "case.submit --no-batch"
         ;;
      eschweilera)
         echo "(cd ${CASE_PATH}; ./case.submit)"
         ;;
      *)
         echo " <Launch interactive job on ${MACH}>"
         echo "cd ${CASE_PATH}"
         echo "<Job submission command> case.submit --no-batch"
         ;;
      esac
   else
      echo "(cd ${CASE_PATH}; ./case.submit)"
   fi
   echo "---------------------------------------------------------------------"
elif [[ "${HESM}" == "E3SM" ]]
then
   N_HESM_LOG=$(cat ${BUILD_LOG}  | grep "ERROR: BUILD FAIL: build e3sm failed | wc -l")
   if [[ ${N_HESM_LOG} -eq 0 ]]
   then
      ERROR_LOG=${BUILD_LOG}
   else
      ERROR_LINE=$(cat ${BUILD_LOG}  | grep "ERROR: BUILD FAIL: build e3sm failed")
      ERROR_LOG=$(echo ${ERROR_LINE} | awk '{print $8}')
   fi


   # Case building failed for E3SM. Stop and report.
   echo ""
   echo ""
   echo ""
   echo "---------------------------------------------------------------------"
   echo " Case building was unsuccessful."
   echo " Check file ${ERROR_LOG} for additional information."
   echo "---------------------------------------------------------------------"
   exit 89
else
   # Case building failed. Stop and report.
   echo ""
   echo ""
   echo ""
   echo "---------------------------------------------------------------------"
   echo " Case building was unsuccessful."
   echo " Check file ${BUILD_LOG} for additional information."
   echo "---------------------------------------------------------------------"
   exit 89
fi
#---~---
