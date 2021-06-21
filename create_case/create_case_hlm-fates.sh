#!/bin/bash                                                                                                                                                       

#==========================================================================================
#
#    This script generates a case for a single-point simulation for a user-defined site. 
# It assumes all the site-specific data files (SITE_NAME) are located in the 
# SITE_BASE_PATH folder.
#
# Developed by: Marcos Longo < m l o n g o -at- l b l -dot- g o v >
#               18 Jun 2021 09:45 PDT
# Based on scripts developed by Ryan Knox and Shawn Serbin.
#
#     This script does not take any arguments. Instead, the beginning of the script has
# many variables to control the simulation settings.
#==========================================================================================


#--- Main settings.
CIME_MODEL="ctsm"        # Model (ctsm, cesm, or e3sm)
PROJECT=ngeet            # Project (may not be needed)
MACH="eschweilera"       # Machine used for preparing the case
GMAKE="make"             # Which make to use.
#---~---


#---~---
#    Path settings:
# HLM_INPUT_PATH: Main path containing the gridded input data (not the site-specific data).
#                 Missing data sets will be downloaded here, so mind that this path should
#                 have plenty of disk space available.
# WORK_PATH: The main working path (typically <host_model>/cime/scripts)
# CASE_ROOT: The main path where to create the directory for this case.
# SIMUL_ROOT: The main path where to create the directory for the simulation output.
#
# In all cases, use XXXX in the part you want to be replaced with either E3SM or CTSM.
#---~---
HLM_INPUT_PATH="${HOME}/Documents/LocalData/XXXX-FATES/XXXX_InputData"
WORK_PATH="${HOME}/Models/XXXX/cime/scripts"
CASE_ROOT="${HOME}/Documents/LocalData/XXXX-FATES/Cases"
SIMUL_ROOT="${HOME}/Documents/LocalData/XXXX-FATES/Simulations"
#---~---


#---~---
#   Main case settings.  These variables will control compilation settings and the 
# case name for this simulation.  It is fine to leave these blank, in which case the
# script will use default settings.
#---~---
COMP=""
CASE_NAME="F0004_BCITest"
#---~---

#---~---
#   Grid resolution.  If this is a standard ELM/CLM grid resolution, variables defined
# in the site information block below will be ignored.  In case you want to use the 
# site information, set RESOL to XXX_USRDAT. (The host model will be replaced later 
# in the script).
#---~---
RESOL="XXX_USRDAT"
# RESOL="1x1_brazil" # Grid resolution
#---~---


#---~---
#   Site information.  This is used only if RESOL is XXX_USRDAT.
#
#     To keep things in a somewhat standardised format, we recommend creating a base 
# directory (SITE_BASE_PATH) where each set of site data (SITE_NAME) will be stored.
# (The full path for the sets of a certain file being ${SITE_BASE_PATH}/${SITE_NAME}
#
#     The site-specific path should contain (1) a sub-directory CLM1PT
# containing the meteorological drivers, (2) the domain and surface data specific for
# this site, and (3) optionally the forest structure data (ED2 pss/css files).
# In case you would like to set other sites and are familiar with R, check for 
# Marcos' pre-processing tools on GitHub:
# 1.  https://github.com/mpaiao/FATES_Utils 
#     Tools available for meteorological driver and domain and surface data.
# 2.  https://github.com/mpaiao/ED2_Support_Files/tree/master/pss%2Bcss_processing
#     ED2 tools to generate pss/css files. A brief tutorial is provided in 
#     https://github.com/EDmodel/ED2/wiki/Initial-conditions (additional adaptations
#     might be needed for FATES).
#---~---
# Path containing all the data sets.
SITE_BASE_PATH="${HOME}/Data/FATES_DataSets"
# Sub-directory with data sets specific to this site.
SITE_NAME="bci_0.1x0.1_v4.0"
# Domain file (it must be in the SITE_NAME sub-directory).
HLM_USRDAT_DOMAIN="domain_bci_clm5.0.dev009_c180523.nc"
# Surface data file (it must be in the SITE_NAME sub-directory).
HLM_USRDAT_SURDAT="surfdata_bci_clm5.0.dev009_c180523.nc"
# Calendar type for the meteorological drivers ('NO_LEAP' or 'GREGORIAN')
METD_CALENDAR="NO_LEAP"
#---~---

#---~---
#    In case the inventory/lidar initialisation is sought, provide the file name of the
# control file specification (the control file should be in the SITE_NAME sub-directory). 
# Otherwise, do not set this variable (INVENTORY_BASE="")
#
#  For additional information, check
# https://github.com/NGEET/fates/wiki/Model-Initialization-Modes#Inventory_Format_Type_1
#---~---
INVENTORY_BASE="bci_inv_file_list.txt"
#---~---


#---~---
#    XML settings to change.  In case you don't want to change any settings, leave this 
# part blank.  Otherwise, the first argument is the variable name, the second argument is
# the value.  Make sure to add single quotes when needed.  If you want, you can use the 
# generic XXX for variables that may be either CLM or ELM (lower case xxx will replace
# with the lower-case model name).  The code will interpret it accordingly.
#
# Example:
#
# No change in xml settings:
# xml_settings=()
#
# Changes in xml settings
# xml_settings=("DEBUG                             FALSE"
#               "GMAKE                             make"
#               "DOUT_S_SAVE_INTERIM_RESTART_FILES TRUE"
#               "DOUT_S                            TRUE"
#               "STOP_N                            10"
#               "XXX_FORCE_COLDSTART               on"
#               "RUN_STARTDATE                     '2001-01-01'")
#---~---
xml_settings=("DEBUG                             FALSE"
              "GMAKE                             make"
              "DOUT_S_SAVE_INTERIM_RESTART_FILES TRUE"
              "DOUT_S                            TRUE"
              "STOP_N                            13"
              "REST_N                            1"
              "RUN_STARTDATE                     2003-01-01"
              "STOP_OPTION                       nyears"
              "XXX_FORCE_COLDSTART               on"
              "JOB_WALLCLOCK_TIME                3:00"
              "DATM_CLMNCEP_YR_START             2003"
              "DATM_CLMNCEP_YR_END               2016")
#---~---



#---~---
#    PFT Parameter settings to change.  In case you don't want to change any parameter, leave
# this part blank.  Otherwise, the first argument is the variable name, the second argument
# is the PFT number, and the third argument is the value.
#
# Example:
#
# No change in PFT settings:
# pft_settings=()
#
# Changes in xml settings
# pft_settings=("fates_wood_density 1      0.65"
#               "fates_wood_density 2      0.75"
#               "fates_smpsc        1 -200000.0"
#               "fates_smpsc        2 -300000.0")
#---~---
pft_settings=("fates_wood_density 1      0.65"
              "fates_wood_density 2      0.75"
              "fates_smpsc        1 -200000.0"
              "fates_smpsc        2 -300000.0")
#---~---



#---~---
#    Additional settings for the host land model namelist.  First argument is the namelist
# variable name, and second argument (and beyond) is/are the values.  IMPORTANT: If the
# argument is a character, enclose the character part in quotes.  If multiple values are
# to be passed, the comma shall not be enclosed in quotes.  If the string is long, you can
# break them into multiple lines using backslash (\). The backslash will not be printed.
#
# Example:
#
# No change in namelist settings:
# hlm_settings=()
#
# Changes in xml settings
# hlm_settings=("hist_empty_htapes  .true."
#               "hist_fincl1        'GPP_BY_AGE', 'PATCH_AREA_BY_AGE', 'BA_SCLS'")
#
# For variable names, you can use hlm or HLM as a wildcard for the host land model.
#---~---
hlm_settings=("hist_empty_htapes  .true."
              "hist_mfilt         480"
              "hist_nhtfrq        -1"
              "hist_fincl1        'GPP_BY_AGE', 'PATCH_AREA_BY_AGE', 'BA_SCLS', \
                                  'BA_SCLS','NPLANT_CANOPY_SCLS','NPLANT_UNDERSTORY_SCLS',\
                                  'DDBH_CANOPY_SCLS','DDBH_UNDERSTORY_SCLS', \
                                  'MORTALITY_CANOPY_SCLS','MORTALITY_UNDERSTORY_SCLS'")
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
#  Make CIME_MODEL upper-case
#---~---
export ESM=$(echo ${CIME_MODEL} | tr '[:lower:]' '[:upper:]')
#---~---

#--- Update cade and simulation paths, in case a generic name was provided.
HLM_INPUT_PATH=$(echo ${HLM_INPUT_PATH} | sed s@"XXXX"@"${ESM}"@g)
WORK_PATH=$(echo ${WORK_PATH}           | sed s@"XXXX"@"${ESM}"@g)
CASE_ROOT=$(echo ${CASE_ROOT}           | sed s@"XXXX"@"${ESM}"@g)
SIMUL_ROOT=$(echo ${SIMUL_ROOT}         | sed s@"XXXX"@"${ESM}"@g)
#---~---

#--- Current date.
TODAY=$(date +"%Y-%m-%d")
#---~---

#--- Current path.
HERE_PATH=$(pwd)
#---~---

#--- Site path.
SITE_PATH="${SITE_BASE_PATH}/${SITE_NAME}"
#---~---


#---~---
#   Make changes to some of the settings based on the host model.
#---~---
case "${ESM}" in
ACME|E3SM)
   #---~---
   # E3SM-FATES
   #---~---

   #--- Set host land model. Set both upper case and lower case for when needed.
   hlm="elm"
   HLM="ELM"
   
   #---~---

   #--- Main path for host model
   HOSTMODEL_PATH=$(dirname $(dirname ${WORK_PATH}))
   #---~---

   #--- Main path for FATES
   FATESMODEL_PATH="${HOSTMODEL_PATH}/components/elm/src/external_models/fates"
   #---~---

   #--- Additional options for "create_newcase"
   NEWCASE_OPTS=""
   #---~---

   #--- Main source path for FATES.
   FATES_SRC_PATH="${HOSTMODEL_PATH}/components/elm/src/external_models/fates"
   #---~---

   #--- In case compilates settings is not defined, use the default settings.
   if [[ "${COMP}" == "" ]]
   then
      COMP="IELMFATES"
   fi
   #---~---


   #--- Define version of host model and FATES
   HLM_HASH="E$(cd ${HOSTMODEL_PATH}; git log -n 1 --pretty=%h)"
   FATES_HASH="F$(cd ${FATESMODEL_PATH}; git log -n 1 --pretty=%h)"
   #---~---


   #--- Define settings for single-point
   V_HLM_USRDAT_NAME="ELM_USRDAT_NAME"
   #---~---

   ;;
CTSM|CESM)
   #---~---
   # CESM-FATES or CTSM-FATES
   #---~---

   #--- Set host land model. Set both upper case and lower case for when needed.
   hlm="clm"
   HLM="CLM"
   #---~---


   #--- Main path for host model
   HOSTMODEL_PATH=$(dirname $(dirname ${WORK_PATH}))
   #---~---

   #--- Main path for FATES
   FATESMODEL_PATH="${HOSTMODEL_PATH}/src/fates"
   #---~---

   #--- Additional options for "create_newcase"
   NEWCASE_OPTS="--run-unsupported"
   #---~---

   #--- Main source path for FATES.
   FATES_SRC_PATH="${HOSTMODEL_PATH}/src/fates"
   #---~---


   #--- In case compilates settings is not defined, use the default settings.
   if [[ "${COMP}" == "" ]]
   then
      COMP="I2000Clm51Fates"
   fi
   #---~---

   #--- Define version of host model and FATES
   HLM_HASH="C$(cd ${HOSTMODEL_PATH}; git log -n 1 --pretty=%h)"
   FATES_HASH="F$(cd ${FATESMODEL_PATH}; git log -n 1 --pretty=%h)"
   #---~---

   #--- Define setting for single-point
   V_HLM_USRDAT_NAME="CLM_USRDAT_NAME"
   #---~---

   ;;
esac
#---~---


#--- Substitute wildcards in the resolution with the actual model
RESOL=$(echo "${RESOL}" | sed s@"XXX"@"${HLM}"@g | sed s@"xxx"@"${hlm}"@g)
#---~---


#---~---
#   Build the case name in case the case name was not provided (I know, lots of "cases"
# in one sentence).
#---~---
if [[ "${CASE_NAME}" == "" ]]
then
   CASE_NAME="FX_${COMP}_${MACH}_${HLM_HASH}-${FATES_HASH}_${TODAY}"
fi
#---~---


#---~---
#    Set paths for case and simulation.
#---~---
CASE_PATH="${CASE_ROOT}/${CASE_NAME}"
SIMUL_PATH="${SIMUL_ROOT}/${CASE_NAME}"
#---~---



#--- Namelist for the host land model.
USER_NL_HLM="${CASE_PATH}/user_nl_${hlm}"
#---~---





#---~---
#  In case the case exists, warn user before assuming it's fine to delete files.
#---~---
if [[ -s ${CASE_PATH} ]] || [[ -s ${SIMUL_PATH} ]]
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
      /bin/rm -rvf ${CASE_PATH} ${SIMUL_PATH}
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
#    Move to the main cime path.
#---~---
cd ${WORK_PATH}
#---~---


#---~---
#    Create the new case
#---~---
./create_newcase --case=${CASE_PATH} --res=${RESOL} --compset=${COMP} --mach=${MACH}       \
   --project=${PROJECT} ${NEWCASE_OPTS}
cd ${CASE_PATH}
#---~---


#---~---
#     Set the CIME output to the main CIME path.
#---~---
./xmlchange --id DIN_LOC_ROOT     --val "${HLM_INPUT_PATH}"
./xmlchange --id CIME_OUTPUT_ROOT --val "${SIMUL_ROOT}"
#---~---


#---~---
#     In case this is a user-defined site simulation, set the user-specified paths.
# DATM_MODE must be set to CLM1PT, even when running E3SM-FATES.
#---~---
case "${RESOL}" in
ELM_USRDAT|CLM_USRDAT)
   ./xmlchange --id ATM_DOMAIN_PATH           --val "${SITE_PATH}"
   ./xmlchange --id LND_DOMAIN_PATH           --val "${SITE_PATH}"
   ./xmlchange --id ATM_DOMAIN_FILE           --val "${HLM_USRDAT_DOMAIN}"
   ./xmlchange --id LND_DOMAIN_FILE           --val "${HLM_USRDAT_DOMAIN}"
   ./xmlchange --id DATM_MODE                 --val "CLM1PT"
   ./xmlchange --id CALENDAR                  --val "${METD_CALENDAR}"
   ./xmlchange --id ${V_HLM_USRDAT_NAME}      --val "${SITE_NAME}"
   ./xmlchange --id DIN_LOC_ROOT_CLMFORC      --val "${SITE_BASE_PATH}"
   ;;
esac
#---~---


#---~---
#     Set the PE layout for a single-site run (unlikely that users would change this).
#---~---
./xmlchange --id NTASKS_ATM --val 1
./xmlchange --id NTASKS_CPL --val 1
./xmlchange --id NTASKS_GLC --val 1
./xmlchange --id NTASKS_OCN --val 1
./xmlchange --id NTASKS_WAV --val 1
./xmlchange --id NTASKS_ICE --val 1
./xmlchange --id NTASKS_LND --val 1
./xmlchange --id NTASKS_ROF --val 1
./xmlchange --id NTASKS_ESP --val 1
./xmlchange --id ROOTPE_ATM --val 0
./xmlchange --id ROOTPE_CPL --val 0
./xmlchange --id ROOTPE_GLC --val 0
./xmlchange --id ROOTPE_OCN --val 0
./xmlchange --id ROOTPE_WAV --val 0
./xmlchange --id ROOTPE_ICE --val 0
./xmlchange --id ROOTPE_LND --val 0
./xmlchange --id ROOTPE_ROF --val 0
./xmlchange --id ROOTPE_ESP --val 0
./xmlchange --id NTHRDS_ATM --val 1
./xmlchange --id NTHRDS_CPL --val 1
./xmlchange --id NTHRDS_GLC --val 1
./xmlchange --id NTHRDS_OCN --val 1
./xmlchange --id NTHRDS_WAV --val 1
./xmlchange --id NTHRDS_ICE --val 1
./xmlchange --id NTHRDS_LND --val 1
./xmlchange --id NTHRDS_ROF --val 1
./xmlchange --id NTHRDS_ESP --val 1
#---~---



#---~---
#     Change XML configurations if needed.
#---~---
if [[ ${#xml_settings[*]} -gt 0 ]]
then
   #--- Loop through the options to update.
   echo " + Update XML settings."
   for x in ${!xml_settings[*]}
   do
      #--- Retrieve settings.
      xml_id=$(echo ${xml_settings[x]}  | awk '{print $1}')
      xml_id=$(echo ${xml_id}           | sed s@"XXX"@${HLM}@g | sed s@"xxx"@${hlm}@g)
      xml_val=$(echo ${xml_settings[x]} | awk '{print $2}')
      echo " ID = ${xml_id}; VAL = ${xml_val}"
      #---~---

      #--- Update settings.
      ./xmlchange --id ${xml_id} --val "${xml_val}"
      #---~---

   done
   #---~---
else
   #--- No changes needed.
   echo " + No XML changes required."
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
#     This part modifies the DATM namelist. Do NOT change this unless you know exactly
# what you are doing.
#---~---
USER_NL_DATM="${CASE_PATH}/user_nl_datm"
touch ${USER_NL_DATM}
echo "taxmode = 'cycle','cycle','cycle'" >> ${USER_NL_DATM}
#---~---


#--- Start setting up the case
./case.setup
./preview_namelists
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
ELM_USRDAT|CLM_USRDAT)
   #--- Define files with meteorological driver settings.
   HLM_USRDAT_ORIG="${SIMUL_PATH}/run/datm.streams.txt.CLM1PT.${RESOL}"
   HLM_USRDAT_USER="${SIMUL_PATH}/user_datm.streams.txt.CLM1PT.${RESOL}"
   #---~---


   ANY_METD_NC=$(/bin/ls -1 ${SITE_PATH}/${DATM_MODE}/????-??.nc 2> /dev/null | wc -l)
   if [[ ${ANY_METD_NC} ]]
   then
      #--- Load one netCDF file.
      METD_NC_1ST=$(/bin/ls -1 ${SITE_PATH}/${DATM_MODE}/????-??.nc 2> /dev/null | head -1)
      ANY_FLDS=$(ncdump -h ${METD_NC_1ST} 2> /dev/null | grep FLDS | wc -l)
      if [[ ${ANY_FLDS} -eq 0 ]]
      then
         #--- Incoming long wave radiation is absent.  Modify the stream file
         /bin/cp ${HLM_USRDAT_ORIG} ${HLM_USRDAT_USER}
         $(sed -i '@FLDS@d' h)
         #---~---
      fi
      #---~---
   else
      #--- Report error.
      echo " Meteorological drivers not found in ${SITE_PATH}/${DATM_MODE}".
      echo " Make sure all the met driver files are named as yyyy-mm.nc"
      exit 91
      #---~---
   fi
   #---~---
   ;;
esac
#---~---



#---~---
#    Append the surface data information to the namelist, in case we are using 
#---~---
case "${RESOL}" in
ELM_USRDAT|CLM_USRDAT)
   # Append surface data file to the namelist.
   HLM_SURDAT_FILE="${SITE_PATH}/${HLM_USRDAT_SURDAT}"
   echo "fsurdat = '${HLM_SURDAT_FILE}'" >> ${USER_NL_HLM}
   ;;
esac
#---~---



#---~---
#     Include settings for the inventory initialisation.
#---~---
if [[ "${INVENTORY_BASE}" != "" ]]
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
#     Change PFT parameters if needed.
#---~---
if [[ ${#pft_settings[*]} -gt 0 ]]
then
   #--- Create a local parameter file.
   echo " + Create local parameter file."
   FATES_PARAMS_ORIG="${FATES_SRC_PATH}/parameter_files/fates_params_default.cdl"
   FATES_PARAMS_CASE="${CASE_PATH}/fates_params_${CASE_NAME}.nc"
   ncgen -o ${FATES_PARAMS_CASE} ${FATES_PARAMS_ORIG}
   #---~---


   #--- Set python script for updating parameters.
   MODIFY_PARAMS_PY="${FATES_SRC_PATH}/tools/modify_fates_paramfile.py"
   #---~---


   #--- Loop through the parameters to update.
   echo " + Create local parameter file."
   for p in ${!pft_settings[*]}
   do
      #--- Retrieve settings.
      pft_var=$(echo ${pft_settings[p]} | awk '{print $1}')
      pft_num=$(echo ${pft_settings[p]} | awk '{print $2}')
      pft_val=$(echo ${pft_settings[p]} | awk '{print $3}')
      #---~---

      #--- Update parameters.
      ${MODIFY_PARAMS_PY} --var ${pft_var} --pft ${pft_num} --val ${pft_val}               \
         --fin ${FATES_PARAMS_CASE} --fout ${FATES_PARAMS_CASE} --O
      #---~---
   done
   #---~---


   #--- Instruct the host land model to use the modified parameter set. 
   touch ${USER_NL_HLM}
   echo "fates_paramfile = '${FATES_PARAMS_CASE}'" >> ${USER_NL_HLM}
   #---~---
else
   #--- No changes needed.
   echo " + No PFT parameter settings required."
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
      hlm_id=$(echo ${hlm_id}           | sed s@"XXX"@${HLM}@g | sed s@"xxx"@${hlm}@g)
      hlm_val=$(echo ${hlm_settings[h]} | awk '{for(i=2;i<=NF;++i)printf $i""FS ; print ""}')
      echo " ID = ${hlm_id}; VAL = ${hlm_val}"
      #---~---

      touch ${USER_NL_HLM}
      echo "${hlm_id} = ${hlm_val}" >> ${USER_NL_HLM}

   done
else
   #--- No changes needed.
   echo " + No PFT parameter settings required."
   #---~---
fi
#---~---

#--- Build case.
./case.build --clean
./case.build
#---~---

#--- Return to the original path.
cd ${HERE_PATH}
#---~---

