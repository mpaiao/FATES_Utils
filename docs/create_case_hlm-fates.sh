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
export HESM="CTSM"              # Host "Earth System Model". (E3SM, CESM, CTSM)
export PROJECT=ngeet            # Project (may not be needed)
export MACH="eschweilera"       # Machine used for preparing the case
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
# CASE_ROOT: The main path where to create the directory for this case.
# SIMUL_ROOT: The main path where to create the directory for the simulation output.
#
# In all cases, use XXXX in the part you want to be replaced with either E3SM or CTSM.
#---~---
export WORK_PATH="${HOME}/Models/XXXX/cime/scripts"
export CASE_ROOT="${HOME}/Documents/LocalData/FATES/Cases"
export SIMUL_ROOT="${HOME}/Documents/LocalData/FATES/Simulations"
#---~---


#---~---
#   Main case settings.  These variables will control compilation settings and the 
# case name for this simulation.  It is fine to leave these blank, in which case the
# script will use default settings.
#---~---
export COMP=""
export CASE_NAME="D0005_ParacouTest"
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
export SITE_BASE_PATH="${HOME}/Data/FATES_DataSets"
# Sub-directory with data sets specific to this site.
export SITE_NAME="1x1pt-paracouGUF_v1.5_c20210713"
# Domain file (it must be in the SITE_NAME sub-directory).
export HLM_USRDAT_DOMAIN="domain.lnd.${SITE_NAME}_navy.nc"
# Surface data file (it must be in the SITE_NAME sub-directory).
export HLM_USRDAT_SURDAT="surfdata_${SITE_NAME}.nc"
# Calendar type for the meteorological drivers ('NO_LEAP' or 'GREGORIAN')
export METD_CALENDAR="GREGORIAN"
# CDL file containing FATES parameters (it must be in the SITE_NAME sub-directory).
#---~---


#---~---
#    Provide the parameter file, in case a site-specific file exists.  This file must
# be in the SITE_NAME sub-directory.  In case no site-specific parameter file is provided
# (i.e. FATES_PARAMS_BASE=""), the case will use the default parameters, but beware that
# results may be very bad.
#---~---
export FATES_PARAMS_BASE="fates_params_1trop_opt224_vm6_hivmphos.c210529.cdl"
#---~---


#---~---
#    In case the inventory/lidar initialisation is sought, provide the file name of the
# control file specification (the control file should be in the SITE_NAME sub-directory). 
# Otherwise, do not set this variable (INVENTORY_BASE="")
#
#  For additional information, check
# https://github.com/NGEET/fates/wiki/Model-Initialization-Modes#Inventory_Format_Type_1
#---~---
export INVENTORY_BASE="${SITE_NAME}_nounder_info.txt"
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
# xml_settings=("DOUT_S_SAVE_INTERIM_RESTART_FILES TRUE"
#               "DOUT_S                            TRUE"
#               "STOP_N                            10"
#               "XXX_FORCE_COLDSTART               on"
#               "RUN_STARTDATE                     '2001-01-01'")
#---~---
xml_settings=("DEBUG                             FALSE"
              "RUN_STARTDATE                     2004-01-01"
              "STOP_N                            16"
              "STOP_OPTION                       nyears"
              "REST_N                            1"
              "YYY_FORCE_COLDSTART               on"
              "DATM_CLMNCEP_YR_START             2004"
              "DATM_CLMNCEP_YR_END               2019")
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
pft_settings=()
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
#               "hist_fincl1        'GPP_BY_AGE', 'PATCH_AREA_BY_AGE', 'BA_SCLS',\
#                                   'FSH','EFLX_LH_TOT'")
#
# Notes:
# 1. For variable names, you can use hlm or HLM as a wildcard for the host land model. 
# 2. Note that variables in CLM and ELM are not always the same. The script will always
#    build the case, but runs will fail if the list of output variables includes any
#    variable that is not recognised by the host model.
#---~---
if ${USE_FATES}
then
   hlm_settings=("hist_empty_htapes .true."
                 "hist_fincl1       'AGB','AGB_SCPF','AR','EFLX_LH_TOT',\
                                    'FSH','ELAI','FIRE','FLDS','FSDS','FSR','GPP','HR',\
                                    'NEP','GPP_BY_AGE','PATCH_AREA_BY_AGE','BA_SCPF',\
                                    'CANOPY_AREA_BY_AGE','NPLANT_CANOPY_SCPF',\
                                    'NPLANT_UNDERSTORY_SCPF','DDBH_CANOPY_SCPF',\
                                    'DDBH_UNDERSTORY_SCPF','MORTALITY_CANOPY_SCPF',\
                                    'MORTALITY_UNDERSTORY_SCPF','GPP_SCPF','AR_SCPF',\
                                    'NPP_SCPF','ELAI','ESAI','TLAI','TSAI','LAI_BY_AGE',\
                                    'LAI_CANOPY_SCLS','LAI_UNDERSTORY_SCLS',\
                                    'PATCH_AREA_BY_AGE','DEMOTION_RATE_SCLS',\
                                    'PROMOTION_RATE_SCLS','M1_SCPF','M2_SCPF','M3_SCPF',\
                                    'M4_SCPF','M5_SCPF','M6_SCPF','M7_SCPF','M8_SCPF',\
                                    'M9_SCPF','M10_SCPF','CARBON_BALANCE_CANOPY_SCLS',\
                                    'CARBON_BALANCE_UNDERSTORY_SCLS',\
                                    'TRIMMING_CANOPY_SCLS','TRIMMING_UNDERSTORY_SCLS',\
                                    'TBOT','QBOT','PBOT','QVEGE','QVEGT','QSOIL',\
                                    'FSH_V','FSH_G','FGR','BTRAN','ZWT','ZWT_PERCH','SMP',\
                                    'TSA','TV','TREFMNAV','TREFMXAV','TG','TAF','QAF',\
                                    'UAF','Q2M','RAIN','QDIRECT_THROUGHFALL','QDRIP',\
                                    'QOVER','QDRAI','QINTR','USTAR','U10'")
else
   hlm_settings=("hist_empty_htapes .true."
                 "hist_fincl1       'AR','EFLX_LH_TOT','FSH','ELAI','FIRE','FLDS',\
                                    'FSDS','FSR','GPP','HR','NEP','ELAI','ESAI','TLAI',\
                                    'TSAI','TBOT','QBOT','PBOT','QVEGE','QVEGT','QSOIL',\
                                    'FSH_V','FSH_G','FGR','BTRAN','Qtau','ZWT','ZWT_PERCH',\
                                    'SMP','TSA','TV','TREFMNAV','TREFMXAV','TG','Q2M',\
                                    'RAIN','QDIRECT_THROUGHFALL','QDRIP','QOVER',\
                                    'QDRAI','QINTR','USTAR','U10'")
fi
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

#--- Current path.
export HERE_PATH=$(pwd)
#---~---

#--- Site path.
export SITE_PATH="${SITE_BASE_PATH}/${SITE_NAME}"
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

   #--- Main path for FATES
   export FATESMODEL_PATH="${HOSTMODEL_PATH}/components/elm/src/external_models/fates"
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

   #--- Main path for FATES
   export FATESMODEL_PATH="${HOSTMODEL_PATH}/src/fates"
   #---~---

   #--- Additional options for "create_newcase"
   export NEWCASE_OPTS="--run-unsupported"
   #---~---

   #--- Main source path for FATES.
   export FATES_SRC_PATH="${HOSTMODEL_PATH}/src/fates"
   #---~---


   #--- In case compilates settings is not defined, use the default settings.
   if ${USE_FATES} && [[ "${COMP}" == "" ]]
   then
      export COMP="I2000Clm51Fates"
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
export FATES_HASH="FATES-$(cd ${FATESMODEL_PATH}; git log -n 1 --pretty=%h)"
#---~---


#--- Define setting for single-point
export V_HLM_USRDAT_NAME="${HLM}_USRDAT_NAME"
#---~---


#--- Substitute wildcards in the resolution with the actual model
export RESOL=$(echo "${RESOL}" | sed s@"YYY"@"${HLM}"@g | sed s@"yyy"@"${hlm}"@g)
#---~---


#---~---
#   Set default case name prefix in case none was provided.
#---~---
if [[ "${CASE_NAME}" == "" ]]
then
   export CASE_NAME="SX_${COMP}_${MACH}_${TODAY}"
fi
#---~---


#---~---
#   Append github commit hash, or a simple host-model / FATES tag.
#---~---
if ${USE_FATES} && ${APPEND_GIT_HASH}
then
   export CASE_NAME="${CASE_NAME}_${HLM_HASH}_${FATES_HASH}"
elif ${APPEND_GIT_HASH}
then
   export CASE_NAME="${CASE_NAME}_${HLM_HASH}_BigLeaf"
elif ${USE_FATES}
then
   export CASE_NAME="${CASE_NAME}_${HLM}_FATES"
else
   export CASE_NAME="${CASE_NAME}_${HLM}_BigLeaf"
fi
#---~---


#---~---
#    Set paths for case and simulation.
#---~---
export CASE_PATH="${CASE_ROOT}/${CASE_NAME}"
export SIMUL_PATH="${SIMUL_ROOT}/${CASE_NAME}"
#---~---



#--- Namelist for the host land model.
export USER_NL_HLM="${CASE_PATH}/user_nl_${hlm}"
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
./xmlchange CIME_OUTPUT_ROOT="${SIMUL_ROOT}"
./xmlchange DOUT_S_ROOT="${SIMUL_PATH}"
./xmlchange PIO_DEBUG_LEVEL="${DEBUG_LEVEL}"
#---~---


#---~---
#     In case this is a user-defined site simulation, set the user-specified paths.
# DATM_MODE must be set to CLM1PT, even when running E3SM-FATES.
#---~---
case "${RESOL}" in
?LM_USRDAT)
   ./xmlchange DATM_MODE="CLM1PT"
   ./xmlchange CALENDAR="${METD_CALENDAR}"
   ./xmlchange ${V_HLM_USRDAT_NAME}="${SITE_NAME}"
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
?LM_USRDAT)
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
         $(sed -i '@FLDS@d' ${HLM_USRDAT_USER})
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
?LM_USRDAT)
   # Append surface data file to the namelist.
   HLM_SURDAT_FILE="${SITE_PATH}/${HLM_USRDAT_SURDAT}"
   echo "fsurdat = '${HLM_SURDAT_FILE}'" >> ${USER_NL_HLM}
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
#     Change PFT parameters if needed.
#---~---
if ${USE_FATES} && [[ ${#pft_settings[*]} -gt 0 ]]
then

   #--- Identify original parameter file
   if [[ "${FATES_PARAMS_BASE}" == "" ]]
   then
      FATES_PARAMS_ORIG="${FATES_SRC_PATH}/parameter_files/fates_params_default.cdl"
   else
      FATES_PARAMS_ORIG="${SITE_PATH}/${FATES_PARAMS_BASE}"
   fi
   #---~---


   #--- Create a local parameter file.
   echo " + Create local parameter file from $(basename ${FATES_PARAMS_ORIG})."
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
         echo " ID = ${hlm_id}; VAL = ${hlm_val}"
         touch ${USER_NL_HLM}
         echo "${hlm_id} = ${hlm_val}" >> ${USER_NL_HLM}
         #---~---
      else
         #--- Do not update.  Instead, warn the user.
         echo " Ignoring ${hlm_id} as this a FATES variable, and USE_FATES=false."
         #---~---
      fi
      #---~---
   done
   #---~---
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

