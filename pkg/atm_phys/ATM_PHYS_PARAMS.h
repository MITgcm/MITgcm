C $Header: /u/gcmpack/MITgcm/pkg/atm_phys/ATM_PHYS_PARAMS.h,v 1.1 2013/05/08 22:14:14 jmc Exp $
C $Name:  $

#ifdef ALLOW_ATM_PHYS

C     Package flag
c     LOGICAL myPa_MNC
c     LOGICAL myPa_MDSIO
c     COMMON /MYPA_PACKAGE/
c    &                     myPa_MNC, myPa_MDSIO

C     MYPA parameters
c     LOGICAL myPa_StaV_Cgrid
c     LOGICAL myPa_Tend_Cgrid
      LOGICAL atmPhys_addTendT
      LOGICAL atmPhys_addTendS
      LOGICAL atmPhys_addTendU
      LOGICAL atmPhys_addTendV

C-    additional parameters:
      LOGICAL atmPhys_stepSST
c     LOGICAL myPa_doSwitch2
c     INTEGER myPa_index1
c     INTEGER myPa_index2
c     _RL myPa_param1
c     _RL myPa_param2
c     CHARACTER*(MAX_LEN_FNAM) myPa_string1
c     CHARACTER*(MAX_LEN_FNAM) myPa_string2

C-    file names for initial conditions:
c     CHARACTER*(MAX_LEN_FNAM) myPa_Scal1File
c     CHARACTER*(MAX_LEN_FNAM) myPa_Scal2File
c     CHARACTER*(MAX_LEN_FNAM) myPa_VelUFile
c     CHARACTER*(MAX_LEN_FNAM) myPa_VelVFile

      CHARACTER*(MAX_LEN_FNAM) atmPhys_SSTFile
      CHARACTER*(MAX_LEN_FNAM) atmPhys_QflxFile

      COMMON /MYPA_PARAMS_L/
c    &       myPa_StaV_Cgrid, myPa_Tend_Cgrid, myPa_doSwitch2,
     &       atmPhys_addTendT, atmPhys_addTendS,
     &       atmPhys_addTendU, atmPhys_addTendV,
     &       atmPhys_stepSST
c     COMMON /MYPA_PARAMS_I/ myPa_index1, myPa_index2
c     COMMON /MYPA_PARAMS_R/ myPa_param1, myPa_param2
      COMMON /MYPA_PARAMS_C/ 
c    &       myPa_string1, myPa_string2,
c    &       myPa_Scal1File, myPa_Scal2File,
c    &       myPa_VelUFile,  myPa_VelVFile,
     &       atmPhys_SSTFile, atmPhys_QflxFile

C-- from driver-atmosphere module:
      logical  module_is_initialized
      logical  turb
      logical  ldry_convection
      logical  do_virtual
      logical  lwet_convection
      logical  two_stream
      logical  mixed_layer_bc
      COMMON /ATMOS_DRIVER_L/
     &   module_is_initialized,
     &   turb, ldry_convection, do_virtual,
     &   lwet_convection, two_stream, mixed_layer_bc

      _RL  roughness_heat
      _RL  roughness_moist
      _RL  roughness_mom
      COMMON /ATMOS_DRIVER_RL/
     &   roughness_heat, roughness_moist, roughness_mom

#endif /* ALLOW_ATM_PHYS */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
