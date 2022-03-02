CBOP
C     !ROUTINE: MYPACKAGE.h
C     !INTERFACE:
C     #include "MYPACKAGE.h"

C     !DESCRIPTION:
C     *================================================================*
C     | MYPACKAGE.h
C     | o Header file defining "mypackage" parameters and variables
C     *================================================================*
CEOP

C     Package flag
      LOGICAL myPa_MNC
      LOGICAL myPa_MDSIO
      COMMON /MYPA_PACKAGE/
     &                     myPa_MNC, myPa_MDSIO

C     MYPA parameters
      LOGICAL myPa_StaV_Cgrid
      LOGICAL myPa_Tend_Cgrid
      LOGICAL myPa_applyTendT
      LOGICAL myPa_applyTendS
      LOGICAL myPa_applyTendU
      LOGICAL myPa_applyTendV

C-    additional parameters:
      LOGICAL myPa_doSwitch1
      LOGICAL myPa_doSwitch2
      INTEGER myPa_index1
      INTEGER myPa_index2
      _RL myPa_param1
      _RL myPa_param2
      CHARACTER*(MAX_LEN_FNAM) myPa_string1
      CHARACTER*(MAX_LEN_FNAM) myPa_string2

C-    file names for initial conditions:
      CHARACTER*(MAX_LEN_FNAM) myPa_Scal1File
      CHARACTER*(MAX_LEN_FNAM) myPa_Scal2File
      CHARACTER*(MAX_LEN_FNAM) myPa_VelUFile
      CHARACTER*(MAX_LEN_FNAM) myPa_VelVFile
      CHARACTER*(MAX_LEN_FNAM) myPa_Surf1File
      CHARACTER*(MAX_LEN_FNAM) myPa_Surf2File

      COMMON /MYPA_PARAMS_L/
     &       myPa_StaV_Cgrid, myPa_Tend_Cgrid,
     &       myPa_applyTendT, myPa_applyTendS,
     &       myPa_applyTendU, myPa_applyTendV,
     &       myPa_doSwitch1, myPa_doSwitch2
      COMMON /MYPA_PARAMS_I/ myPa_index1, myPa_index2
      COMMON /MYPA_PARAMS_R/ myPa_param1, myPa_param2
      COMMON /MYPA_PARAMS_C/ myPa_string1, myPa_string2,
     &       myPa_Scal1File, myPa_Scal2File,
     &       myPa_VelUFile,  myPa_VelVFile,
     &       myPa_Surf1File, myPa_Surf2File

#ifdef MYPACKAGE_3D_STATE
C     MYPA 3-dim. fields
      _RL myPa_StatScal1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_StatScal2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_StatVelU (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_StatVelV (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /MYPA_STATE_3D/
     &    myPa_StatScal1, myPa_StatScal2,
     &    myPa_StatVelU,  myPa_StatVelV
#endif /* MYPACKAGE_3D_STATE */
#ifdef MYPACKAGE_2D_STATE
C     MYPA 2-dim. fields
      _RL myPa_Surf1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myPa_Surf2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /MYPA_STATE_2D/
     &    myPa_Surf1, myPa_Surf2
#endif /* MYPACKAGE_2D_STATE */

#ifdef MYPACKAGE_TENDENCY
      _RL myPa_TendScal1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_TendScal2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_TendVelU (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myPa_TendVelV (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /MYPA_TENDENCY/
     &    myPa_TendScal1, mypa_TendScal2,
     &    myPa_TendVelU,  mypa_TendVelV
#endif /* MYPACKAGE_TENDENCY */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
