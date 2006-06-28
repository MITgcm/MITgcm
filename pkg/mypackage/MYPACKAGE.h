C $Header: /u/gcmpack/MITgcm/pkg/mypackage/MYPACKAGE.h,v 1.1 2006/06/28 21:26:20 heimbach Exp $
C $Name:  $

#ifdef ALLOW_MYPACKAGE

C     Package flag
      logical MYPAisON
      logical MYPA_MNC
      logical MYPA_MDSIO
      COMMON /MYPA_PACKAGE/ MYPAisON, 
     &                     MYPA_MNC, MYPA_MDSIO

C     MYPA parameters
      LOGICAL MYPA_doSwitch1
      LOGICAL MYPA_doSwitch2
      INTEGER MYPA_index1
      INTEGER MYPA_index2
      _RL MYPA_param1
      _RL MYPA_param2
      CHARACTER*(MAX_LEN_FNAM) MYPA_string1
      CHARACTER*(MAX_LEN_FNAM) MYPA_string2
      CHARACTER*(MAX_LEN_FNAM) mypaStatScal1File
      CHARACTER*(MAX_LEN_FNAM) mypaStatScal2File
      CHARACTER*(MAX_LEN_FNAM) mypaStatUvelFile
      CHARACTER*(MAX_LEN_FNAM) mypaStatVvelFile
      CHARACTER*(MAX_LEN_FNAM) mypaSurf1File
      CHARACTER*(MAX_LEN_FNAM) mypaSurf2File

      COMMON /MYPA_PARAMS_L/ MYPA_doSwitch1, MYPA_doSwitch2
      COMMON /MYPA_PARAMS_I/ MYPA_index1, MYPA_index2
      COMMON /MYPA_PARAMS_R/ MYPA_param1, MYPA_param2
      COMMON /MYPA_PARAMS_C/ MYPA_string1, MYPA_string2,
     &       mypaStatScal1File, mypaStatScal2File,
     &       mypaStatUvelFile, mypaStatVvelFile,
     &       mypaSurf1File, mypaSurf2File


C     MYPA 3-dim. fields
      _RL mypaStatScal1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL mypaStatScal2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL mypaStatUvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL mypaStatVvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL mypaGScal1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL mypaGScal2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL mypaGUvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL mypaGVvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /MYPA_real_3d/
     &    mypaStatScal1, mypaStatScal2, 
     &    mypaStatUvel, mypaStatVvel,
     &    mypaGScal1, mypaGScal2, 
     &    mypaGUvel, mypaGVvel

C     MYPA 2-dim. fields
      _RL mypaSurf1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mypaSurf2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /MYPA_real_2d/ mypaSurf1, mypaSurf2



#endif /* ALLOW_MYPACKAGE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
