C $Header: /u/gcmpack/MITgcm/pkg/seaice/SEAICE_TAVE.h,v 1.1 2010/01/03 00:27:36 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: SEAICE_TAVE.h
C     !INTERFACE:
C     include "SEAICE_TAVE.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | SEAICE_TAVE.h
C     | o Header for time-average SEAICE variables
C     *==========================================================*
C     \ev
CEOP

#ifdef ALLOW_TIMEAVE

C--   COMMON /SEAICE_TAVE_VARS/ time-averaged variables
C     SEAICE_timeAve :: cumulated time [s]
      COMMON /SEAICE_TAVE_VARS/
     &        SEAICE_timeAve,
     &        FUtave, FVtave, EmPmRtave, QNETtave, QSWtave,
     &        UICEtave, VICEtave, HEFFtave, AREAtave

      _RL SEAICE_timeAve(nSx,nSy)
      _RL FUtave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL FVtave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL EmPmRtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL QNETtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL QSWtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL UICEtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VICEtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL HEFFtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL AREAtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif /* ALLOW_TIMEAVE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
