C $Header: /u/gcmpack/MITgcm/pkg/seaice/Attic/SEAICE_DIAGS.h,v 1.7 2009/06/24 09:06:11 mlosch Exp $
C $Name:  $

C     /==========================================================\
C     | SEAICE_DIAGS.h                                           |
C     | o Header for SEAICE diagnostic output                    |
C     \==========================================================/

#ifdef ALLOW_TIMEAVE

C     Keep track of time
      _RL SEAICE_TimeAve(Nr,nSx,nSy)
      COMMON /SEAICE_TAVE/ SEAICE_TimeAve

C     Storage arrays for time-averages
      _RL FUtave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL FVtave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL EmPmRtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL QNETtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL QSWtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL UICEtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VICEtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL HEFFtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL AREAtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /SEAICE_TAVE_DIAGS/
     &        FUtave, FVtave, EmPmRtave, QNETtave, QSWtave,
     &        UICEtave, VICEtave, HEFFtave, AREAtave

#endif /* ALLOW_TIMEAVE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
