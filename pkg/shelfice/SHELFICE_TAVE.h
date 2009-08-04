C $Header: /u/gcmpack/MITgcm/pkg/shelfice/SHELFICE_TAVE.h,v 1.3 2009/08/04 16:43:52 jmc Exp $
C $Name:  $

C     *==========================================================*
C     | SHELFICE_TAVE.h
C     | o Header for SHELFICE time-average output
C     *==========================================================*

#ifdef ALLOW_TIMEAVE

C     Keep track of time
      _RL SHELFICE_TimeAve(Nr,nSx,nSy)
      COMMON /SHELFICE_TAVE/ SHELFICE_TimeAve

C     Storage arrays for time-averages
      _RL shelfice_heatFluxtave
     &     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL shelfice_frshFluxtave
     &     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      COMMON /SHELFICE_TAVE_DIAGS/
     &     shelfice_heatFluxtave,
     &     shelfice_frshFluxtave

#endif /* ALLOW_TIMEAVE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
