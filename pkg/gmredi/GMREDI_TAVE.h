C $Header: /u/gcmpack/MITgcm/pkg/gmredi/GMREDI_TAVE.h,v 1.3 2010/01/02 23:10:47 jmc Exp $
C $Name:  $

#ifdef ALLOW_GMREDI

C     *==========================================================*
C     | GMREDI_TAVE.h
C     | o Header for GM-Redi time-average output
C     *==========================================================*

#ifdef ALLOW_TIMEAVE


C     Storage arrays for time-averages
      _RL GM_timeAve  (nSx,nSy)
      _RL GM_Kwx_T    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GM_Kwy_T    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GM_Kwz_T    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_TAVE_VARS/
     &                       GM_timeAve,
     &                       GM_Kwx_T,
     &                       GM_Kwy_T,
     &                       GM_Kwz_T

#ifdef GM_VISBECK_VARIABLE_K
      _RL Visbeck_K_T (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /VIS_TAVE/
     &                       Visbeck_K_T
#endif

#ifdef GM_BOLUS_ADVEC
      _RL GM_PsiXtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GM_PsiYtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_TAVE_PSI_B/ GM_PsiXtave, GM_PsiYtave
#endif

#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_GMREDI */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
