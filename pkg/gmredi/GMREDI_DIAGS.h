C $Header: /u/gcmpack/MITgcm/pkg/gmredi/Attic/GMREDI_DIAGS.h,v 1.4 2001/12/16 18:54:49 jmc Exp $
C $Name:  $

#ifdef ALLOW_GMREDI

C     /==========================================================\
C     | GM_DIAGS.h                                               |
C     | o Header for GM?Redi diagnostic output                   |
C     \==========================================================/

#ifdef ALLOW_TIMEAVE

C     Keep track of time
      _RL GM_TimeAve(Nr,nSx,nSy)
      COMMON /GM_TAVE/ GM_TimeAve


C     Storage arrays for time-averages
      _RL GM_Kwx_T    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GM_Kwy_T    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GM_Kwz_T    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_TAVE_ARRAYS/
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
