C $Header: /u/gcmpack/MITgcm/pkg/gmredi/Attic/GMREDI_DIAGS.h,v 1.1 2000/06/21 19:42:54 adcroft Exp $

#ifdef ALLOW_GMREDI

C     /==========================================================\
C     | GM_DIAGS.h                                               |
C     | o Header for GM?Redi diagnostic output                   |
C     \==========================================================/

#ifdef INCLUDE_DIAGNOSTICS_INTERFACE_CODE

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

#endif /* INCLUDE_DIAGNOSTICS_INTERFACE_CODE */

#endif /* ALLOW_GMREDI */
