C $Header: /u/gcmpack/MITgcm/model/inc/SURFACE.h,v 1.3 2001/08/27 18:41:44 jmc Exp $
C $Name:  $
C
C     /==========================================================\
C     | SURFACE.h                                                |
C     | o Header file defining surface-related model varaibles   |
C     |==========================================================|
C     | Contains variables relative to the surface position      |
C     | that are held fixed in linear free-surface formulation   |
C     | but can vary with time with a non-linear free-surface.   |
C     \==========================================================/

C--   COMMON /SOLVE_BAROT/  Barotropic variables common block
C     Bo_surf  -Boyancy|1/rho [ocean|atmos] at surface level [= g | alpha(p_o)]
C     recip_Bo     = 1/Bo_surf
      COMMON /SOLVE_BAROT/ Bo_surf, recip_Bo
      _RL  Bo_surf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  recip_Bo(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C--   COMMON /SURF_INDEX/ Common block for surface related index
C     ksurfC - vertical index of the surface tracer cell
C     ksurfW - vertical index of the surface U point 
C     ksurfS - vertical index of the surface V point 
C IMPORTANT:  ksurfC,W,S = Nr+1  where the fluid column is empty (continent)
      COMMON /SURF_INDEX/ ksurfC, ksurfW, ksurfS
      INTEGER ksurfC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER ksurfW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER ksurfS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef NONLIN_FRSURF
C--   COMMON /SURF_CHANGE/ transient varariable used for Non-Lin Free-Surf
C     hFac_surfC - New thickness factor of the surface level
C                        center (Tracer point)
C     hFac_surfW - idem, West  interface (U point)
C     hFac_surfS - idem, South interface (V point)
      COMMON /SURF_CHANGE/
     &     hFac_surfC, hFac_surfW, hFac_surfS
      _RS  hFac_surfC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  hFac_surfW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  hFac_surfS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* NONLIN_FRSURF */
