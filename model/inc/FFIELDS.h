C $Header: /u/gcmpack/MITgcm/model/inc/FFIELDS.h,v 1.8 2000/11/13 16:32:57 heimbach Exp $
C
C     /==========================================================\
C     | FFIELDS.h                                                |
C     | o Model forcing fields                                   |
C     |==========================================================|
C     | The arrays here will need changing and customising for a |
C     | particular experiment.                                   |
C     \==========================================================/
C
C--   For a classical "gyre" type experiment just one term is needed.
C
C     fu     - Zonal velocity tendency term
C                -> read assumes     N/m^2 (>0 from East to West)
C                -> transformed to   m/s^2
C                   via              fu   ->   -fu/(rhoNil*dR)
C
C     fv     - Meridional velocity tendency term
C                -> read assumes     N/m^2 (>0 from North to South))
C                -> transformed to   m/s^2
C                   via              fv   ->   -fv/(rhoNil*dR)
C
C     EmPmR  - Evaporation - Precipitation - Runoff
C                -> read assumes     m/s (>0 for ocean salting)
C                -> transformed to   psu/s
C                   via              empmr -> empmr*35./dR
C
C     Qnet   - Surface heat flux
C                -> read assumes     W/m^2=kg/s^3 (>0 for ocean cooling)
C                -> transformed to   K/s
C                   via              Qnet -> -Qnet/(rhonil*Cp*dR)
C
C     Qsw    - Short-wave surface heat flux
C                -> read assumes     W/m^2=kg/s^3 (>0 for ocean cooling)
C                -> transformed to   K/s
C                   via              Qsw -> -Qsw/(rhonil*Cp*dR)
C
C     SST    - Sea surface temperature (degrees) for relaxation
C     SSS    - Sea surface salinity (psu) for relaxation

      COMMON /FFIELDS/
     &                 fu,
     &                 fv,
     &                 Qnet,
     &                 EmPmR,
     &                 SST,
     &                 SSS,
     &                 Qsw
      _RS  fu       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  fv       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qnet     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  EmPmR    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SST      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qsw      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     surfaceTendencyU
C                -> usage in gU:     gU = gU + surfaceTendencyU[m/s^2]
C
C     surfaceTendencyV
C                -> usage in gV:     gV = gV + surfaceTendencyV[m/s^2]
C
C     surfaceTendencyS   
C            - EmPmR plus salinity relaxation term
C                -> calculate        -lambda*(S(model)-S(clim))
C                -> usage in gS:     gS = gS + surfaceTendencyS[psu/s]
C
C     surfaceTendencyT
C            - Qnet plus temp. relaxation
C                -> calculate        -lambda*(T(model)-T(clim))
C            >>> Qnet assumed to be total flux minus s/w rad. <<<
C                -> usage in gT:     gT = gT + surfaceTendencyT[K/s]
C
      COMMON /TENDENCY_FORCING/
     &                         surfaceTendencyU,
     &                         surfaceTendencyV,
     &                         surfaceTendencyT,
     &                         surfaceTendencyS, 
     &                         tempQsw
      _RS  surfaceTendencyU  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  surfaceTendencyV  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  surfaceTendencyT  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  surfaceTendencyS  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  tempQsw           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
