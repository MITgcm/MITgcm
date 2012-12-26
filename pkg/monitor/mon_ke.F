C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_ke.F,v 1.23 2012/12/26 23:57:46 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_KE

C     !INTERFACE:
      SUBROUTINE MON_KE(
     I     myIter, myThid )

C     !DESCRIPTION:
C     Calculates stats for Kinetic Energy, (barotropic) Potential Energy
C                      and total Angular Momentum

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "MONITOR.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT PARAMETERS:
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi, bj
      INTEGER i,j,k
      INTEGER ks, kp1
      _RL numPnts,theVol,tmpVal, mskp1, msk_1
      _RL weight0, weight1
      _RL theMax,theMean,theVolMean,potEnMean
      _RL uBarC, vBarC, totAMu, totAMs
      _RL tileMean(nSx,nSy)
      _RL tileVlAv(nSx,nSy)
      _RL tilePEav(nSx,nSy)
      _RL tileVol (nSx,nSy)
      _RL tileAMu (nSx,nSy)
      _RL tileAMs (nSx,nSy)
      _RL radDist(1:sNx,1:sNy)
#ifdef ALLOW_NONHYDROSTATIC
      _RL tmpWke
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      numPnts=0.
      theVol=0.
      theMax=0.
      theMean=0.
      theVolMean=0.
      potEnMean =0.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        tileVol(bi,bj)  = 0. _d 0
        tileMean(bi,bj) = 0. _d 0
        tileVlAv(bi,bj) = 0. _d 0
        tilePEav(bi,bj) = 0. _d 0
        DO k=1,Nr
         kp1 = MIN(k+1,Nr)
         mskp1 = 1.
         IF ( k.GE.Nr ) mskp1 = 0.
C- Note: Present NH implementation does not account for D.w/dt at k=1.
C        Consequently, wVel(k=1) does not contribute to NH KE (msk_1=0).
         msk_1 = 1.
         IF ( k.EQ.1 .AND. selectNHfreeSurf.LE.0 ) msk_1 = 0.
         DO j=1,sNy
          DO i=1,sNx
           tileVol(bi,bj) = tileVol(bi,bj)
     &                    + rA(i,j,bi,bj)*deepFac2C(k)
     &                     *rhoFacC(k)*drF(k)*_hFacC(i,j,k,bi,bj)
     &                     *maskInC(i,j,bi,bj)

C- Vector Invariant form (like in pkg/mom_vecinv/mom_vi_calc_ke.F)
c          tmpVal=0.25*( uVel( i , j ,k,bi,bj)*uVel( i , j ,k,bi,bj)
c    &                  +uVel(i+1, j ,k,bi,bj)*uVel(i+1, j ,k,bi,bj)
c    &                  +vVel( i , j ,k,bi,bj)*vVel( i , j ,k,bi,bj)
c    &                  +vVel( i ,j+1,k,bi,bj)*vVel( i ,j+1,k,bi,bj) )
c          tileVlAv(bi,bj) = tileVlAv(bi,bj)
c    &              +tmpVal*rA(i,j,bi,bj)*drF(k)*hFacC(i,j,k,bi,bj)

C- Energy conservative form (like in pkg/mom_fluxform/mom_calc_ke.F)
C    this is the safe way to check the energy conservation
C    with no assumption on how grid spacing & area are defined.
           tmpVal=0.25*(
     &       uVel( i ,j,k,bi,bj)*uVel( i ,j,k,bi,bj)
     &         *dyG( i ,j,bi,bj)*dxC( i ,j,bi,bj)*_hFacW( i ,j,k,bi,bj)
     &      +uVel(i+1,j,k,bi,bj)*uVel(i+1,j,k,bi,bj)
     &         *dyG(i+1,j,bi,bj)*dxC(i+1,j,bi,bj)*_hFacW(i+1,j,k,bi,bj)
     &      +vVel(i, j ,k,bi,bj)*vVel(i, j ,k,bi,bj)
     &         *dxG(i, j ,bi,bj)*dyC(i, j ,bi,bj)*_hFacS(i, j ,k,bi,bj)
     &      +vVel(i,j+1,k,bi,bj)*vVel(i,j+1,k,bi,bj)
     &         *dxG(i,j+1,bi,bj)*dyC(i,j+1,bi,bj)*_hFacS(i,j+1,k,bi,bj)
     &        )*maskInC(i,j,bi,bj)
           tileVlAv(bi,bj) = tileVlAv(bi,bj)
     &                     + tmpVal*deepFac2C(k)*rhoFacC(k)*drF(k)
           tmpVal= tmpVal*_recip_hFacC(i,j,k,bi,bj)*recip_rA(i,j,bi,bj)

#ifdef ALLOW_NONHYDROSTATIC
           IF ( nonHydrostatic ) THEN
            tmpWke = 0.25*
     &        ( wVel(i,j, k, bi,bj)*wVel(i,j, k, bi,bj)*msk_1
     &                             *deepFac2F( k )*rhoFacF( k )
     &         +wVel(i,j,kp1,bi,bj)*wVel(i,j,kp1,bi,bj)*mskp1
     &                             *deepFac2F(kp1)*rhoFacF(kp1)
     &        )*maskC(i,j,k,bi,bj)*maskInC(i,j,bi,bj)
            tileVlAv(bi,bj) = tileVlAv(bi,bj)
     &             + tmpWke*rA(i,j,bi,bj)*drF(k)*_hFacC(i,j,k,bi,bj)
            tmpVal = tmpVal
     &             + tmpWke*recip_deepFac2C(k)*recip_rhoFacC(k)
           ENDIF
#endif

           theMax=MAX(theMax,tmpVal)
           IF (tmpVal.NE.0.) THEN
            tileMean(bi,bj)=tileMean(bi,bj)+tmpVal
            numPnts=numPnts+1.
           ENDIF

          ENDDO
         ENDDO
        ENDDO
C- Potential Energy (external mode):
         DO j=1,sNy
          DO i=1,sNx
           tmpVal = 0.5 _d 0*Bo_surf(i,j,bi,bj)
     &                      *etaN(i,j,bi,bj)*etaN(i,j,bi,bj)
C- jmc: if geoid not flat (phi0surf), needs to add this term.
C       not sure for atmos/ocean in P ; or atmos. loading in ocean-Z
           tmpVal = tmpVal
     &            + phi0surf(i,j,bi,bj)*etaN(i,j,bi,bj)
           tilePEav(bi,bj) = tilePEav(bi,bj)
     &            + tmpVal*rA(i,j,bi,bj)*deepFac2F(1)
     &                    *maskInC(i,j,bi,bj)
c          tmpVal = etaN(i,j,bi,bj)
c    &            + phi0surf(i,j,bi,bj)*recip_Bo(i,j,bi,bj)
c          tilePEav(bi,bj) = tilePEav(bi,bj)
c    &        + 0.5 _d 0*Bo_surf(i,j,bi,bj)*tmpVal*tmpVal
c    &                  *rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
          ENDDO
         ENDDO
C- end bi,bj loops
       ENDDO
      ENDDO
      _GLOBAL_SUM_RL(numPnts,myThid)
      _GLOBAL_MAX_RL(theMax,myThid)
      CALL GLOBAL_SUM_TILE_RL( tileMean, theMean   , myThid )
      CALL GLOBAL_SUM_TILE_RL( tileVol , theVol    , myThid )
      CALL GLOBAL_SUM_TILE_RL( tileVlAv, theVolMean, myThid )
      CALL GLOBAL_SUM_TILE_RL( tilePEav, potEnMean , myThid )
      IF (numPnts.NE.0.) theMean=theMean/numPnts
      IF (theVol.NE.0.) THEN
        theVolMean=theVolMean/theVol
        potEnMean = potEnMean/theVol
      ENDIF

C--   Compute total angular momentum
      IF ( mon_output_AM ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
C-    calculate radial distance
         DO j=1,sNy
          DO i=1,sNx
            radDist(i,j) = rSphere*COS( deg2rad*yC(i,j,bi,bj) )
     &                            *maskInC(i,j,bi,bj)
          ENDDO
         ENDDO
C-    calculate contribution from zonal velocity
         tileAMu(bi,bj) = 0. _d 0
         tileAMs(bi,bj) = 0. _d 0
         DO k=1,Nr
          DO j=1,sNy
           DO i=1,sNx
            uBarC = (uVel(i,j,k,bi,bj)+uVel(i+1,j,k,bi,bj))*0.5 _d 0
            vBarC = (vVel(i,j,k,bi,bj)+vVel(i,j+1,k,bi,bj))*0.5 _d 0
            tmpVal = ( angleCosC(i,j,bi,bj)*uBarC
     &                -angleSinC(i,j,bi,bj)*vBarC
     &               )*radDist(i,j)*deepFacC(k)
            tileAMu(bi,bj) = tileAMu(bi,bj)
     &             + tmpVal*rA(i,j,bi,bj)*deepFac2C(k)
     &                     *rhoFacC(k)*drF(k)*_hFacC(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
C-    and contribution from mass distribution anomaly (i.e., free-surface)
c        IF ( .FALSE. ) THEN
         IF ( exactConserv ) THEN
C-    To improve balance between zonal-wind (AMu) and mass (AMs) AM, need
C     a consistent time-stepping of meridional mass transport in Coriolis
C     (zonal momentum, go through AB) and mass transport divergence;
C     try to account for AB-2 in AMs the same way it applies to f*v:
#ifdef ALLOW_ADAMSBASHFORTH_3
          weight1 = alph_AB
#else
          weight1 = 0.5 _d 0 + abEps
#endif
          weight0 = 1.0 _d 0 - weight1
          DO j=1,sNy
           DO i=1,sNx
            ks = kSurfC(i,j,bi,bj)
            tmpVal = weight1*etaH(i,j,bi,bj)
#ifdef EXACT_CONSERV
     &             + weight0*etaHnm1(i,j,bi,bj)
#endif
            tmpVal = omega*tmpVal
     &             * radDist(i,j)*radDist(i,j)*deepFac2F(ks)
            tileAMs(bi,bj) = tileAMs(bi,bj)
     &             + tmpVal*rA(i,j,bi,bj)*deepFac2F(ks)*rhoFacF(ks)
           ENDDO
          ENDDO
         ELSE
          DO j=1,sNy
           DO i=1,sNx
            ks = kSurfC(i,j,bi,bj)
            tmpVal = omega*etaN(i,j,bi,bj)
     &             * radDist(i,j)*radDist(i,j)*deepFac2F(ks)
            tileAMs(bi,bj) = tileAMs(bi,bj)
     &             + tmpVal*rA(i,j,bi,bj)*deepFac2F(ks)*rhoFacF(ks)
           ENDDO
          ENDDO
         ENDIF
C- end bi,bj loops
        ENDDO
       ENDDO
       CALL GLOBAL_SUM_TILE_RL( tileAMu , totAMu, myThid )
       CALL GLOBAL_SUM_TILE_RL( tileAMs , totAMs, myThid )

C--   Print stats for total Angular Momentum (per unit area, in kg/s):
       CALL MON_SET_PREF('am',myThid)
       totAMu = totAMu*rUnit2mass
       totAMs = totAMs*rUnit2mass
       IF ( globalArea.GT.0. ) totAMu = totAMu/globalArea
       IF ( globalArea.GT.0. ) totAMs = totAMs/globalArea
       CALL MON_OUT_RL( mon_string_none, totAMs,
     &         '_eta_mean', myThid )
       CALL MON_OUT_RL( mon_string_none, totAMu,
     &         '_uZo_mean', myThid )
       totAMu = totAMu + totAMs
       CALL MON_OUT_RL( mon_string_none, totAMu,
     &         '_tot_mean', myThid )

      ENDIF

C--   Print stats for (barotropic) Potential Energy:
      CALL MON_SET_PREF('pe_b',myThid)
      CALL MON_OUT_RL(mon_string_none,potEnMean,
     &         mon_foot_mean,myThid)

C--   Print stats for KE
      CALL MON_SET_PREF('ke',myThid)
      CALL MON_OUT_RL(mon_string_none,theMax,mon_foot_max,myThid)
c     CALL MON_OUT_RL(mon_string_none,theMean,mon_foot_mean,myThid)
      CALL MON_OUT_RL(mon_string_none,theVolMean,
     &         mon_foot_mean,myThid)
      CALL MON_OUT_RL(mon_string_none,theVol,
     &         mon_foot_vol,myThid)

      RETURN
      END
