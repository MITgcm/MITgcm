C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_ke.F,v 1.24 2013/02/17 04:07:30 jmc Exp $
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
      _RL abFac1, abFac2, R_drK, cosLat
      _RL theMax,theMean,theVolMean,potEnMean
      _RL totAMu, totAMs
      _RL tileMean(nSx,nSy)
      _RL tileVlAv(nSx,nSy)
      _RL tilePEav(nSx,nSy)
      _RL tileVol (nSx,nSy)
      _RL tileAMu (nSx,nSy)
      _RL tileAMs (nSx,nSy)
      _RL tmpFld(1:sNx,1:sNy)
      _RS cos2LatG(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_NONHYDROSTATIC
      _RL tmpWke
#endif
#ifdef ALLOW_ADAMSBASHFORTH_3
      INTEGER m1, m2
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
C-    Calculate contribution from zonal velocity
         abFac1 = 0. _d 0
         abFac2 = 0. _d 0
#ifdef ALLOW_ADAMSBASHFORTH_3
          m1 = 1 + mod(myIter+1,2)
          m2 = 1 + mod( myIter ,2)
          IF ( myIter.GE.2 ) abFac2 = beta_AB
          IF ( myIter.GE.1 ) abFac1 = -( alph_AB + abFac2 )
#else
          IF ( myIter.GE.1 ) abFac1 = -( 0.5 _d 0 + abEps )
#endif
C-    contribution from uVel component: 1rst integrate vertically
         DO j=1,sNy
          DO i=1,sNx
            tmpFld(i,j) = 0. _d 0
          ENDDO
         ENDDO
         DO k=1,Nr
          R_drK = rSphere*deepFacC(k)*deepFac2C(k)
     &                   *rhoFacC(k)*drF(k)
          DO j=1,sNy
           DO i=1,sNx
#ifdef ALLOW_ADAMSBASHFORTH_3
            tmpVal = abFac1*guNm(i,j,k,bi,bj,m1)
     &             + abFac2*guNm(i,j,k,bi,bj,m2)
#else
            tmpVal = abFac1*guNm1(i,j,k,bi,bj)
#endif
            tmpVal = tmpVal*deltaTMom + uVel(i,j,k,bi,bj)
            tmpFld(i,j) = tmpFld(i,j)
     &             + R_drK*tmpVal*_hFacW(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
C-    and then integrate horizontally over this tile
         DO j=1,sNy
          DO i=1,sNx
            cosLat = COS( deg2rad*
     &             ( yG(i,j,bi,bj) + yG(i,j+1,bi,bj) )*halfRL )
            tmpFld(i,j) = tmpFld(i,j)*u2zonDir(i,j,bi,bj)
     &                   *cosLat*rAw(i,j,bi,bj)
     &                   *maskInW(i,j,bi,bj)
          ENDDO
         ENDDO
         tileAMu(bi,bj) = 0. _d 0
         DO j=1,sNy
          DO i=1,sNx
            tileAMu(bi,bj) = tileAMu(bi,bj) + tmpFld(i,j)
          ENDDO
         ENDDO
C-    contribution from vVel component: 1rst integrate vertically
         DO j=1,sNy
          DO i=1,sNx
            tmpFld(i,j) = 0. _d 0
          ENDDO
         ENDDO
         DO k=1,Nr
          R_drK = rSphere*deepFacC(k)*deepFac2C(k)
     &                   *rhoFacC(k)*drF(k)
          DO j=1,sNy
           DO i=1,sNx
#ifdef ALLOW_ADAMSBASHFORTH_3
            tmpVal = abFac1*gvNm(i,j,k,bi,bj,m1)
     &             + abFac2*gvNm(i,j,k,bi,bj,m2)
#else
            tmpVal = abFac1*gvNm1(i,j,k,bi,bj)
#endif
            tmpVal = tmpVal*deltaTMom + vVel(i,j,k,bi,bj)
            tmpFld(i,j) = tmpFld(i,j)
     &             + R_drK*tmpVal*_hFacS(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
C-    and then integrate horizontally over this tile
         DO j=1,sNy
          DO i=1,sNx
            cosLat = COS( deg2rad*
     &             ( yG(i,j,bi,bj) + yG(i+1,j,bi,bj) )*halfRL )
            tmpFld(i,j) = tmpFld(i,j)*v2zonDir(i,j,bi,bj)
     &                   *cosLat*rAs(i,j,bi,bj)
     &                   *maskInS(i,j,bi,bj)
          ENDDO
         ENDDO
         DO j=1,sNy
          DO i=1,sNx
            tileAMu(bi,bj) = tileAMu(bi,bj) + tmpFld(i,j)
          ENDDO
         ENDDO
C-    Calculate contribution from mass distribution anomaly (i.e., free-surface)
         IF ( exactConserv ) THEN
          DO j=1,sNy
           DO i=1,sNx
#ifdef EXACT_CONSERV
            tmpFld(i,j) = etaHnm1(i,j,bi,bj)
#else
            tmpFld(i,j) = 0.
#endif
           ENDDO
          ENDDO
         ELSE
          DO j=1,sNy
           DO i=1,sNx
            tmpFld(i,j) = etaN(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDIF
C-    calculate angular momentum from mass-distribution anomaly
C     using square of radial distance (averaged @ center point)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            cosLat = COS( deg2rad*yG(i,j,bi,bj) )
            cos2LatG(i,j) = cosLat*cosLat
          ENDDO
         ENDDO
         DO j=1,sNy
          DO i=1,sNx
            tmpFld(i,j) = tmpFld(i,j)
     &        *omega*rSphere*rSphere
     &        *( ( cos2LatG(i,j) + cos2LatG(i+1,j+1) )
     &         + ( cos2LatG(i+1,j) + cos2LatG(i,j+1) )
     &         )*0.25 _d 0
          ENDDO
         ENDDO
         DO j=1,sNy
          DO i=1,sNx
            ks = kSurfC(i,j,bi,bj)
            tmpFld(i,j) = tmpFld(i,j)
     &             *maskInC(i,j,bi,bj)*deepFac2F(ks)
     &             *rA(i,j,bi,bj)*deepFac2F(ks)*rhoFacF(ks)
          ENDDO
         ENDDO
         tileAMs(bi,bj) = 0. _d 0
         DO j=1,sNy
          DO i=1,sNx
            tileAMs(bi,bj) = tileAMs(bi,bj) + tmpFld(i,j)
          ENDDO
         ENDDO
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
       totAMu = totAMu + freeSurfFac*totAMs
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
