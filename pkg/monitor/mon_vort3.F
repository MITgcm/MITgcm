C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_vort3.F,v 1.15 2009/06/28 01:05:41 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_VORT3

C     !INTERFACE:
      SUBROUTINE MON_VORT3(
     I     myIter, myThid )

C     !DESCRIPTION:
C     Calculates stats for Vorticity (z-component).

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "MONITOR.h"
#include "GRID.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */

C     !INPUT PARAMETERS:
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
      INTEGER iMax,jMax
      _RL theVol, theArea, tmpVal, tmpAre, tmpVol
      _RL theMin, theMax, theMean, theVar, volMean, volVar, theSD
c     _RL areaTile, volTile, sumTile, sqsTile, vSumTile, vSqsTile
      _RL tileArea(nSx,nSy)
      _RL tileVol (nSx,nSy)
      _RL tileSum (nSx,nSy)
      _RL tileVar (nSx,nSy)
      _RL tileVSum(nSx,nSy)
      _RL tileVSq (nSx,nSy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL AZcorner
#ifdef MONITOR_TEST_HFACZ
      _RL tmpFac
      _RL etaFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
      LOGICAL northWestCorner, northEastCorner
      LOGICAL southWestCorner, southEastCorner
      INTEGER iG
#ifdef ALLOW_EXCH2
      INTEGER myTile
#endif

      theMin = 1. _d 20
      theMax =-1. _d 20
      theArea= 0. _d 0
      theMean= 0. _d 0
      theVar = 0. _d 0
      theVol = 0. _d 0
      volMean= 0. _d 0
      volVar = 0. _d 0
      theSD  = 0. _d 0
      AZcorner = 1. _d 0

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         tileArea(bi,bj)= 0. _d 0
         tileVol(bi,bj) = 0. _d 0
         tileSum(bi,bj) = 0. _d 0
         tileVar(bi,bj) = 0. _d 0
         tileVSum(bi,bj)= 0. _d 0
         tileVSq(bi,bj) = 0. _d 0
#ifdef MONITOR_TEST_HFACZ
         tmpFac = 0.
         IF( implicDiv2Dflow.GT.0 .AND. abEps.GT.-0.5 )
     &    tmpFac = (0.5+abEps) / implicDiv2Dflow
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            etaFld(i,j) = etaH(i,j,bi,bj)
     &          + tmpFac*(etaN(i,j,bi,bj)-etaH(i,j,bi,bj))
          ENDDO
         ENDDO
#endif
        DO k=1,Nr

         iMax = sNx
         jMax = sNy
         DO j=1,sNy
          DO i=1,sNx
#ifdef MONITOR_TEST_HFACZ
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    Test various definitions of hFacZ (for 1 layer, flat bottom ocean):
c          hFacZ(i,j) = 1. +
c    &           0.25 _d 0*( etaFld(i-1,j-1)
c    &                     + etaFld( i ,j-1)
c    &                     + etaFld(i-1, j )
c    &                     + etaFld( i , j )
c    &                     )*recip_drF(k)
c          hFacZ(i,j) = 1. +
c    &           0.25 _d 0*( etaFld(i-1,j-1)*rA(i-1,j-1,bi,bj)
c    &                     + etaFld( i ,j-1)*rA( i ,j-1,bi,bj)
c    &                     + etaFld(i-1, j )*rA(i-1, j ,bi,bj)
c    &                     + etaFld( i , j )*rA( i , j ,bi,bj)
c    &                     )*recip_drF(k)*recip_rAz(i,j,bi,bj)
           hFacZ(i,j) = 1. + 0.125 _d 0*
     &                   ( ( etaFld(i-1,j-1)*rA(i-1,j-1,bi,bj)
     &                      +etaFld( i ,j-1)*rA( i ,j-1,bi,bj)
     &                     )*recip_rAw(i,j-1,bi,bj)
     &                   + ( etaFld(i-1, j )*rA(i-1, j ,bi,bj)
     &                      +etaFld( i , j )*rA( i , j ,bi,bj)
     &                     )*recip_rAw(i, j ,bi,bj)
     &                   + ( etaFld(i-1,j-1)*rA(i-1,j-1,bi,bj)
     &                      +etaFld(i-1, j )*rA(i-1, j ,bi,bj)
     &                     )*recip_rAs(i-1,j,bi,bj)
     &                   + ( etaFld( i ,j-1)*rA( i ,j-1,bi,bj)
     &                     + etaFld( i , j )*rA( i , j ,bi,bj)
     &                     )*recip_rAs( i ,j,bi,bj)
     &                   )*recip_drF(k)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#else
C-    Standard definition of hFac at vorticity point:
           hFacZ(i,j) =
     &           0.25 _d 0*( _hFacW(i,j-1,k,bi,bj)
     &                     + _hFacW(i, j ,k,bi,bj)
     &                     + _hFacS(i-1,j,k,bi,bj)
     &                     + _hFacS( i ,j,k,bi,bj)
     &                     )
#endif /* MONITOR_TEST_HFACZ */
           vort3(i,j) = recip_rAz(i,j,bi,bj)*(
     &       vVel( i ,j,k,bi,bj)*dyC( i ,j,bi,bj)
     &      -vVel(i-1,j,k,bi,bj)*dyC(i-1,j,bi,bj)
     &      -uVel(i, j ,k,bi,bj)*dxC(i, j ,bi,bj)
     &      +uVel(i,j-1,k,bi,bj)*dxC(i,j-1,bi,bj)
     &                                       )
          ENDDO
         ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Special stuff for Cubed Sphere:
         IF (useCubedSphereExchange) THEN
c          AZcorner = 0.75 _d 0
           iMax = sNx+1
           jMax = sNy+1
           DO i=1,iMax
            hFacZ(i,jMax)=0.
            vort3(i,jMax)=0.
           ENDDO
           DO j=1,jMax
            hFacZ(iMax,j)=0.
            vort3(iMax,j)=0.
           ENDDO

           southWestCorner = .TRUE.
           southEastCorner = .TRUE.
           northWestCorner = .TRUE.
           northEastCorner = .TRUE.
           iG = bi+(myXGlobalLo-1)/sNx
#ifdef ALLOW_EXCH2
           myTile = W2_myTileList(bi,bj)
           iG = exch2_myFace(myTile)
           southWestCorner = exch2_isWedge(myTile).EQ.1
     &                 .AND. exch2_isSedge(myTile).EQ.1
           southEastCorner = exch2_isEedge(myTile).EQ.1
     &                 .AND. exch2_isSedge(myTile).EQ.1
           northEastCorner = exch2_isEedge(myTile).EQ.1
     &                 .AND. exch2_isNedge(myTile).EQ.1
           northWestCorner = exch2_isWedge(myTile).EQ.1
     &                 .AND. exch2_isNedge(myTile).EQ.1
#endif /* ALLOW_EXCH2 */

C--        avoid to count 3 times the same corner:
           southEastCorner = southEastCorner .AND. iG.EQ.2
           northWestCorner = northWestCorner .AND. iG.EQ.1
           northEastCorner = .FALSE.

C--       S.W. corner:
          IF ( southWestCorner ) THEN
           i=1
           j=1
           vort3(i,j)=
     &       +recip_rAz(i,j,bi,bj)/AZcorner*(
     &        vVel(i,j,k,bi,bj)*dyC(i,j,bi,bj)
     &       -uVel(i,j,k,bi,bj)*dxC(i,j,bi,bj)
     &       +uVel(i,j-1,k,bi,bj)*dxC(i,j-1,bi,bj)
     &       )
           hFacZ(i,j) = ( _hFacW(i,j-1,k,bi,bj)
     &                  + _hFacW(i, j ,k,bi,bj)
     &                  + _hFacS( i ,j,k,bi,bj)
     &                  )/3. _d 0
          ENDIF
          IF ( southEastCorner ) THEN
C--        S.E. corner:
           i=iMax
           j=1
           vort3(i,j)=
     &       +recip_rAz(i,j,bi,bj)/AZcorner*(
     &       -vVel(i-1,j,k,bi,bj)*dyC(i-1,j,bi,bj)
     &       -uVel(i,j,k,bi,bj)*dxC(i,j,bi,bj)
     &       +uVel(i,j-1,k,bi,bj)*dxC(i,j-1,bi,bj)
     &       )
           hFacZ(i,j) = ( _hFacW(i,j-1,k,bi,bj)
     &                  + _hFacW(i, j ,k,bi,bj)
     &                  + _hFacS(i-1,j,k,bi,bj)
     &                  )/3. _d 0
          ENDIF
          IF ( northWestCorner ) THEN
C--        N.W. corner:
           i=1
           j=jMax
           vort3(i,j)=
     &       +recip_rAz(i,j,bi,bj)/AZcorner*(
     &        vVel(i,j,k,bi,bj)*dyC(i,j,bi,bj)
     &       -uVel(i,j,k,bi,bj)*dxC(i,j,bi,bj)
     &       +uVel(i,j-1,k,bi,bj)*dxC(i,j-1,bi,bj)
     &       )
           hFacZ(i,j) = ( _hFacW(i,j-1,k,bi,bj)
     &                  + _hFacW(i, j ,k,bi,bj)
     &                  + _hFacS( i ,j,k,bi,bj)
     &                  )/3. _d 0
          ENDIF
          IF ( northEastCorner ) THEN
C--        N.E. corner:
           i=iMax
           j=jMax
           vort3(i,j)=
     &       +recip_rAz(i,j,bi,bj)/AZcorner*(
     &       -vVel(i-1,j,k,bi,bj)*dyC(i-1,j,bi,bj)
     &       -uVel(i,j,k,bi,bj)*dxC(i,j,bi,bj)
     &       +uVel(i,j-1,k,bi,bj)*dxC(i,j-1,bi,bj)
     &       )
           hFacZ(i,j) = ( _hFacW(i,j-1,k,bi,bj)
     &                  + _hFacW(i, j ,k,bi,bj)
     &                  + _hFacS(i-1,j,k,bi,bj)
     &                  )/3. _d 0
          ENDIF
         ENDIF

C-    Special stuff for North & South Poles, LatLon grid
         IF ( usingSphericalPolarGrid ) THEN
          IF (yG(1,sNy+1,bi,bj).EQ.90.) THEN
           jMax = sNy+1
           j = jMax
           DO i=1,sNx
            vort3(i,j) = 0.
            vort3(1,j) = vort3(1,j)
     &                 + uVel(i,j-1,k,bi,bj)*dxC(i,j-1,bi,bj)
            hFacZ(i,j) = 0.
#ifndef MONITOR_TEST_HFACZ
            hFacZ(1,j) = hFacZ(1,j) + _hFacW(i,j-1,k,bi,bj)
           ENDDO
#else
            hFacZ(1,j) = hFacZ(1,j) + etaFld(i,j-1)
           ENDDO
            hFacZ(1,j) = sNx + hFacZ(1,j)*recip_drF(k)
#endif
            hFacZ(1,j) = hFacZ(1,j) / FLOAT(sNx)
            vort3(1,j) = vort3(1,j)*recip_rAz(1,j,bi,bj)
          ENDIF
          IF (yG(1,1,bi,bj).EQ.-90.) THEN
           j = 1
           DO i=1,sNx
            vort3(i,j) = 0.
            vort3(1,j) = vort3(1,j)
     &                 - uVel(i,j,k,bi,bj)*dxC(i,j,bi,bj)
            hFacZ(i,j) = 0.
#ifndef MONITOR_TEST_HFACZ
            hFacZ(1,j) = hFacZ(1,j) + _hFacW(i,j,k,bi,bj)
           ENDDO
#else
            hFacZ(1,j) = hFacZ(1,j) + etaFld(i,j)
           ENDDO
            hFacZ(1,j) = sNx + hFacZ(1,j)*recip_drF(k)
#endif
            hFacZ(1,j) = hFacZ(1,j) / FLOAT(sNx)
            vort3(1,j) = vort3(1,j)*recip_rAz(1,j,bi,bj)
          ENDIF
         ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

         DO j=1,jMax
          DO i=1,iMax
           IF (hFacZ(i,j).GT.0. _d 0) THEN
            tmpVal = vort3(i,j)
            tmpAre = rAz(i,j,bi,bj)*drF(k)
            tmpVol = rAz(i,j,bi,bj)*drF(k)*hFacZ(i,j)
            tileArea(bi,bj)  = tileArea(bi,bj)  + tmpAre
C-       min,max of relative vorticity ("r")
            theMin = MIN(theMin,tmpVal)
            theMax = MAX(theMax,tmpVal)
C-       average & std.dev of absolute vorticity ("a")
            tmpVal = tmpVal + fCoriG(i,j,bi,bj)
            tileSum(bi,bj) = tileSum(bi,bj) + tmpAre*tmpVal
            tileVar(bi,bj) = tileVar(bi,bj) + tmpAre*tmpVal*tmpVal
C-       average & std.dev of potential vorticity ("p")
            tmpVal = tmpVal / hFacZ(i,j)
            tileVol(bi,bj) = tileVol(bi,bj) + tmpVol
            tileVSum(bi,bj)= tileVSum(bi,bj)+ tmpVol*tmpVal
            tileVSq(bi,bj) = tileVSq(bi,bj) + tmpVol*tmpVal*tmpVal
           ENDIF
          ENDDO
         ENDDO

        ENDDO
c       theArea= theArea + tileArea(bi,bj)
c       theVol = theVol  + tileVol (bi,bj)
c       theMean= theMean + tileSum(bi,bj)
c       theVar = theVar  + tileVar(bi,bj)
c       volMean= volMean + tileVSum(bi,bj)
c       volVar = volVar  + tileVSq(bi,bj)
       ENDDO
      ENDDO

      theMin = -theMin
      _GLOBAL_MAX_RL(theMin, myThid)
      _GLOBAL_MAX_RL(theMax, myThid)
      theMin = -theMin
c     _GLOBAL_SUM_RL(theArea,myThid)
c     _GLOBAL_SUM_RL(theVol, myThid)
c     _GLOBAL_SUM_RL(theMean,myThid)
c     _GLOBAL_SUM_RL(theVar, myThid)
c     _GLOBAL_SUM_RL(volMean,myThid)
c     _GLOBAL_SUM_RL(volVar ,myThid)
      CALL GLOBAL_SUM_TILE_RL( tileArea, theArea, myThid )
      CALL GLOBAL_SUM_TILE_RL( tileVol,  theVol,  myThid )
      CALL GLOBAL_SUM_TILE_RL( tileSum,  theMean, myThid )
      CALL GLOBAL_SUM_TILE_RL( tileVar,  theVar,  myThid )
      CALL GLOBAL_SUM_TILE_RL( tileVSum, volMean, myThid )
      CALL GLOBAL_SUM_TILE_RL( tileVSq,  volVar,  myThid )
      IF (theArea.GT.0.) THEN
        theMean= theMean/theArea
        theVar = theVar /theArea
        theVar = theVar - theMean*theMean
c       IF (theVar.GT.0.) theSD = SQRT(theVar)
        IF (theVar.GT.0.) theVar = SQRT(theVar)
      ENDIF
      IF (theVol.GT.0.) THEN
        volMean= volMean/theVol
        volVar = volVar /theVol
        volVar = volVar - volMean*volMean
        IF (volVar.GT.0.) theSD = SQRT(volVar)
      ENDIF

C-    Print stats for (relative/absolute) Vorticity AND Pot.Vort.
      CALL MON_SET_PREF('vort',myThid)
      CALL MON_OUT_RL(mon_string_none,theMin, '_r_min',  myThid)
      CALL MON_OUT_RL(mon_string_none,theMax, '_r_max',  myThid)
      CALL MON_OUT_RL(mon_string_none,theMean,'_a_mean', myThid)
      CALL MON_OUT_RL(mon_string_none,theVar, '_a_sd',   myThid)
      CALL MON_OUT_RL(mon_string_none,volMean,'_p_mean', myThid)
      CALL MON_OUT_RL(mon_string_none,theSD,  '_p_sd',   myThid)
c     CALL MON_OUT_RL(mon_string_none,theVol,mon_foot_vol,myThid)

      RETURN
      END
