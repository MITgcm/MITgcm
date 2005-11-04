C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_ke.F,v 1.14 2005/11/04 01:33:05 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_KE

C     !INTERFACE:
      SUBROUTINE MON_KE(
     I     myIter, myThid )

C     !DESCRIPTION:
C     Calculates stats for Kinetic energy                    

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "MONITOR.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT PARAMETERS:
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi,bj,I,J,K
      _RL numPnts,theVol,tmpVal
      _RL theMax,theMean,theVolMean,potEnMean
      _RL meanTile, volMeanTile, potEnMnTile, volTile

      numPnts=0.
      theVol=0.
      theMax=0.
      theMean=0.
      theVolMean=0.
      potEnMean =0.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        volTile     = 0. _d 0
        meanTile    = 0. _d 0 
        volMeanTile = 0. _d 0
        potEnMnTile = 0. _d 0
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           volTile=volTile+rA(i,j,bi,bj)*drF(k)*hFacC(i,j,k,bi,bj)

C- Vector Invariant form (like in pkg/mom_vecinv/mom_vi_calc_ke.F)
c          tmpVal=0.25*( uVel( I , J ,K,bi,bj)*uVel( I , J ,K,bi,bj)
c    &                  +uVel(I+1, J ,K,bi,bj)*uVel(I+1, J ,K,bi,bj)
c    &                  +vVel( I , J ,K,bi,bj)*vVel( I , J ,K,bi,bj)
c    &                  +vVel( I ,J+1,K,bi,bj)*vVel( I ,J+1,K,bi,bj) )
c          volMeanTile=volMeanTile+tmpVal
c    &           *ra(i,j,bi,bj)*drf(k)*hFacC(i,j,k,bi,bj)

C- Energy conservative form (like in pkg/mom_fluxform/mom_calc_ke.F)
C    this is the safe way to check the energy conservation
C    with no assumption on how grid spacing & area are defined.
           tmpVal=0.25*(
     &       uVel( i ,j,k,bi,bj)*uVel( i ,j,k,bi,bj)
     &         *dyG( i ,j,bi,bj)*dxC( i ,j,bi,bj)*hFacW( i ,j,k,bi,bj)
     &      +uVel(i+1,j,k,bi,bj)*uVel(i+1,j,k,bi,bj)
     &         *dyG(i+1,j,bi,bj)*dxC(i+1,j,bi,bj)*hFacW(i+1,j,k,bi,bj)
     &      +vVel(i, j ,k,bi,bj)*vVel(i, j ,k,bi,bj)
     &         *dxG(i, j ,bi,bj)*dyC(i, j ,bi,bj)*hFacS(i, j ,k,bi,bj)
     &      +vVel(i,j+1,k,bi,bj)*vVel(i,j+1,k,bi,bj)
     &         *dxG(i,j+1,bi,bj)*dyC(i,j+1,bi,bj)*hFacS(i,j+1,k,bi,bj)
     &        )
           volMeanTile= volMeanTile + tmpVal*drF(k)
           tmpVal= tmpVal*recip_hFacC(i,j,k,bi,bj)*recip_rA(i,j,bi,bj)

           theMax=max(theMax,tmpVal)
           IF (tmpVal.NE.0.) THEN
            meanTile=meanTile+tmpVal
            numPnts=numPnts+1.
           ENDIF

          ENDDO
         ENDDO
        ENDDO
C- Potential Energy (external mode):
         DO J=1,sNy
          DO I=1,sNx
           tmpVal = 0.5 _d 0*Bo_surf(i,j,bi,bj)
     &                      *etaN(i,j,bi,bj)*etaN(i,j,bi,bj)
C- jmc: if geoid not flat (phi0surf), needs to add this term.
C       not sure for atmos/ocean in P ; or atmos. loading in ocean-Z
           tmpVal = tmpVal
     &            + phi0surf(i,j,bi,bj)*etaN(i,j,bi,bj)
           potEnMnTile = potEnMnTile
     &               + tmpVal*rA(i,j,bi,bj)*maskH(i,j,bi,bj)
c          tmpVal = etaN(i,j,bi,bj)
c    &            + phi0surf(i,j,bi,bj)*recip_Bo(i,j,bi,bj)
c          potEnMnTile = potEnMnTile
c    &        + 0.5 _d 0*Bo_surf(i,j,bi,bj)*tmpVal*tmpVal
c    &                  *rA(i,j,bi,bj)*maskH(i,j,bi,bj)
          ENDDO
         ENDDO
         theMean    = theMean    + meanTile
         theVol     = theVol     + volTile
         theVolMean = theVolMean + volMeanTile
         potEnMean  = potEnMean  + potEnMnTile
C- end bi,bj loops
       ENDDO
      ENDDO
      _GLOBAL_SUM_R8(numPnts,myThid)
      _GLOBAL_MAX_R8(theMax,myThid)
      _GLOBAL_SUM_R8(theMean,myThid)
      IF (numPnts.NE.0.) theMean=theMean/numPnts
      _GLOBAL_SUM_R8(theVol,myThid)
      _GLOBAL_SUM_R8(theVolMean,myThid)
      _GLOBAL_SUM_R8(potEnMean, myThid)
      IF (theVol.NE.0.) THEN
        theVolMean=theVolMean/theVol
        potEnMean = potEnMean/theVol
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
