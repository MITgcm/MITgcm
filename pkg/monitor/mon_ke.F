C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_ke.F,v 1.9 2002/06/15 03:14:53 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MON_KE(
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE MON_KE                                        |
C     | o Calculates stats for Kinetic energy                    |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "MONITOR.h"
#include "GRID.h"

C     === Routine arguments ===
      INTEGER myThid

C     === Local variables ====
      INTEGER bi,bj,I,J,K
      _RL tmpVal,tmpVol,theMax,theMean,theVolMean,theVol
      INTEGER numPnts

      theMax=0.
      numPnts=0
      theMean=0.
      theVolMean=0.
      theVol=0.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           theVol=theVol+rA(i,j,bi,bj)*drF(k)*hFacC(i,j,k,bi,bj)

C- Vector Invariant form (like in pkg/mom_vecinv/mom_vi_calc_ke.F)
c          tmpVal=0.25*( uVel( I , J ,K,bi,bj)*uVel( I , J ,K,bi,bj)
c    &                  +uVel(I+1, J ,K,bi,bj)*uVel(I+1, J ,K,bi,bj)
c    &                  +vVel( I , J ,K,bi,bj)*vVel( I , J ,K,bi,bj)
c    &                  +vVel( I ,J+1,K,bi,bj)*vVel( I ,J+1,K,bi,bj) )
c          theVolMean=theVolMean+tmpVal
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
           theVolMean= theVolMean + tmpVal*drF(k)
           tmpVal= tmpVal*recip_hFacC(i,j,k,bi,bj)*recip_rA(i,j,bi,bj)

           theMax=max(theMax,tmpVal)
           IF (tmpVal.NE.0.) THEN
            theMean=theMean+tmpVal
            numPnts=numPnts+1
           ENDIF

          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _GLOBAL_MAX_R8(theMax,myThid)
      _GLOBAL_SUM_R8(theMean,myThid)
      tmpVal=float(numPnts)
      _GLOBAL_SUM_R8(tmpVal,myThid)
      IF (tmpVal.NE.0.) theMean=theMean/tmpVal
      _GLOBAL_SUM_R8(theVol,myThid)
      _GLOBAL_SUM_R8(theVolMean,myThid)

      CALL MON_OUT_RL(mon_string_none,theMax,mon_foot_max,myThid)
      CALL MON_OUT_RL(mon_string_none,theMean,mon_foot_mean,myThid)
      CALL MON_OUT_RL(mon_string_none,theVolMean,
     &         mon_foot_volint,myThid)
      IF (theVol.NE.0.) theVolMean=theVolMean/theVol
      CALL MON_OUT_RL(mon_string_none,theVolMean,
     &         mon_foot_volmean,myThid)
      CALL MON_OUT_RL(mon_string_none,theVol,
     &         mon_foot_vol,myThid)

      RETURN
      END
