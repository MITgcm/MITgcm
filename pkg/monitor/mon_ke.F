C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_ke.F,v 1.8 2001/11/08 16:47:34 jmc Exp $
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
      _RL tmpVal,theMax,theMean,theVolMean,theVol
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
           theVol=theVol+ra(i,j,bi,bj)*drf(k)*hFacC(i,j,k,bi,bj)
           tmpVal=0.25*( uVel( I , J ,K,bi,bj)*uVel( I , J ,K,bi,bj)
     &                  +uVel(I+1, J ,K,bi,bj)*uVel(I+1, J ,K,bi,bj)
     &                  +vVel( I , J ,K,bi,bj)*vVel( I , J ,K,bi,bj)
     &                  +vVel( I ,J+1,K,bi,bj)*vVel( I ,J+1,K,bi,bj) )
           theMax=max(theMax,tmpVal)
           IF (tmpVal.NE.0.) THEN
            theMean=theMean+tmpVal
            numPnts=numPnts+1
           ENDIF
           theVolMean=theVolMean+tmpVal
     &           *ra(i,j,bi,bj)*drf(k)*hFacC(i,j,k,bi,bj)
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

      RETURN
      END
