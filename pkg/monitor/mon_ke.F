C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_ke.F,v 1.2 2001/06/04 13:53:35 adcroft Exp $
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

C     === Routine arguments ===
      INTEGER myThid

C     === Local variables ====
      INTEGER bi,bj,I,J,K
      _RL tmpVal,theMax,theMean
      INTEGER numPnts

      theMax=0.
      theMean=0.
      numPnts=0

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           tmpVal=0.25*( uVel( I , J ,K,bi,bj)*uVel( I , J ,K,bi,bj)
     &                  +uVel(I+1, J ,K,bi,bj)*uVel(I+1, J ,K,bi,bj)
     &                  +vVel( I , J ,K,bi,bj)*vVel( I , J ,K,bi,bj)
     &                  +vVel( I ,J+1,K,bi,bj)*vVel( I ,J+1,K,bi,bj) )
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
      theMean=theMean/tmpVal

      _BEGIN_MASTER( myThid )
      WRITE(*,'(A,24x,A,1PE22.14)')
     &      'MON_KE: ','  max=',theMax
      WRITE(*,'(A,24x,A,1PE22.14)')
     &      'MON_KE: ',' mean=',theMean
      _END_MASTER( )

      RETURN
      END
