C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_calc.F,v 1.2 2001/02/02 21:36:30 adcroft Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_CALC( bi, bj, futureTime, 
     &                      uVel, vVel, wVel, theta, salt, 
     &                      myThid )
C     /==========================================================\
C     | SUBROUTINE OBCS_CALC                                     |
C     | o Calculate future boundary data at open boundaries      |
C     |   at time = futureTime                                   |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"

C     == Routine arguments ==
      INTEGER bi, bj
      _RL futureTime
      _RL uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER I, J ,K

C     Eastern OB
      IF (useOrlanskiEast) THEN
        CALL ORLANSKI_EAST(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
      ELSE
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
            OBEu(J,K,bi,bj)=0.
            OBEv(J,K,bi,bj)=0.
            OBEt(J,K,bi,bj)=tRef(K)
            OBEs(J,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBEw(J,K,bi,bj)=0.
#endif
          ENDDO
        ENDDO
      ENDIF

C     Western OB
      IF (useOrlanskiWest) THEN
        CALL ORLANSKI_WEST(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
      ELSE
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
            OBWu(J,K,bi,bj)=0.
            OBWv(J,K,bi,bj)=0.
            OBWt(J,K,bi,bj)=tRef(K)
            OBWs(J,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBWw(J,K,bi,bj)=0.
#endif
          ENDDO
        ENDDO
      ENDIF

C         Northern OB, template for forcing
      IF (useOrlanskiNorth) THEN
        CALL ORLANSKI_NORTH(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
      ELSE
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            OBNv(I,K,bi,bj)=0.
            OBNu(I,K,bi,bj)=0.
            OBNt(I,K,bi,bj)=tRef(K)
            OBNs(I,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBNw(I,K,bi,bj)=0.
#endif
          ENDDO
        ENDDO
      ENDIF

C         Southern OB, template for forcing
      IF (useOrlanskiSouth) THEN   
        CALL ORLANSKI_SOUTH(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
      ELSE
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            OBSu(I,K,bi,bj)=0.
            OBSv(I,K,bi,bj)=0.
            OBSt(I,K,bi,bj)=tRef(K)
            OBSs(I,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBSw(I,K,bi,bj)=0.
#endif
          ENDDO
        ENDDO
      ENDIF

#endif /* ALLOW_OBCS */
      RETURN
      END
