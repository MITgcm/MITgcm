C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_calc.F,v 1.6 2002/02/08 22:16:09 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_CALC( bi, bj, futureTime, futureIter,
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
      INTEGER futureIter
      _RL futureTime
      _RL uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER I, J , K, I_obc, J_obc

C     Eastern OB
      IF (useOrlanskiEast) THEN
        CALL ORLANSKI_EAST(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
      ELSE
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
            I_obc=OB_Ie(J,bi,bj)
            IF (I_obc.ne.0) THEN
              OBEu(J,K,bi,bj)=0.
              OBEv(J,K,bi,bj)=0.
              OBEt(J,K,bi,bj)=tRef(K)
              OBEs(J,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
              OBEw(J,K,bi,bj)=0.
#endif
#ifdef NONLIN_FRSURF
              OBEeta(J,bi,bj)=0.
#endif
            ENDIF
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
            I_obc=OB_Iw(J,bi,bj)
            IF (I_obc.ne.0) THEN
              OBWu(J,K,bi,bj)=0.
              OBWv(J,K,bi,bj)=0.
              OBWt(J,K,bi,bj)=tRef(K)
              OBWs(J,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
              OBWw(J,K,bi,bj)=0.
#endif
#ifdef NONLIN_FRSURF
              OBWeta(J,bi,bj)=0.
#endif
            ENDIF
          ENDDO
        ENDDO
      ENDIF

C         Northern OB
      IF (useOrlanskiNorth) THEN
        CALL ORLANSKI_NORTH(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
      ELSE
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            J_obc=OB_Jn(I,bi,bj)
            IF (J_obc.ne.0) THEN
              OBNv(I,K,bi,bj)=0.
              OBNu(I,K,bi,bj)=0.
              OBNt(I,K,bi,bj)=tRef(K)
              OBNs(I,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
              OBNw(I,K,bi,bj)=0.
#endif
#ifdef NONLIN_FRSURF
              OBNeta(I,bi,bj)=0.
#endif
            ENDIF
          ENDDO
        ENDDO
      ENDIF

C         Southern OB
      IF (useOrlanskiSouth) THEN   
        CALL ORLANSKI_SOUTH(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
      ELSE
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            J_obc=OB_Js(I,bi,bj)
            IF (J_obc.ne.0) THEN
              OBSu(I,K,bi,bj)=0.
              OBSv(I,K,bi,bj)=0.
              OBSt(I,K,bi,bj)=tRef(K)
              OBSs(I,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
              OBSw(I,K,bi,bj)=0.
#endif
#ifdef NONLIN_FRSURF
              OBSeta(I,bi,bj)=0.
#endif
            ENDIF
          ENDDO
        ENDDO
      ENDIF

#endif /* ALLOW_OBCS */
      RETURN
      END
