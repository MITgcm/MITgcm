C $Header: /u/gcmpack/MITgcm/model/src/Attic/set_obcs.F,v 1.2 1998/12/09 16:11:53 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SET_OBCS( myCurrentTime, myThid )
C     /==========================================================\
C     | SUBROUTINE SET_OBCS                                      |
C     | o Set boundary conditions at open boundaries             |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "OBCS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_DEPTHS
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     xG, yG - Global coordinate location.
C     zG
C     zUpper - Work arrays for upper and lower
C     zLower   cell-face heights.
C     phi    - Temporary scalar
C     iG, jG - Global coordinate index
C     bi,bj  - Loop counters
C     zUpper - Temporary arrays holding z coordinates of
C     zLower   upper and lower faces.
C     I,i,K
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER  I,  J, K
      _RL OBtimeScale
c
      OBtimeScale = 2000.0

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          OBEu(J,K,bi,bj)=0.
c    &       *sin(2.*PI*myCurrentTime/OBtimeScale)
c    &       *max(myCurrentTime/OBtimeScale,1.)
          OBEv(J,K,bi,bj)=0.
          OBEt(J,K,bi,bj)=tRef(K)
          OBWu(J,K,bi,bj)=0.
c    &       *sin(2.*PI*myCurrentTime/OBtimeScale)
c    &       *max(myCurrentTime/OBtimeScale,1.)
          OBWv(J,K,bi,bj)=0.
          OBWt(J,K,bi,bj)=tRef(K)
         ENDDO
         DO I=1-Olx,sNx+Olx
          OBNu(I,K,bi,bj)=0.
          OBNv(I,K,bi,bj)=0.
          OBNt(I,K,bi,bj)=tRef(K)
          OBSu(I,K,bi,bj)=0.
          OBSv(I,K,bi,bj)=0.
          OBSt(I,K,bi,bj)=tRef(K)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef SIMPLE_OBCS
C This is an example of some rudimentary OBCs.
C These work but aren't very sophisticated...
C (The above specified OBCs produce nicer results!)
C [Remove the CPP #ifdef and #endif to use this code]

C     Simple upwind-type radiation OBCs
      DO bj = myByLo(myThid), myByHi(myThid)
      DO bi = myBxLo(myThid), myBxHi(myThid)

       DO K=1,Nr
        DO J=1-Oly,sNy+Oly
C        Eastern boundary
         IF (uVel( OB_Ie(J,bi,bj) ,J,K,bi,bj).LE.0.) THEN
C         Incoming flow or forced in-flow
          OBEu(J,K,bi,bj)=0.
          OBEt(J,K,bi,bj)=tRef(K)
         ELSE
C         Outgoing flow
          OBEu(J,K,bi,bj)=uVel( OB_Ie(J,bi,bj)-1 ,J,K,bi,bj)
          OBEt(J,K,bi,bj)=theta( OB_Ie(J,bi,bj)-1 ,J,K,bi,bj)
         ENDIF
         OBEv(J,K,bi,bj)=0.
C        Western boundary
         IF (uVel( OB_Iw(J,bi,bj)+1 ,J,K,bi,bj).GE.0.) THEN
C         Incoming flow or forced in-flow
          OBWu(J,K,bi,bj)=0.
          OBWt(J,K,bi,bj)=tRef(K)
         ELSE
C         Outgoing flow
          OBEu(J,K,bi,bj)=uVel( OB_Iw(J,bi,bj)+1 ,J,K,bi,bj)
          OBEt(J,K,bi,bj)=theta( OB_Iw(J,bi,bj)+1 ,J,K,bi,bj)
         ENDIF
         OBWv(J,K,bi,bj)=0.
        ENDDO
       ENDDO

       DO K=1,Nr
        DO I=1-Olx,sNx+Olx
C        Northern boundary
         IF (vVel(I, OB_Jn(I,bi,bj) ,K,bi,bj).LE.0.) THEN
C         Incoming flow or forced in-flow
          OBNv(I,K,bi,bj)=0.
          OBNt(I,K,bi,bj)=tRef(K)
         ELSE
C         Outgoing
          OBNv(I,K,bi,bj)=vVel(I, OB_Jn(I,bi,bj)-1 ,K,bi,bj)
          OBNt(I,K,bi,bj)=theta(I, OB_Jn(I,bi,bj)-1 ,K,bi,bj)
         ENDIF
         OBNu(I,K,bi,bj)=0.
C        Southern boundary
         IF (vVel(I, OB_Js(I,bi,bj)+1 ,K,bi,bj).GE.0.) THEN
C         Incoming flow or forced in-flow
          OBSv(I,K,bi,bj)=0.
          OBSt(I,K,bi,bj)=tRef(K)
         ELSE
C         Outgoing
          OBSv(I,K,bi,bj)=vVel(I, OB_Js(I,bi,bj)+1 ,K,bi,bj)
          OBSt(I,K,bi,bj)=theta(I, OB_Js(I,bi,bj)+1 ,K,bi,bj)
         ENDIF
         OBSu(I,K,bi,bj)=0.
        ENDDO
       ENDDO

      ENDDO
      ENDDO
#endif


C
      RETURN
      END
