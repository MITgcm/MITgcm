C $Header: /u/gcmpack/MITgcm/verification/exp4/code/Attic/set_obcs.F,v 1.4 2000/03/14 17:47:36 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SET_OBCS( K, bi, bj, myCurrentTime, myThid )
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
      INTEGER K, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_OBCS

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
      INTEGER  I,  J
      _RL obTimeScale,Uinflow
c
      obTimeScale = 2000.0
      Uinflow = 0.25

      DO J=1-Oly,sNy+Oly
       OBEu(J,K,bi,bj)=Uinflow
c    &       *sin(2.*PI*myCurrentTime/obTimeScale)
c    &       *max(myCurrentTime/obTimeScale,1.)
       OBEv(J,K,bi,bj)=0.
       OBEt(J,K,bi,bj)=tRef(K)
       OBWu(J,K,bi,bj)=Uinflow
c    &       *sin(2.*PI*myCurrentTime/obTimeScale)
c    &       *max(myCurrentTime/obTimeScale,1.)
       OBWv(J,K,bi,bj)=0.
       OBWt(J,K,bi,bj)=tRef(K)
      ENDDO
      DO I=1-Olx,sNx+Olx
       OBNu(I,K,bi,bj)=Uinflow
       OBNv(I,K,bi,bj)=0.
       OBNt(I,K,bi,bj)=tRef(K)
       OBSu(I,K,bi,bj)=Uinflow
       OBSv(I,K,bi,bj)=0.
       OBSt(I,K,bi,bj)=tRef(K)
      ENDDO

#endif
      RETURN
      END
