C     $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/aim_aim2dyn_exchanges.F,v 1.2 2001/08/03 19:01:34 adcroft Exp $
C     $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE AIM_AIM2DYN_EXCHANGES(
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R AIM_AIM2DYN_EXCHANGES                                |
C     | o Do overlap exchanges for AIM related quantities.       |
C     |   AIM does not compute into overlaps so exchanges        |
C     |   are needed for AIM quantities.                         |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#ifdef ALLOW_AIM
#include "AIM2DYN.h"
#endif /* ALLOW_AIM */

C     == Routine arguments ==
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I,J,K - Loop counters
C     bi,bj
      INTEGER I,J,K, bi,bj 

#ifdef ALLOW_AIM

      _EXCH_XY_R8( aim_drag, myThid )
c     _EXCH_XYZ_R8( gT, myThid )
c     _EXCH_XYZ_R8( gS, myThid )
c     _EXCH_XYZ_R8( gTNM1, myThid )
c     _EXCH_XYZ_R8( gSNM1, myThid )
c     DO bj=myByLo(myThid),myByHi(myThid)
c      DO bi=myBxLo(myThid),myBxHi(myThid)
c       DO K=1,Nr
c        DO J=1-OLy,sNy+OLy
c         DO I=1-OLx,sNx+OLx
c          gT(i,j,k,bi,bj) = 
c    &      maskC(i,j,k,bi,bj)*gT(i,j,k,bi,bj)
c          gTNM1(i,j,k,bi,bj) = 
c    &      maskC(i,j,k,bi,bj)*gTNM1(i,j,k,bi,bj)
c          gS(i,j,k,bi,bj) = 
c    &      maskC(i,j,k,bi,bj)*gS(i,j,k,bi,bj)
c          gSNM1(i,j,k,bi,bj) = 
c    &      maskC(i,j,k,bi,bj)*gSNM1(i,j,k,bi,bj)
c         ENDDO
c        ENDDO
c       ENDDO
c      ENDDO
c     ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
