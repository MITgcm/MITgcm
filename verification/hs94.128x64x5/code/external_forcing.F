C $Header: /u/gcmpack/MITgcm/verification/hs94.128x64x5/code/external_forcing.F,v 1.3 2001/02/04 14:38:52 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_U(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R EXTERNAL_FORCING_U                                   |
C     | o Contains problem specific forcing for zonal velocity.  |
C     |==========================================================|
C     | Adds terms to gU for forcing by external sources         |
C     | e.g. wind stress, bottom friction etc..................  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     Loop counters
      INTEGER I, J
C     _RL uKf
C     _RL levelOfGround
C     _RL criticalLevel
C     _RL levelOfVelPoint
C     _RL dist1
C     _RL dist2
C     _RL decayFac
C     _RL velDragHeightFac
      _RL termP,kV,kF

      kF=1./86400.
      DO J=jMin,jMax
       DO I=iMin,iMax
        IF ( HFacW(i,j,kLev,bi,bj) .GT. 0. ) THEN
C        termP=0.5*( rF(kLev) + min( rF(kLev+1) ,
C    &           min(H(I,J,bi,bj),H(I,J-1,bi,bj))            ) )
         termP=0.5*( rF(kLev) + rF(kLev+1) )
C        termP=rC(kLev)
         kV=kF*MAX(0., (termP*recip_H(I,J,bi,bj)-0.7)/(1.-0.7) )
         gU(i,j,kLev,bi,bj)=gU(i,j,kLev,bi,bj)
     &                      -kV*uVel(i,j,kLev,bi,bj)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_V(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R EXTERNAL_FORCING_V                                   |
C     | o Contains problem specific forcing for merid velocity.  |
C     |==========================================================|
C     | Adds terms to gV for forcing by external sources         |
C     | e.g. wind stress, bottom friction etc..................  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"


C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface
C     == Local variables ==
C     Loop counters
      INTEGER I, J
C     _RL uKf
C     _RL levelOfGround
C     _RL criticalLevel
C     _RL levelOfVelPoint
C     _RL dist1
C     _RL dist2
C     _RL decayFac
C     _RL velDragHeightFac
      _RL termP,kV,kF

      kF=1./86400.
      DO J=jMin,jMax
       DO I=iMin,iMax
        IF ( HFacS(i,j,kLev,bi,bj) .GT. 0. ) THEN
C        termP=0.5*( rF(kLev) + min( rF(kLev+1) ,
C    &           min(H(I,J,bi,bj),H(I,J-1,bi,bj))            ) )
         termP=0.5*( rF(kLev) + rF(kLev+1) )
C        termP=rC(kLev)
         kV=kF*MAX(0., (termP*recip_H(I,J,bi,bj)-0.7)/(1.-0.7) )
         gV(i,j,kLev,bi,bj)=gV(i,j,kLev,bi,bj)
     &                      -kV*vVel(i,j,kLev,bi,bj)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_T(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           maskC,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R EXTERNAL_FORCING_T                                   |
C     | o Contains problem specific forcing for temperature.     |
C     |==========================================================|
C     | Adds terms to gT for forcing by external sources         |
C     | e.g. heat flux, climatalogical relaxation..............  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      _RS maskC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     Loop counters
      INTEGER I, J
      _RL thetaLim,kT,ka,ks,term1,term2,thetaEq,termP,rSurf

      rSurf=1.E5
      ka=1./(40.*86400.)
      ks=1./(4. *86400.)
      DO J=jMin,jMax
       term1=60.*(sin(yC(1,J,bi,bj)*deg2rad)**2)
C      termP=0.5*( rF(kLev) + min( rF(kLev+1) , H(I,J,bi,bj) ) )
       termP=0.5*( rF(kLev) + rF(kLev+1) )
C      termP=rC(kLev)
       term2=10.*log(termP/rSurf)
     &          *(cos(yC(1,J,bi,bj)*deg2rad)**2)
       thetaLim = 200. / ((termP/rSurf)**(2./7.))
       thetaEq=315.-term1-term2
       thetaEq=MAX(thetaLim,thetaEq)
       DO I=iMin,iMax
        kT=ka+(ks-ka)
     &    *MAX(0., (termP*recip_H(I,J,bi,bj)-0.7)/(1.-0.7) )
     &    *COS((yC(1,J,bi,bj)*deg2rad))**4
         gT(i,j,kLev,bi,bj)=gT(i,j,kLev,bi,bj)
     &        - kT*( theta(I,J,kLev,bi,bj)-thetaEq )
     &            *maskC(i,j)
       ENDDO
      ENDDO

      RETURN
      END
CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_S(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           maskC,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R EXTERNAL_FORCING_S                                   |
C     | o Contains problem specific forcing for merid velocity.  |
C     |==========================================================|
C     | Adds terms to gS for forcing by external sources         |
C     | e.g. fresh-water flux, climatalogical relaxation.......  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      _RS maskC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     Loop counters
      INTEGER I, J


      RETURN
      END
