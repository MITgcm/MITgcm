C $Header: /u/gcmpack/MITgcm/verification/hs94.cs-32x32x5/code/Attic/external_forcing.F,v 1.5 2001/07/06 22:13:37 jmc Exp $
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
      _RL recip_P0g,termP,kV,kF,sigma_b

C--   Forcing term(s)
      kF=1. _d 0/86400. _d 0
      sigma_b = 0.7 _d 0
c     DO J=jMin,jMax
c      DO I=iMin,iMax
      DO J=1,sNy
       DO I=1,sNx+1
        IF ( hFacW(i,j,kLev,bi,bj) .GT. 0. ) THEN
         recip_P0g=MAX(recip_Rcol(I,J,bi,bj),recip_Rcol(I-1,J,bi,bj))
         termP=0.5 _d 0*( MIN(rF(kLev)*recip_P0g,1. _d 0)
     &                   +rF(kLev+1)*recip_P0g )
c        termP=0.5 _d 0*( rF(kLev) + rF(kLev+1) )*recip_P0g
         kV=kF*MAX( 0. _d 0, (termP-sigma_b)/(1. _d 0-sigma_b) )
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
      _RL recip_P0g,termP,kV,kF,sigma_b

C--   Forcing term(s)
      kF=1. _d 0/86400. _d 0
      sigma_b = 0.7 _d 0
c     DO J=jMin,jMax
c      DO I=iMin,iMax
      DO J=1,sNy+1
       DO I=1,sNx
        IF ( hFacS(i,j,kLev,bi,bj) .GT. 0. ) THEN
         recip_P0g=MAX(recip_Rcol(I,J,bi,bj),recip_Rcol(I,J-1,bi,bj))
         termP=0.5 _d 0*( MIN(rF(kLev)*recip_P0g,1. _d 0)
     &                   +rF(kLev+1)*recip_P0g )
c        termP=0.5 _d 0*( rF(kLev) + rF(kLev+1) )*recip_P0g
         kV=kF*MAX( 0. _d 0, (termP-sigma_b)/(1. _d 0-sigma_b) )
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
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     Loop counters
      INTEGER I, J
      _RL thetaLim,kT,ka,ks,sigma_b,term1,term2,thetaEq,termP

C--   Forcing term(s)
      ka=1. _d 0/(40. _d 0*86400. _d 0)
      ks=1. _d 0/(4. _d 0 *86400. _d 0)
      sigma_b = 0.7 _d 0
      DO J=jMin,jMax
       DO I=iMin,iMax
         term1=60. _d 0*(sin(yC(I,J,bi,bj)*deg2rad)**2)
         termP=0.5 _d 0*( rF(kLev) + rF(kLev+1) )
         term2=10. _d 0*log(termP/atm_po)
     &            *(cos(yC(I,J,bi,bj)*deg2rad)**2)
         thetaLim = 200. _d 0/ ((termP/atm_po)**atm_kappa)
         thetaEq=315. _d 0-term1-term2
         thetaEq=MAX(thetaLim,thetaEq)
         termP=0.5 _d 0*( MIN(rF(kLev),Ro_surf(I,J,bi,bj))+rF(kLev+1) ) 
         kT=ka+(ks-ka)
     &     *MAX(0. _d 0,
     &       (termP*recip_Rcol(I,J,bi,bj)-sigma_b)/(1. _d 0-sigma_b) )
     &     *COS((yC(I,J,bi,bj)*deg2rad))**4
         gT(i,j,kLev,bi,bj)=gT(i,j,kLev,bi,bj)
     &        - kT*( theta(I,J,kLev,bi,bj)-thetaEq )
     &            *maskC(i,j,kLev,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_S(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
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
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     Loop counters
      INTEGER I, J

C--   Forcing term(s)

      RETURN
      END
