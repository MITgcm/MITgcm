C $Header: /u/gcmpack/MITgcm/verification/hs94.cs-32x32x5/code/Attic/external_forcing.F,v 1.7 2010/09/24 20:43:35 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: EXTERNAL_FORCING_U
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_U(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXTERNAL_FORCING_U
C     | o Contains problem specific forcing for zonal velocity.
C     *==========================================================*
C     | Adds terms to gU for forcing by external sources
C     | e.g. wind stress, bottom friction etc ...
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     bi,bj     :: Current tile indices
C     kLev      :: Current vertical level index
C     myTime    :: Current time in simulation
C     myThid    :: Thread Id number
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myTime
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j       :: Loop counters
      INTEGER i, j
CEOP
      _RL recip_P0g, termP, rFullDepth
      _RL kV, kF, sigma_b

C--   Forcing term(s)
      kF=1. _d 0/86400. _d 0
      sigma_b = 0.7 _d 0
      rFullDepth = rF(1)-rF(Nr+1)
c     DO j=1,sNy
C-jmc: Without CD-scheme, this is OK ; but with CD-scheme, needs to cover [0:sNy+1]
      DO j=0,sNy+1
       DO i=1,sNx+1
        IF ( hFacW(i,j,kLev,bi,bj) .GT. 0. ) THEN
         IF ( selectSigmaCoord.EQ.0 ) THEN
          recip_P0g=MAX(recip_Rcol(i,j,bi,bj),recip_Rcol(i-1,j,bi,bj))
          termP=0.5 _d 0*( MIN(rF(kLev)*recip_P0g,1. _d 0)
     &                    +rF(kLev+1)*recip_P0g )
         ELSE
C-- Pressure at U.point :
c         midP = rLowW(i,j,bi,bj) + aHybSigmC(k)*rFullDepth
c    &         + bHybSigmC(k)
c    &          *(etaHw(i,j,bi,bj)+rSurfW(i,j,bi,bj)-rLowW(i,j,bi,bj))
C-- Sigma at U.point :
c         termP = ( midP - rLowW(i,j,bi,bj))
c    &          /(etaHw(i,j,bi,bj)+rSurfW(i,j,bi,bj)-rLowW(i,j,bi,bj))
C-  which simplifies to:
          termP = aHybSigmC(kLev)*rFullDepth
     &          /(etaHw(i,j,bi,bj)+rSurfW(i,j,bi,bj)-rLowW(i,j,bi,bj))
     &          + bHybSigmC(kLev)
         ENDIF
         kV=kF*MAX( 0. _d 0, (termP-sigma_b)/(1. _d 0-sigma_b) )
         gU(i,j,kLev,bi,bj)=gU(i,j,kLev,bi,bj)
     &                     -kV*uVel(i,j,kLev,bi,bj)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: EXTERNAL_FORCING_V
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_V(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXTERNAL_FORCING_V
C     | o Contains problem specific forcing for merid velocity.
C     *==========================================================*
C     | Adds terms to gV for forcing by external sources
C     | e.g. wind stress, bottom friction etc ...
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     bi,bj     :: Current tile indices
C     kLev      :: Current vertical level index
C     myTime    :: Current time in simulation
C     myThid    :: Thread Id number
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myTime
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j       :: Loop counters
      INTEGER i, j
CEOP
      _RL recip_P0g, termP, rFullDepth
      _RL kV, kF, sigma_b

C--   Forcing term(s)
      kF=1. _d 0/86400. _d 0
      sigma_b = 0.7 _d 0
      rFullDepth = rF(1)-rF(Nr+1)
      DO j=1,sNy+1
c      DO i=1,sNx
C-jmc: Without CD-scheme, this is OK ; but with CD-scheme, needs to cover [0:sNx+1]
       DO i=0,sNx+1
        IF ( hFacS(i,j,kLev,bi,bj) .GT. 0. ) THEN
         IF ( selectSigmaCoord.EQ.0 ) THEN
          recip_P0g=MAX(recip_Rcol(i,j,bi,bj),recip_Rcol(i,j-1,bi,bj))
          termP=0.5 _d 0*( MIN(rF(kLev)*recip_P0g,1. _d 0)
     &                    +rF(kLev+1)*recip_P0g )
         ELSE
C-- Pressure at V.point :
c         midP = rLowS(i,j,bi,bj) + aHybSigmC(k)*rFullDepth
c    &         + bHybSigmC(k)
c    &          *(etaHs(i,j,bi,bj)+rSurfS(i,j,bi,bj)-rLowS(i,j,bi,bj))
C-- Sigma at V.point :
c         termP = ( midP - rLowS(i,j,bi,bj))
c    &          /(etaHs(i,j,bi,bj)+rSurfS(i,j,bi,bj)-rLowS(i,j,bi,bj))
C-  which simplifies to:
          termP = aHybSigmC(kLev)*rFullDepth
     &          /(etaHs(i,j,bi,bj)+rSurfS(i,j,bi,bj)-rLowS(i,j,bi,bj))
     &          + bHybSigmC(kLev)
         ENDIF
         kV=kF*MAX( 0. _d 0, (termP-sigma_b)/(1. _d 0-sigma_b) )
         gV(i,j,kLev,bi,bj)=gV(i,j,kLev,bi,bj)
     &                      -kV*vVel(i,j,kLev,bi,bj)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: EXTERNAL_FORCING_T
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_T(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXTERNAL_FORCING_T
C     | o Contains problem specific forcing for temperature.
C     *==========================================================*
C     | Adds terms to gT for forcing by external sources
C     | e.g. heat flux, climatalogical relaxation, etc ...
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     bi,bj     :: Current tile indices
C     kLev      :: Current vertical level index
C     myTime    :: Current time in simulation
C     myThid    :: Thread Id number
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myTime
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j       :: Loop counters
      INTEGER i, j
CEOP
      _RL thetaLim,kT,ka,ks,sigma_b,term1,term2,thetaEq
      _RL termP, rFullDepth

C--   Forcing term(s)
      ka=1. _d 0/(40. _d 0*86400. _d 0)
      ks=1. _d 0/(4. _d 0 *86400. _d 0)
      sigma_b = 0.7 _d 0
      rFullDepth = rF(1)-rF(Nr+1)
      DO j=1,sNy
       DO i=1,sNx
         term1=60. _d 0*(SIN(yC(i,j,bi,bj)*deg2rad)**2)
         termP=0.5 _d 0*( rF(kLev) + rF(kLev+1) )
         term2=10. _d 0*LOG(termP/atm_po)
     &            *(COS(yC(i,j,bi,bj)*deg2rad)**2)
         thetaLim = 200. _d 0/ ((termP/atm_po)**atm_kappa)
         thetaEq=315. _d 0-term1-term2
         thetaEq=MAX(thetaLim,thetaEq)
         IF ( selectSigmaCoord.EQ.0 ) THEN
          termP=0.5 _d 0*( MIN(rF(kLev),Ro_surf(i,j,bi,bj))+rF(kLev+1) )
     &                  *recip_Rcol(i,j,bi,bj)
         ELSE
C-- Pressure at T.point :
c         midP = R_low(i,j,bi,bj) + aHybSigmC(k)*rFullDepth
c    &         + bHybSigmC(k)
c    &          *(etaH(i,j,bi,bj)+Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj))
C-- Sigma at T.point :
c         termP = ( midP - R_low(i,j,bi,bj))
c    &          /(etaH(i,j,bi,bj)+Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj))
C-  which simplifies to:
          termP = aHybSigmC(kLev)*rFullDepth
     &          /(etaH(i,j,bi,bj)+Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj))
     &          + bHybSigmC(kLev)
         ENDIF
         kT=ka+(ks-ka)
     &     *MAX(0. _d 0, (termP-sigma_b)/(1. _d 0-sigma_b) )
     &     *COS((yC(i,j,bi,bj)*deg2rad))**4
         gT(i,j,kLev,bi,bj)=gT(i,j,kLev,bi,bj)
     &        - kT*( theta(i,j,kLev,bi,bj)-thetaEq )
     &            *maskC(i,j,kLev,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: EXTERNAL_FORCING_S
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_S(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXTERNAL_FORCING_S
C     | o Contains problem specific forcing for merid velocity.
C     *==========================================================*
C     | Adds terms to gS for forcing by external sources
C     | e.g. fresh-water flux, climatalogical relaxation, etc ...
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     bi,bj     :: Current tile indices
C     kLev      :: Current vertical level index
C     myTime    :: Current time in simulation
C     myThid    :: Thread Id number
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myTime
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j       :: Loop counters
c     INTEGER i, j
CEOP

C--   Forcing term(s)

      RETURN
      END
