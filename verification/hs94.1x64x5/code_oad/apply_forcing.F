#include "CPP_OPTIONS.h"

C--  File apply_forcing.F:
C--   Contents
C--   o APPLY_FORCING_U
C--   o APPLY_FORCING_V
C--   o APPLY_FORCING_T
C--   o APPLY_FORCING_S

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: APPLY_FORCING_U
C     !INTERFACE:
      SUBROUTINE APPLY_FORCING_U(
     U                     gU_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R APPLY_FORCING_U
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
C     gU_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gU_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i,j       :: Loop counters
      INTEGER i, j
CEOP
      _RL recip_P0g, termP, rFullDepth
      _RL kV, kF, sigma_b

C--   Forcing term
      kF = 1. _d 0/86400. _d 0
      sigma_b = 0.7 _d 0
      rFullDepth = rF(1)-rF(Nr+1)
c     DO j=1,sNy
C-jmc: Without CD-scheme, this is OK ; but with CD-scheme, needs to cover [0:sNy+1]
      DO j=0,sNy+1
       DO i=1,sNx+1
        IF ( maskW(i,j,k,bi,bj).EQ.oneRS ) THEN
         IF ( selectSigmaCoord.EQ.0 ) THEN
          recip_P0g = MAX(recip_Rcol(i,j,bi,bj),recip_Rcol(i-1,j,bi,bj))
          termP = 0.5 _d 0*( MIN( rF(k)*recip_P0g, oneRL )
     &                      +rF(k+1)*recip_P0g )
c         termP = 0.5 _d 0*( rF(k) + rF(k+1) )*recip_P0g
         ELSE
C-- Pressure at U.point :
c         midP = rLowW(i,j,bi,bj) + aHybSigmC(k)*rFullDepth
c    &         + bHybSigmC(k)
c    &          *(etaHw(i,j,bi,bj)+rSurfW(i,j,bi,bj)-rLowW(i,j,bi,bj))
C-- Sigma at U.point :
c         termP = ( midP - rLowW(i,j,bi,bj))
c    &          /(etaHw(i,j,bi,bj)+rSurfW(i,j,bi,bj)-rLowW(i,j,bi,bj))
C-  which simplifies to:
          termP = aHybSigmC(k)*rFullDepth
#ifdef NONLIN_FRSURF
     &          /(etaHw(i,j,bi,bj)+rSurfW(i,j,bi,bj)-rLowW(i,j,bi,bj))
#else
     &          /(rSurfW(i,j,bi,bj)-rLowW(i,j,bi,bj))
#endif
     &          + bHybSigmC(k)
         ENDIF
         kV = kF*MAX( zeroRL, (termP-sigma_b)/(1. _d 0-sigma_b) )
         gU_arr(i,j) = gU_arr(i,j)
     &               - kV*uVel(i,j,k,bi,bj)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: APPLY_FORCING_V
C     !INTERFACE:
      SUBROUTINE APPLY_FORCING_V(
     U                     gV_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R APPLY_FORCING_V
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
C     gV_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gV_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i,j       :: Loop counters
      INTEGER i, j
CEOP
      _RL recip_P0g, termP, rFullDepth
      _RL kV, kF, sigma_b

C--   Forcing term
      kF = 1. _d 0/86400. _d 0
      sigma_b = 0.7 _d 0
      rFullDepth = rF(1)-rF(Nr+1)
      DO j=1,sNy+1
c      DO i=1,sNx
C-jmc: Without CD-scheme, this is OK ; but with CD-scheme, needs to cover [0:sNx+1]
       DO i=0,sNx+1
        IF ( maskS(i,j,k,bi,bj).EQ.oneRS ) THEN
         IF ( selectSigmaCoord.EQ.0 ) THEN
          recip_P0g = MAX(recip_Rcol(i,j,bi,bj),recip_Rcol(i,j-1,bi,bj))
          termP = 0.5 _d 0*( MIN( rF(k)*recip_P0g, oneRL )
     &                      +rF(k+1)*recip_P0g )
c         termP = 0.5 _d 0*( rF(k) + rF(k+1) )*recip_P0g
         ELSE
C-- Pressure at V.point :
c         midP = rLowS(i,j,bi,bj) + aHybSigmC(k)*rFullDepth
c    &         + bHybSigmC(k)
c    &          *(etaHs(i,j,bi,bj)+rSurfS(i,j,bi,bj)-rLowS(i,j,bi,bj))
C-- Sigma at V.point :
c         termP = ( midP - rLowS(i,j,bi,bj))
c    &          /(etaHs(i,j,bi,bj)+rSurfS(i,j,bi,bj)-rLowS(i,j,bi,bj))
C-  which simplifies to:
          termP = aHybSigmC(k)*rFullDepth
#ifdef NONLIN_FRSURF
     &          /(etaHs(i,j,bi,bj)+rSurfS(i,j,bi,bj)-rLowS(i,j,bi,bj))
#else
     &          /(rSurfS(i,j,bi,bj)-rLowS(i,j,bi,bj))
#endif
     &          + bHybSigmC(k)
         ENDIF
         kV = kF*MAX( zeroRL, (termP-sigma_b)/(1. _d 0-sigma_b) )
         gV_arr(i,j) = gV_arr(i,j)
     &               - kV*vVel(i,j,k,bi,bj)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: APPLY_FORCING_T
C     !INTERFACE:
      SUBROUTINE APPLY_FORCING_T(
     U                     gT_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R APPLY_FORCING_T
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
C     gT_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gT_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i,j       :: Loop counters
C     kSurface  :: index of surface level
      INTEGER i, j
CEOP
      _RL thetaLim, kT, ka, ks, sigma_b, term1, term2, thetaEq
      _RL termP, rFullDepth

C--   Forcing term
      ka = 1. _d 0/(40. _d 0*86400. _d 0)
      ks = 1. _d 0/(4. _d 0 *86400. _d 0)
      sigma_b = 0.7 _d 0
      rFullDepth = rF(1)-rF(Nr+1)
      DO j=0,sNy+1
       DO i=0,sNx+1
         term1 = 60. _d 0*(SIN(yC(i,j,bi,bj)*deg2rad)**2)
         termP = 0.5 _d 0*( rF(k) + rF(k+1) )
         term2 = 10. _d 0*LOG(termP/atm_po)
     &            *(COS(yC(i,j,bi,bj)*deg2rad)**2)
         thetaLim = 200. _d 0/ ((termP/atm_po)**atm_kappa)
         thetaEq = 315. _d 0 - term1 - term2
         thetaEq = MAX(thetaLim,thetaEq)
         IF ( selectSigmaCoord.EQ.0 ) THEN
          termP = 0.5 _d 0*( MIN(rF(k),Ro_surf(i,j,bi,bj))
     &                     + rF(k+1) )
     &                    *recip_Rcol(i,j,bi,bj)
         ELSE
C-- Pressure at T.point :
c         midP = R_low(i,j,bi,bj) + aHybSigmC(k)*rFullDepth
c    &         + bHybSigmC(k)
c    &          *(etaH(i,j,bi,bj)+Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj))
C-- Sigma at T.point :
c         termP = ( midP - R_low(i,j,bi,bj))
c    &          /(etaH(i,j,bi,bj)+Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj))
C-  which simplifies to:
          termP = aHybSigmC(k)*rFullDepth
#ifdef NONLIN_FRSURF
     &          /(etaH(i,j,bi,bj)+Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj))
#else
     &          /(Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj))
#endif
     &          + bHybSigmC(k)
         ENDIF
         kT = ka+(ks-ka)
     &      *MAX( zeroRL, (termP-sigma_b)/(1. _d 0-sigma_b) )
     &      *COS((yC(i,j,bi,bj)*deg2rad))**4
         gT_arr(i,j) = gT_arr(i,j)
     &               - kT*( theta(i,j,k,bi,bj)-thetaEq )
     &                *maskC(i,j,k,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: APPLY_FORCING_S
C     !INTERFACE:
      SUBROUTINE APPLY_FORCING_S(
     U                     gS_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R APPLY_FORCING_S
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
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gS_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gS_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i,j       :: Loop counters
c     INTEGER i, j
CEOP

C--   Forcing term

      RETURN
      END
