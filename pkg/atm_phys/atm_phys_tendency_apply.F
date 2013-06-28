C $Header: /u/gcmpack/MITgcm/pkg/atm_phys/atm_phys_tendency_apply.F,v 1.2 2013/06/28 21:25:25 jmc Exp $
C $Name:  $

#include "ATM_PHYS_OPTIONS.h"
C--  File atm_phys_tendency_apply.F: Routines to apply ATM_PHYS tendencies
C--   Contents
C--   o ATM_PHYS_TENDENCY_APPLY_U
C--   o ATM_PHYS_TENDENCY_APPLY_V
C--   o ATM_PHYS_TENDENCY_APPLY_T
C--   o ATM_PHYS_TENDENCY_APPLY_S

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ATM_PHYS_TENDENCY_APPLY_U

C !INTERFACE:
      SUBROUTINE ATM_PHYS_TENDENCY_APPLY_U(
     &                     iMin, iMax, jMin, jMax,
     &                     bi, bj, k, myTime, myThid )

C     !DESCRIPTION:
C     Add AtmPhys tendency terms to U tendency.  Routine works for one
C     level at a time. Assumes that U and V tendencies are on A-Grid

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "ATM_PHYS_PARAMS.h"
#include "ATM_PHYS_VARS.h"

C     !INPUT PARAMETERS:
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj, myThid
      _RL myTime
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j
      _RL damp_dKE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      IF ( atmPhys_addTendU ) THEN

C--   Assume Agrid position:
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)
     &     + maskW(i,j,k,bi,bj)*( atmPhys_dU(i-1,j,k,bi,bj)
     &                           +atmPhys_dU( i ,j,k,bi,bj)
     &                          )*0.5 _d 0
         ENDDO
        ENDDO

       IF ( atmPhys_dampUVfac(k).GT.zeroRL ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)
     &     - maskW(i,j,k,bi,bj)*uVel(i,j,k,bi,bj)*atmPhys_dampUVfac(k)
          damp_dKE(i,j) =
     &     - uVel(i,j,k,bi,bj)*uVel(i,j,k,bi,bj)*atmPhys_dampUVfac(k)
     &                       *hFacW(i,j,k,bi,bj)*drF(k)*rUnit2mass
         ENDDO
        ENDDO
#ifdef ALLOW_FRICTION_HEATING
        IF ( addFrictionHeating ) THEN
         DO j=1,sNy
          DO i=1,sNx
           frictionHeating(i,j,k,bi,bj) = frictionHeating(i,j,k,bi,bj)
     &        -0.5 _d 0 * ( damp_dKE( i, j)*rAw( i, j,bi,bj)
     &                    + damp_dKE(i+1,j)*rAw(i+1,j,bi,bj) )
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_FRICTION_HEATING */
       ENDIF

      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ATM_PHYS_TENDENCY_APPLY_V

C !INTERFACE:
      SUBROUTINE ATM_PHYS_TENDENCY_APPLY_V(
     &                     iMin, iMax, jMin, jMax,
     &                     bi, bj, k, myTime, myThid )

C     !DESCRIPTION:
C     Add AtmPhys tendency terms to V tendency.  Routine works for one
C     level at a time. Assumes that U and V tendencies are on A-Grid

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "ATM_PHYS_PARAMS.h"
#include "ATM_PHYS_VARS.h"

C     !INPUT PARAMETERS:
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj, myThid
      _RL myTime
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j
      _RL damp_dKE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      IF ( atmPhys_addTendV ) THEN

C--   Assume Agrid position:
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)
     &     + maskS(i,j,k,bi,bj)*( atmPhys_dV(i,j-1,k,bi,bj)
     &                           +atmPhys_dV(i, j ,k,bi,bj)
     &                          )*0.5 _d 0
         ENDDO
        ENDDO

       IF ( atmPhys_dampUVfac(k).GT.zeroRL ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)
     &     - maskS(i,j,k,bi,bj)*vVel(i,j,k,bi,bj)*atmPhys_dampUVfac(k)
          damp_dKE(i,j) =
     &     - vVel(i,j,k,bi,bj)*vVel(i,j,k,bi,bj)*atmPhys_dampUVfac(k)
     &                        *hFacS(i,j,k,bi,bj)*drF(k)*rUnit2mass
         ENDDO
        ENDDO
#ifdef ALLOW_FRICTION_HEATING
        IF ( addFrictionHeating ) THEN
         DO j=1,sNy
          DO i=1,sNx
           frictionHeating(i,j,k,bi,bj) = frictionHeating(i,j,k,bi,bj)
     &        -0.5 _d 0 * ( damp_dKE( i, j)*rAs(i, j ,bi,bj)
     &                    + damp_dKE(i+1,j)*rAs(i,j+1,bi,bj) )
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_FRICTION_HEATING */
       ENDIF

      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ATM_PHYS_TENDENCY_APPLY_T

C !INTERFACE:
      SUBROUTINE ATM_PHYS_TENDENCY_APPLY_T(
     &                     iMin, iMax, jMin, jMax,
     &                     bi, bj, k, myTime, myThid )

C     !DESCRIPTION:
C     Add AtmPhys tendency terms to T (theta) tendency.  Routine works
C     for one level at a time

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "ATM_PHYS_PARAMS.h"
#include "ATM_PHYS_VARS.h"

C     !INPUT PARAMETERS:
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj, myThid
      _RL myTime
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j

      IF ( atmPhys_addTendT ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gT(i,j,k,bi,bj) = gT(i,j,k,bi,bj)
     &     + maskC(i,j,k,bi,bj) * atmPhys_dT(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ATM_PHYS_TENDENCY_APPLY_S

C !INTERFACE:
      SUBROUTINE ATM_PHYS_TENDENCY_APPLY_S(
     &                     iMin, iMax, jMin, jMax,
     &                     bi, bj, k, myTime, myThid )

C     !DESCRIPTION:
C     Add AtmPhys tendency terms to S tendency.  Routine works for one
C     level at a time

C     !INPUT PARAMETERS:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "ATM_PHYS_PARAMS.h"
#include "ATM_PHYS_VARS.h"

C     !INPUT PARAMETERS:
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj, myThid
      _RL myTime
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j

      IF ( atmPhys_addTendS ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gS(i,j,k,bi,bj) = gS(i,j,k,bi,bj)
     &     + maskC(i,j,k,bi,bj) * atmPhys_dQ(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF

      RETURN
      END
