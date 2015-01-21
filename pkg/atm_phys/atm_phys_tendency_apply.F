C $Header: /u/gcmpack/MITgcm/pkg/atm_phys/atm_phys_tendency_apply.F,v 1.5 2015/01/21 19:24:07 jmc Exp $
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
     U                    gU_arr,
     I                    iMin,iMax,jMin,jMax, k, bi, bj,
     I                    myTime, myIter, myThid )

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
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j
      _RL damp_dKE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      IF ( atmPhys_addTendU ) THEN

C--   Assume Agrid position:
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU_arr(i,j) = gU_arr(i,j)
     &     + maskW(i,j,k,bi,bj)*( atmPhys_dU(i-1,j,k,bi,bj)
     &                           +atmPhys_dU( i ,j,k,bi,bj)
     &                          )*0.5 _d 0
         ENDDO
        ENDDO

       IF ( atmPhys_dampUVfac(k).GT.zeroRL ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU_arr(i,j) = gU_arr(i,j)
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
     &         - halfRL * ( damp_dKE( i, j)*rAw( i, j,bi,bj)
     &                    + damp_dKE(i+1,j)*rAw(i+1,j,bi,bj)
     &                    )*recip_rA(i,j,bi,bj)
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
     U                    gV_arr,
     I                    iMin,iMax,jMin,jMax, k, bi, bj,
     I                    myTime, myIter, myThid )

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
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j
      _RL damp_dKE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      IF ( atmPhys_addTendV ) THEN

C--   Assume Agrid position:
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV_arr(i,j) = gV_arr(i,j)
     &     + maskS(i,j,k,bi,bj)*( atmPhys_dV(i,j-1,k,bi,bj)
     &                           +atmPhys_dV(i, j ,k,bi,bj)
     &                          )*0.5 _d 0
         ENDDO
        ENDDO

       IF ( atmPhys_dampUVfac(k).GT.zeroRL ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV_arr(i,j) = gV_arr(i,j)
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
     &         - halfRL * ( damp_dKE( i, j)*rAs(i, j ,bi,bj)
     &                    + damp_dKE(i,j+1)*rAs(i,j+1,bi,bj)
     &                    )*recip_rA(i,j,bi,bj)
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
     U                    gT_arr,
     I                    iMin,iMax,jMin,jMax, k, bi, bj,
     I                    myTime, myIter, myThid )

C     !DESCRIPTION:
C     Add AtmPhys tendency terms to T (theta) tendency.  Routine works
C     for one level at a time

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
c#include "DYNVARS.h"
#include "ATM_PHYS_PARAMS.h"
#include "ATM_PHYS_VARS.h"

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
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j

      IF ( atmPhys_addTendT ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gT_arr(i,j) = gT_arr(i,j)
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
     U                    gS_arr,
     I                    iMin,iMax,jMin,jMax, k, bi, bj,
     I                    myTime, myIter, myThid )

C     !DESCRIPTION:
C     Add AtmPhys tendency terms to S tendency.  Routine works for one
C     level at a time

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
c#include "DYNVARS.h"
#include "ATM_PHYS_PARAMS.h"
#include "ATM_PHYS_VARS.h"

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
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j

      IF ( atmPhys_addTendS ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gS_arr(i,j) = gS_arr(i,j)
     &     + maskC(i,j,k,bi,bj) * atmPhys_dQ(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF

      RETURN
      END
