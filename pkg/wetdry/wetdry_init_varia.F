#include "WETDRY_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: WETDRY_INIT_VARIA

C !INTERFACE:
      SUBROUTINE WETDRY_INIT_VARIA( myThid )

C     !DESCRIPTION:
C     Initialize pkg/wetdry variables and apply the film floor to the
C     initial free surface. The initial face mask is computed later from
C     etaH (S/R WETDRY_UPDATE_MASKS, called from INITIALISE_VARIA just
C     before UPDATE_R_STAR).

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "WETDRY.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_WETDRY
C     !LOCAL VARIABLES:
      INTEGER i, j, bi, bj, ks

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          wdMaskW(i,j,bi,bj) = oneRS
          wdMaskS(i,j,bi,bj) = oneRS
          wdDryC (i,j,bi,bj) = zeroRS
C-    no Qsw seen yet: the first WETDRY_DRY_FORCING call takes Qsw as is
          wdQswIn (i,j,bi,bj) = zeroRS
          wdQswOut(i,j,bi,bj) = -1. _d 30
          wdRelaxW(i,j,bi,bj) = oneRL
          wdRelaxS(i,j,bi,bj) = oneRL
          wdAdvFacW(i,j,bi,bj) = oneRL
          wdAdvFacS(i,j,bi,bj) = oneRL
          wdOutFac(i,j,bi,bj) = oneRL
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _BEGIN_MASTER(myThid)
      wdVolAdj     = 0. _d 0
      wdVolAdjStep = 0. _d 0
      wdCumIn      = 0. _d 0
      wdNflips     = 0
      wdNlimit     = 0
      _END_MASTER(myThid)
      _BARRIER

#ifdef NONLIN_FRSURF
C--   Surface-level NLFS: INI_NLFS_VARS keeps the surface above the bed
C     of shallower neighbours (+hFacInf*drF), which would stop a channel
C     from draining below the level of the adjacent flats. With wetdry the
C     faces take care of that: keep only the column's own limit.
      IF ( select_rStar.EQ.0 .AND. nonlinFreeSurf.GT.0 ) THEN
       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           ks = kSurfC(i,j,bi,bj)
           IF ( ks.LE.Nr ) Rmin_surf(i,j,bi,bj) =
     &       MAX( rF(ks+1), R_low(i,j,bi,bj) ) + hFacInf*drF(ks)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif /* NONLIN_FRSURF */

C--   Initial eta can be below the bed (e.g. uniform eta over a beach):
C     raise it to the film
      CALL WETDRY_APPLY_FLOOR( .FALSE., startTime, nIter0, myThid )

C--   Upwind side of each face (wdUpwindFace) for the initial mask
C     call: from the initial velocity; replaced by the pickup on restart
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          ks = 1
          IF ( select_rStar.EQ.0 ) ks = MIN( kSurfW(i,j,bi,bj), Nr )
          wdUpW(i,j,bi,bj) = 0. _d 0
          IF ( uVel(i,j,ks,bi,bj).GT.zeroRL ) wdUpW(i,j,bi,bj) = 1.
          IF ( uVel(i,j,ks,bi,bj).LT.zeroRL ) wdUpW(i,j,bi,bj) = -1.
          ks = 1
          IF ( select_rStar.EQ.0 ) ks = MIN( kSurfS(i,j,bi,bj), Nr )
          wdUpS(i,j,bi,bj) = 0. _d 0
          IF ( vVel(i,j,ks,bi,bj).GT.zeroRL ) wdUpS(i,j,bi,bj) = 1.
          IF ( vVel(i,j,ks,bi,bj).LT.zeroRL ) wdUpS(i,j,bi,bj) = -1.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Restart: restore the volume-budget scalars and the upwind sides
C     (masks are recomputed)
      IF ( .NOT.( startTime.EQ.baseTime .AND. nIter0.EQ.0
     &            .AND. pickupSuff.EQ.' ' ) ) THEN
        CALL WETDRY_READ_PICKUP( nIter0, myThid )
      ENDIF

#endif /* ALLOW_WETDRY */

      RETURN
      END
