C $Header: /u/gcmpack/MITgcm/pkg/iceberg/iceberg_init_varia.F,v 1.14 2015/01/20 14:54:36 dgoldberg Exp $
C $Name: checkpoint65m $

#include "ICEBERG_OPTIONS.h"

CBOP
      SUBROUTINE ICEBERG_INIT_VARIA( myThid )
C     *============================================================*
C     | SUBROUTINE ICEBERG_INIT_VARIA
C     | o Routine to initialize ICEBERG variables.
C     *============================================================*
C     | Initialize ICEBERG parameters and variables.
C     *============================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif /* ALLOW_PTRACERS */
#include "ICEBERG_SIZE.h"
#include "ICEBERG.h"

#ifdef ALLOW_COST
#include "ICEBERG_COST.h"
#endif /* ALLOW_COST */

#ifdef ALLOW_PTRACERS
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#endif /* ALLOW_PTRACERS */

C     === Routine arguments ===
C     myThid -  Number of this instance of ICEBERG_INIT_VARIA
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_ICEBERG
C     === Local variables ===
C     i,j,bi,bj,K - Loop counters
      INTEGER i, j, bi, bj, K, iTracer
CEOP

C     Update the spatially varying version of the drag coefficient
C     brg_CDragFld could be used in iceberg_thermodynamics
C     brg_CDragQuadFld used in iceberg_u/v_drag_coeff
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
          DO K = 1,Nr
           brg_HeatFlux3D      (i,j,K,bi,bj) = 0. _d 0
           brg_FwFlux3D        (i,j,K,bi,bj) = 0. _d 0
           brg_MeltRate3D      (i,j,K,bi,bj) = 0. _d 0
           brg_TendT3D         (i,j,K,bi,bj) = 0. _d 0
           brg_TendS3D         (i,j,K,bi,bj) = 0. _d 0
#ifdef ALLOW_DIAGNOSTICS
            IF ( useDiagnostics ) THEN
             brg_DragU(i,j,k,bi,bj) = 0.
             brg_DragV(i,j,k,bi,bj) = 0.
            ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#ifdef ALLOW_PTRACERS
            DO iTracer = 1,PTRACERS_num
            brg_ptr_Addmass3D(I,J,K,bi,bj,iTracer)
     &                 = 0
            ENDDO
#endif /* ALLOW_PTRACERS */            
          ENDDO !k
         ENDDO !i
        ENDDO !j
       ENDDO !bi
      ENDDO !bj



#endif /* ALLOW_ICEBERG */

      RETURN
      END
