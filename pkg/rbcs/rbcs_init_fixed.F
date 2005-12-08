#include "CPP_OPTIONS.h"
#include "PACKAGES_CONFIG.h"

C !INTERFACE: ==========================================================
      SUBROUTINE RBCS_INIT_FIXED(myThid )

C !DESCRIPTION:
C calls subroutines that initialized fixed variables for relaxed
c boundary conditions

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif
#include "RBCS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_RBCS


C     !LOCAL VARIABLES:
C     i,j,k,bi,bj,iTracer  :: loop indices
      INTEGER i,j,k,bi,bj
#ifdef ALLOW_PTRACERS
      INTEGER iTracer
#endif

C       Loop over tiles
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)

C           Initialize arrays in common blocks :
            DO k=1,Nr
              DO j=1-Oly,sNy+OLy
                DO i=1-Olx,sNx+Olx
                  RBC_mask(i,j,k,bi,bj) = 0. _d 0
                  RBCtemp(i,j,k,bi,bj)    = 0. _d 0
                  RBCsalt(i,j,k,bi,bj) = 0. _d 0
                ENDDO
              ENDDO
             ENDDO
          ENDDO
         ENDDO
#ifdef ALLOW_PTRACERS
C     Loop over tracers
      DO iTracer = 1, PTRACERS_num

C       Loop over tiles
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)

C           Initialize arrays in common blocks :
            DO k=1,Nr
              DO j=1-Oly,sNy+OLy
                DO i=1-Olx,sNx+Olx
                  RBC_ptracers(i,j,k,bi,bj,iTracer) = 0. _d 0
               ENDDO
              ENDDO
            ENDDO
C           end bi,bj loops
          ENDDO
        ENDDO

C       end of Tracer loop
      ENDDO
#endif

C read in mask for relaxing
      IF ( relaxBoundaryFile.NE. ' ' ) THEN
       _BEGIN_MASTER( myThid )
        CALL READ_FLD_XYZ_RS(relaxBoundaryFile,' ', RBC_mask, 0, myThid)
       _END_MASTER(myThid)
      ENDIF
C
      _EXCH_XYZ_R4(RBC_mask, myThid )

      CALL PLOT_FIELD_XYRS( RBC_mask, 'Boundary Relaxing' ,
     &                       1, myThid )

#endif /* ALLOW_RBCS */

      RETURN
      END
