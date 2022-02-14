#include "SHELFICE_OPTIONS.h"

CBOP
C     !ROUTINE: SHELFICE_INIT_DEPTHS
C     !INTERFACE:
      SUBROUTINE SHELFICE_INIT_DEPTHS(
     U                    rLowC, rSurfC,
     I                    myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_INIT_DEPTHS
C     | o Modify ocean upper boundary position according to
C     |   ice-shelf topography
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif /* ALLOW_SHELFICE */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     rLowC     :: base of fluid column in r_unit at grid-cell center
C     rSurfC    :: surface reference position (r_unit) at grid-cell center
C     myThid    :: my Thread Id number
      _RS rLowC  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rSurfC (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_SHELFICE
C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi, bj    :: tile indices
C     i, j      :: Loop counters
      INTEGER bi, bj
      INTEGER i, j
CEOP

C--   Initialize R_shelfIce
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          R_shelfIce(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( SHELFICEtopoFile .NE. ' ' ) THEN
       _BARRIER
C-    Read the shelfIce draught using the mid-level I/O pacakage read_write_rec
C     The 0 is the "iteration" argument. The 1 is the record number.
       CALL READ_REC_XY_RS( SHELFICEtopoFile, R_shelfIce,
     &                      1, 0, myThid )
C-    Read the shelfIce draught using the mid-level I/O pacakage read_write_fld
C     The 0 is the "iteration" argument. The ' ' is an empty suffix
C      CALL READ_FLD_XY_RS( SHELFICEtopoFile, ' ', R_shelfIce,
C    &                      0, myThid )
C-     end setup R_shelfIce in the interior
      ENDIF

#ifdef ALLOW_SHELFICE_REMESHING
      IF ( SHELFICEremeshFrequency.GT.zeroRL .AND.
     &     ( nIter0.NE.0 .OR. startTime.NE.baseTime
     &                   .OR. pickupSuff.NE.' ') ) THEN
        CALL SHELFICE_READ_PICKUP( 0, nIter0, myThid )
      ENDIF
#endif /* ALLOW_SHELFICE_REMESHING */

C- fill in the overlap (+ BARRIER):
      _EXCH_XY_RS( R_shelfIce, myThid )

C--   Modify ocean upper boundary position according to ice-shelf topography
      IF ( usingZCoords ) THEN
        DO bj=myByLo(myThid), myByHi(myThid)
         DO bi=myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            rSurfC(i,j,bi,bj) =
     &        MIN( rSurfC(i,j,bi,bj), R_shelfIce(i,j,bi,bj) )
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ELSE
        STOP 'SHELFICE_INIT_DEPTHS: Missing code for P-coords'
      ENDIF

#endif /* ALLOW_SHELFICE */

      RETURN
      END
