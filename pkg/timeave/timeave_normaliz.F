C $Header: /u/gcmpack/MITgcm/pkg/timeave/Attic/timeave_normaliz.F,v 1.5 2010/01/03 20:46:12 jmc Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: TIMEAVE_NORMALIZ(
C     !INTERFACE:
      SUBROUTINE TIMEAVE_NORMALIZ(
     U                   fldtave,
     I                   cumulTime, kSize, bi, bj, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE TIMEAVE_NORMALIZ
C     | o Get average of field : Normalize by cumulated time.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     fldtave   :: time averaged Field
C     cumulTime :: cumulated time for average
C     kSize     :: 3rd dimension of both averaged field AND cumulated time arrays
C     bi, bj    :: tile indices
C     myThid    :: my Thread Id number
      INTEGER kSize
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL cumulTime(kSize,nSx,nSy)
      INTEGER bi, bj
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i, j, k   :: Loop counters
      INTEGER i, j, k
CEOP

C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,kSize
         IF ( cumulTime(k,bi,bj) .NE. 0. ) THEN
          DO j=1,sNy
           DO i=1,sNx
             fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj)
     &                            / cumulTime(k,bi,bj)
           ENDDO
          ENDDO
         ENDIF
        ENDDO
C      ENDDO
C     ENDDO

      RETURN
      END
