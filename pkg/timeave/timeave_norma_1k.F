C $Header: /u/gcmpack/MITgcm/pkg/timeave/Attic/timeave_norma_1k.F,v 1.1 2001/03/06 15:47:31 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVER_NORMA_1K( fldtave, timeave, 
     &                               bi, bj, K, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVE_NORMA_1K                              |
C     | o apply to 1 level of a 3D array : Normalize by          |
C     |   cumulated time                                         |
C     | (replace "timeaver_final_XYZ" and use the same argments) |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     timeave - time averaged
C     fldtave - time averaged Field
      INTEGER bi, bj, K, myThid
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL timeave(Nr,nSx,nSy)

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j
 
C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
C       DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           IF ( timeave(k,bi,bj) .NE. 0. ) THEN
             fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj) / 
     &                              timeave(k,bi,bj)
           ENDIF
          ENDDO
         ENDDO
C       ENDDO
C      ENDDO
C     ENDDO
 
      RETURN
      END
