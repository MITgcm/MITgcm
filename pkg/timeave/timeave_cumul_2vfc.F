C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_cumul_2vfc.F,v 1.4 2005/08/19 22:50:26 heimbach Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVE_CUMUL_2VFC( 
     O   fldtave,
     I   fld1, fld2, fld3, Ksize, dir, deltaTloc,
     I   bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVE_CUMUL_2VFC                            |
C     | o Sum over time a product of 2 fields and 1 factor array |
C     |   depending on the relative position on the grid of the  |
C     |    2nd field & factor (tracer_grid, u_grid, v_grid).     |
C     |   The first array is on tracer_grid or at same position  |
C     |    as the 2 others ; 2nd and 3rd always at same position |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     fldtave - time averaged Field
C     fld1,fld2 - Input fields
C     fld3  - Input factor
C     dir - type of grid for 2nd field and factor, relatively to the 1rst :
C         0: same location ; 1: zonal,West ;  2: merid, South 
C     Ksize - 3rd dimension of local arrays (Input and Output fields)
      INTEGER Ksize, dir
      _RL fld1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL fld2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RS fld3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL deltaTloc
      INTEGER bi, bj, myThid

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j, k
 
      IF ( dir.eq.0 ) THEN 

C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Ksize
         DO j=1,sNy
          DO i=1,sNx
            fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj) + deltaTloc*
     &         fld1(i,j,k,bi,bj)*fld2(i,j,k,bi,bj)*fld3(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
C       ENDDO
C      ENDDO

      ELSEIF ( dir.eq.1 )  THEN
 
C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Ksize
         DO j=1,sNy
          DO i=1,sNx
            fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj) + deltaTloc*
     &         0.5*( fld1(i-1,j,k,bi,bj) + fld1(i,j,k,bi,bj) )
     &            *fld2(i,j,k,bi,bj)*fld3(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
C       ENDDO
C      ENDDO

      ELSEIF ( dir.eq.2 ) THEN
 
C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Ksize
         DO j=1,sNy
          DO i=1,sNx
            fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj) + deltaTloc*
     &         0.5*( fld1(i,j-1,k,bi,bj) + fld1(i,j,k,bi,bj) )
     &            *fld2(i,j,k,bi,bj)*fld3(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
C       ENDDO
C      ENDDO

      ENDIF

      RETURN
      END
