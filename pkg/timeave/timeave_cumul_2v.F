C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_cumul_2v.F,v 1.1 2001/03/06 15:50:09 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVE_CUMUL_2V( 
     O   fldtave,
     I   fld1, fld2, Ksize, dir, deltaT,
     I   bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVE_CUMUL_2V                              |
C     | o Sum over time a product of two arrays depending on the |
C     |   type of the second array (tracer_grid, u_grid, v_grid) |
C     |   The first array must be on tracer_grid                 |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     fldtave - time averaged Field
C     fld1,fld2  - Input Field
C     dir - type of grid for 2nd array (0: tracer, 1: zonal, 2 merid)
C     Ksize - 3rd dimension of local arrays (Input and Output fields)
      INTEGER Ksize, dir
      _RL fld1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL fld2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL deltaT
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
             fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       +  fld1(i,j,k,bi,bj)*fld2(i,j,k,bi,bj)*deltaT
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
             fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       + .5 * ( fld1(i-1,j,k,bi,bj) + fld1(i,j,k,bi,bj) )
     &                *   fld2(i,j,k,bi,bj)
     &                *   deltaT
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
             fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       + .5 * ( fld1(i,j-1,k,bi,bj) + fld1(i,j,k,bi,bj) )
     &                *   fld2(i,j,k,bi,bj)
     &                *   deltaT
           ENDDO
          ENDDO
         ENDDO
C       ENDDO
C      ENDDO

      ENDIF

      RETURN
      END
