C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_cumul_2v.F,v 1.3 2003/05/01 22:17:42 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVE_CUMUL_2V( 
     O   fldtave,
     I   fld1, fld2, Ksize, dir, deltaT,
     I   bi, bj, myThid )
C     /==========================================================*
C     | SUBROUTINE TIMEAVE_CUMUL_2V
C     | o Sum over time a product of two arrays depending on the
C     |   relative position of the 2 fields.
C     |      (tracer point, u, v, w ...)
C     \==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     fldtave - time averaged Field
C     fld1,fld2  - Input Field
C     dir - type of grid for 2nd array relatively to the 1rst array
C     0: same grid ; 1: dX/2 shift ; 2: dY/2 shift ; 3: dr/2 shift
C        (2 digits => also shift the 1rst array)
C     Ksize - 3rd dimension of local arrays (Input and Output fields)
      INTEGER Ksize, dir
      _RL fld1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL fld2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL deltaT
      INTEGER bi, bj, myThid

CEndofinterface

#ifdef ALLOW_TIMEAVE
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
C      ENDDO
C     ENDDO

      ELSEIF ( dir.eq.1 )  THEN
 
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

      ELSEIF ( dir.eq.2 ) THEN
 
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

      ELSEIF ( dir.eq.3 ) THEN
 
        DO k=2,Ksize
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       + .5 * ( fld1(i,j,k-1,bi,bj) + fld1(i,j,k,bi,bj) )
     &                *   fld2(i,j,k,bi,bj)
     &                *   deltaT
          ENDDO
         ENDDO
        ENDDO

      ELSEIF ( dir.eq.12 ) THEN
 
        DO k=1,Ksize
         DO j=1,sNy
          DO i=1,sNx
           fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj)
     &      + .25 _d 0*( fld1(i,j-1,k,bi,bj) + fld1(i,j,k,bi,bj) )
     &                *( fld2(i-1,j,k,bi,bj) + fld2(i,j,k,bi,bj) )
     &                * deltaT
          ENDDO
         ENDDO
        ENDDO

      ENDIF

#endif /* ALLOW_TIMEAVE */

      RETURN
      END
