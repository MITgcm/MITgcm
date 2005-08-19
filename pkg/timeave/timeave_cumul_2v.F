C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_cumul_2v.F,v 1.7 2005/08/19 22:50:26 heimbach Exp $
C $Name:  $
#include "TIMEAVE_OPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVE_CUMUL_2V( 
     O   fldtave,
     I   fld1, fld2, Ksize, dir, deltaTloc,
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
#include "GRID.h"

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
      _RL deltaTloc
      INTEGER bi, bj, myThid

CEndofinterface

#ifdef ALLOW_TIMEAVE
C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j, k
      INTEGER km1
 
      IF ( dir.eq.0 ) THEN 
c-    both fields at the same location :

C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Ksize
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       +  fld1(i,j,k,bi,bj)*fld2(i,j,k,bi,bj)*deltaTloc
          ENDDO
         ENDDO
        ENDDO
C      ENDDO
C     ENDDO

      ELSEIF ( dir.eq.1 )  THEN
c-    2nd field shifted by -dX/2 (e.g.: 1=T, 2=U) :
 
        DO k=1,Ksize
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       + .5 * ( fld1(i-1,j,k,bi,bj) + fld1(i,j,k,bi,bj) )
     &                *   fld2(i,j,k,bi,bj)
     &                *   deltaTloc
          ENDDO
         ENDDO
        ENDDO

      ELSEIF ( dir.eq.2 ) THEN
c-    2nd field shifted by -dY/2 (e.g.: 1=T, 2=V) :
 
        DO k=1,Ksize
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       + .5 * ( fld1(i,j-1,k,bi,bj) + fld1(i,j,k,bi,bj) )
     &                *   fld2(i,j,k,bi,bj)
     &                *   deltaTloc
          ENDDO
         ENDDO
        ENDDO

      ELSEIF ( dir.eq.3 ) THEN
c-    2nd field shifted by -dR/2 (e.g.: 1=T, 2=W) :
 
        DO k=1,Ksize
         km1 = MAX(k-1,1)
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       + .5 * ( fld1(i,j,km1,bi,bj) + fld1(i,j,k,bi,bj) )
     &                *   fld2(i,j,k,bi,bj)
     &                *   deltaTloc
          ENDDO
         ENDDO
        ENDDO

      ELSEIF ( dir.eq.12 ) THEN
c-    1rst & 2nd fields shifted by -dY/2 & -dX/2 
c           (e.g.: 1=U, 2=V, product at the corner) :
 
        DO k=1,Ksize
         DO j=1,sNy
          DO i=1,sNx
           fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj)
     &      + .25 _d 0*( fld1(i,j-1,k,bi,bj) + fld1(i,j,k,bi,bj) )
     &                *( fld2(i-1,j,k,bi,bj) + fld2(i,j,k,bi,bj) )
     &                * deltaTloc
          ENDDO
         ENDDO
        ENDDO

      ELSEIF ( dir.eq.13 ) THEN
c-    1rst & 2nd fields shifted by -dR/2 & -dX/2 (e.g.: 1=U, 2=W):
 
        DO k=1,Ksize
         km1 = MAX(k-1,1)
         DO j=1,sNy
          DO i=1,sNx
           fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj)
     &      + .25 _d 0*( fld1(i,j,km1,bi,bj) + fld1(i,j,k,bi,bj) )
     &                *( fld2(i-1,j,k,bi,bj)*rA(i-1,j,bi,bj)
     &                  +fld2( i ,j,k,bi,bj)*rA( i ,j,bi,bj)
     &                 )*recip_rAw(i,j,bi,bj)
     &                * deltaTloc
          ENDDO
         ENDDO
        ENDDO

      ELSEIF ( dir.eq.23 ) THEN
c-    1rst & 2nd fields shifted by -dR/2 & -dY/2 (e.g.: 1=V, 2=W):
 
        DO k=1,Ksize
         km1 = MAX(k-1,1)
         DO j=1,sNy
          DO i=1,sNx
           fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj)
     &      + .25 _d 0*( fld1(i,j,km1,bi,bj) + fld1(i,j,k,bi,bj) )
     &                *( fld2(i,j-1,k,bi,bj)*rA(i,j-1,bi,bj)
     &                  +fld2(i, j ,k,bi,bj)*rA(i, j ,bi,bj)
     &                 )*recip_rAs(i,j,bi,bj)
     &                * deltaTloc
          ENDDO
         ENDDO
        ENDDO

      ELSEIF ( dir.eq.-13 ) THEN
c-    gradient of the 1rst field * 2nd fields, shifted by -dR/2 & -dX/2 resp.
c-    (e.g.: used for advective form of vertical advection: w.du/dr)
 
        DO k=2,Ksize
         DO j=1,sNy
          DO i=1,sNx
           fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj)
     &       + .5 _d 0*( fld1(i,j,k-1,bi,bj) - fld1(i,j,k,bi,bj) )
     &                *( fld2(i-1,j,k,bi,bj)*rA(i-1,j,bi,bj)
     &                  +fld2( i ,j,k,bi,bj)*rA( i ,j,bi,bj)
     &                 )*recip_rAw(i,j,bi,bj)
     &                * deltaTloc
          ENDDO
         ENDDO
        ENDDO

      ELSEIF ( dir.eq.-23 ) THEN
c-    gradient of the 1rst field * 2nd fields, shifted by -dR/2 & -dY/2 resp.
c-    (e.g.: used for advective form of vertical advection: w.dv/dr)
 
        DO k=2,Ksize
         DO j=1,sNy
          DO i=1,sNx
           fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj)
     &       + .5 _d 0*( fld1(i,j,k-1,bi,bj) - fld1(i,j,k,bi,bj) )
     &                *( fld2(i,j-1,k,bi,bj)*rA(i,j-1,bi,bj)
     &                  +fld2(i, j ,k,bi,bj)*rA(i, j ,bi,bj)
     &                 )*recip_rAs(i,j,bi,bj)
     &                * deltaTloc
          ENDDO
         ENDDO
        ENDDO

      ENDIF

#endif /* ALLOW_TIMEAVE */

      RETURN
      END
