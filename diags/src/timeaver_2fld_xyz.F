C $Header: /u/gcmpack/MITgcm/diags/src/Attic/timeaver_2fld_xyz.F,v 1.6 2001/02/15 19:01:39 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVER_2FLD_XYZ( fld1, fld2, fldtave, dir, deltaT,
     I   bi, bj, K, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVER_2FLD_XYZ                             |
C     | o Sum over time a product of two arrays depending on the |
C     |   type of the second array (tracer_grid, u_grid, v_grid) |
C     |   The first array must be on tracer_grid                 |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "AVER.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     dir - type of grid for 2nd array (0: tracer, 1: zonal, 2 merid)
C     fld  - Input Field
C     fldtave - time averaged Field
      _RL fld1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL fld2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER dir
      _RL deltaT
      INTEGER bi, bj, K, myThid

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j
 
      IF ( dir.eq.0 ) THEN 

C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
C       DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       +  fld1(i,j,k,bi,bj)*fld2(i,j,k,bi,bj)*deltaT
           ENDDO
          ENDDO
C        ENDDO
C       ENDDO
C      ENDDO

      ELSEIF ( dir.eq.1 )  THEN
 
C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
C       DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       + .5 * ( fld1(i-1,j,k,bi,bj) + fld1(i,j,k,bi,bj) )
     &                *   fld2(i,j,k,bi,bj)
     &                *   deltaT
           ENDDO
          ENDDO
C        ENDDO
C       ENDDO
C      ENDDO

      ELSEIF ( dir.eq.2 ) THEN
 
C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
C       DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       + .5 * ( fld1(i,j-1,k,bi,bj) + fld1(i,j,k,bi,bj) )
     &                *   fld2(i,j,k,bi,bj)
     &                *   deltaT
           ENDDO
          ENDDO
C        ENDDO
C       ENDDO
C      ENDDO

      ENDIF

      RETURN
      END
