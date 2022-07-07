#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: CONVECTIVELY_MIXTRACER
C !INTERFACE:
      SUBROUTINE CONVECTIVELY_MIXTRACER(
     I                              bi, bj, k, weightA, weightB,
     U                              Tracer,
     I                              myThid )
C !DESCRIPTION:
C Mixes a tracer over two layers according to the weights pre-calculated
C as a function of stability.
C
C Mixing is represented by:
C                       T(k-1) = T(k-1) + A * ( T(k) - T(k-1) )
C                       T(k)   = T(k)   + B * ( T(k-1) - T(k) )

C !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C !INPUT/OUTPUT PARAMETERS:
C     bi,bj,k :: indices
C     weightA :: weight for level k-1
C     weightB :: weight for level  k
C     myThid  :: my Thread Id number
      INTEGER bi,bj,k
      _RL weightA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL weightB(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

#if (defined INCLUDE_CONVECT_CALL || defined INCLUDE_CONVECT_INI_CALL)

C !LOCAL VARIABLES:
C     i,j      :: Loop counter
C     delTrac  :: Difference between tracer in each layer
      INTEGER i,j
      _RL delTrac
CEOP

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx

        delTrac=Tracer(i,j,k,bi,bj)-Tracer(i,j,k-1,bi,bj)
        Tracer(i,j,k-1,bi,bj)=Tracer(i,j,k-1,bi,bj)
     &                       +weightA(i,j)*delTrac
        Tracer(i,j,k,bi,bj)=Tracer(i,j,k,bi,bj)
     &                       -weightB(i,j)*delTrac

       ENDDO
      ENDDO

#endif /* INCLUDE_CONVECT_CALL or INCLUDE_CONVECT_INI_CALL */

      RETURN
      END
