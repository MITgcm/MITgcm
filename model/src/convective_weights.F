#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: CONVECTIVE_WEIGHTS
C !INTERFACE:
      SUBROUTINE CONVECTIVE_WEIGHTS(
     I                              bi, bj, k, rhoKm1, rhoK,
     O                              weightA, weightB, convectCount,
     I                              myThid )

C !DESCRIPTION:
C Calculates the weights used to represent convective mixing
C between two layers.
C
C Mixing is represented by:
C                       T(k-1) = T(k-1) + A * ( T(k) - T(k-1) )
C                       T(k)   = T(k)   + B * ( T(k-1) - T(k) )
C
C In the stable case, A = B = 0
C In the unstable case, A and B are non-zero and are chosen so as to
C conserve total volume of T.

C !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT/OUTPUT PARAMETERS:
C     bi,bj,k :: indices
C     rhoKm1  :: rho in level k-1
C     rhoK    :: rho in level  k
C     weightA :: weight for tracer @ level k-1
C     weightB :: weight for tracer @ level  k
C     convectCount :: counter to diagnose where convection occurs
C     myThid  :: my Thread Id number
      INTEGER bi,bj,k
      _RL rhoKm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoK   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL weightA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL weightB(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL convectCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

#if (defined INCLUDE_CONVECT_CALL || defined INCLUDE_CONVECT_INI_CALL)

C !LOCAL VARIABLES:
C     i,j      :: Loop counter
C     dS,d1,d2 :: Layer thicknesses
      INTEGER i,j
      _RL dS,d1,d2
CEOP

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         IF ( _hFacC(i,j,k-1,bi,bj)* _hFacC(i,j,k,bi,bj) .GT. 0. .AND.
     &        (rhok(i,j)-rhokm1(i,j))*rkSign*gravitySign .LT. 0.
     &      ) THEN

C      Where statically unstable, mix the tracers conserving volume
          d1 = _hFacC(i,j,k-1,bi,bj)*drF(k-1)
          d2 = _hFacC(i,j, k ,bi,bj)*drF( k )
          dS = d1+d2
          weightA(i,j) = d2/dS
          weightB(i,j) = d1/dS
          convectCount(i,j,k) = 1.
         ELSE
C      Where statically stable, do nothing
          weightA(i,j) = 0.
          weightB(i,j) = 0.
          convectCount(i,j,k) = 0.
         ENDIF
       ENDDO
      ENDDO

#endif /* INCLUDE_CONVECT_CALL or INCLUDE_CONVECT_INI_CALL */

      RETURN
      END
