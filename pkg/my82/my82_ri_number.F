C $Header: /u/gcmpack/MITgcm/pkg/my82/my82_ri_number.F,v 1.5 2009/01/20 00:26:04 jmc Exp $
C $Name:  $

#include "MY82_OPTIONS.h"

CBOP
C !ROUTINE: MY82_RI_NUMBER

C !INTERFACE: ==========================================================
      subroutine MY82_RI_NUMBER(
     I     bi, bj, K, iMin, iMax, jMin, jMax,
     O     RiNumber, buoyFreq, vertShear,
     I     myTime, myThid )

C !DESCRIPTION: \bv
C     /==========================================================\
C     | SUBROUTINE MY82_RI_NUMBER                                |
C     | o Compute gradient Richardson number for Mellor and      |
C     |   Yamada (1981) turbulence model                         |
C     \==========================================================/
      IMPLICIT NONE
c
c-------------------------------------------------------------------------

c \ev

C !USES: ===============================================================
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "MY82.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C Routine arguments
C     bi, bj - array indices on which to apply calculations
C     iMin, iMax, jMin, jMax
C            - array boundaries
C     k      - depth level
C     myTime - Current time in simulation
C     RiNumber  - (output) Richardson number
C     buoyFreq  - (output) (neg.) buoyancy frequency -N^2
C     vertShear - (output) vertical shear of velocity
      INTEGER bi, bj, iMin, iMax, jMin, jMax, k
      INTEGER myThid
      _RL     myTime
      _RL     RiNumber  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     buoyFreq  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     vertShear (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_MY82

C !LOCAL VARIABLES: ====================================================
C     I,J,kUp      - loop indices
C     p0-125       - averaging coefficients
C     tempu, tempv - temporary variables
C     rhoK, rhoKm1 - Density below and above current interface
C     epsilon      - small number
      INTEGER I,J,Km1
      _RL        p5    , p125
      PARAMETER( p5=0.5D0, p125=0.125D0 )
      _RL tempu, tempv
      _RL epsilon
      PARAMETER    (  epsilon = 1.D-10 )
      _RL rhoKm1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoK     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef MY82_SMOOTH_RI
      _RL RiTmp   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /* MY82_SMOOTH_RI */
CEOP

      Km1   = MAX(1,K-1)
C     Not sure what is correct for pressure coordinates:
C     RiFlux is always correct because it quadratic
C     buoyFreq should also be correct in pressure coordinates:
C     N^2=g^2drho/dp and K=1 at the bottom leads to the correct sign
C     So the following is wrong:
CML      IF ( buoyancyRelation .EQ. 'OCEANIC') THEN
CML       kUp   = MAX(1,K-1)
CML       kDown = K
CML      ELSEIF  ( buoyancyRelation .EQ. 'OCEANIP') THEN
CML       kUp   = K
CML       kDown = MAX(1,K-1)
CML      ELSE
CML       STOP 'MY82_RI_NUMBER: We should never reach this point'
CML      ENDIF
C     preparation: find density a kth and k-1st level
      CALL FIND_RHO_2D(
     I     iMin, iMax, jMin, jMax, K,
     I     theta(1-OLx,1-OLy,Km1,bi,bj), salt(1-OLx,1-OLy,Km1,bi,bj),
     O     rhoKm1,
     I     Km1, bi, bj, myThid )
      CALL FIND_RHO_2D(
     I     iMin, iMax, jMin, jMax, K,
     I     theta(1-OLx,1-OLy,K,bi,bj), salt(1-OLx,1-OLy,K,bi,bj),
     O     rhoK,
     I     K, bi, bj, myThid )

C     first step:  calculate flux Richardson number.
C     calculate (du/dz)^2 + (dv/dz)^2 on W (between T) points.
      DO J= jMin, jMax
       DO I = iMin, iMax
        tempu= .5 _d 0*(  uVel(I,J,Km1,bi,bj)+uVel(I+1,J,Km1,bi,bj)
     &            - (uVel(I,J,K  ,bi,bj)+uVel(I+1,J,K  ,bi,bj)) )
     &       *recip_drC(K)
        tempv= .5 _d 0*(  vVel(I,J,Km1,bi,bj)+vVel(I,J+1,Km1,bi,bj)
     &            - (vVel(I,J,K  ,bi,bj)+vVel(I,J+1,K  ,bi,bj)) )
     &       *recip_drC(K)
        vertShear(I,J) = tempu*tempu+tempv*tempv

C
C     calculate g*(drho/dz)/rho0= -N^2  .
C
        buoyFreq(I,J) = gravity*mass2rUnit *
     &       (rhoKm1(I,J) - rhoK(I,J))*recip_drC(K)
C
C     calculates gradient Richardson number, bounded by
C     a very large negative value.
C
        RiNumber(I,J) = -buoyFreq(I,J)/max(vertShear(I,J),epsilon)

       ENDDO
      ENDDO

#ifdef MY82_SMOOTH_RI
C     average Richardson number horizontally
      DO J=jMin,jMax
       DO I=iMin,iMax
        RiTmp(I,J) = RiNumber(I,J)
       ENDDO
      ENDDO
      DO J=1-Oly+1,sNy+Oly-1
       DO I=1-Olx+1,sNx+Olx-1
        RiNumber(I,J) = p5*RiTmp(I,J)
     &       + p125*RiTmp(I-1,J) + p125*RiTmp(I+1,J)
     &       + p125*RiTmp(I,J-1) + p125*RiTmp(I,J+1)
       ENDDO
      ENDDO
#endif /* MY82_SMOOTH_RI */

#endif /* ALLOW_MY82 */

      RETURN
      END
