C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_diffusivity.F,v 1.2 1998/06/09 16:48:02 cnh Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterFace
      SUBROUTINE CALC_DIFFUSIVITY( 
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     I        maskC,maskUp,KapGM,K33,
     O        KappaZT,
     I        myThid)

C     /==========================================================\
C     | SUBROUTINE CALC_DIFFUSIVITY                              |
C     | o Calculate net diffusivity                              |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C     maskC   - land/water mask for tracer points
C     maskUp  - land/water mask for Wvel points (above tracer level)
C     myThid - Instance number for this innvocation of CALC_COMMON_FACTORS
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL maskC(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL maskUp(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL KapGM(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL K33(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nz)
      _RL KappaZT(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nz)
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaZT(i,j,k) = maskC(i,j)*maskUp(i,j)*
     &      ( diffKzT + KapGM(i,j)*K33(i,j,k) )
       ENDDO
      ENDDO

      RETURN
      END
