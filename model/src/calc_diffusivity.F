C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_diffusivity.F,v 1.7 1998/11/06 22:44:43 cnh Exp $

#include "CPP_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CALC_DIFFUSIVITY( 
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     I        maskC,maskUp,KapGM,K33,
     O        KappaRT,KappaRS,
     I        myThid)

C     /==========================================================\
C     | SUBROUTINE CALC_DIFFUSIVITY                              |
C     | o Calculate net diffusivity                              |
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
      _RS maskC(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RS maskUp(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL KapGM(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL K33(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRT(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRS(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRT(i,j,k) = maskC(i,j)*maskUp(i,j)*
     &      ( diffKrT + KapGM(i,j)*K33(i,j,k) )
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRS(i,j,k) = maskC(i,j)*maskUp(i,j)*
     &      ( diffKrS + KapGM(i,j)*K33(i,j,k) )
       ENDDO
      ENDDO

      RETURN
      END
