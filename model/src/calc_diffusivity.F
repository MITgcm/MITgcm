C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_diffusivity.F,v 1.15 2001/08/13 18:05:26 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CALC_DIFFUSIVITY( 
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     I        maskUp,
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
#include "GRID.h"

C     == Routine arguments ==
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C     maskUp  - land/water mask for Wvel points (above tracer level)
C     myThid - Instance number for this innvocation of CALC_COMMON_FACTORS
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RS maskUp(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL KappaRT(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRS(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRT(i,j,k) = maskUp(i,j)*
     &      (
     &         KappaRT(i,j,k)
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL))
     &       + diffKr(i,j,k,bi,bj)
#else
     &       + diffKrT
#endif
     &      )
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRS(i,j,k) = maskUp(i,j)*
     &      (
     &         KappaRS(i,j,k)
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL))
     &       + diffKr(i,j,k,bi,bj)
#else
     &       + diffKrS
#endif
     &      )
       ENDDO
      ENDDO

#ifdef ALLOW_GMREDI
      IF (useGMRedi) CALL GMREDI_CALC_DIFF(
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     I        maskUp,
     O        KappaRT,KappaRS,
     I        myThid)
#endif

#ifdef ALLOW_KPP
      IF (useKPP) CALL KPP_CALC_DIFF(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,K,
     I        maskUp,
     O        KappaRT,KappaRS,
     I        myThid)
#endif


      RETURN
      END
