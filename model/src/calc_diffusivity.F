C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_diffusivity.F,v 1.8 1999/05/18 18:01:12 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CALC_DIFFUSIVITY( 
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     I        maskC,maskUp,KapGM,K33,
     O        KappaRT,KappaRS,KappaRU,KappaRV,
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
#ifdef ALLOW_KPP
#include "KPPMIX.h"
#endif

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
      _RL KappaRU(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRV(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRT(i,j,k) = maskC(i,j)*maskUp(i,j)*
     &      (
#ifdef ALLOW_KPP
     &         KPPdiffKzT(i,j,k,bi,bj)
#else
     &         diffKrT
#endif
     &       + KapGM(i,j)*K33(i,j,k)
     &      )
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRS(i,j,k) = maskC(i,j)*maskUp(i,j)*
     &      (
#ifdef ALLOW_KPP
     &         KPPdiffKzS(i,j,k,bi,bj)
#else
     &         diffKrS
#endif
     &       + KapGM(i,j)*K33(i,j,k)
     &      )
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
#ifdef ALLOW_KPP
Ckpp  interpolate KPPviscAz from "t-grid" to "u-grid"
               KappaRU(i,j,k) =
     &         _maskW(i,j,k,bi,bj)*maskUp(i,j)*maskUp(i-1,j)*
     &         ( KPPviscAz(i,j,k,bi,bj) + KPPviscAz(i-1,j,k,bi,bj) )*0.5
#else
        KappaRU(i,j,k) = viscAr
#endif
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
#ifdef ALLOW_KPP
Ckpp  interpolate KPPviscAz from "t-grid" to "v-grid"
               KappaRV(i,j,k) =
     &         _maskS(i,j,k,bi,bj)*maskUp(i,j)*maskUp(i,j-1)*
     &         ( KPPviscAz(i,j,k,bi,bj)+KPPviscAz(i,j-1,k,bi,bj) )*0.5
#else
        KappaRV(i,j,k) = viscAr
#endif
       ENDDO
      ENDDO

      RETURN
      END
