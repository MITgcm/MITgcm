C $Header: /u/gcmpack/MITgcm/model/src/calc_viscosity.F,v 1.7 2004/09/16 09:35:11 mlosch Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_VISCOSITY
C     !INTERFACE:
      SUBROUTINE CALC_VISCOSITY( 
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     O        KappaRU,KappaRV,
     I        myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_VISCOSITY                                 
C     | o Calculate net vertical viscosity                        
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C     myThid - Instance number for this innvocation of CALC_COMMON_FACTORS
C     KappaRU :: Total vertical viscosity for zonal flow.
C     KappaRV :: Total vertical viscosity for meridional flow.
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL KappaRU(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRV(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j
CEOP

      DO j = 1-Oly, sNy+Oly
       DO i = 1-Olx, sNx+Olx
        KappaRU(i,j,k) = viscAr
       ENDDO
      ENDDO

      DO j = 1-Oly, sNy+Oly
       DO i = 1-Olx, sNx+Olx
        KappaRV(i,j,k) = viscAr
       ENDDO
      ENDDO

#ifdef ALLOW_KPP
      IF (useKPP) CALL KPP_CALC_VISC(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,K,
     O        KappaRU,KappaRV,
     I        myThid)
#endif

#ifdef ALLOW_PP81
      IF (usePP81) CALL PP81_CALC_VISC(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,K,
     O        KappaRU,KappaRV,
     I        myThid)
#endif

#ifdef ALLOW_MY82
      IF (useMY82) CALL MY82_CALC_VISC(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,K,
     O        KappaRU,KappaRV,
     I        myThid)
#endif

#ifdef ALLOW_GGL90
      IF (useGGL90) CALL GGL90_CALC_VISC(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,K,
     O        KappaRU,KappaRV,
     I        myThid)
#endif


      RETURN
      END
