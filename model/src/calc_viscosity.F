C $Header: /u/gcmpack/MITgcm/model/src/calc_viscosity.F,v 1.9 2009/10/08 20:03:18 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_VISCOSITY
C     !INTERFACE:
      SUBROUTINE CALC_VISCOSITY(
     I           bi,bj, iMin,iMax,jMin,jMax,
     O           KappaRU, KappaRV,
     I           myThid )

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
C     iMin,iMax,jMin,jMax :: Range of points for which calculation
C     bi,bj   :: current tile indices
C     KappaRU :: Total vertical viscosity for zonal flow.
C     KappaRV :: Total vertical viscosity for meridional flow.
C     myThid  :: my Thread Id number
      INTEGER iMin,iMax,jMin,jMax
      INTEGER bi,bj
      _RL KappaRU(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRV(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j, k :: Loop counters
      INTEGER i,j,k
CEOP

      DO k = 1,Nr

       DO j = 1-Oly, sNy+Oly
        DO i = 1-Olx, sNx+Olx
         KappaRU(i,j,k) = viscArNr(k)
         KappaRV(i,j,k) = viscArNr(k)
        ENDDO
       ENDDO

#ifdef ALLOW_KPP
       IF (useKPP) CALL KPP_CALC_VISC(
     I        bi,bj, iMin,iMax,jMin,jMax, k,
     O        KappaRU,KappaRV,
     I        myThid)
#endif

#ifdef ALLOW_PP81
       IF (usePP81) CALL PP81_CALC_VISC(
     I        bi,bj, iMin,iMax,jMin,jMax, k,
     O        KappaRU,KappaRV,
     I        myThid)
#endif

#ifdef ALLOW_MY82
       IF (useMY82) CALL MY82_CALC_VISC(
     I        bi,bj, iMin,iMax,jMin,jMax, k,
     O        KappaRU,KappaRV,
     I        myThid)
#endif

#ifdef ALLOW_GGL90
       IF (useGGL90) CALL GGL90_CALC_VISC(
     I        bi,bj, iMin,iMax,jMin,jMax, k,
     O        KappaRU,KappaRV,
     I        myThid)
#endif

C--   end of k loop
      ENDDO

      RETURN
      END
