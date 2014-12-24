C $Header: /u/gcmpack/MITgcm/model/src/calc_viscosity.F,v 1.11 2014/12/24 19:09:33 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_VISCOSITY
C     !INTERFACE:
      SUBROUTINE CALC_VISCOSITY(
     I           bi,bj, iMin,iMax,jMin,jMax,
     O           kappaRU, kappaRV,
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
C     kappaRU :: Total vertical viscosity for zonal flow.
C     kappaRV :: Total vertical viscosity for meridional flow.
C     myThid  :: my Thread Id number
      INTEGER iMin,iMax,jMin,jMax
      INTEGER bi,bj
      _RL kappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL kappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j, k :: Loop counters
      INTEGER i,j,k
      INTEGER ki
CEOP

      DO k = 1,Nr+1
       ki = MIN(k,Nr)

       DO j = 1-OLy, sNy+OLy
        DO i = 1-OLx, sNx+OLx
         kappaRU(i,j,k) = viscArNr(ki)
         kappaRV(i,j,k) = viscArNr(ki)
        ENDDO
       ENDDO

#ifdef ALLOW_KPP
       IF ( useKPP .AND. k.LE.Nr ) THEN
         CALL KPP_CALC_VISC(
     I        bi,bj, iMin,iMax,jMin,jMax, k,
     O        kappaRU, kappaRV,
     I        myThid)
       ENDIF
#endif

#ifdef ALLOW_PP81
       IF ( usePP81 .AND. k.LE.Nr ) THEN
         CALL PP81_CALC_VISC(
     I        bi,bj, iMin,iMax,jMin,jMax, k,
     O        kappaRU, kappaRV,
     I        myThid)
       ENDIF
#endif

#ifdef ALLOW_KL10
       IF ( useKL10 .AND. k.LE.Nr ) THEN
         CALL KL10_CALC_VISC(
     I        bi,bj, iMin,iMax,jMin,jMax, k,
     O        kappaRU, kappaRV,
     I        myThid)
       ENDIF
#endif

#ifdef ALLOW_MY82
       IF ( useMY82 .AND. k.LE.Nr ) THEN
         CALL MY82_CALC_VISC(
     I        bi,bj, iMin,iMax,jMin,jMax, k,
     O        kappaRU, kappaRV,
     I        myThid)
       ENDIF
#endif

#ifdef ALLOW_GGL90
       IF ( useGGL90 .AND. k.LE.Nr ) THEN
         CALL GGL90_CALC_VISC(
     I        bi,bj, iMin,iMax,jMin,jMax, k,
     O        kappaRU, kappaRV,
     I        myThid)
       ENDIF
#endif

       IF ( k.EQ.Nr+1 .AND.
     &     ( usePP81 .OR. useKL10 .OR. useMY82 .OR. useGGL90 )
     &    ) THEN
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
          kappaRU(i,j,k) = kappaRU(i,j,ki)
          kappaRV(i,j,k) = kappaRV(i,j,ki)
         ENDDO
        ENDDO
       ENDIF

C--   end of k loop
      ENDDO

      RETURN
      END
