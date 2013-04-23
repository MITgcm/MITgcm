C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_get_precip.F,v 1.3 2013/04/23 16:34:24 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
#ifdef ALLOW_BULK_FORCE
#include "BULK_FORCE_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_GET_PRECIP
C     !INTERFACE:
      SUBROUTINE THSICE_GET_PRECIP(
     I                  iceMsk, locSST,
     O                  precip, snowPrc, qPrcRnO, flxSW,
     I                  iMin,iMax,jMin,jMax, bi,bj, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_GET_PRECIP
C     | Interface S/R : get Precip, Snow-precip
C     |                 and downward short-wave
C     |                 from pkg BULK_FORCE
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_BULK_FORCE
#include "BULKF_PARAMS.h"
#include "BULKF.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     iceMsk    :: sea-ice fraction: no ice=0, grid all ice 1  []
C     locSST    :: local Sea-Surface Temperature [deg.C]
C     precip    :: Total Precipitation (including run-off) [kg/m2/s]
C     snowPrc   :: Snow Precipitation [kg/m2/s]
C     qPrcRnO   :: Energy content of Precip+RunOff (+=down) [W/m2]
C     flxSW     :: Downward short-wave surface flux (+=down) [W/m2]
C     iMin,iMax :: range of indices of computation domain
C     jMin,jMax :: range of indices of computation domain
C     bi,bj     :: current tile indices
C     myThid      :: Thread no. that called this routine.
      _RL iceMsk (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL locSST (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL precip (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL snowPrc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL qPrcRnO(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxSW  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin,iMax
      INTEGER jMin,jMax
      INTEGER bi,bj
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
#ifdef ALLOW_BULK_FORCE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     === Local variables ===
C     i,j   :: current grid point indices
      INTEGER i,j

      DO j = jMin, jMax
        DO i = iMin, iMax
          precip(i,j) = ( rain(i,j,bi,bj)+runoff(i,j,bi,bj) )*rhofw
          flxSW (i,j) = solar(i,j,bi,bj)
        ENDDO
      ENDDO

c     IF ( SnowFile .NE. ' ' ) THEN
c     ELSE
C     If specific snow precipitiation is not available, use
C     precipitation when ever the air temperature is below 0 degC
        DO j = jMin, jMax
         DO i = iMin, iMax
          IF ( iceMsk(i,j,bi,bj).GT.0. _d 0
     &      .AND. Tair(i,j,bi,bj).LE.Tf0kel )  THEN
             snowPrc(i,j) = rain(i,j,bi,bj)*rhofw
          ENDIF
         ENDDO
        ENDDO
c     ENDIF

      IF ( temp_EvPrRn .NE. UNSET_RL ) THEN
C--   Account for energy content of Precip + RunOff :
C     assume 1) rain has same temp as Air (higher altitude, e.g., 850.mb would
C      be better); 2) Snow has no heat capacity (+ is counted separately)
C     3) no distinction between sea-water Cp and fresh-water Cp
C     4) Run-Off comes at the temp of surface water (with same Cp)
       DO j = jMin, jMax
        DO i = iMin, iMax
         qPrcRnO(i,j) = HeatCapacity_Cp*(
     &           ( Tair(i,j,bi,bj) - Tf0kel - temp_EvPrRn )
     &          *( rain(i,j,bi,bj)*rhofw - snowPrc(i,j) )
     &         + ( locSST(i,j,bi,bj) - temp_EvPrRn )
     &          *runoff(i,j,bi,bj)*rhofw )
        ENDDO
       ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_BULK_FORCE */
#endif /* ALLOW_THSICE */

      RETURN
      END
