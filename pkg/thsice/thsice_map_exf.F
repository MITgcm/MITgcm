C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_map_exf.F,v 1.7 2013/04/04 16:44:34 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
#ifdef ALLOW_EXF
#include "EXF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_MAP_EXF
C     !INTERFACE:
      SUBROUTINE THSICE_MAP_EXF(
     I                  iceMsk, locSST,
     O                  totPrc, snowPrc, qPrcRnO, flxSW,
     I                  iMin,iMax,jMin,jMax, bi,bj, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_MAP_EXF
C     | Interface S/R : map Precip, Snow and shortwave fluxes
C     |                 from pkg EXF to thsice variables
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#ifdef ALLOW_EXF
# include "EXF_CONSTANTS.h"
# include "EXF_PARAM.h"
# include "EXF_FIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     iceMsk    :: sea-ice fraction: no ice=0, grid all ice 1  []
C     locSST    :: local Sea-Surface Temperature [deg.C]
C     totPrc    :: Total Precipitation (including run-off) [kg/m2/s]
C     snowPrc   :: Snow Precipitation [kg/m2/s]
C     qPrcRnO   :: Energy content of Precip+RunOff (+=down) [W/m2]
C     flxSW     :: Downward short-wave surface flux (+=down) [W/m2]
C     iMin,iMax :: range of indices of computation domain
C     jMin,jMax :: range of indices of computation domain
C     bi,bj     :: current tile indices
C     myThid      :: Thread no. that called this routine.
      _RL iceMsk (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL locSST (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL totPrc (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL snowPrc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL qPrcRnO(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxSW  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin,iMax
      INTEGER jMin,jMax
      INTEGER bi,bj
      INTEGER myThid
CEOP

#ifdef ALLOW_EXF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     === Local variables ===
C     i,j   :: current grid point indices
      INTEGER i,j

      DO j = jMin, jMax
       DO i = iMin, iMax
#ifdef ALLOW_ATM_TEMP
        totPrc(i,j) = precip(i,j,bi,bj)*rhoConstFresh
#endif
#ifdef ALLOW_RUNOFF
        totPrc(i,j) = totPrc(i,j) + runoff(i,j,bi,bj)*rhoConstFresh
#else
        STOP 'ABNORMAL END: S/R THSICE_MAP_EXF: ALLOW_RUNOFF undef'
#endif
#ifdef ALLOW_DOWNWARD_RADIATION
        flxSW (i,j) = swdown(i,j,bi,bj)
#else
      STOP 'ABNORMAL END: S/R THSICE_MAP_EXF: DOWNWARD_RADIATION undef'
#endif
       ENDDO
      ENDDO

#ifdef ALLOW_ATM_TEMP
      IF ( snowPrecipFile .NE. ' ' ) THEN
       DO j = jMin, jMax
        DO i = iMin, iMax
         snowPrc(i,j) = snowPrecip(i,j,bi,bj)*rhoConstFresh
        ENDDO
       ENDDO
      ELSE
C     If specific snow precipitiation is not available, use
C     precipitation when ever the air temperature is below 0 degC
       DO j = jMin, jMax
        DO i = iMin, iMax
         IF ( iceMsk(i,j,bi,bj).GT.0. _d 0
     &        .AND. atemp(i,j,bi,bj).LE.cen2kel )  THEN
cML     &        .AND. atemp(i,j,bi,bj).LE.Tf0kel )  THEN
          snowPrc(i,j) = precip(i,j,bi,bj)*rhoConstFresh
         ENDIF
        ENDDO
       ENDDO
      ENDIF
      IF ( temp_EvPrRn .NE. UNSET_RL ) THEN
C--   Account for energy content of Precip + RunOff :
C     assume 1) rain has same temp as Air (higher altitude, e.g., 850.mb would
C      be better); 2) Snow has no heat capacity (+ is counted separately)
C     3) no distinction between sea-water Cp and fresh-water Cp
C     4) Run-Off comes at the temp of surface water (with same Cp)
       DO j = jMin, jMax
        DO i = iMin, iMax
         qPrcRnO(i,j) = HeatCapacity_Cp
     &          *( atemp(i,j,bi,bj) - cen2kel - temp_EvPrRn )
     &          *( precip(i,j,bi,bj)*rhoConstFresh - snowPrc(i,j) )
     &
#ifdef ALLOW_RUNOFF
         qPrcRnO(i,j) = qPrcRnO(i,j)
     &                + HeatCapacity_Cp
     &                 *( locSST(i,j,bi,bj) - temp_EvPrRn )
     &                 *runoff(i,j,bi,bj)*rhoConstFresh
#endif
        ENDDO
       ENDDO
      ENDIF
#else /* ALLOW_ATM_TEMP */
      STOP 'ABNORMAL END: S/R THSICE_MAP_EXF: ATM_TEMP undef'
#endif /* ALLOW_ATM_TEMP */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_EXF */

      RETURN
      END
