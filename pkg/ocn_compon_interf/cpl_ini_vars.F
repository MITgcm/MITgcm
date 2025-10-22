#include "OCN_CPL_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_INI_VARS
C     !INTERFACE:
      SUBROUTINE CPL_INI_VARS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CPL_INI_VARS
C     | o Initialisation routine for arrays that are used to
C     |   buffer data that interfaces to the coupling layer.
C     | - Oceanic version -
C     *==========================================================*
C     | For now this routine sets these arrays to the right
C     | state for an initial start-up. Eventually it should
C     | include some way to load this data from non-volatile
C     | external storage if they are needed during a restart.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "OCNCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id Number
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE

C     !LOCAL VARIABLES:
C     i,j,bi,bj :: Loop counters
      INTEGER i,j,bi,bj

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
C.        Export fields
          ocMxlD2cpl(i,j,bi,bj) = 0.
          SSTocn2cpl(i,j,bi,bj) = 0.
          SSSocn2cpl(i,j,bi,bj) = 0.
          vSqocn2cpl(i,j,bi,bj) = 0.
#ifdef ALLOW_DIC
          fluxCO2cpl(i,j,bi,bj) = 0.
#endif
C-        Import and/or export fields
#if (defined ALLOW_DIC) || (defined ALLOW_THSICE)
          sIceFrac_cpl  (i,j,bi,bj) = 0.
#endif
#ifdef ALLOW_THSICE
          sIceThick_cpl (i,j,bi,bj) = 0.
          sIceSnowH_cpl (i,j,bi,bj) = 0.
          sIceQ1_cpl    (i,j,bi,bj) = 0.
          sIceQ2_cpl    (i,j,bi,bj) = 0.
#endif
C-        Import fields
          atmSLPr   (i,j,bi,bj) = 0.
          HeatFlux  (i,j,bi,bj) = 0.
          qShortWave(i,j,bi,bj) = 0.
          tauX      (i,j,bi,bj) = 0.
          tauY      (i,j,bi,bj) = 0.
          FWflux    (i,j,bi,bj) = 0.
          iceSaltFlx(i,j,bi,bj) = 0.
          seaIceMass(i,j,bi,bj) = 0.
#ifdef ALLOW_SALT_PLUME
          saltPlmFlx_cpl(i,j,bi,bj) = 0.
#endif
#ifdef ALLOW_DIC
          RunOff_cpl(i,j,bi,bj) = 0.
          airCO2    (i,j,bi,bj) = 0.
          surfWSpeed(i,j,bi,bj) = 0.
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( startTime.NE.baseTime .OR. nIter0.NE.0
     &                   .OR. pickupSuff .NE. ' ' ) THEN
       CALL OCN_CPL_READ_PICKUP( nIter0, myThid )
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
