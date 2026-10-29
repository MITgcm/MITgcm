#include "BLING_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

CBOP
      subroutine BLING_INIT_FIXED( myThid )

C     ==========================================================
C     | subroutine bling_init_fixed
C     | o Initialize fixed parameters
C     ==========================================================

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BLING_VARS.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
# include "EXF_INTERP_SIZE.h"
# include "BLING_EXF.h"
#endif

C     !INPUT PARAMETERS:
C     myThid       :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_BLING
      INTEGER errCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

C set up coefficients for DIC chemistry
C define Schmidt no. coefficients for CO2
      sca1 = 2073.1 _d 0
      sca2 = -125.62 _d 0
      sca3 =    3.6276 _d 0
      sca4 =   -0.043219 _d 0
C define Schmidt no. coefficients for O2
C based on Keeling et al [GBC, 12, 141, (1998)]
CAV new Wanninkhof numbers
      sox1 = 1929.7 _d 0
      sox2 = -117.46 _d 0
      sox3 =    3.116 _d 0
      sox4 =   -0.0306 _d 0

C coefficients for determining saturation O2
      oA0=  2.00907 _d 0
      oA1=  3.22014 _d 0
      oA2=  4.05010 _d 0
      oA3=  4.94457 _d 0
      oA4= -2.56847 _d -1
      oA5=  3.88767 _d 0
      oB0= -6.24523 _d -3
      oB1= -7.37614 _d -3
      oB2= -1.03410 _d -2
      oB3= -8.17083 _d -3
      oC0= -4.88682 _d -7

#ifdef ALLOW_EXF
      IF ( bling_useEXF ) THEN
       errCount = 0

      bling_apco2Mask = 'c'
      IF ( useSEAICE ) THEN
       bling_apco2Mask = ' '
      ENDIF

      IF ( bling_apco2File .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START pCO2',
     &  myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                         'bling', 'apco2', bling_apco2Period,
     I                         bling_apco2StartDate1, 
     I                         bling_apco2StartDate2,
     U                         bling_apco2StartTime, errCount,
     I                         myThid )
      ENDIF

      IF ( bling_ironFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START iron',
     &  myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                         'bling', 'InputFe', bling_ironPeriod,
     I                         bling_ironStartDate1,
     I                         bling_ironStartDate2,
     U                         bling_ironStartTime, errCount,
     I                         myThid )
      ENDIF

      IF ( bling_silicaFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START silica',
     &  myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                         'bling', 'silica', bling_silicaPeriod,
     I                         bling_silicaStartDate1,
     I                         bling_silicaStartDate2,
     U                         bling_silicaStartTime, errCount,
     I                         myThid )
       ENDIF

      IF ( bling_apressFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START apress',
     &  myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                         'bling', 'AtmosP', bling_apressPeriod,
     I                         bling_apressStartDate1,
     I                         bling_apressStartDate2,
     U                         bling_apressStartTime, errCount,
     I                         myThid )
       ENDIF

      IF ( bling_windFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START wind',
     &  myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                         'bling', 'wind', bling_windPeriod,
     I                         bling_windStartDate1,
     I                         bling_windStartDate2,
     U                         bling_windStartTime, errCount,
     I                         myThid )
       ENDIF

      IF ( bling_iceFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START ice',
     &  myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                         'bling', 'fIce', bling_icePeriod,
     I                         bling_iceStartDate1,
     I                         bling_iceStartDate2,
     U                         bling_iceStartTime, errCount,
     I                         myThid )
       ENDIF

       IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'BLING_INIT_FIXED: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R BLING_INIT_FIXED'
       ENDIF

      ENDIF /* bling_useEXF */
#endif

      _END_MASTER(myThid)

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDIAGNOSTICS ) THEN
        CALL BLING_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_BLING */

      RETURN
      END
