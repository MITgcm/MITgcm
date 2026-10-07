#include "DIC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DIC_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE DIC_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize fixed quantities

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"
#include "DIC_ATMOS.h"
#ifdef ALLOW_EXF
# include "DIC_EXF.h"
# include "EXF_PARAM.h"
# ifdef USE_EXF_INTERPOLATION
#  include "EXF_INTERP_SIZE.h"
#  include "DIC_INTERP_PARAM.h"
# endif
#endif
C     !INPUT PARAMETERS:
C     myThid       :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIC
      INTEGER k
      INTEGER iUnit
#if ( defined DIC_BIOTIC || defined READ_PAR )
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
#ifdef ALLOW_EXF
C     errCount  :: error counter
      INTEGER errCount
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

C--   Set up coefficients for DIC chemistry
C define Schmidt no. coefficients for CO2
      sca1 = 2073.1 _d 0
      sca2 = -125.62 _d 0
      sca3 =    3.6276 _d 0
      sca4 =   -0.043219 _d 0
C define Schmidt no. coefficients for O2
C based on Keeling et al [GBC, 12, 141, (1998)]
      sox1 = 1638.0 _d 0
      sox2 =  -81.83 _d 0
      sox3 =    1.483 _d 0
      sox4 =   -0.008004 _d 0

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

C--   Set other constant/flag

      IF ( dic_int1.EQ.2 ) THEN
        CALL MDSFINDUNIT( iUnit, myThid )
        OPEN(UNIT=iUnit,FILE='co2atmos.dat',STATUS='old')
        DO k=1,dic_int2
          READ(iUnit,*) co2atmos(k)
          WRITE(standardMessageUnit,*) 'co2atmos',co2atmos(k)
        ENDDO
        CLOSE(iUnit)
      ENDIF

#ifdef DIC_BIOTIC
#ifdef ALLOW_AUTODIFF
cph nlev=Nr used in TAF store directive
      nlev=Nr
#else
      k = 1
      DO WHILE ( k .LT. Nr .AND. rC(k) .GE. -zcrit )
        k = k+1
      ENDDO
      nlev = k
#endif
C--   Print out nlev values :
      iUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)') '//  DIC_INIT_FIXED parameters :'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)

      CALL WRITE_0D_I( nlev, INDEX_NONE, 'nlev =',
     &   ' /* Number of level over which Bio act is computed */')
#endif /* DIC_BIOTIC */

      _END_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--  Check some option & parameter combinations:

C    for now, just collect here few check & stop from various places:
#ifdef READ_PAR
#ifdef USE_QSW
c     WRITE(msgBuf,'(2A)') ' DIC_INI_FORCING: ',
      WRITE(msgBuf,'(2A)') 'DIC_INIT_FIXED: ',
     &     'You can not use READ_PAR  and USE_QSW together'
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R DIC_INIT_FIXED'
#endif
      IF ( DIC_forcingCycle.GT.0. _d 0
     &     .AND. DIC_parFile .EQ. ' '  ) THEN
c       WRITE(msgBuf,'(2A)') ' DIC_FIELDS_LOAD: ',
        WRITE(msgBuf,'(2A)') 'DIC_INIT_FIXED: ',
     &     'You need to provide a file if you want to use READ_PAR'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R DIC_INIT_FIXED'
      ENDIF
#endif /* READ_PAR */

#ifdef ALLOW_EXF
      IF ( dic_useEXF ) THEN
       _BEGIN_MASTER( myThid )
       errCount = 0

       IF ( dic_silicaFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL(
     &           'GETFFIELD_START silicaSurf',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I               'dic', 'silicaSurf', dic_silicaSurfPeriod,
     I               dic_silicaSurfStartDate1, dic_silicaSurfStartDate2,
     U               dic_silicaSurfStartTime, errCount,
     I               myThid )
       ENDIF

#ifdef DIC_3D_SILICA
# ifdef USE_EXF_INTERPOLATION
C-    Default silicaDeep vertical grid parameters if not set by user in namelist
       IF ( silicaDeep_nzin .EQ. UNSET_I ) THEN
         silicaDeep_nzin = Nr
         DO k = 1, Nr
           silicaDeep_dzin(k) = drF(k)
         ENDDO
       ELSEIF ( silicaDeep_dzin(1) .EQ. UNSET_RL ) THEN
C-      User set silicaDeep_nzin but not silicaDeep_dzin: fill from model grid
         DO k = 1, MIN(silicaDeep_nzin, Nr)
           silicaDeep_dzin(k) = drF(k)
         ENDDO
       ENDIF
# endif /* USE_EXF_INTERPOLATION */

       IF ( dic_silicaDeepFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL(
     &           'GETFFIELD_START 3D silica',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                'dic', 'silicaDeep', dic_silicaDeepPeriod,
     I                dic_silicaDeepStartDate1, dic_silicaDeepStartDate2,
     U                dic_silicaDeepStartTime, errCount,
     I                myThid )
       ENDIF
#endif /* DIC_3D_SILICA */

#ifdef READ_PAR
       IF ( dic_parFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START PAR',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'dic', 'PAR', dic_parPeriod,
     I                    dic_parStartDate1, dic_parStartDate2,
     U                    dic_parStartTime, errCount,
     I                    myThid )
       ENDIF
#endif /* READ_PAR */

#ifndef USE_PLOAD
       IF ( dic_atmospFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START ATMOSP',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'dic', 'ATMOSP', dic_atmospPeriod,
     I                    dic_atmospStartDate1, dic_atmospStartDate2,
     U                    dic_atmospStartTime, errCount,
     I                    myThid )
       ENDIF
#endif /* USE_PLOAD */

       IF ( dic_windFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START wind',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'dic', 'wind', dic_windPeriod,
     I                    dic_windStartDate1, dic_windStartDate2,
     U                    dic_windStartTime, errCount,
     I                    myThid )
       ENDIF
       
       IF ( dic_iceFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START fIce',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'dic', 'fIce', dic_icePeriod,
     I                    dic_iceStartDate1, dic_iceStartDate2,
     U                    dic_iceStartTime, errCount,
     I                    myThid )
       ENDIF

#ifdef ALLOW_FE
       IF ( dic_ironFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START inputFe',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'dic', 'inputFe', dic_ironPeriod,
     I                    dic_ironStartDate1, dic_ironStartDate2,
     U                    dic_ironStartTime, errCount,
     I                    myThid )
       ENDIF
#endif /* ALLOW_FE */

       IF ( dic_atmospCO2File .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START atmpCO2',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'dic', 'atmospCO2', dic_atmospCO2Period,
     I                    dic_atmospCO2StartDate1, 
     I                    dic_atmospCO2StartDate2,
     U                    dic_atmospCO2StartTime, errCount,
     I                    myThid )
       ENDIF

#ifdef LIGHT_CHL
       IF ( dic_chlfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START CHL',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                   'dic', 'chl', dic_chlperiod,
     I                   dic_chlstartdate1, dic_chlstartdate2,
     U                   dic_chlStartTime, errCount,
     I                   myThid )
       ENDIF
#endif /* LIGHT_CHL */

       IF ( dic_SSTFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START SST',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                   'dic', 'dicSST', dic_SSTPeriod,
     I                   dic_SSTStartDate1, dic_SSTStartDate2,
     U                   dic_SSTStartTime, errCount,
     I                   myThid )
       ENDIF

            IF ( dic_SSSFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START SSS',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                   'dic', 'dicSSS', dic_SSSPeriod,
     I                   dic_SSSStartDate1, dic_SSSStartDate2,
     U                   dic_SSSStartTime, errCount,
     I                   myThid )
       ENDIF
       
       IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'DIC_INIT_FIXED: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R DIC_INIT_FIXED'
       ENDIF
       _END_MASTER( myThid )

C     endif dic_useEXF
      ENDIF

      _BARRIER
#endif /* ALLOW_EXF */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_MNC
      IF ( useMNC ) THEN
        CALL DIC_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDIAGNOSTICS ) THEN
        CALL DIC_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_DIC */

      RETURN
      END
