C $Header: /u/gcmpack/MITgcm/pkg/land/land_check.F,v 1.1 2014/05/28 21:59:32 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_CHECK
C     !INTERFACE:
      SUBROUTINE LAND_CHECK( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_CHECK
C     | o Check parameters and other package dependences
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C-- size for MITgcm & Land package :
#include "LAND_SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAND_PARAMS.h"
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid     :: my Thread Id number
      INTEGER myThid

#ifdef ALLOW_LAND
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
C     iL         :: Work variable for length of file-name
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit, iL
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

      iUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') 'LAND_CHECK: #define ALLOW_LAND'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)

C--   Print out parameter values :
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)') '// Land package parameters :'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)

C- namelist LAND_MODEL_PAR:
       CALL WRITE_0D_L( land_calc_grT, INDEX_NONE,
     &                 'land_calc_grT =',
     &   ' /* step forward ground Temp. on/off flag */')
       CALL WRITE_0D_L( land_calc_grW, INDEX_NONE,
     &                 'land_calc_grW =',
     &   ' /* step forward soil moiture on/off flag */')
       CALL WRITE_0D_L( land_impl_grT, INDEX_NONE,
     &                 'land_impl_grT =',
     &   ' /* solve ground temperature implicitly */')
       CALL WRITE_0D_L( land_calc_snow, INDEX_NONE,
     &                 'land_calc_snow =',
     &   ' /* step forward snow thickness */')
       CALL WRITE_0D_L( land_calc_alb, INDEX_NONE,
     &                 'land_calc_alb =',
     &   ' /* compute land+snow albedo */')
      iL = ILNBLNK( land_grT_iniFile )
      IF ( iL.GE.1 ) THEN
       WRITE(msgBuf,'(A,A)') 'land_grT_iniFile = ',
     &                       '/* Initial ground-Temp Input-File */'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
       WRITE(msgBuf,'(16X,A)') land_grT_iniFile(1:iL)
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
       msgBuf='    ;'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF
      iL = ILNBLNK( land_grW_iniFile )
      IF ( iL.GE.1 ) THEN
       WRITE(msgBuf,'(A,A)') 'land_grW_iniFile = ',
     &                       '/* Initial soil-Water Input-File */'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
       WRITE(msgBuf,'(16X,A)') land_grW_iniFile(1:iL)
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
       msgBuf='    ;'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF
      iL = ILNBLNK( land_snow_iniFile )
      IF ( iL.GE.1 ) THEN
       WRITE(msgBuf,'(A,A)') 'land_snow_iniFile= ',
     &                  '/* Initial snow thickness Input-File */'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
       WRITE(msgBuf,'(16X,A)') land_grW_iniFile(1:iL)
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
       msgBuf='    ;'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF
       CALL WRITE_0D_RL( land_deltaT, INDEX_NONE,'land_deltaT =',
     &  ' /* land model Time-Step (s) */')
       CALL WRITE_0D_RL( land_taveFreq, INDEX_NONE,'land_taveFreq =',
     &   ' /* Frequency^-1 for time-Aver. output (s) */')
       CALL WRITE_0D_RL( land_diagFreq, INDEX_NONE,'land_diagFreq =',
     &   ' /* Frequency^-1 for diagnostic output (s) */')
       CALL WRITE_0D_RL( land_diagFreq, INDEX_NONE,'land_monFreq =',
     &   ' /* Frequency^-1 for monitor output (s) */')
       CALL WRITE_1D_RL( land_dzF,land_nLev, INDEX_K,'land_dzF = ',
     &   ' /* layer thickness ( m ) */')
       CALL WRITE_1D_RL(land_rec_dzC,land_nLev,INDEX_K,'land_rec_dzC= '
     &  ,' /* recip. vertical spacing (m-1) */')
      CALL WRITE_0D_L( land_timeave_mdsio, INDEX_NONE,
     &     'land_timeave_mdsio =',' /* use MDSIO for Tave output */')
      CALL WRITE_0D_L( land_timeave_mnc, INDEX_NONE,
     &     'land_timeave_mnc =',' /* use MNC for Tave output */')
      CALL WRITE_0D_L( land_snapshot_mdsio, INDEX_NONE,
     &     'land_snapshot_mdsio =',
     &     ' /* use MDSIO for snapshot output */')
      CALL WRITE_0D_L( land_snapshot_mnc, INDEX_NONE,
     &     'land_snapshot_mnc =',' /* use MNC for snapshot output */')
      CALL WRITE_0D_L( land_mon_stdio, INDEX_NONE,
     &     'land_mon_stdio =',' /* use STDOUT for monitor output */')
      CALL WRITE_0D_L( land_mon_mnc, INDEX_NONE,
     &     'land_mon_mnc =',' /* use MNC for monitor output */')
      CALL WRITE_0D_L( land_pickup_write_mnc, INDEX_NONE,
     &     'land_pickup_write_mnc =',
     &     ' /* use MNC for writing pickups */')
      CALL WRITE_0D_L( land_pickup_read_mnc, INDEX_NONE,
     &     'land_pickup_read_mnc =',
     &     ' /* use MNC for reading pickups */')

C- namelist LAND_PHYS_PAR:
       CALL WRITE_0D_RL(land_grdLambda,INDEX_NONE,'land_grdLambda =',
     &   ' /* Thermal conductivity of the ground (W/m/K)*/')
       CALL WRITE_0D_RL( land_heatCs,INDEX_NONE,'land_heatCs =',
     &   ' /* Heat capacity of dry soil (J/m3/K) */')
       CALL WRITE_0D_RL( land_CpWater,INDEX_NONE,'land_CpWater =',
     &   ' /* Heat capacity of water    (J/kg/K) */')
       CALL WRITE_0D_RL( land_wTauDiff,INDEX_NONE,'land_wTauDiff =',
     &   ' /* soil moisture diffusion time scale (s) */')
       CALL WRITE_0D_RL( land_waterCap,INDEX_NONE,'land_waterCap =',
     &   ' /* field capacity per meter of soil (1) */')
       CALL WRITE_0D_RL(land_fractRunOff,INDEX_NONE,'land_fractRunOff='
     &  ,' /* fraction of water in excess which run-off */')
       CALL WRITE_0D_RL(land_rhoLiqW,INDEX_NONE,'land_rhoLiqW =',
     &   ' /* density of liquid water (kg/m3) */')
       CALL WRITE_0D_RL(land_rhoSnow,INDEX_NONE,'land_rhoSnow =',
     &   ' /* density of snow (kg/m3) */')
       CALL WRITE_0D_RL(land_Lfreez,INDEX_NONE,'land_Lfreez =',
     &   ' /* Latent heat of freezing (J/kg) */')
       CALL WRITE_0D_RL(land_hMaxSnow,INDEX_NONE,'land_hMaxSnow =',
     &   ' /* maximum snow-thickness (m) */')
       CALL WRITE_0D_RL(diffKsnow,INDEX_NONE,'diffKsnow =',
     &   ' /* thermal conductivity of snow (W/m/K) */')
       CALL WRITE_0D_RL(timeSnowAge,INDEX_NONE,'timeSnowAge =',
     &   ' /* snow aging time scale   (s) */')
       CALL WRITE_0D_RL(hNewSnowAge,INDEX_NONE,'hNewSnowAge =',
     &   ' /* new snow thickness to refresh snow-age by 1/e */')
       CALL WRITE_0D_RL(albColdSnow,INDEX_NONE,'albColdSnow =',
     &   ' /* albedo of cold (=dry) new snow */')
       CALL WRITE_0D_RL(albWarmSnow,INDEX_NONE,'albWarmSnow =',
     &   ' /* albedo of warm (=wet) new snow */')
       CALL WRITE_0D_RL(tempSnowAlbL,INDEX_NONE,'tempSnowAlbL =',
     &   ' /* Temp. transition from ColdSnow to WarmSnow Alb. (oC) */')
       CALL WRITE_0D_RL(albOldSnow, INDEX_NONE,'albOldSnow =',
     &   ' /* albedo of old snow (snowAge >35.d)*/')
       CALL WRITE_0D_RL(hAlbSnow, INDEX_NONE,'hAlbSnow =',
     &   ' /* snow depth for albedo transition */')

      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Check parameters and model configuration

C-    for now, Land package works only with AIM package:
      IF ( .NOT.useAIM ) THEN
        WRITE(msgBuf,'(A)')
     &  'LAND_CHECK: land pkg only implemented with AIM physics'
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(A)')
     &  'DO NOT use Land (useLand=T) without AIM (useAIM=F, data.pkg)'
        CALL PRINT_ERROR( msgBuf, myThid)
        STOP 'ABNORMAL END: S/R LAND_CHECK'
      ENDIF

      IF ( land_nLev.NE.2 .AND. land_impl_grT ) THEN
        WRITE(msgBuf,'(2A,I3)') 'LAND_CHECK: ',
     &  ' land_impl_grT=.T. but land_nLev=',land_nLev
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(A)')
     &  'Implicit scheme only implemented for 2 levels land Temp'
        CALL PRINT_ERROR( msgBuf, myThid)
        STOP 'ABNORMAL END: S/R LAND_CHECK'
      ENDIF

C-    If land_taveFreq is positive, then must compile the land-diagnostics code
#ifndef ALLOW_LAND_TAVE
      IF (land_taveFreq.GT.0.) THEN
        WRITE(msgBuf,'(2A)') 'LAND_CHECK:',
     &  ' land_taveFreq > 0 but ALLOW_LAND_TAVE undefined'
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(2A)') 'Re-compile setting: ',
     &  '#define ALLOW_LAND_TAVE (in LAND_OPTIONS.h)'
        CALL PRINT_ERROR( msgBuf, myThid)
        STOP 'ABNORMAL END: S/R LAND_CHECK'
      ENDIF
#endif /* ALLOW_LAND_TAVE */

C-    If land_monFreq is > 0, then must compile the monitor pkg
#ifndef ALLOW_MONITOR
      IF (land_monFreq.GT.0.) THEN
        WRITE(msgBuf,'(2A)') 'LAND_CHECK:',
     &  ' land_monFreq > 0 but ALLOW_MONITOR undefined'
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(2A)')
     &  'Re-compile with pkg monitor (in packages.conf)'
        CALL PRINT_ERROR( msgBuf, myThid)
        STOP 'ABNORMAL END: S/R LAND_CHECK'
      ENDIF
#endif /* ALLOW_MONITOR */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

#endif /* ALLOW_LAND */

      RETURN
      END
