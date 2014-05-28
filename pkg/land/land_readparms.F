C $Header: /u/gcmpack/MITgcm/pkg/land/land_readparms.F,v 1.14 2014/05/28 21:59:32 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_READPARMS
C     !INTERFACE:
      SUBROUTINE LAND_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_READPARMS
C     | o Read Land package parameters from parameter
C     |   file "data.land"
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
C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
C     k          :: loop counter
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit, k
      _RL tmpvar
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Land model parameters:
C     land_calc_grT  :: step forward ground Temperature
C     land_calc_grW  :: step forward soil moiture
C     land_impl_grT  :: solve ground Temperature implicitly
C     land_calc_snow :: step forward snow thickness
C     land_calc_alb  :: compute albedo of snow over land
C     land_oldPickup :: restart from an old pickup (= before checkpoint 52j)
C     land_grT_iniFile  :: File containing initial ground Temp.
C     land_grW_iniFile  :: File containing initial ground Water.
C     land_snow_iniFile :: File containing initial snow thickness.
C     land_deltaT    :: land model time-step
C     land_taveFreq  :: Frequency^-1 for time-Aver. output (s)
C     land_diagFreq  :: Frequency^-1 for diagnostic output (s)
C     land_monFreq   :: Frequency^-1 for monitor    output (s)
C     land_dzF       :: layer thickness
      NAMELIST /LAND_MODEL_PAR/
     &    land_calc_grT, land_calc_grW,
     &    land_impl_grT, land_calc_snow,
     &    land_calc_alb, land_oldPickup,
     &    land_grT_iniFile, land_grW_iniFile, land_snow_iniFile,
     &    land_deltaT, land_taveFreq, land_diagFreq, land_monFreq,
     &    land_dzF,
     &    land_timeave_mnc, land_snapshot_mnc, land_mon_mnc,
     &    land_pickup_write_mnc, land_pickup_read_mnc

C--   Physical constants :
C     land_grdLambda  :: Thermal conductivity of the ground
C     land_heatCs     :: Heat capacity of dry soil (J/m3/K)
C     land_CpWater    :: Heat capacity of water    (J/kg/K)
C     land_wTauDiff   :: soil moisture diffusion time scale
C     land_waterCap   :: field capacity per meter of soil
C     land_fractRunOff:: fraction of water in excess which run-off
C     land_rhoLiqW    :: density of liquid water (kg/m3)
C     land_rhoSnow    :: density of snow (kg/m3)
C     land_Lfreez     :: Latent heat of freezing (J/kg)
C     land_hMaxSnow   :: Maximum snow-thickness  (m)
C     diffKsnow       :: thermal conductivity of snow (W/m/K)
C     timeSnowAge     :: snow aging time scale   (s)
C     hNewSnowAge     :: new snow thickness that refresh the snow-age (by 1/e)
C     albColdSnow     :: albedo of cold (=dry) new snow (Tsfc < -10)
C     albWarmSnow     :: albedo of warm (=wet) new snow (Tsfc = 0)
C     albOldSnow      :: albedo of old snow (snowAge > 35.d)
C     hAlbSnow        :: snow thickness for albedo transition: snow/ground

      NAMELIST /LAND_PHYS_PAR/
     &    land_grdLambda, land_heatCs, land_CpWater,
     &    land_wTauDiff, land_waterCap, land_fractRunOff,
     &    land_rhoLiqW,
     &    land_rhoSnow, land_Lfreez,
     &    land_hMaxSnow, diffKsnow, timeSnowAge, hNewSnowAge,
     &    albColdSnow, albWarmSnow, tempSnowAlbL, albOldSnow, hAlbSnow

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useLand ) THEN
C-    pkg LAND is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.land is found
         CALL PACKAGES_UNUSED_MSG( 'useLand', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C-    Set default value:
      land_calc_grT = .TRUE.
      land_calc_grW = .TRUE.
      land_impl_grT = .TRUE.
      land_calc_snow= .TRUE.
      land_calc_alb = .TRUE.
      land_oldPickup= .FALSE.
      land_grT_iniFile = ' '
      land_grW_iniFile = ' '
      land_snow_iniFile= ' '
      land_deltaT   = deltaTclock
      land_taveFreq = taveFreq
      land_diagFreq = dumpFreq
      land_monFreq = monitorFreq
#ifdef ALLOW_MNC
      land_timeave_mnc       = timeave_mnc .AND. useMNC
      land_snapshot_mnc      = snapshot_mnc .AND. useMNC
      land_mon_mnc           = monitor_mnc .AND. useMNC
      land_pickup_write_mnc  = pickup_write_mnc .AND. useMNC
      land_pickup_read_mnc   = pickup_read_mnc .AND. useMNC
#else
      land_timeave_mnc       = .FALSE.
      land_snapshot_mnc      = .FALSE.
      land_mon_mnc           = .FALSE.
      land_pickup_write_mnc  = .FALSE.
      land_pickup_read_mnc   = .FALSE.
#endif
      land_grdLambda= 0.42 _d 0
      land_heatCs   = 1.13 _d 6
      land_CpWater  =  4.2 _d 3
c     land_CpWater  = HeatCapacity_Cp
      land_wTauDiff =  48. _d 0*3600. _d 0
      land_waterCap = 0.24 _d 0
      land_fractRunOff = 0.5 _d 0
      land_rhoLiqW  = rhoConstFresh
C-    snow parameters:
      land_rhoSnow  = 330. _d 0
      land_Lfreez   = 334. _d 3
      land_hMaxSnow = 1. _d 3
      diffKsnow     = 0.30 _d 0
      timeSnowAge   = 50. _d 0 * 86400. _d 0
      hNewSnowAge   = 2. _d -3
      albColdSnow   = 0.85 _d 0
      albWarmSnow   = 0.70 _d 0
      tempSnowAlbL  = -10. _d 0
      albOldSnow    = 0.55 _d 0
      hAlbSnow      = 0.30 _d 0
C-    layer thickness:
      DO k=1,land_nLev
       land_dzF(k) = -1.
       land_rec_dzC(k) = -1.
      ENDDO

      WRITE(msgBuf,'(A)') ' LAND_READPARMS: opening data.land'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

      CALL OPEN_COPY_DATA_FILE( 'data.land', 'LAND_READPARMS',
     O                          iUnit, myThid )

C--   Read parameters from open data file:

C-    Parameters for Land model:
      READ(UNIT=iUnit,NML=LAND_MODEL_PAR)

C-    Physical Constants for Land package
      READ(UNIT=iUnit,NML=LAND_PHYS_PAR)

      WRITE(msgBuf,'(A)')
     &   ' LAND_READPARMS: finished reading data.land'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

C--   Close the open data file
      CLOSE(iUnit)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Derive other parameters:

      land_impl_grT = land_calc_grT .AND. land_impl_grT

      tmpvar = 0. _d 0
      DO k=1,land_nLev
       tmpvar = tmpvar+land_dzF(k)
       IF (tmpvar.GT.0. _d 0) land_rec_dzC(k) = 2. _d 0 / tmpvar
       tmpvar = land_dzF(k)
      ENDDO
      IF ( land_Lfreez.NE. 0. _d 0 ) THEN
        recip_Lfreez = 1. _d 0 / land_Lfreez
      ELSE
        recip_Lfreez = 0. _d 0
      ENDIF

#ifdef ALLOW_MNC
      land_timeave_mnc      = useMNC .AND. land_timeave_mnc
      land_snapshot_mnc     = useMNC .AND. land_snapshot_mnc
      land_mon_mnc          = useMNC .AND. land_mon_mnc
      land_pickup_write_mnc = useMNC .AND. land_pickup_write_mnc
      land_pickup_read_mnc  = useMNC .AND. land_pickup_read_mnc

      land_timeave_mdsio      = (.NOT.land_timeave_mnc)
     &                      .OR. outputTypesInclusive
      land_snapshot_mdsio     = (.NOT.land_snapshot_mnc)
     &                      .OR. outputTypesInclusive
      land_mon_stdio          = (.NOT.land_mon_mnc)
     &                      .OR. outputTypesInclusive
      land_pickup_write_mdsio = (.NOT.land_pickup_write_mnc)
     &                      .OR. outputTypesInclusive
#else
      land_timeave_mnc        = .FALSE.
      land_snapshot_mnc       = .FALSE.
      land_mon_mnc            = .FALSE.
      land_pickup_write_mnc   = .FALSE.
      land_pickup_read_mnc    = .FALSE.
      land_timeave_mdsio      = .TRUE.
      land_snapshot_mdsio     = .TRUE.
      land_mon_stdio          = .TRUE.
      land_pickup_write_mdsio = .TRUE.
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_LAND */

      RETURN
      END
