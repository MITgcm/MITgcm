#include "EXF_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: EXF_INIT_VARIA
C     !INTERFACE:
      SUBROUTINE EXF_INIT_VARIA( myThid )

C !DESCRIPTION: \bv
C  *=================================================================*
C  | SUBROUTINE EXF_INIT_VARIA
C  | o This routine initialises the forcing
C  *=================================================================*
C  |  started: Ralf.Giering@FastOpt.de 25-Mai-20000
C  |  mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
C  *=================================================================*
C \ev

C !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_INTERP_SIZE.h"
#include "EXF_INTERP_PARAM.h"
#include "EXF_FIELDS.h"
#include "PARAMS.h"
#ifdef ALLOW_BULK_OFFLINE
# include "DYNVARS.h"
#endif

C !INPUT/OUTPUT PARAMETERS:
C     myThid         :: My Thread Id number
      INTEGER myThid

C !LOCAL VARIABLES:
      INTEGER i,j,ks,bi,bj
CEOP

      ks = 1
      IF ( usingPCoords ) ks = Nr

C--   Initialise to zero intermediate fields (in common block)
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          wStress(i,j,bi,bj) = 0.
          cw(i,j,bi,bj) = 0.
          sw(i,j,bi,bj) = 0.
          sh(i,j,bi,bj) = 0.
#ifdef ALLOW_ATM_TEMP
          hs(i,j,bi,bj) = 0.
          hl(i,j,bi,bj) = 0.
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( .NOT.useAtmWind ) THEN
C- Note: In case of constant-in-time wind-stress ( u,v stressperiod = 0 &
C      u,v stressfile.NE.' ' ) interpolated from 2-component wind-stress field
C      with ( usingCurvilinearGrid .OR. rotateGrid .OR. uvInterp_stress )
C      this initial setting of ustress & vstress (+ ustress0,1 & vstress0,1)
C      will be over-written at first time-step when calling EXF_SET_UV.
       CALL EXF_INIT_FLD(
     I     'ustress', ustressfile, ustressmask,
     I     ustressperiod, exf_inscal_ustress, ustressconst,
     O     ustress, ustress0, ustress1,
#ifdef USE_EXF_INTERPOLATION
     I     ustress_lon0, ustress_lon_inc,
     I     ustress_lat0, ustress_lat_inc,
     I     ustress_nlon, ustress_nlat, xC, yC, ustress_interpMethod,
#endif
     I     myThid )

       CALL EXF_INIT_FLD(
     I     'vstress', vstressfile, vstressmask,
     I     vstressperiod, exf_inscal_vstress, vstressconst,
     O     vstress, vstress0, vstress1,
#ifdef USE_EXF_INTERPOLATION
     I     vstress_lon0, vstress_lon_inc,
     I     vstress_lat0, vstress_lat_inc,
     I     vstress_nlon, vstress_nlat, xC, yC, vstress_interpMethod,
#endif
     I     myThid )

      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uwind(i,j,bi,bj) = 0.
           vwind(i,j,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      IF ( useAtmWind ) THEN
C- Note: In case of constant-in-time winds ( u,v windperiod = 0 &
C      u,v windfile.NE.' ' ) interpolated from 2-component wind field
C      with ( usingCurvilinearGrid .OR. rotateGrid .OR. uvInterp_wind )
C      this initial setting of uwind & vwind (+ uwind0,1 & vwind0,1)
C      will be over-written at first time-step when calling EXF_SET_UV.
       CALL EXF_INIT_FLD(
     I     'uwind', uwindfile, uwindmask,
     I     uwindperiod, exf_inscal_uwind, uwindconst,
     O     uwind, uwind0, uwind1,
#ifdef USE_EXF_INTERPOLATION
     I     uwind_lon0, uwind_lon_inc,
     I     uwind_lat0, uwind_lat_inc,
     I     uwind_nlon, uwind_nlat, xC, yC, uwind_interpMethod,
#endif
     I     myThid )

       CALL EXF_INIT_FLD(
     I     'vwind', vwindfile, vwindmask,
     I     vwindperiod, exf_inscal_vwind, vwindconst,
     O     vwind, vwind0, vwind1,
#ifdef USE_EXF_INTERPOLATION
     I     vwind_lon0, vwind_lon_inc,
     I     vwind_lat0, vwind_lat_inc,
     I     vwind_nlon, vwind_nlat, xC, yC, vwind_interpMethod,
#endif
     I     myThid )

      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uwind(i,j,bi,bj) = 0.
           vwind(i,j,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      CALL EXF_INIT_FLD(
     I     'wspeed', wspeedfile, wspeedmask,
     I     wspeedperiod, exf_inscal_wspeed, wspeedconst,
     O     wspeed, wspeed0, wspeed1,
#ifdef USE_EXF_INTERPOLATION
     I     wspeed_lon0, wspeed_lon_inc,
     I     wspeed_lat0, wspeed_lat_inc,
     I     wspeed_nlon, wspeed_nlat, xC, yC, wspeed_interpMethod,
#endif
     I     myThid )

      CALL EXF_INIT_FLD(
     I     'hflux', hfluxfile, hfluxmask,
     I     hfluxperiod, exf_inscal_hflux, hfluxconst,
     O     hflux, hflux0, hflux1,
#ifdef USE_EXF_INTERPOLATION
     I     hflux_lon0, hflux_lon_inc,
     I     hflux_lat0, hflux_lat_inc,
     I     hflux_nlon, hflux_nlat, xC, yC, hflux_interpMethod,
#endif
     I     myThid )

      CALL EXF_INIT_FLD(
     I     'sflux', sfluxfile, sfluxmask,
     I     sfluxperiod, exf_inscal_sflux, sfluxconst,
     O     sflux, sflux0, sflux1,
#ifdef USE_EXF_INTERPOLATION
     I     sflux_lon0, sflux_lon_inc,
     I     sflux_lat0, sflux_lat_inc,
     I     sflux_nlon, sflux_nlat, xC, yC, sflux_interpMethod,
#endif
     I     myThid )

#ifdef ALLOW_ATM_TEMP

      CALL EXF_INIT_FLD(
     I     'atemp', atempfile, atempmask,
     I     atempperiod, exf_inscal_atemp, atempconst,
     O     atemp, atemp0, atemp1,
#ifdef USE_EXF_INTERPOLATION
     I     atemp_lon0, atemp_lon_inc,
     I     atemp_lat0, atemp_lat_inc,
     I     atemp_nlon, atemp_nlat, xC, yC, atemp_interpMethod,
#endif
     I     myThid )

      CALL EXF_INIT_FLD(
     I     'aqh', aqhfile, aqhmask,
     I     aqhperiod, exf_inscal_aqh, aqhconst,
     O     aqh, aqh0, aqh1,
#ifdef USE_EXF_INTERPOLATION
     I     aqh_lon0, aqh_lon_inc,
     I     aqh_lat0, aqh_lat_inc,
     I     aqh_nlon, aqh_nlat, xC, yC, aqh_interpMethod,
#endif
     I     myThid )

# ifdef ALLOW_READ_TURBFLUXES

      CALL EXF_INIT_FLD(
     I     'hs', hs_file, hs_mask,
     I     hs_period, exf_inscal_hs, hs_const,
     O     hs, hs0, hs1,
#  ifdef USE_EXF_INTERPOLATION
     I     hs_lon0, hs_lon_inc,
     I     hs_lat0, hs_lat_inc,
     I     hs_nlon, hs_nlat, xC, yC, hs_interpMethod,
#  endif
     I     myThid )

      CALL EXF_INIT_FLD(
     I     'hl', hl_file, hl_mask,
     I     hl_period, exf_inscal_hl, hl_const,
     O     hl, hl0, hl1,
#  ifdef USE_EXF_INTERPOLATION
     I     hl_lon0, hl_lon_inc,
     I     hl_lat0, hl_lat_inc,
     I     hl_nlon, hl_nlat, xC, yC, hl_interpMethod,
#  endif
     I     myThid )

# endif /* ALLOW_READ_TURBFLUXES */

      CALL EXF_INIT_FLD(
     I     'lwflux', lwfluxfile, lwfluxmask,
     I     lwfluxperiod, exf_inscal_lwflux, lwfluxconst,
     O     lwflux, lwflux0, lwflux1,
#ifdef USE_EXF_INTERPOLATION
     I     lwflux_lon0, lwflux_lon_inc,
     I     lwflux_lat0, lwflux_lat_inc,
     I     lwflux_nlon, lwflux_nlat, xC, yC, lwflux_interpMethod,
#endif
     I     myThid )

#ifdef EXF_READ_EVAP
      CALL EXF_INIT_FLD(
     I     'evap', evapfile, evapmask,
     I     evapperiod, exf_inscal_evap, evapconst,
     O     evap, evap0, evap1,
#ifdef USE_EXF_INTERPOLATION
     I     evap_lon0, evap_lon_inc,
     I     evap_lat0, evap_lat_inc,
     I     evap_nlon, evap_nlat, xC, yC, evap_interpMethod,
#endif
     I     myThid )
#endif /* EXF_READ_EVAP */

      CALL EXF_INIT_FLD(
     I     'precip', precipfile, precipmask,
     I     precipperiod, exf_inscal_precip, precipconst,
     O     precip, precip0, precip1,
#ifdef USE_EXF_INTERPOLATION
     I     precip_lon0, precip_lon_inc,
     I     precip_lat0, precip_lat_inc,
     I     precip_nlon, precip_nlat, xC, yC, precip_interpMethod,
#endif
     I     myThid )

      CALL EXF_INIT_FLD(
     I     'snowprecip', snowprecipfile, snowprecipmask,
     I     snowprecipperiod, exf_inscal_snowprecip, snowprecipconst,
     O     snowprecip, snowprecip0, snowprecip1,
#ifdef USE_EXF_INTERPOLATION
     I     snowprecip_lon0, snowprecip_lon_inc,
     I     snowprecip_lat0, snowprecip_lat_inc,
     I     snowprecip_nlon, snowprecip_nlat, xC, yC,
     I     snowprecip_interpMethod,
#endif
     I     myThid )

#endif /* ALLOW_ATM_TEMP */

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      CALL EXF_INIT_FLD(
     I     'swflux', swfluxfile, swfluxmask,
     I     swfluxperiod,  exf_inscal_swflux, swfluxconst,
     O     swflux, swflux0, swflux1,
#ifdef USE_EXF_INTERPOLATION
     I     swflux_lon0, swflux_lon_inc,
     I     swflux_lat0, swflux_lat_inc,
     I     swflux_nlon, swflux_nlat, xC, yC, swflux_interpMethod,
#endif
     I     myThid )
#endif /* defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING) */

#ifdef ALLOW_DOWNWARD_RADIATION

      CALL EXF_INIT_FLD(
     I     'swdown', swdownfile, swdownmask,
     I     swdownperiod, exf_inscal_swdown, swdownconst,
     O     swdown, swdown0, swdown1,
#ifdef USE_EXF_INTERPOLATION
     I     swdown_lon0, swdown_lon_inc,
     I     swdown_lat0, swdown_lat_inc,
     I     swdown_nlon, swdown_nlat, xC, yC, swdown_interpMethod,
#endif
     I     myThid )

      CALL EXF_INIT_FLD(
     I     'lwdown', lwdownfile, lwdownmask,
     I     lwdownperiod, exf_inscal_lwdown, lwdownconst,
     O     lwdown, lwdown0, lwdown1,
#ifdef USE_EXF_INTERPOLATION
     I     lwdown_lon0, lwdown_lon_inc,
     I     lwdown_lat0, lwdown_lat_inc,
     I     lwdown_nlon, lwdown_nlat, xC, yC, lwdown_interpMethod,
#endif
     I     myThid )

#endif /* ALLOW_DOWNWARD_RADIATION */

#ifdef ATMOSPHERIC_LOADING
      CALL EXF_INIT_FLD(
     I     'apressure', apressurefile, apressuremask,
     I     apressureperiod, exf_inscal_apressure, apressureconst,
     O     apressure, apressure0, apressure1,
#ifdef USE_EXF_INTERPOLATION
     I     apressure_lon0, apressure_lon_inc,
     I     apressure_lat0, apressure_lat_inc,
     I     apressure_nlon, apressure_nlat, xC, yC,
     I     apressure_interpMethod,
#endif
     I     myThid )
#endif /* ATMOSPHERIC_LOADING */

#ifdef EXF_ALLOW_TIDES
      CALL EXF_INIT_FLD(
     I     'tidePot', tidePotFile, tidePotMask,
     I     tidePotPeriod, exf_inscal_tidePot, tidePotconst,
     O     tidePot, tidePot0, tidePot1,
#ifdef USE_EXF_INTERPOLATION
     I     tidePot_lon0, tidePot_lon_inc,
     I     tidePot_lat0, tidePot_lat_inc,
     I     tidePot_nlon, tidePot_nlat, xC, yC, tidePot_interpMethod,
#endif
     I     myThid )
#endif /* EXF_ALLOW_TIDES */

#ifdef EXF_SEAICE_FRACTION
      CALL EXF_INIT_FLD(
     I     'areamask', areamaskfile, areamaskmask,
     I     areamaskperiod, exf_inscal_areamask, areamaskconst,
     O     areamask, areamask0, areamask1,
#ifdef USE_EXF_INTERPOLATION
     I     areamask_lon0, areamask_lon_inc,
     I     areamask_lat0, areamask_lat_inc,
     I     areamask_nlon, areamask_nlat, xC, yC, areamask_interpMethod,
#endif
     I     myThid )
#endif /* EXF_SEAICE_FRACTION */

#ifdef ALLOW_RUNOFF
      CALL EXF_INIT_FLD(
     I     'runoff', runofffile, runoffmask,
     I     runoffperiod, exf_inscal_runoff, runoffconst,
     O     runoff, runoff0, runoff1,
# ifdef USE_EXF_INTERPOLATION
     I     runoff_lon0, runoff_lon_inc,
     I     runoff_lat0, runoff_lat_inc,
     I     runoff_nlon, runoff_nlat, xC, yC, runoff_interpMethod,
# endif
     I     myThid )
# ifdef ALLOW_RUNOFTEMP
      CALL EXF_INIT_FLD(
     I     'runoftemp', runoftempfile, runoffmask,
     I     runoffperiod, exf_inscal_runoftemp, runoftempconst,
     O     runoftemp, runoftemp0, runoftemp1,
#  ifdef USE_EXF_INTERPOLATION
     I     runoff_lon0, runoff_lon_inc,
     I     runoff_lat0, runoff_lat_inc,
     I     runoff_nlon, runoff_nlat, xC, yC, runoff_interpMethod,
#  endif
     I     myThid )
# endif /* ALLOW_RUNOFTEMP */
#endif /* ALLOW_RUNOFF */

#ifdef ALLOW_SALTFLX
      CALL EXF_INIT_FLD(
     I     'saltflx', saltflxfile, saltflxmask,
     I     saltflxperiod, exf_inscal_saltflx, saltflxconst,
     O     saltflx, saltflx0, saltflx1,
#ifdef USE_EXF_INTERPOLATION
     I     saltflx_lon0, saltflx_lon_inc,
     I     saltflx_lat0, saltflx_lat_inc,
     I     saltflx_nlon, saltflx_nlat, xC, yC, saltflx_interpMethod,
#endif
     I     myThid )
#endif /* ALLOW_SALTFLX */

#ifdef ALLOW_CLIMSST_RELAXATION
      CALL EXF_INIT_FLD(
     I     'climsst', climsstfile, climsstmask,
     I     climsstperiod, exf_inscal_climsst, climsstconst,
     O     climsst, climsst0, climsst1,
#ifdef USE_EXF_INTERPOLATION
     I     climsst_lon0, climsst_lon_inc,
     I     climsst_lat0, climsst_lat_inc,
     I     climsst_nlon, climsst_nlat, xC, yC, climsst_interpMethod,
#endif
     I     myThid )
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
      CALL EXF_INIT_FLD(
     I     'climsss', climsssfile, climsssmask,
     I     climsssperiod, exf_inscal_climsss, climsssconst,
     O     climsss, climsss0, climsss1,
#ifdef USE_EXF_INTERPOLATION
     I     climsss_lon0, climsss_lon_inc,
     I     climsss_lat0, climsss_lat_inc,
     I     climsss_nlon, climsss_nlat, xC, yC, climsss_interpMethod,
#endif
     I     myThid )
#endif

#ifdef ALLOW_CLIMSTRESS_RELAXATION
      CALL EXF_INIT_FLD(
     I     'climustr', climustrfile, climustrmask,
     I     climustrperiod, exf_inscal_climustr, climustrconst,
     O     climustr, climustr0, climustr1,
#ifdef USE_EXF_INTERPOLATION
     I     climustr_lon0, climustr_lon_inc,
     I     climustr_lat0, climustr_lat_inc,
     I     climustr_nlon, climustr_nlat, xC, yC, climustr_interpMethod,
#endif
     I     myThid )

      CALL EXF_INIT_FLD(
     I     'climvstr', climvstrfile, climvstrmask,
     I     climvstrperiod, exf_inscal_climvstr, climvstrconst,
     O     climvstr, climvstr0, climvstr1,
#ifdef USE_EXF_INTERPOLATION
     I     climvstr_lon0, climvstr_lon_inc,
     I     climvstr_lat0, climvstr_lat_inc,
     I     climvstr_nlon, climvstr_nlat, xC, yC, climvstr_interpMethod,
#endif
     I     myThid )
#endif /* CLIMSTRESS_RELAXATION */

#ifdef ALLOW_BULK_OFFLINE
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

# ifdef ALLOW_CLIMSST_RELAXATION
      _EXCH_XY_RL(climsst, myThid)
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
      _EXCH_XY_RL(climsss, myThid)
# endif
# ifdef ALLOW_CLIMSTRESS_RELAXATION
      CALL EXCH_UV_XY_RL( climustr, climvstr, .TRUE., myThid )
# endif
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
# ifdef ALLOW_CLIMSST_RELAXATION
           IF ( climsstfile .NE. ' ' .AND.
     &          climsstperiod .EQ. 0. )
     &          theta(i,j,ks,bi,bj) = climsst(i,j,bi,bj)
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
           IF ( climsssfile .NE. ' ' .AND.
     &          climsssperiod .EQ. 0. )
     &          salt(i,j,ks,bi,bj) = climsss(i,j,bi,bj)
# endif
# ifdef ALLOW_CLIMSTRESS_RELAXATION
           IF ( climustrfile .NE. ' ' .AND.
     &          climustrperiod .EQ. 0. )
     &          uVel(i,j,ks,bi,bj) = climustr(i,j,bi,bj)
           IF ( climvstrfile .NE. ' ' .AND.
     &          climvstrperiod .EQ. 0. )
     &          vVel(i,j,ks,bi,bj) = climvstr(i,j,bi,bj)
# endif
           IF ( maskC(i,j,ks,bi,bj) .NE. 0. .AND.
     &             theta(i,j,ks,bi,bj) .EQ. 0. ) THEN
                PRINT *, 'ph-warn-exf-init ', i, j, theta(i,j,ks,bi,bj)
cph                STOP 'in exf_init'
           ENDIF
          ENDDO
         ENDDO
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_BULK_OFFLINE */

      RETURN
      END
