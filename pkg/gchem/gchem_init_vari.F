#include "GCHEM_OPTIONS.h"
#ifdef ALLOW_DARWIN
# include "DARWIN_OPTIONS.h"
#endif

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_INIT_VARI( myThid )

C !DESCRIPTION:
C calls subroutines that initialize any time dependent variables
C for any tracer experiment

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_AUTODIFF
# include "DYNVARS.h"
#endif /* ALLOW_AUTODIFF */
#include "GCHEM.h"
#include "GCHEM_SIZE.h"
#include "GCHEM_FIELDS.h"
#include "GCHEM_AUX_FIELDS.h"
#ifdef ALLOW_EXF
# include "GCHEM_EXF.h"
# ifdef USE_EXF_INTERPOLATION
#  include "EXF_INTERP_SIZE.h"
#  include "GCHEM_INTERP_PARAM.h"
# endif
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

C !LOCAL VARIABLES: ====================================================
C  i,j,k,bi,bj          :: loop indices
C  jTr                  :: ptracer number
      INTEGER bi,bj
      INTEGER i,j
#ifdef GCHEM_ALLOW_FFIELDS
      INTEGER m
      INTEGER intimeP, intime0, intime1
      _RL aWght,bWght
# ifdef ALLOW_EXF
      _RL myLocTime
# endif
#endif
#ifdef GCHEM_ADD2TR_TENDENCY
      INTEGER k
      INTEGER jTr
#endif /* GCHEM_ADD2TR_TENDENCY */

C--   Initialise GCHEM variables:

#ifdef GCHEM_ADD2TR_TENDENCY
      DO jTr = 1, GCHEM_tendTr_num
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k = 1, Nr
          DO j = 1-OLy, sNy+OLy
           DO i = 1-OLx, sNx+OLx
            gchemTendency(i,j,k,bi,bj,jTr) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* GCHEM_ADD2TR_TENDENCY */

C-- Initialize Geo-Chemistry forcing fields

#ifdef GCHEM_ALLOW_FFIELDS
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C     First call requires that we initialize everything to zero for safety
        GCHEM_ldRec(bi,bj) = 0
C     set reasonable values fields that need at least something
        m = 1
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          gchemapCO2 (i,j,bi,bj) = gchem_apco2_const*maskC(i,j,1,bi,bj)
          gchemWind  (i,j,bi,bj) = gchem_wind_const *maskC(i,j,1,bi,bj)
          gchemAtmosP(i,j,bi,bj) = gchem_apres_const*maskC(i,j,1,bi,bj)
          gchemPAR   (i,j,bi,bj) = gchem_par_const  *maskC(i,j,1,bi,bj)
          gchemFe    (i,j,bi,bj) = gchem_Fe_const   *maskC(i,j,1,bi,bj)
          gchemSi    (i,j,bi,bj) = gchem_Si_const   *maskC(i,j,1,bi,bj)
#ifdef GCHEM_3D_SILICA
          DO k = 1, Nr
           gchemSi3D(i,j,k,bi,bj)= gchem_3DSi_const *maskC(i,j,k,bi,bj)
          ENDDO
#endif
          gchemIce   (i,j,bi,bj) = gchem_ice_const  *maskC(i,j,1,bi,bj)
          gchemChl   (i,j,bi,bj) = gchem_chl_const  *maskC(i,j,1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( .NOT. gchem_useEXF ) THEN
       CALL LEF_ZERO( gchemSi0,myThid )
       CALL LEF_ZERO( gchemSi1,myThid )
#ifdef GCHEM_3D_SILICA
       CALL LEF_ZERO( gchem3DSi0,myThid )
       CALL LEF_ZERO( gchem3DSi1,myThid )
#endif
       CALL LEF_ZERO( gchemWind0,myThid )
       CALL LEF_ZERO( gchemWind1,myThid )
       CALL LEF_ZERO( gchemIce0,myThid )
       CALL LEF_ZERO( gchemIce1,myThid )
       CALL LEF_ZERO( gchemApres0,myThid )
       CALL LEF_ZERO( gchemApres1,myThid )
       CALL LEF_ZERO( gchemPAR0,myThid )
       CALL LEF_ZERO( gchemPAR1,myThid )
       CALL LEF_ZERO( gchemFe0,myThid )
       CALL LEF_ZERO( gchemFe1,myThid )
       CALL LEF_ZERO( gchemapCO20,myThid )
       CALL LEF_ZERO( gchemapCO21,myThid )
       CALL LEF_ZERO( gchemchl0,myThid )
       CALL LEF_ZERO( gchemchl1,myThid )

C--   Now calculate whether it is time to update the forcing arrays
        CALL GET_PERIODIC_INTERVAL(
     O                   intimeP, intime0, intime1, bWght, aWght,
     I                   externForcingCycle, externForcingPeriod,
     I                   deltaTClock, startTime, myThid )

        IF ( gchem_WindFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_WindFile,gchemWind0,intime0,
     &         nIter0,myThid )
          CALL READ_REC_XY_RS( gchem_WindFile,gchemWind1,intime1,
     &         nIter0,myThid )
        ENDIF

        IF ( gchem_SiFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_SiFile,gchemSi0,intime0,
     &         nIter0,myThid )
          CALL READ_REC_XY_RS( gchem_SiFile,gchemSi1,intime1,
     &         nIter0,myThid )
        ENDIF

#ifdef GCHEM_3D_SILICA
        IF ( gchem_3DSiFile .NE. ' ' ) THEN
          CALL READ_REC_XYZ_RS( gchem_3DSiFile,gchem3DSi0,intime0,
     &         nIter0,myThid )
          CALL READ_REC_XYZ_RS( gchem_3DSiFile,gchem3DSi1,intime1,
     &         nIter0,myThid )
        ENDIF
#endif

        IF ( gchem_parFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_parFile,gchemPAR0,intime0,
     &         nIter0,myThid )
          CALL READ_REC_XY_RS( gchem_parFile,gchemPAR1,intime1,
     &         nIter0,myThid )
        ENDIF

        IF ( gchem_FeFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_FeFile,gchemFe0,intime0,
     &         nIter0,myThid )
          CALL READ_REC_XY_RS( gchem_FeFile,gchemFe1,intime1,
     &         nIter0,myThid )
         ENDIF
        ENDIF

        IF ( gchem_IceFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_IceFile,gchemIce0,intime0,
     &         nIter0,myThid )
          CALL READ_REC_XY_RS( gchem_IceFile,gchemIce1,intime1,
     &         nIter0,myThid )
        ENDIF

        IF ( gchem_apco2File .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_apco2File,gchemapco20,intime0,
     &         nIter0,myThid )
          CALL READ_REC_XY_RS( gchem_apco2File,gchemapco21,intime1,
     &         nIter0,myThid )
        ENDIF

        IF ( gchem_iceFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_iceFile,gchemIce0,intime0,
     &         nIter0,myThid )
          CALL READ_REC_XY_RS( gchem_iceFile,gchemIce1,intime1,
     &         nIter0,myThid )
        ENDIF

        IF ( gchem_apresFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_apresFile,gchemApres0,intime0,
     &         nIter0,myThid )
          CALL READ_REC_XY_RS( gchem_apresFile,gchemApres1,intime1,
     &         nIter0,myThid )
        ENDIF

        IF ( gchem_chlFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_chlFile,gchemCHL0,intime0,
     &         nIter0,myThid )
          CALL READ_REC_XY_RS( gchem_chlFile,gchemCHL1,intime1,
     &         nIter0,myThid )
        ENDIF

C     if not gchem_useEXF
      ENDIF

#ifdef ALLOW_EXF
      IF ( gchem_useEXF ) THEN

       IF ( gchem_SiFile .NE. ' ' ) THEN
C     This would be the exf-way to initialise a forcing field: just use
C     a constant value. But, because in pkg/bling (and maybe elsewhere)
C     gchemSi is already used before the first timestep and before
C     gchem_fields_load is called, gchemSi needs to have the correct
C     value with respect to the current timestep (nIter0) and myTime
C     (startTime + deltaTClock*nIter0). Therefore, we do not call
C     exf_init_fld but exf_set_fld instead.
c        CALL EXF_INIT_FLD (
c     &     'gchemSi', gchem_SiFile, gchem_SiMask,
c     &     gchem_SiPeriod, gchem_inscal_Si, gchem_Si_const,
c     &     gchemSi, gchemSi0, gchemSi1,
c#ifdef USE_EXF_INTERPOLATION
c     &     Si_lon0, Si_lon_inc,
c     &     Si_lat0, Si_lat_inc,
c     &     Si_nlon, Si_nlat, xC, yC,
c     &     Si_interpMethod,
c#endif
c     &     mythid )
        myLocTime = startTime + deltaTClock*nIter0
        CALL EXF_SET_FLD(
     &     'gchemSi', gchem_SiFile, gchem_SiMask,
     &     gchem_SiStartTime, gchem_SiPeriod, gchem_SiRepCycle,
     &     gchem_inscal_Si,
     &     gchem_Si_exfremo_intercept, gchem_Si_exfremo_slope,
     &     gchemSi, gchemSi0, gchemSi1,
# ifdef USE_EXF_INTERPOLATION
     &     Si_lon0, Si_lon_inc,
     &     Si_lat0, Si_lat_inc,
     &     Si_nlon, Si_nlat, xC, yC,
     &     Si_interpMethod,
# endif
     &     myLocTime, nIter0, mythid )
       ENDIF

#ifdef GCHEM_3D_SILICA
       IF ( gchem_3DSiFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'gchem3DSi', gchem_3DSiFile, gchem_3DSiMask,
     &     gchem_3DSiPeriod, gchem_inscal_3DSi, gchem_3DSi_const,
     &     gchem3DSi, gchem3DSi0, gchem3DSi1,
#ifdef USE_EXF_INTERPOLATION
     &     3DSi_lon0, 3DSi_lon_inc,
     &     3DSi_lat0, 3DSi_lat_inc,
     &     3DSi_nlon, 3DSi_nlat, xC, yC,
     &     3DSi_interpMethod,
#endif
     &     mythid )
       ENDIF
#endif /* GCHEM_3D_SILICA */

       IF ( gchem_WindFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
      &     'gchemWind', gchem_WindFile, gchem_WindMask,
      &     gchem_WindPeriod, gchem_inscal_Wind, gchem_wind_const,
      &     gchemWind, gchemWind0, gchemWind1,
#ifdef USE_EXF_INTERPOLATION
      &     wind_lon0, wind_lon_inc,
      &     wind_lat0, wind_lat_inc,
      &     wind_nlon, wind_nlat, xC, yC,
      &     wind_interpMethod,
#endif
      &     mythid )
       ENDIF

       IF ( gchem_apresFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'gchemApres', gchem_apresFile, gchem_apresMask,
     &     gchem_apresPeriod, gchem_inscal_apres, gchem_apres_const,
     &     gchemApres, gchemApres0, gchemApres1,
#ifdef USE_EXF_INTERPOLATION
     &     apres_lon0, apres_lon_inc,
     &     apres_lat0, apres_lat_inc,
     &     apres_nlon, apres_nlat, xC, yC,
     &     apres_interpMethod,
#endif
     &     mythid )
       ENDIF

       IF ( gchem_iceFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'gchemIce', gchem_iceFile, gchem_iceMask,
     &     gchem_icePeriod, gchem_inscal_ice, gchem_ice_const,
     &     gchemIce, gchemIce0, gchemIce1,
#ifdef USE_EXF_INTERPOLATION
     &     ice_lon0, ice_lon_inc,
     &     ice_lat0, ice_lat_inc,
     &     ice_nlon, ice_nlat, xC, yC,
     &     ice_interpMethod,
#endif
     &     mythid )
       ENDIF

#ifdef GCHEM_3D_SILICA
       IF ( gchem_3DSiFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'gchem3DSi', gchem_3DSiFile, gchem_3DSiMask,
     &     gchem_3DSiPeriod, gchem_inscal_3DSi, gchem_3DSi_const,
     &     gchem3DSi, gchem3DSi0, gchem3DSi1,
#ifdef USE_EXF_INTERPOLATION
     &     3DSi_lon0, 3DSi_lon_inc,
     &     3DSi_lat0, 3DSi_lat_inc,
     &     3DSi_nlon, 3DSi_nlat, xC, yC,
     &     3DSi_interpMethod,
#endif
     &     mythid )
       ENDIF
#endif /* GCHEM_3D_SILICA */

       IF ( gchem_WindFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
      &     'gchemWind', gchem_WindFile, gchem_WindMask,
      &     gchem_WindPeriod, gchem_inscal_Wind, gchem_wind_const,
      &     gchemWind, gchemWind0, gchemWind1,
#ifdef USE_EXF_INTERPOLATION
      &     wind_lon0, wind_lon_inc,
      &     wind_lat0, wind_lat_inc,
      &     wind_nlon, wind_nlat, xC, yC,
      &     wind_interpMethod,
#endif
      &     mythid )
       ENDIF

       IF ( gchem_apresFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'gchemApres', gchem_apresFile, gchem_apresMask,
     &     gchem_apresPeriod, gchem_inscal_apres, gchem_apres_const,
     &     gchemApres, gchemApres0, gchemApres1,
#ifdef USE_EXF_INTERPOLATION
     &     apres_lon0, apres_lon_inc,
     &     apres_lat0, apres_lat_inc,
     &     apres_nlon, apres_nlat, xC, yC,
     &     apres_interpMethod,
#endif
     &     mythid )
       ENDIF

       IF ( gchem_iceFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'gchemIce', gchem_iceFile, gchem_iceMask,
     &     gchem_icePeriod, gchem_inscal_ice, gchem_ice_const,
     &     gchemIce, gchemIce0, gchemIce1,
#ifdef USE_EXF_INTERPOLATION
     &     ice_lon0, ice_lon_inc,
     &     ice_lat0, ice_lat_inc,
     &     ice_nlon, ice_nlat, xC, yC,
     &     ice_interpMethod,
#endif
     &     mythid )
       ENDIF

       IF ( gchem_parFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'gchemPAR', gchem_parFile, gchem_parMask,
     &     gchem_parPeriod, gchem_inscal_par, gchem_par_const,
     &     gchemPAR, gchemPAR0, gchemPAR1,
#ifdef USE_EXF_INTERPOLATION
     &     par_lon0, par_lon_inc,
     &     par_lat0, par_lat_inc,
     &     par_nlon, par_nlat, xC, yC,
     &     par_interpMethod,
#endif
     &     mythid )
       ENDIF

       IF ( gchem_FeFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'gchemFe', gchem_FeFile, gchem_FeMask,
     &     gchem_FePeriod, gchem_inscal_Fe, gchem_Fe_const,
     &     gchemFe, gchemFe0, gchemFe1,
#ifdef USE_EXF_INTERPOLATION
     &     Fe_lon0, Fe_lon_inc,
     &     Fe_lat0, Fe_lat_inc,
     &     Fe_nlon, Fe_nlat, xC, yC,
     &     Fe_interpMethod,
#endif
     &     mythid )
       ENDIF

       IF ( gchem_apco2File .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'gchemapCO2', gchem_apco2File, gchem_apco2Mask,
     &     gchem_apco2Period, gchem_inscal_apco2, gchem_apco2_const,
     &     gchemapCO2, gchemApco20, gchemApco21,
#ifdef USE_EXF_INTERPOLATION
     &     apco2_lon0, apco2_lon_inc,
     &     apco2_lat0, apco2_lat_inc,
     &     apco2_nlon, apco2_nlat, xC, yC,
     &     apco2_interpMethod,
#endif
     &     mythid )
       ENDIF

       IF ( gchem_chlFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'gchemCHL', gchem_chlFile, gchem_chlMask,
     &     gchem_chlPeriod, gchem_inscal_chl, gchem_chl_const,
     &     gchemchl, gchemchl0, gchemchl1,
#ifdef USE_EXF_INTERPOLATION
     &     chl_lon0, chl_lon_inc,
     &     chl_lat0, chl_lat_inc,
     &     chl_nlon, chl_nlat, xC, yC,
     &     chl_interpMethod,
#endif
     &     mythid )
       ENDIF

C     useEXF
      ENDIF
#endif /* ALLOW_EXF */

      _EXCH_XY_RL( gchemSi, myThid )
#ifdef GCHEM_3D_SILICA
      _EXCH_XYZ_RL( gchem3DSi, myThid )
#endif
      _EXCH_XY_RL( gchemWind, myThid )
      _EXCH_XY_RL( gchemIce, myThid )
      _EXCH_XY_RL( gchemAtmosP, myThid )
      _EXCH_XY_RL( gchemPAR, myThid )
      _EXCH_XY_RL( gchemFe, myThid )
      _EXCH_XY_RL( gchemapCO2, myThid )
      _EXCH_XY_RL( gchemchl, myThid )
      #endif /* GCHEM_ALLOW_FFIELDS */

C--   Initialise other Geo-Chemistry pkg variables:

#ifdef ALLOW_DIC
# ifdef ALLOW_AUTODIFF
      IF ( .NOT.useDIC ) STOP 'ABNORMAL END: S/R GCHEM_INIT_VARI'
# else /* ALLOW_AUTODIFF */
      IF ( useDIC ) THEN
# endif /* ALLOW_AUTODIFF */
         CALL DIC_INIT_VARIA(myThid)
# ifndef ALLOW_AUTODIFF
      ENDIF
# endif
#endif /* ALLOW_DIC */

#ifdef ALLOW_BLING
# ifndef ALLOW_AUTODIFF_TAMC
      IF ( useBLING ) THEN
# endif
         CALL BLING_INIT_VARIA(myThid)
         CALL BLING_INI_FORCING(myThid)
         CALL BLING_CARBONATE_INIT(myThid)
# ifndef ALLOW_AUTODIFF_TAMC
      ENDIF
# endif
#endif /* ALLOW_BLING */

#ifdef ALLOW_SPOIL
      IF ( useSPOIL ) THEN
         CALL SPOIL_INIT_VARIA( myThid )
      ENDIF
#endif /* ALLOW_SPOIL */

#ifdef ALLOW_DARWIN
      IF ( useDARWIN ) THEN
         CALL DARWIN_INIT_VARI(myThid )
#ifdef ALLOW_CARBON
         CALL DIC_SURFFORCING_INIT(myThid)
         CALL DIC_DIAGS_INIT(myThid)
#endif
      ENDIF
#endif /* ALLOW_DARWIN */

      RETURN
      END
