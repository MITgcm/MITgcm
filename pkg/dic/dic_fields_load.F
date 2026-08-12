#include "DIC_OPTIONS.h"

CBOP
C !ROUTINE: DIC_FIELDS_LOAD

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_FIELDS_LOAD (
     I           myTime, myIter, myThid )

C !DESCRIPTION:
C  Read in fields needed for CO2,O2 fluxterms, silica for pH calculation

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"
#include "DIC_LOAD.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
# include "EXF_FIELDS.h"
# include "DIC_EXF.h"
# ifdef USE_EXF_INTERPOLATION
#  include "EXF_INTERP_SIZE.h"
#  include "DIC_INTERP_PARAM.h"
# endif
#endif
C !INPUT PARAMETERS: ===================================================
C  myTime               :: current time
C  myIter               :: current timestep
C  myThid               :: thread number
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIC

c !LOCAL VARIABLES: ===================================================
      INTEGER bi, bj, i, j
      INTEGER intimeP, intime0, intime1
      _RL aWght,bWght
#ifdef DIC_CALCITE_SAT
      INTEGER k
#endif
CEOP
      IF ( .NOT. dic_useEXF ) THEN
       IF (  DIC_forcingCycle.GT.0. _d 0 ) THEN

C--    Now calculate whether it is time to update the forcing arrays
        CALL GET_PERIODIC_INTERVAL(
     O                    intimeP, intime0, intime1, bWght, aWght,
     I                    DIC_forcingCycle, DIC_forcingPeriod,
     I                    deltaTClock, myTime, myThid )

        bi = myBxLo(myThid)
        bj = myByLo(myThid)
#ifdef ALLOW_DEBUG
        IF ( debugLevel.GE.debLevB ) THEN
         _BEGIN_MASTER(myThid)
         WRITE(standardMessageUnit,'(A,I10,A,4I5,A,2F14.10)')
     &    ' DIC_FIELDS_LOAD,', myIter,
     &    ' : iP,iLd,i0,i1=', intimeP,DIC_ldRec(bi,bj), intime0,intime1,
     &    ' ; Wght=', bWght, aWght
         _END_MASTER(myThid)
        ENDIF
#endif /* ALLOW_DEBUG */

#ifdef ALLOW_AUTODIFF
C-    assuming that we call S/R DIC_FIELDS_LOAD at each time-step and
C     with increasing time, this will catch when we need to load new records;
C     But with Adjoint run, this is not always the case => might end-up using
C     the wrong time-records
        IF ( intime0.NE.intimeP .OR. myIter.EQ.nIter0 ) THEN
#else /* ALLOW_AUTODIFF */
C-    Make no assumption on sequence of calls to DIC_FIELDS_LOAD ;
C     This is the correct formulation (works in Adjoint run).
C     Unfortunatly, produces many recomputations <== not used until it is fixed
        IF ( intime1.NE.DIC_ldRec(bi,bj) ) THEN
#endif /* ALLOW_AUTODIFF */

C--   If the above condition is met then we need to read in
C     data for the period ahead and the period behind myTime.
         IF ( debugLevel.GE.debLevZero ) THEN
          _BEGIN_MASTER(myThid)
          WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &     ' DIC_FIELDS_LOAD, it=', myIter,
     &     ' : Reading new data, i0,i1=', intime0, intime1,
     &     ' (prev=', intimeP, DIC_ldRec(bi,bj), ' )'
          _END_MASTER(myThid)
         ENDIF

         _BARRIER

         IF ( DIC_windFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS( DIC_windFile,dicwind0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( DIC_windFile,dicwind1,intime1,
     &         myIter,myThid )
         ENDIF
         IF ( DIC_atmospFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS( DIC_atmospFile,atmosp0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( DIC_atmospFile,atmosp1,intime1,
     &         myIter,myThid )
         ENDIF
         IF ( DIC_silicaFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS( DIC_silicaFile,silicaSurf0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( DIC_silicaFile,silicaSurf1,intime1,
     &         myIter,myThid )
         ENDIF
#ifdef DIC_CALCITE_SAT
         IF ( DIC_silicaDeepFile .NE. ' '  ) THEN
          CALL READ_REC_XYZ_RS( DIC_silicaDeepFile,silicaDeep0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XYZ_RS( DIC_silicaDeepFile,silicaDeep1,intime1,
     &         myIter,myThid )
         ENDIF
#endif
         IF ( DIC_iceFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS( DIC_iceFile,ice0,intime0,
     &        myIter,myThid )
          CALL READ_REC_XY_RS( DIC_iceFile,ice1,intime1,
     &        myIter,myThid )
         ENDIF
#ifdef READ_PAR
         IF ( DIC_parFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS( DIC_parFile,par0,intime0,
     &        myIter,myThid )
          CALL READ_REC_XY_RS( DIC_parFile,par1,intime1,
     &        myIter,myThid )
         ENDIF
#endif
#ifdef LIGHT_CHL
C--   Load chlorophyll climatology data, unit for chlorophyll : mg/m3
         IF ( DIC_chlaFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS( DIC_chlaFile,chlinput,1,
     &        myIter,myThid )
         ENDIF
#endif
#ifdef ALLOW_FE
         IF ( DIC_ironFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS( DIC_ironFile,feinput0,intime0,
     &        myIter,myThid )
          CALL READ_REC_XY_RS( DIC_ironFile,feinput1,intime1,
     &        myIter,myThid )
         ENDIF
#endif

C--   fill-in overlap after loading temp arrays:
         _EXCH_XY_RS(dicwind0, myThid )
         _EXCH_XY_RS(dicwind1, myThid )
         _EXCH_XY_RS(atmosp0, myThid )
         _EXCH_XY_RS(atmosp1, myThid )
         _EXCH_XY_RS(silicaSurf0, myThid )
         _EXCH_XY_RS(silicaSurf1, myThid )
#ifdef DIC_CALCITE_SAT
       IF ( DIC_silicaDeepFile .NE. ' '  ) THEN
         _EXCH_XYZ_RS(silicaDeep0, myThid )
         _EXCH_XYZ_RS(silicaDeep1, myThid )
       ENDIF
#endif
         _EXCH_XY_RS(ice0, myThid )
         _EXCH_XY_RS(ice1, myThid )
#ifdef READ_PAR
         _EXCH_XY_RS(par0, myThid )
         _EXCH_XY_RS(par1, myThid )
#endif
#ifdef LIGHT_CHL
         _EXCH_XY_RS(chlinput, myThid )
#endif
#ifdef ALLOW_FE
         _EXCH_XY_RS(feinput0, myThid )
         _EXCH_XY_RS(feinput1, myThid )
#endif

C-    save newly loaded time-record
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DIC_ldRec(bi,bj) = intime1
          ENDDO
         ENDDO

C-     end if-bloc (time to load new fields)
        ENDIF

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          IF ( DIC_windFile .NE. ' '  ) THEN
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              WIND(i,j,bi,bj) = bWght*dicwind0(i,j,bi,bj)
     &                        + aWght*dicwind1(i,j,bi,bj)
             ENDDO
            ENDDO

C calculate piston velocity
C QQ: note - we should have wind speed variance in here
C QQ         also need to check units, and conversion factors
c          pisvel(i,j,bi,bj)  =0.337*wind(i,j,bi,bj)**2/3.6d5    !QQQQ
          ENDIF
#ifndef USE_PLOAD
          IF ( DIC_atmospFile .NE. ' '  ) THEN
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              AtmosP(i,j,bi,bj) = bWght*atmosp0(i,j,bi,bj)
     &                          + aWght*atmosp1(i,j,bi,bj)
             ENDDO
            ENDDO
          ENDIF
#endif
#ifdef DIC_CALCITE_SAT
          IF ( useCalciteSaturation .AND.
     &         DIC_silicaDeepFile .NE. ' '  ) THEN
           DO k=1,Nr
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              silicaDeep(i,j,k,bi,bj) = bWght*silicaDeep0(i,j,k,bi,bj)
     &                                + aWght*silicaDeep1(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDIF
#endif
          IF ( DIC_silicaFile .NE. ' '  ) THEN
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
C If file provided for surface silicate, read it in
              silicaSurf(i,j,bi,bj) = bWght*silicaSurf0(i,j,bi,bj)
     &                              + aWght*silicaSurf1(i,j,bi,bj)
             ENDDO
            ENDDO
#ifndef ALLOW_AUTODIFF
#ifdef DIC_CALCITE_SAT
          ELSEIF ( DIC_silicaDeepFile .NE. ' '  ) THEN
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
C If no surface silicate file but deep (3d) silicate provided, use top level
              silicaSurf(i,j,bi,bj) = bWght*silicaDeep0(i,j,1,bi,bj)
     &                              + aWght*silicaDeep1(i,j,1,bi,bj)
             ENDDO
            ENDDO
#endif /* DIC_CALCITE_SAT */
#endif /* not ALLOW_AUTODIFF */
          ENDIF

          IF ( DIC_iceFile .NE. ' '  ) THEN
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              fIce(i,j,bi,bj) = bWght*ice0(i,j,bi,bj)
     &                        + aWght*ice1(i,j,bi,bj)
             ENDDO
            ENDDO
          ENDIF

#ifdef READ_PAR
          IF ( DIC_parFile .NE. ' '  ) THEN
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              PAR(i,j,bi,bj) = bWght*par0(i,j,bi,bj)
     &                       + aWght*par1(i,j,bi,bj)
             ENDDO
            ENDDO
          ENDIF
#endif
#ifdef LIGHT_CHL
          IF ( DIC_chlaFile .NE. ' '  ) THEN
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              CHL(i,j,bi,bj) = chlinput(i,j,bi,bj)
             ENDDO
            ENDDO
          ENDIF
#endif
#ifdef ALLOW_FE
          IF ( DIC_ironFile .NE. ' '  ) THEN
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              InputFe(i,j,bi,bj) = bWght*feinput0(i,j,bi,bj)
     &                           + aWght*feinput1(i,j,bi,bj)
             ENDDO
            ENDDO
          ENDIF
#endif
         ENDDO
        ENDDO

C endif for DIC_forcingCycle
       ENDIF
C endif .not. dic_useEXF
      ENDIF

C-----------------------------------------------------------
#ifdef ALLOW_EXF
C     forcing load fields with pkg/exf tools
      IF ( dic_useEXF ) THEN

C If a wind file is given in data.dic, read it
       IF ( dic_WindFile .NE. ' ' ) THEN
        CALL EXF_SET_FLD(
     &     'dicWind', dic_windfile, dic_windMask,
     &     dic_windStartTime, dic_windPeriod, dic_windRepCycle,
     &     dic_inscal_wind,
     &     dic_wind_exfremo_intercept, dic_wind_exfremo_slope,
     &     Wind, dicWind0, dicWind1,
# ifdef USE_EXF_INTERPOLATION
     &     wind_lon0, wind_lon_inc,
     &     wind_lat0, wind_lat_inc,
     &     wind_nlon, wind_nlat, xC, yC,
     &     wind_interpMethod,
# endif
     &     mytime, myiter, mythid )
C Otherwise get winds from PKG/EXF (useEXF ensures physical model is actually using pkg/EXF)
       ELSEIF ( useEXF
     &   .AND. (( uwindfile .NE. ' ' .AND.
     &           vwindfile .NE. ' ' )
     &  .OR. ( ustressfile .NE. ' ' .AND.
     &         vstressfile .NE. ' ' ))) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            Wind(i,j,bi,bj) = wspeed(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

#ifndef USE_PLOAD
C If a pressure files is given in data.dic, read it
       IF ( dic_atmospFile .NE. ' ' ) THEN
        CALL EXF_SET_FLD(
     &     'dicAtmosP', dic_atmospFile, dic_atmospMask,
     &     dic_atmospStartTime, dic_atmospPeriod, dic_atmospRepCycle,
     &     dic_inscal_atmosp,
     &     dic_atmosp_exfremo_intercept, dic_atmosp_exfremo_slope,
     &     AtmosP, atmosp0, atmosp1,
# ifdef USE_EXF_INTERPOLATION
     &     atmosp_lon0, atmosp_lon_inc,
     &     atmosp_lat0, atmosp_lat_inc,
     &     atmosp_nlon, atmosp_nlat, xC, yC,
     &     atmosp_interpMethod,
# endif
     &     mytime, myiter, mythid )
C Otherwise get atmospheric pressure from PKG/EXF
       ELSEIF ( useEXF .AND. dic_atmospFile .NE. ' ' ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            AtmosP(i,j,bi,bj) = apressure(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
#endif /* NOT USE_PLOAD */

       IF ( dic_SilicaFile .NE. ' ' ) THEN
        CALL EXF_SET_FLD(
     &     'dicSilicaSurf', dic_SilicaFile, dic_SilicaSurfMask,
     &     dic_SilicaSurfStartTime, dic_SilicaSurfPeriod, 
     &     dic_SilicaSurfRepCycle, dic_inscal_SilicaSurf,
     &     dic_SilicaSurf_exfremo_intercept, 
     &     dic_SilicaSurf_exfremo_slope,
     &     silicaSurf, silicaSurf0, silicaSurf1,
# ifdef USE_EXF_INTERPOLATION
     &     silicaSurfLon0, silicaSurfLonInc,
     &     silicaSurfLat0, silicaSurfLatInc,
     &     silicaSurfNlon, silicaSurfNlat, xC, yC,
     &     silicaSurfInterpMethod,
# endif
     &     mytime, myiter, mythid )
       ENDIF

#ifdef DIC_3D_SILICA
       IF ( useCalciteSaturation .AND.
     &         DIC_silicaDeepFile .NE. ' '  ) THEN
        CALL EXF_SET_FLD3D(
     &      'dicSilicaDeep', dic_SilicaDeepFile, dic_SilicaDeepMask,
     &      dic_SilicaDeepStartTime, dic_SilicaDeepPeriod,
     &      dic_SilicaDeepRepCycle,
     &      dic_inscal_SilicaDeep,
     &      dic_SilicaDeep_exfremo_intercept,
     &      dic_SilicaDeep_exfremo_slope,
     &      silicaDeep, silicaDeep0, silicaDeep1,
# ifdef USE_EXF_INTERPOLATION
     &      SilicaDeep_lon0, SilicaDeep_lon_inc,
     &      SilicaDeep_lat0, SilicaDeep_lat_inc,
     &      SilicaDeep_nlon, SilicaDeep_nlat, xC, yC,
     &      SilicaDeep_interpMethod,
     &      SilicaDeep_nzin, SilicaDeep_dzin,
# endif
     &      mytime, myiter, mythid )
#ifndef ALLOW_AUTODIFF
        IF ( dic_SilicaFile .NE. ' ' ) THEN
          DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
C If no surface silicate file but deep (3d) silicate provided, use top level
              silicaSurf(i,j,bi,bj) = silicaDeep(i,j,1,bi,bj)
            ENDDO
          ENDDO
        ENDIF
#endif /* ALLOW_AUTODIFF */
       ENDIF
#endif

#ifdef READ_PAR
       IF ( dic_parFile .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     &     'dicPAR', dic_parFile, dic_parMask,
     &     dic_parStartTime, dic_parPeriod, dic_parRepCycle,
     &     dic_inscal_par,
     &     dic_par_exfremo_intercept, dic_par_exfremo_slope,
     &     PAR, PAR0, PAR1,
# ifdef USE_EXF_INTERPOLATION
     &     par_lon0, par_lon_inc,
     &     par_lat0, par_lat_inc,
     &     par_nlon, par_nlat, xC, yC,
     &     par_interpMethod,
# endif
     &     mytime, myiter, mythid )
       ENDIF
#endif /* READ_PAR */

#ifdef ALLOW_FE
       IF ( dic_ironFile .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     &     'InputFe', dic_ironFile, dic_ironMask,
     &     dic_ironStartTime, dic_ironPeriod, dic_ironRepCycle,
     &     dic_inscal_iron,
     &     dic_iron_exfremo_intercept, dic_iron_exfremo_slope,
     &     InputFe, feinput0, feinput1,
# ifdef USE_EXF_INTERPOLATION
     &     iron_lon0, iron_lon_inc,
     &     iron_lat0, iron_lat_inc,
     &     iron_nlon, iron_nlat, xC, yC,
     &     iron_interpMethod,
# endif
     &     mytime, myiter, mythid )
       ENDIF
#endif /* ALLOW_FE */

       IF ( dic_atmospCO2file .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     &     'dicAtmosCO2', dic_atmospCO2file, dic_atmospCO2mask,
     &     dic_atmospCO2StartTime, dic_atmospCO2Period, 
     &     dic_atmospCO2RepCycle, dic_inscal_atmospCO2,
     &     dic_atmospCO2_exfremo_intercept, 
     &     dic_atmospCO2_exfremo_slope,
     &     AtmospCO2, AtmospCO20, AtmospCO21,
# ifdef USE_EXF_INTERPOLATION
     &     atmospCO2_lon0, atmospCO2_lon_inc,
     &     atmospCO2_lat0, atmospCO2_lat_inc,
     &     atmospCO2_nlon, atmospCO2_nlat, xC, yC,
     &     atmospCO2_interpMethod,
# endif
     &     mytime, myiter, mythid )
       ENDIF

       IF ( dic_IceFile .NE. ' ' ) THEN
C     forcing load fields with pkg/exf tools
        CALL EXF_SET_FLD(
     &     'dicIce', dic_iceFile, dic_iceMask,
     &     dic_iceStartTime, dic_icePeriod, dic_iceRepCycle,
     &     dic_inscal_ice,
     &     dic_ice_exfremo_intercept, dic_ice_exfremo_slope,
     &     fIce, Ice0, Ice1,
# ifdef USE_EXF_INTERPOLATION
     &     ice_lon0, ice_lon_inc,
     &     ice_lat0, ice_lat_inc,
     &     ice_nlon, ice_nlat, xC, yC,
     &     ice_interpMethod,
# endif
     &     mytime, myiter, mythid )
#ifdef EXF_SEAICE_FRACTION
C Otherwise get sea ice fraction from PKG/EXF
       ELSEIF ( areamaskfile .NE. ' ' ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            fIce(i,j,bi,bj) = areamask(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
#endif /* EXF_SEAICE_FRACTION */
       ENDIF

#ifdef LIGHT_CHL
       IF ( dic_chlaFile .NE. ' ' ) THEN
C     forcing load fields with pkg/exf tools
        CALL EXF_SET_FLD(
     &     'dicChl', dic_chlfile, dic_chlmask,
     &     dic_chlStartTime, dic_chlperiod,
     &     dic_chlRepCycle,
     &     dic_inscal_chl,
     &     dic_chl_exfremo_intercept,
     &     dic_chl_exfremo_slope,
     &     chl, chl0, chl1,
# ifdef USE_EXF_INTERPOLATION
     &     chl_lon0, chl_lon_inc,
     &     chl_lat0, chl_lat_inc,
     &     chl_nlon, chl_nlat, xC, yC,
     &     chl_interpMethod,
# endif
     &     mytime, myiter, mythid )
       ENDIF
#endif /* LIGHT_CHL */

       IF ( dic_SSTFile .NE. ' ' ) THEN
C     forcing load fields with pkg/exf tools
        CALL EXF_SET_FLD(
     &     'dicSST', dic_SSTfile, dic_SSTmask,
     &     dic_SSTStartTime, dic_SSTPeriod, dic_SSTRepCycle,
     &     dic_inscal_SST,
     &     dic_SST_exfremo_intercept, dic_SST_exfremo_slope,
     &     dicSST, dicSST0, dicSST1,
# ifdef USE_EXF_INTERPOLATION
     &     SST_lon0, SST_lon_inc,
     &     SST_lat0, SST_lat_inc,
     &     SST_nlon, SST_nlat, xC, yC,
     &     SST_interpMethod,
# endif
     &     mytime, myiter, mythid )
       ENDIF

       IF ( dic_SSSFile .NE. ' ' ) THEN
C     forcing load fields with pkg/exf tools
        CALL EXF_SET_FLD(
     &     'dicSSS', dic_SSSfile, dic_SSSmask,
     &     dic_SSSStartTime, dic_SSSPeriod, dic_SSSRepCycle,
     &     dic_inscal_SSS,
     &     dic_SSS_exfremo_intercept, dic_SSS_exfremo_slope,
     &     dicSSS, dicSSS0, dicSSS1,
# ifdef USE_EXF_INTERPOLATION
     &     SSS_lon0, SSS_lon_inc,
     &     SSS_lat0, SSS_lat_inc,
     &     SSS_nlon, SSS_nlat, xC, yC,
     &     SSS_interpMethod,
# endif
     &     mytime, myiter, mythid )
       ENDIF
C     endif dic_useEXF
      ENDIF
#endif /* ALLOW_EXF */
#endif /* ALLOW_DIC */
      RETURN
      END
