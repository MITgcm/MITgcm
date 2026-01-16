#include "GCHEM_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: GCHEM_FIELDS_LOAD

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_FIELDS_LOAD (
     I           myTime, myIter, myThid )

C !DESCRIPTION:
C  calls routines which read in fields needed for any tracer experiment
C !USES: ===============================================================
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GCHEM_SIZE.h"
#include "GCHEM.h"
#include "GCHEM_FIELDS.h"
#include "GCHEM_AUX_FIELDS.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
# include "EXF_FIELDS.h"
# include "GCHEM_EXF.h"
# ifdef USE_EXF_INTERPOLATION
#  include "GRID.h"
#  include "EXF_INTERP_SIZE.h"
#  include "GCHEM_INTERP_PARAM.h"
# endif
#endif

C !INPUT PARAMETERS: ===================================================
C  myTime               :: current time
C  myIter               :: current iteration
C  myThid               :: thread number
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef GCHEM_ALLOW_FFIELDS
C !LOCAL VARIABLES: ===================================================
      INTEGER bi, bj, i, j, k
      INTEGER intimeP, intime0, intime1
      _RL aWght,bWght
CEOP

cccccccccccccccccccccccccc
c load external data     c
cccccccccccccccccccccccccc
      IF ( .NOT. gchem_useEXF ) THEN

       IF (  GCHEM_forcingCycle.gt.0. _d 0 ) THEN

C--   Now calculate whether it is time to update the forcing arrays
        CALL GET_PERIODIC_INTERVAL(
     O       intimeP, intime0, intime1, bWght, aWght,
     I       GCHEM_forcingCycle, GCHEM_forcingPeriod,
     I       deltaTClock, myTime, myThid )

        bi = myBxLo(myThid)
        bj = myByLo(myThid)
#ifdef ALLOW_DEBUG
        IF ( debugLevel.GE.debLevB ) THEN
         _BEGIN_MASTER(myThid)
         WRITE(standardMessageUnit,'(A,I10,A,4I5,A,2F14.10)')
     &        ' GCHEM_FIELDS_LOAD,', myIter,
     &        ' : iP,iLd,i0,i1=', intimeP,GCHEM_ldRec(bi,bj),
     &         intime0,intime1,
     &        ' ; Wght=', bWght, aWght
         _END_MASTER(myThid)
        ENDIF
#endif /* ALLOW_DEBUG */

#ifdef ALLOW_AUTODIFF
C-    assuming that we call S/R GCHEM_FIELDS_LOAD at each time-step and
C     with increasing time, this will catch when we need to load new records;
C     But with Adjoint run, this is not always the case => might end-up using
C     the wrong time-records
        IF ( intime0.NE.intimeP .OR. myIter.EQ.nIter0 ) THEN
# else /* ALLOW_AUTODIFF */
C-    Make no assumption on sequence of calls to GCHEM_FIELDS_LOAD ;
C     This is the correct formulation (works in Adjoint run).
C     Unfortunatly, produces many recomputations <== not used until it is fixed
        IF ( intime1.NE.GCHEM_ldRec(bi,bj) ) THEN
#endif /* ALLOW_AUTODIFF */

C--   If the above condition is met then we need to read in
C     data for the period ahead and the period behind myTime.
         IF ( debugLevel.GE.debLevZero ) THEN
          _BEGIN_MASTER(myThid)
          WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &         ' GCHEM_FIELDS_LOAD, it=', myIter,
     &         ' : Reading new data, i0,i1=', intime0, intime1,
     &         ' (prev=', intimeP, GCHEM_ldRec(bi,bj), ' )'
          _END_MASTER(myThid)
         ENDIF

         _BARRIER

         IF ( gchem_windFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_windFile,gchemWind0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( gchem_windFile,gchemWind1,intime1,
     &         myIter,myThid )
         ENDIF
         IF ( gchem_apresFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_apresFile,gchemApres0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( gchem_apresFile,gchemApres1,intime1,
     &         myIter,myThid )
         ENDIF
         IF ( gchem_SiFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_SiFile,gchemSi0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( gchem_SiFile,gchemSi1,intime1,
     &         myIter,myThid )
         ENDIF
#ifdef GCHEM_3D_SILICA
         IF ( gchem_Si3DFile .NE. ' '  ) THEN
          CALL READ_REC_XYZ_RS( gchem_Si3DFile,gchemSi3D0,intime0,
     &        myIter,myThid )
          CALL READ_REC_XYZ_RS( gchem_Si3DFile,gchemSi3D1,intime1,
     &        myIter,myThid )
         ENDIF
#endif
         IF ( gchem_iceFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_iceFile,gchemIce0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( gchem_iceFile,gchemIce1,intime1,
     &         myIter,myThid )
         ENDIF
         IF ( gchem_FeFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_FeFile,gchemFe0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( gchem_FeFile,gchemFe1,intime1,
     &         myIter,myThid )
         ENDIF
         IF ( gchem_parFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_parFile,gchemPAR0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( gchem_parFile,gchemPAR1,intime1,
     &         myIter,myThid )
         ENDIF
         IF ( gchem_apco2File .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_apco2File,gchemApco20,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( gchem_apco2File,gchemApco21,intime1,
     &         myIter,myThid )
         ENDIF
         IF ( gchem_chlFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_chlFile,gchemchl0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( gchem_chlFile,gchemchl1,intime1,
     &         myIter,myThid )
         ENDIF

C--   fill-in overlap after loading temp arrays:
         _EXCH_XY_RS(gchemWind0, myThid )
         _EXCH_XY_RS(gchemWind1, myThid )
         _EXCH_XY_RS(gchemApres0, myThid )
         _EXCH_XY_RS(gchemApres1, myThid )
         _EXCH_XY_RS(gchemIce0, myThid )
         _EXCH_XY_RS(gchemIce1, myThid )
         _EXCH_XY_RS(gchemFe0, myThid )
         _EXCH_XY_RS(gchemFe1, myThid )
         _EXCH_XY_RS(gchemSi0, myThid )
         _EXCH_XY_RS(gchemSi1, myThid )
#ifdef GCHEM_3D_SILICA
         IF ( gchem_Si3DFile .NE. ' '  ) THEN
           _EXCH_XYZ_RS(gchemSi3D0, myThid )
           _EXCH_XYZ_RS(gchemSi3D1, myThid )
         ENDIF
#endif
         _EXCH_XY_RS(gchemPAR0, myThid )
         _EXCH_XY_RS(gchemPAR1, myThid )
         _EXCH_XY_RS(gchemApco20, myThid )
         _EXCH_XY_RS(gchemApco21, myThid )
         _EXCH_XY_RS(gchemchl0, myThid )
         _EXCH_XY_RS(gchemchl1, myThid )

C-    save newly loaded time-record
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           GCHEM_ldRec(bi,bj) = intime1
          ENDDO
         ENDDO

C-     end if-bloc (time to load new fields)
        ENDIF

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          IF ( gchem_windFile .NE. ' ' ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemWind(i,j,bi,bj) = bWght*gchemWind0(i,j,bi,bj)
     &                            + aWght*gchemWind1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF

          IF ( gchem_apresFile .NE. ' ' ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemAtmosP(i,j,bi,bj) = bWght*gchemApres0(i,j,bi,bj)
     &                              + aWght*gchemApres1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF

          IF ( gchem_SiFile .NE. ' ' ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemSi(i,j,bi,bj) = bWght*gchemSi0(i,j,bi,bj)
     &                          + aWght*gchemSi1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF

#ifdef GCHEM_3D_SILICA
          IF ( gchem_Si3DFile .NE. ' '  ) THEN
           DO k=1,Nr
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              gchemSi3D(i,j,k,bi,bj) = bWght*gchemSi3D0(i,j,k,bi,bj)
     &                              + aWght*gchemSi3D1(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDIF
#endif

          IF ( gchem_iceFile .NE. ' ' ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemIce(i,j,bi,bj) = bWght*gchemIce0(i,j,bi,bj)
     &                           + aWght*gchemIce1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF

          IF ( gchem_FeFile .NE. ' ' ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemFe(i,j,bi,bj) = bWght*gchemFe0(i,j,bi,bj)
     &                          + aWght*gchemFe1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF

          IF ( GCHEM_parFile .NE. ' ' ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemPAR(i,j,bi,bj) = bWght*gchemPAR0(i,j,bi,bj)
     &                           + aWght*gchemPAR1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF

          IF ( gchem_apco2File .NE. ' ' ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemapCO2(i,j,bi,bj) = bWght*gchemApco20(i,j,bi,bj)
     &                             + aWght*gchemApco21(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF

          IF ( GCHEM_chlFile .NE. ' ' ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemChl(i,j,bi,bj) = bWght*gchemChl0(i,j,bi,bj)
     &                           + aWght*gchemChl1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF
         ENDDO
        ENDDO
C--   endif for GCHEM_forcingCycle
       ENDIF
C--   endif .not. gchem_useEXF
      ENDIF

C-----------------------------------------------------------
#ifdef ALLOW_EXF
C     forcing load fields with pkg/exf tools
      IF ( gchem_useEXF ) THEN

C If a wind file is given in data.gchem, read it
       IF ( gchem_WindFile .NE. ' ' ) THEN
        CALL EXF_SET_FLD(
     &     'gchemWind', gchem_windfile, gchem_windMask,
     &     gchem_windStartTime, gchem_windPeriod, gchem_windRepCycle,
     &     gchem_inscal_wind,
     &     gchem_wind_exfremo_intercept, gchem_wind_exfremo_slope,
     &     gchemWind, gchemWind0, gchemWind1,
# ifdef USE_EXF_INTERPOLATION
     &     wind_lon0, wind_lon_inc,
     &     wind_lat0, wind_lat_inc,
     &     wind_nlon, wind_nlat, xC, yC,
     &     wind_interpMethod,
# endif
     &     mytime, myiter, mythid )
C Otherwise get winds from PKG/EXF
       ELSEIF ( useEXF
     &   .AND. ( uwindfile .NE. ' ' .AND.
     &           vwindfile .NE. ' ' )
     &  .OR. ( ustressfile .NE. ' ' .AND.
     &         vstressfile .NE. ' ' )) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            gchemWind(i,j,bi,bj) = wspeed(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

C If a pressure files is given in data.gchem, read it
       IF ( gchem_apresFile .NE. ' ' ) THEN
        CALL EXF_SET_FLD(
     &     'gchemAtmosP', gchem_apresFile, gchem_apresMask,
     &     gchem_apresStartTime, gchem_apresPeriod, gchem_apresRepCycle,
     &     gchem_inscal_apres,
     &     gchem_apres_exfremo_intercept, gchem_apres_exfremo_slope,
     &     gchemAtmosP, gchemApres0, gchemApres1,
# ifdef USE_EXF_INTERPOLATION
     &     apres_lon0, apres_lon_inc,
     &     apres_lat0, apres_lat_inc,
     &     apres_nlon, apres_nlat, xC, yC,
     &     apres_interpMethod,
# endif
     &     mytime, myiter, mythid )
C Otherwise get atmospheric pressure from PKG/EXF
       ELSEIF ( useEXF .AND. apressurefile .NE. ' ' ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            gchemAtmosP(i,j,bi,bj) = apressure(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

       IF ( gchem_SiFile .NE. ' ' ) THEN
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
     &     mytime, myiter, mythid )
       ENDIF

#ifdef GCHEM_3D_SILICA
       IF ( gchem_Si3DFile .NE. ' '  ) THEN
        CALL EXF_SET_FLD3D(
     &      'gchemSi3D', gchem_Si3DFile, gchem_Si3DMask,
     &      gchem_Si3DStartTime, gchem_Si3DPeriod,
     &      gchem_Si3DRepCycle,
     &      gchem_inscal_Si3D,
     &      gchem_Si3D_exfremo_intercept,
     &      gchem_Si3D_exfremo_slope,
     &      gchemSi3D, gchemSi3D0, gchemSi3D1,
# ifdef USE_EXF_INTERPOLATION
     &      Si3D_lon0, Si3D_lon_inc,
     &      Si3D_lat0, Si3D_lat_inc,
     &      Si3D_nlon, Si3D_nlat, xC, yC,
     &      Si3D_interpMethod,
# endif
     &      mytime, myiter, mythid )
       ENDIF
#endif

       IF ( gchem_parFile .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     &     'gchemPAR', gchem_parFile, gchem_parMask,
     &     gchem_parStartTime, gchem_parPeriod, gchem_parRepCycle,
     &     gchem_inscal_par,
     &     gchem_par_exfremo_intercept, gchem_par_exfremo_slope,
     &     gchemPAR, gchemPAR0, gchemPAR1,
# ifdef USE_EXF_INTERPOLATION
     &     par_lon0, par_lon_inc,
     &     par_lat0, par_lat_inc,
     &     par_nlon, par_nlat, xC, yC,
     &     par_interpMethod,
# endif
     &     mytime, myiter, mythid )
       ENDIF

       IF ( gchem_FeFile .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     &     'gchemFe', gchem_FeFile, gchem_FeMask,
     &     gchem_FeStartTime, gchem_FePeriod, gchem_FeRepCycle,
     &     gchem_inscal_Fe,
     &     gchem_Fe_exfremo_intercept, gchem_Fe_exfremo_slope,
     &     gchemFe, gchemFe0, gchemFe1,
# ifdef USE_EXF_INTERPOLATION
     &     Fe_lon0, Fe_lon_inc,
     &     Fe_lat0, Fe_lat_inc,
     &     Fe_nlon, Fe_nlat, xC, yC,
     &     Fe_interpMethod,
# endif
     &     mytime, myiter, mythid )
       ENDIF

       IF ( gchem_apco2file .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     &     'gchemapCO2', gchem_apco2File, gchem_apco2Mask,
     &     gchem_apco2StartTime, gchem_apco2Period, gchem_apco2RepCycle,
     &     gchem_inscal_apco2,
     &     gchem_apco2_exfremo_intercept, gchem_apco2_exfremo_slope,
     &     gchemApco2, gchemApco20, gchemApco21,
# ifdef USE_EXF_INTERPOLATION
     &     apco2_lon0, apco2_lon_inc,
     &     apco2_lat0, apco2_lat_inc,
     &     apco2_nlon, apco2_nlat, xC, yC,
     &     apco2_interpMethod,
# endif
     &     mytime, myiter, mythid )
       ENDIF

       IF ( gchem_IceFile .NE. ' ' ) THEN
C     forcing load fields with pkg/exf tools
        CALL EXF_SET_FLD(
     &     'gchemIce', gchem_icefile, gchem_iceMask,
     &     gchem_iceStartTime, gchem_icePeriod, gchem_iceRepCycle,
     &     gchem_inscal_ice,
     &     gchem_ice_exfremo_intercept, gchem_ice_exfremo_slope,
     &     gchemIce, gchemIce0, gchemIce1,
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
            gchemIce(i,j,bi,bj) = areamask(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
#endif /* EXF_SEAICE_FRACTION */
       ENDIF

       IF ( gchem_chlFile .NE. ' ' ) THEN
C     forcing load fields with pkg/exf tools
        CALL EXF_SET_FLD(
     &     'gchemChl', gchem_chlfile, gchem_chlmask,
     &     gchem_chlStartTime, gchem_chlperiod,
     &     gchem_chlRepCycle,
     &     gchem_inscal_chl,
     &     gchem_chl_exfremo_intercept,
     &     gchem_chl_exfremo_slope,
     &     gchemchl, gchemchl0, gchemchl1,
# ifdef USE_EXF_INTERPOLATION
     &     chl_lon0, chl_lon_inc,
     &     chl_lat0, chl_lat_inc,
     &     chl_nlon, chl_nlat, xC, yC,
     &     chl_interpMethod,
# endif
     &     mytime, myiter, mythid )
       ENDIF
C     endif gchem_useEXF
      ENDIF
#endif /* ALLOW_EXF */

C------------------------------------------------------------
C If there is no ice file given in data.gchem, try fields from
C pkg/seaice/thsIce

      IF ( gchem_IceFile .EQ. ' ' ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef ALLOW_SEAICE
         IF ( useSEAICE ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            gchemIce(i,j,bi,bj) = AREA(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDIF
#endif
#ifdef ALLOW_THSICE
         IF ( useThSIce ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            gchemIce(i,j,bi,bj) = iceMask(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDIF
#endif
        ENDDO
       ENDDO
      ENDIF
#endif /* GCHEM_ALLOW_FFIELDS */

C     now the various BGC packages can use the gchem fields
#ifdef ALLOW_DIC
      IF ( useDIC ) THEN
       CALL DIC_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_BLING
      IF ( useBLING ) THEN
       CALL BLING_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_CFC
      IF ( useCFC ) THEN
       CALL CFC_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_DARWIN
      IF ( useDARWIN ) THEN
       CALL DARWIN_FIELDS_LOAD( myIter,myTime,myThid )
      ENDIF
#endif

      RETURN
      END
