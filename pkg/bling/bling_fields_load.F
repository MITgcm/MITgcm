#include "BLING_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

CBOP
      SUBROUTINE BLING_FIELDS_LOAD (
     I           myTime, myIter, myThid )

C     *========================================================*
C     | subroutine bling_fields_load
C     | o Read in fields needed for CO2, O2 flux terms, silica
C     |   for pH calculation
C     | o Update fields from EXF package
C     *========================================================*

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
# include "EXF_FIELDS.h"
# ifdef USE_EXF_INTERPOLATION
#  include "EXF_INTERP_SIZE.h"
# endif
# include "BLING_EXF.h"
#endif
#include "BLING_VARS.h"
#include "BLING_LOAD.h"
#ifdef ALLOW_THSICE
# include "THSICE_VARS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
      INTEGER myIter
      _RL myTime
      INTEGER myThid

#ifdef ALLOW_BLING

C !LOCAL VARIABLES: ===================================================
      INTEGER bi, bj, i, j
      INTEGER intimeP, intime0, intime1
      _RL aWght,bWght
CEOP

      IF ( .NOT.bling_useEXF .AND.
     &     bling_forcingCycle.GT.0. _d 0 ) THEN

C--   Now calculate whether it is time to update the forcing arrays
       CALL GET_PERIODIC_INTERVAL(
     O                   intimeP, intime0, intime1, bWght, aWght,
     I                   bling_forcingCycle, bling_forcingPeriod,
     I                   deltaTClock, myTime, myThid )

       bi = myBxLo(myThid)
       bj = myByLo(myThid)
#ifdef ALLOW_DEBUG
       IF ( debugLevel.GE.debLevB ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,4I5,A,2F14.10)')
     &   ' BLING_FIELDS_LOAD,', myIter,
     &   ' : iP,iLd,i0,i1=', intimeP,bling_ldRec(bi,bj), intime0,
     &                       intime1,
     &   ' ; Wght=', bWght, aWght
        _END_MASTER(myThid)
       ENDIF
#endif /* ALLOW_DEBUG */

#ifdef ALLOW_AUTODIFF
C-    assuming that we call S/R BLING_FIELDS_LOAD at each time-step and
C     with increasing time, this will catch when we need to load new records;
C     But with Adjoint run, this is not always the case => might end-up using
C     the wrong time-records
       IF ( intime0.NE.intimeP .OR. myIter.EQ.nIter0 ) THEN
#else /* ALLOW_AUTODIFF */
C-    Make no assumption on sequence of calls to BLING_FIELDS_LOAD ;
C     This is the correct formulation (works in Adjoint run).
C     Unfortunatly, produces many recomputations <== not used until it is fixed
       IF ( intime1.NE.bling_ldRec(bi,bj) ) THEN
#endif /* ALLOW_AUTODIFF */

C--   If the above condition is met then we need to read in
C     data for the period ahead and the period behind myTime.
        IF ( debugLevel.GE.debLevZero ) THEN
         _BEGIN_MASTER(myThid)
         WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &    ' BLING_FIELDS_LOAD, it=', myIter,
     &    ' : Reading new data, i0,i1=', intime0, intime1,
     &    ' (prev=', intimeP, bling_ldRec(bi,bj), ' )'
         _END_MASTER(myThid)
        ENDIF

        _BARRIER

        IF ( bling_windFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( bling_windFile,dicwind0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RL( bling_windFile,dicwind1,intime1,
     &        myIter,myThid )
        ENDIF
        IF ( bling_apressFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( bling_apressFile,apress0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RL( bling_apressFile,apress1,intime1,
     &        myIter,myThid )
        ENDIF
        IF ( bling_silicaFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( bling_silicaFile,silica0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RL( bling_silicaFile,silica1,intime1,
     &        myIter,myThid )
        ENDIF
        IF ( bling_iceFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( bling_iceFile,ice0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RL( bling_iceFile,ice1,intime1,
     &       myIter,myThid )
        ENDIF
        IF ( bling_ironFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( bling_ironFile,InputFe0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RL( bling_ironFile,InputFe1,intime1,
     &       myIter,myThid )
        ENDIF
        IF ( bling_apco2File .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( bling_apco2File,bling_apco20,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RL( bling_apco2File,bling_apco21,intime1,
     &       myIter,myThid )
        ENDIF

C--   fill-in overlap after loading temp arrays:
        _EXCH_XY_RL(dicwind0, myThid )
        _EXCH_XY_RL(dicwind1, myThid )
        _EXCH_XY_RL(apress0, myThid )
        _EXCH_XY_RL(apress1, myThid )
        _EXCH_XY_RL(ice0, myThid )
        _EXCH_XY_RL(ice1, myThid )
        _EXCH_XY_RL(InputFe0, myThid )
        _EXCH_XY_RL(InputFe1, myThid )
        _EXCH_XY_RL(silica0, myThid )
        _EXCH_XY_RL(silica1, myThid )
        _EXCH_XY_RL(bling_apco20, myThid )
        _EXCH_XY_RL(bling_apco21, myThid )

C-    save newly loaded time-record
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           bling_ldRec(bi,bj) = intime1
         ENDDO
        ENDDO

C-     end if-bloc (time to load new fields)
       ENDIF

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         IF ( bling_windFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             wind(i,j,bi,bj) = bWght*dicwind0(i,j,bi,bj)
     &                       + aWght*dicwind1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

         IF ( bling_apressFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             atmosP(i,j,bi,bj) = bWght*apress0(i,j,bi,bj)
     &                         + aWght*apress1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

         IF ( bling_silicaFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             silica(i,j,bi,bj) = bWght*silica0(i,j,bi,bj)
     &                         + aWght*silica1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

         IF ( bling_iceFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             fIce(i,j,bi,bj) = bWght*ice0(i,j,bi,bj)
     &                       + aWght*ice1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

         IF ( bling_ironFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             InputFe(i,j,bi,bj) = bWght*InputFe0(i,j,bi,bj)
     &                          + aWght*InputFe1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

         IF ( bling_apco2File .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             apco2(i,j,bi,bj) = bWght*bling_apco20(i,j,bi,bj)
     &                          + aWght*bling_apco21(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
        ENDDO
       ENDDO

C endif for bling_forcingCycle
      ENDIF

C-----------------------------------------------------------
C Get wind speed, ice fraction and atmospheric pressure from PKG/EXF
C (ice fraction may be overwritten below by pkg/seaice or pkg/thsice)
#ifdef ALLOW_EXF
      IF ( bling_useEXF ) THEN
C      Wind speed
       IF ( bling_windFile .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     I     'wind', bling_windFile, bling_windMask,
     I     bling_windStartTime, bling_windPeriod,
     I     bling_windRepCycle,
     I     bling_inscal_wind,
     I     bling_wind_exfremo_intercept, bling_wind_exfremo_slope,
     U     wind, dicwind0, dicwind1,
# ifdef USE_EXF_INTERPOLATION
     I     bling_wind_lon0, bling_wind_lon_inc,
     I     bling_wind_lat0, bling_wind_lat_inc,
     I     bling_wind_nlon, bling_wind_nlat, xC, yC,
     I     bling_wind_interpMethod,
# endif
     I     myTime, myIter, myThid )
       ENDIF
C      Sea ice fraction
       IF ( bling_iceFile .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     I     'fIce', bling_iceFile, bling_iceMask,
     I     bling_iceStartTime, bling_icePeriod,
     I     bling_iceRepCycle,
     I     bling_inscal_ice,
     I     bling_ice_exfremo_intercept, bling_ice_exfremo_slope,
     U     fIce, ice0, ice1,
# ifdef USE_EXF_INTERPOLATION
     I     bling_ice_lon0, bling_ice_lon_inc,
     I     bling_ice_lat0, bling_ice_lat_inc,
     I     bling_ice_nlon, bling_ice_nlat, xC, yC,
     I     bling_ice_interpMethod,
# endif
     I     myTime, myIter, myThid )
       ENDIF
C      Atmospheric pressure (in atm)
       IF ( bling_apressFile .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     I     'AtmosP', bling_apressFile, bling_apressMask,
     I     bling_apressStartTime, bling_apressPeriod,
     I     bling_apressRepCycle,
     I     bling_inscal_apress,
     I     bling_apress_exfremo_intercept, bling_apress_exfremo_slope,
     U     AtmosP, apress0, apress1,
# ifdef USE_EXF_INTERPOLATION
     I     bling_apress_lon0, bling_apress_lon_inc,
     I     bling_apress_lat0, bling_apress_lat_inc,
     I     bling_apress_nlon, bling_apress_nlat, xC, yC,
     I     bling_apress_interpMethod,
# endif
     I     myTime, myIter, myThid )
       ENDIF
      ENDIF
#endif

C-----------------------------------------------------------
C Get ice fraction from PKG/SEAICE or PKG/THSICE
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef ALLOW_SEAICE
         IF ( useSEAICE ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             fIce(i,j,bi,bj) = AREA(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif
#ifdef ALLOW_THSICE
         IF ( useThSIce ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             fIce(i,j,bi,bj) = iceMask(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif
       ENDDO
      ENDDO

C-----------------------------------------------------------
C Get winds from PKG/EXF (wspeed), unless bling_windFile is set
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef ALLOW_EXF
         IF ( bling_useEXF .AND. bling_windFile .EQ. ' ' ) THEN
          IF ( uwindfile .NE. ' '  ) THEN
          IF ( vwindfile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             wind(i,j,bi,bj) = wspeed(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF
          ENDIF
         ENDIF
#endif
       ENDDO
      ENDDO

C-----------------------------------------------------------
C Get atmospheric pressure from PKG/EXF (apressure),
C unless bling_apressFile is set
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef ALLOW_EXF
         IF ( bling_useEXF .AND. bling_apressFile .EQ. ' ' ) THEN
          IF ( apressurefile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
C Atm pressure in Pascals, convert to atm
             AtmosP(i,j,bi,bj) = apressure(i,j,bi,bj)/Pa2atm
            ENDDO
           ENDDO
          ENDIF
         ENDIF
#endif
       ENDDO
      ENDDO

C-----------------------------------------------------------
C Get Atmospheric carbon dioxide concentration from PKG/EXF
#ifdef ALLOW_EXF
C     Atmospheric carbon dioxide concentration
      IF ( bling_useEXF ) THEN
       IF ( bling_apco2File .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     I     'apco2', bling_apco2File, bling_apco2Mask,
     I     bling_apco2StartTime, bling_apco2Period,
     I     bling_apco2RepCycle,
     I     bling_inscal_apco2,
     I     bling_apco2_exfremo_intercept, bling_apco2_exfremo_slope,
     U     apco2, bling_apco20, bling_apco21,
# ifdef USE_EXF_INTERPOLATION
     I     bling_apco2_lon0, bling_apco2_lon_inc,
     I     bling_apco2_lat0, bling_apco2_lat_inc,
     I     bling_apco2_nlon, bling_apco2_nlat, xC, yC,
     I     bling_apco2_interpMethod,
# endif
     I     myTime, myIter, myThid )
       ENDIF
      ENDIF
#endif

C-----------------------------------------------------------
C Get silica concentration and surface iron flux from PKG/EXF
#ifdef ALLOW_EXF
      IF ( bling_useEXF ) THEN
C     Silica concentration
       IF ( bling_silicaFile .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     I     'silica', bling_silicaFile, bling_silicaMask,
     I     bling_silicaStartTime, bling_silicaPeriod,
     I     bling_silicaRepCycle,
     I     bling_inscal_silica,
     I     bling_silica_exfremo_intercept, bling_silica_exfremo_slope,
     U     silica, silica0, silica1,
# ifdef USE_EXF_INTERPOLATION
     I     bling_silica_lon0, bling_silica_lon_inc,
     I     bling_silica_lat0, bling_silica_lat_inc,
     I     bling_silica_nlon, bling_silica_nlat, xC, yC,
     I     bling_silica_interpMethod,
# endif
     I     myTime, myIter, myThid )
       ENDIF
C      Surface iron flux
       IF ( bling_ironFile .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     I     'InputFe', bling_ironFile, bling_ironMask,
     I     bling_ironStartTime, bling_ironPeriod, bling_ironRepCycle,
     I     bling_inscal_iron,
     I     bling_iron_exfremo_intercept, bling_iron_exfremo_slope,
     U     InputFe, InputFe0, InputFe1,
# ifdef USE_EXF_INTERPOLATION
     I     bling_iron_lon0, bling_iron_lon_inc,
     I     bling_iron_lat0, bling_iron_lat_inc,
     I     bling_iron_nlon, bling_iron_nlat, xC, yC,
     I     bling_iron_interpMethod,
# endif
     I     myTime, myIter, myThid )
       ENDIF
      ENDIF
#endif

#endif /* ALLOW_BLING */
      RETURN
      END
