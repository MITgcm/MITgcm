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
#include "GRID.h"
#include "PARAMS.h"
#ifdef ALLOW_EXF
# include "EXF_INTERP_SIZE.h"
# include "EXF_PARAM.h"
# include "EXF_FIELDS.h"
#endif
#include "GCHEM_SIZE.h"
#include "GCHEM_EXF.h"
#include "GCHEM_FIELDS.h"
#include "GCHEM.h"

C !INPUT PARAMETERS: ===================================================
C  myTime               :: current time
C  myIter               :: current iteration
C  myThid               :: thread number
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !LOCAL VARIABLES: ===================================================
      INTEGER bi, bj, i, j
      INTEGER intimeP, intime0, intime1
      _RL aWght,bWght
CEOP

#ifdef ALLOW_GCHEM

cccccccccccccccccccccccccc
c load external data     c
cccccccccccccccccccccccccc
c     IF ( .NOT. useEXF ) THEN

       IF (  GCHEM_forcingCycle.gt.0. _d 0 ) THEN

C--   Now calculate whether it is time to update the forcing arrays
        CALL GET_PERIODIC_INTERVAL(
     O       intimeP, intime0, intime1, bWght, aWght,
     I       GCHEM_forcingCycle, GCHEM_forcingPeriod,
     I       deltaTClock, myTime, myThid )

        bi = myBxLo(myThid)
        bj = myByLo(myThid)
# ifdef ALLOW_DEBUG
        IF ( debugLevel.GE.debLevB ) THEN
         _BEGIN_MASTER(myThid)
         WRITE(standardMessageUnit,'(A,I10,A,4I5,A,2F14.10)')
     &        ' GCHEM_FIELDS_LOAD,', myIter,
     &        ' : iP,iLd,i0,i1=', intimeP,GCHEM_ldRec(bi,bj),
     &         intime0,intime1,
     &        ' ; Wght=', bWght, aWght
         _END_MASTER(myThid)
        ENDIF
# endif /* ALLOW_DEBUG */

# ifdef ALLOW_AUTODIFF
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
# endif /* ALLOW_AUTODIFF */

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

         IF ( GCHEM_windFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( GCHEM_windFile,gchemWind0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( GCHEM_windFile,gchemWind1,intime1,
     &         myIter,myThid )
         ENDIF
         IF ( GCHEM_atmospFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( GCHEM_atmospFile,gchemAP0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( GCHEM_atmospFile,gchemAP1,intime1,
     &         myIter,myThid )
         ENDIF
         IF ( gchem_silicaFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_silicaFile,gchemSi0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( GCHEM_silicaFile,gchemSi1,intime1,
     &         myIter,myThid )
         ENDIF
         IF ( gchem_iceFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( GCHEM_iceFile,gchemIce0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( GCHEM_iceFile,gchemIce1,intime1,
     &         myIter,myThid )
         ENDIF
         IF ( gchem_ironFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( GCHEM_ironFile,gchemIron0,intime0,
     &         myIter,myThid )
          CALL READ_REC_XY_RS( GCHEM_ironFile,gchemIron1,intime1,
     &         myIter,myThid )
         ENDIF

C--   fill-in overlap after loading temp arrays:
         _EXCH_XY_RS(gchemWind0, myThid )
         _EXCH_XY_RS(gchemWind1, myThid )
         _EXCH_XY_RS(gchemAP0, myThid )
         _EXCH_XY_RS(gchemAP1, myThid )
         _EXCH_XY_RS(gchemIce0, myThid )
         _EXCH_XY_RS(gchemIce1, myThid )
         _EXCH_XY_RS(gchemIron0, myThid )
         _EXCH_XY_RS(gchemIron1, myThid )
         _EXCH_XY_RS(gchemSi0, myThid )
         _EXCH_XY_RS(gchemSi1, myThid )

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
          IF ( GCHEM_windFile .NE. ' ' ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemWind(i,j,bi,bj) = bWght*gchemWind0(i,j,bi,bj)
     &                            + aWght*gchemWind1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF

          IF ( GCHEM_atmospFile .NE. ' ' ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemAtmosP(i,j,bi,bj) = bWght*gchemAP0(i,j,bi,bj)
     &                              + aWght*gchemAP1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF

          IF ( GCHEM_silicaFile .NE. ' ' ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemSi(i,j,bi,bj) = bWght*gchemSi0(i,j,bi,bj)
     &                          + aWght*gchemSi1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF

          IF ( GCHEM_iceFile .NE. ' ' ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemIce(i,j,bi,bj) = bWght*gchemIce0(i,j,bi,bj)
     &                           + aWght*gchemIce1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF

          IF ( GCHEM_ironFile .NE. ' ' ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemFe(i,j,bi,bj) = bWght*gchemIron0(i,j,bi,bj)
     &                          + aWght*gchemIron1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF
         ENDDO
        ENDDO
C--   endif for GCHEM_forcingCycle
       ENDIF
C--   endif .not. useEXF
c     ENDIF

C-----------------------------------------------------------
C If an ice file is given in data.gchem, read it
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL EXF_SET_FLD(
     &     'gchemIce', gchem_icefile, icemask,
     &     iceStartTime, iceperiod, iceRepCycle,
     &     gchem_inscal_ice,
     &     ice_exfremo_intercept, ice_exfremo_slope,
     &     gchemIce, gchemIce0, gchemIce1,
#  ifdef USE_EXF_INTERPOLATION
     &     ice_lon0, ice_lon_inc,
     &     ice_lat0, ice_lat_inc,
     &     ice_nlon, ice_nlat, xC, yC,
     &     ice_interpMethod,
#  endif
     &     mytime, myiter, mythid )
C Otherwise get ice fraction from PKG/SEAICE or PKG/THSICE
# ifdef ALLOW_SEAICE
         IF ( useSEAICE ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemIce(i,j,bi,bj) = AREA(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
# endif
# ifdef ALLOW_THSICE
         IF ( useThSIce ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemIce(i,j,bi,bj) = iceMask(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
# endif
       ENDDO
      ENDDO

C-----------------------------------------------------------
# ifdef ALLOW_EXF
C     forcing load fields with pkg/exf tools
      IF ( useEXF ) THEN

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

C If a wind file is given in data.gchem, read it
         IF ( gchem_WindFile .NE. ' ' ) THEN
          CALL EXF_SET_FLD(
     &     'gchemWind', gchem_windfile, windmask,
     &     windStartTime, windperiod, windRepCycle,
     &     gchem_inscal_wind,
     &     wind_exfremo_intercept, wind_exfremo_slope,
     &     gchemWind, gchemWind0, gchemWind1,
#  ifdef USE_EXF_INTERPOLATION
     &     wind_lon0, wind_lon_inc,
     &     wind_lat0, wind_lat_inc,
     &     wind_nlon, wind_nlat, xC, yC,
     &     wind_interpMethod,
#  endif
     &     mytime, myiter, mythid )
C Otherwise get winds from PKG/EXF
         ELSEIF ( uwindfile .NE. ' ' .AND. vwindfile .NE. ' ' ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            gchemWind(i,j,bi,bj) = wspeed(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDIF

C If a pressure files is given in data.gchem, read it
         IF ( gchem_atmosPfile .EQ. ' ' ) THEN
          CALL EXF_SET_FLD(
     &     'gchemAtmosP', gchem_atmosPfile, apresmask,
     &     apresStartTime, apresperiod, apresRepCycle,
     &     gchem_inscal_apres,
     &     apres_exfremo_intercept, apres_exfremo_slope,
     &     gchemAtmosP, gchemAP0, gchemAP1,
#  ifdef USE_EXF_INTERPOLATION
     &     apres_lon0, apres_lon_inc,
     &     apres_lat0, apres_lat_inc,
     &     apres_nlon, apres_nlat, xC, yC,
     &     apres_interpMethod,
#  endif
     &     mytime, myiter, mythid )
C Otherwise get atmospheric pressure from PKG/EXF
         ELSEIF ( apressurefile .NE. ' ' ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
C Atm pressure in Pascals, convert to atm
            gchemAtmosP(i,j,bi,bj) = apressure(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDIF
        ENDDO
       ENDDO

       IF ( gchem_silicafile .NE. ' ' ) THEN
        CALL EXF_SET_FLD(
     &     'silica', gchem_silicafile, silicamask,
     &     silicaStartTime, silicaperiod, silicaRepCycle,
     &     gchem_inscal_silica,
     &     silica_exfremo_intercept, silica_exfremo_slope,
     &     gchemSi, gchemSi0, gchemSi1,
#  ifdef USE_EXF_INTERPOLATION
     &     silica_lon0, silica_lon_inc,
     &     silica_lat0, silica_lat_inc,
     &     silica_nlon, silica_nlat, xC, yC,
     &     silica_interpMethod,
#  endif
     &     mytime, myiter, mythid )
       ENDIF

       IF ( gchem_PARfile .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     &     'PAR', gchem_PARfile, PARmask,
     &     PARStartTime, PARperiod, PARRepCycle,
     &     gchem_inscal_PAR,
     &     PAR_exfremo_intercept, PAR_exfremo_slope,
     &     gchemPAR, gchemPAR0, gchemPAR1,
#  ifdef USE_EXF_INTERPOLATION
     &     PAR_lon0, PAR_lon_inc,
     &     PAR_lat0, PAR_lat_inc,
     &     PAR_nlon, PAR_nlat, xC, yC,
     &     PAR_interpMethod,
#  endif
     &     mytime, myiter, mythid )
       ENDIF

       IF ( gchem_ironfile .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     &     'iron', gchem_ironfile, ironmask,
     &     ironStartTime, ironperiod, ironRepCycle,
     &     gchem_inscal_iron,
     &     iron_exfremo_intercept, iron_exfremo_slope,
     &     gchemFe, gchemIron0, gchemIron1,
#  ifdef USE_EXF_INTERPOLATION
     &     iron_lon0, iron_lon_inc,
     &     iron_lat0, iron_lat_inc,
     &     iron_nlon, iron_nlat, xC, yC,
     &     iron_interpMethod,
#  endif
     &     mytime, myiter, mythid )
       ENDIF

       IF ( gchem_apco2file .NE. ' '  ) THEN
        CALL EXF_SET_FLD(
     &     'apCO2', gchem_apCO2file, apCO2mask,
     &     apCO2StartTime, apCO2period, apCO2RepCycle,
     &     gchem_inscal_apCO2,
     &     apCO2_exfremo_intercept, apCO2_exfremo_slope,
     &     gchemApCO2, gchemApCO20, gchemApCO21,
#  ifdef USE_EXF_INTERPOLATION
     &     apCO2_lon0, apCO2_lon_inc,
     &     apCO2_lat0, apCO2_lat_inc,
     &     apCO2_nlon, apCO2_nlat, xC, yC,
     &     apCO2_interpMethod,
#  endif
     &     mytime, myiter, mythid )
       ENDIF

      ENDIF
#  endif /* ALLOW_EXF */

C     now the various BGC packages can use the gchem fields
# ifdef ALLOW_DIC
      IF ( useDIC ) THEN
       CALL DIC_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
# endif

# ifdef ALLOW_BLING
      IF ( useBLING ) THEN
       CALL BLING_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
# endif

# ifdef ALLOW_CFC
      IF ( useCFC ) THEN
       CALL CFC_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
# endif

# ifdef ALLOW_DARWIN
      IF ( useDARWIN ) THEN
       CALL DARWIN_FIELDS_LOAD( myIter,myTime,myThid )
      ENDIF
# endif

#endif /* ALLOW_GCHEM */

      RETURN
      END
