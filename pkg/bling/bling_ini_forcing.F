#include "BLING_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

CBOP
      subroutine BLING_INI_FORCING( myThid )

C     ==========================================================
C     | subroutine bling_ini_forcing
C     | o Initialize forcing fields
C     ==========================================================

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BLING_VARS.h"
#include "BLING_LOAD.h"
#ifdef ALLOW_EXF
# ifdef USE_EXF_INTERPOLATION
#  include "EXF_INTERP_SIZE.h"
# endif
# include "BLING_EXF.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_BLING

c !LOCAL VARIABLES: ===================================================
       INTEGER bi,bj,i,j
       INTEGER intimeP, intime0, intime1
       _RL aWght, bWght, myLocTime
#ifdef USE_BLING_V1
       CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif

C First call requires that we initialize everything to zero for safety
C-    Initialise BLING_LOAD.h arrays:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        bling_ldRec(bi,bj) = 0
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          dicwind0(i,j,bi,bj) = 0. _d 0
          dicwind1(i,j,bi,bj) = 0. _d 0
          apress0 (i,j,bi,bj) = 0. _d 0
          apress1 (i,j,bi,bj) = 0. _d 0
          silica0 (i,j,bi,bj) = 0. _d 0
          silica1 (i,j,bi,bj) = 0. _d 0
          InputFe0(i,j,bi,bj) = 0. _d 0
          InputFe1(i,j,bi,bj) = 0. _d 0
          bling_apco20  (i,j,bi,bj) = 0. _d 0
          bling_apco21  (i,j,bi,bj) = 0. _d 0
          ice0    (i,j,bi,bj) = 0. _d 0
          ice1    (i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef USE_BLING_V1
#ifdef USE_SIBLING
        WRITE(msgBuf,'(2A)')
     &       ' BLING_INI_FORCING: You can not have USE_SIBLING ',
     &       ' with USE_BLING_V1'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R BLING_INI_FORCING'
#endif
#ifdef USE_BLING_DVM
        WRITE(msgBuf,'(2A)')
     &       ' BLING_INI_FORCING: You can not have USE_BLING_DVM ',
     &       ' with USE_BLING_V1'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R BLING_INI_FORCING'
#endif
#ifdef ADVECT_PHYTO
        WRITE(msgBuf,'(2A)')
     &       ' BLING_INI_FORCING: You can not use ADVECT_PHYTO ',
     &       ' with USE_BLING_V1'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R BLING_INI_FORCING'
#endif
#endif /* USE_BLING_V1 */

c set reasonable values to those that need at least something
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          wind(i,j,bi,bj)    = bling_windConst*maskC(i,j,1,bi,bj)
          AtmosP(i,j,bi,bj)  = bling_apressConst/Pa2Atm
     &                                  *maskC(i,j,1,bi,bj)
          silica(i,j,bi,bj)  = bling_silicaConst
     &                                  *maskC(i,j,1,bi,bj)
          InputFe(i,j,bi,bj) = bling_ironConst
     &                                  *maskC(i,j,1,bi,bj)
          apco2(i,j,bi,bj)   = bling_apco2Const
     &                                  *maskC(i,j,1,bi,bj)
          fIce(i,j,bi,bj)    = bling_iceConst
     &                                  *maskC(i,j,1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( .NOT. bling_useEXF ) THEN
c     IF ( bling_forcingCycle .LE. zeroRL ) THEN
       IF ( bling_windFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( bling_windFile, wind,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( wind, myThid )
       ENDIF
       IF ( bling_apressFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( bling_apressFile, AtmosP,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( AtmosP, myThid )
       ENDIF
       IF ( bling_silicaFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( bling_silicaFile, silica,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( silica, myThid )
       ENDIF
       IF ( bling_iceFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( bling_iceFile, fIce,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( fIce, myThid )
       ENDIF
       IF ( bling_ironFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( bling_ironFile, InputFe,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( InputFe, myThid )
       ENDIF
       IF ( bling_apco2File .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( bling_apco2File, apco2,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( apco2, myThid )
       ENDIF
c     ENDIF
      IF ( bling_forcingCycle .GT. zeroRL ) THEN
C--   Load some initial forcing from file

C-    Read in surface silica field (used to compute initial surf. pH)
C     Note: For this purpose, might be accurate enough to use first record
C           in silica file (already loaded) and skip all this block.

C-    Get reccord number and weight for time interpolation:
       CALL GET_PERIODIC_INTERVAL(
     O                   intimeP, intime0, intime1, bWght, aWght,
     I                   bling_forcingCycle, bling_forcingPeriod,
     I                   deltaTClock, startTime, myThid )

       _BARRIER
       _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &   ' BLING_INI_FORCING, it=', nIter0,
     &   ' : Reading new data, i0,i1=', intime0, intime1
       _END_MASTER(myThid)

       IF ( bling_silicaFile .NE. ' '  ) THEN
C-    If file provided for surface silicate, read it in
         CALL READ_REC_XY_RL( bling_silicaFile,silica0,intime0,
     &        nIter0,myThid )
         CALL READ_REC_XY_RL( bling_silicaFile,silica1,intime1,
     &        nIter0,myThid )
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              silica(i,j,bi,bj) = bWght*silica0(i,j,bi,bj)
     &                              + aWght*silica1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
       ENDIF
       _EXCH_XY_RL( silica, myThid )

C end if bling_forcingCycle > 0
      ENDIF
C     if not bling_useEXF
      ENDIF

#ifdef ALLOW_EXF
      IF ( bling_useEXF ) THEN
       IF ( bling_apco2File .NE. ' ' ) THEN
      CALL EXF_INIT_FLD(
     I     'apco2', bling_apco2File, bling_apco2Mask,
     I     bling_apco2Period, bling_inscal_apco2,
     I     bling_apco2Const,
     O     apco2, bling_apco20, bling_apco21,
# ifdef USE_EXF_INTERPOLATION
     I     bling_apco2_lon0, bling_apco2_lon_inc,
     I     bling_apco2_lat0, bling_apco2_lat_inc,
     I     bling_apco2_nlon, bling_apco2_nlat, xC, yC,
     I     bling_apco2_interpMethod,
# endif
     I     myThid )
       ENDIF
C      Silica concentration
C     Because in pkg/bling
C     silica is already used before the first timestep (to initialize 
C     pH/pCO2) and before dic_fields_load is called, silica needs 
C     to have the correct value with respect to the current timestep 
C     (nIter0) and myTime (startTime + deltaTClock*nIter0). Therefore, 
C     we do not call exf_init_fld but exf_set_fld instead.
       IF ( bling_silicaFile .NE. ' ' ) THEN
      myLocTime = startTime + deltaTClock*nIter0
      CALL EXF_SET_FLD(
     I     'silica', bling_silicaFile, bling_silicaMask,
     I     bling_silicaStartTime, bling_silicaPeriod, 
     I     bling_silicaRepCycle, bling_inscal_silica, 
     &     bling_silica_exfremo_intercept, 
     &     bling_silica_exfremo_slope,
     O     silica, silica0, silica1,
# ifdef USE_EXF_INTERPOLATION
     I     bling_silica_lon0, bling_silica_lon_inc,
     I     bling_silica_lat0, bling_silica_lat_inc,
     I     bling_silica_nlon, bling_silica_nlat, xC, yC,
     I     bling_silica_interpMethod,
# endif
     I     myLocTime, nIter0, myThid )
       ENDIF
C      Surface iron flux
       IF ( bling_ironFile .NE. ' ' ) THEN
      CALL EXF_INIT_FLD(
     I     'InputFe', bling_ironFile, bling_ironMask,
     I     bling_ironPeriod, bling_inscal_iron, bling_ironConst,
     O     InputFe, InputFe0, InputFe1,
# ifdef USE_EXF_INTERPOLATION
     I     bling_iron_lon0, bling_iron_lon_inc,
     I     bling_iron_lat0, bling_iron_lat_inc,
     I     bling_iron_nlon, bling_iron_nlat, xC, yC,
     I     bling_iron_interpMethod,
# endif
     I     myThid )
       ENDIF
C      wind
       IF ( bling_windFile .NE. ' ' ) THEN
      CALL EXF_INIT_FLD(
     I     'wind', bling_windFile, bling_windMask,
     I     bling_windPeriod, bling_inscal_wind, bling_windConst,
     O     wind, dicwind0, dicwind1,
# ifdef USE_EXF_INTERPOLATION
     I     bling_wind_lon0, bling_wind_lon_inc,
     I     bling_wind_lat0, bling_wind_lat_inc,
     I     bling_wind_nlon, bling_wind_nlat, xC, yC,
     I     bling_wind_interpMethod,
# endif
     I     myThid )
       ENDIF
C      Sea ice
       IF ( bling_iceFile .NE. ' ' ) THEN
      CALL EXF_INIT_FLD(
     I     'fIce', bling_iceFile, bling_iceMask,
     I     bling_icePeriod, bling_inscal_ice, bling_iceConst,
     O     fIce, ice0, ice1,
# ifdef USE_EXF_INTERPOLATION
     I     bling_ice_lon0, bling_ice_lon_inc,
     I     bling_ice_lat0, bling_ice_lat_inc,
     I     bling_ice_nlon, bling_ice_nlat, xC, yC,
     I     bling_ice_interpMethod,
# endif
     I     myThid )
       ENDIF
C      Atmospheric pressure (AtmosP is in atm, bling_apressConst in Pa)
       IF ( bling_apressFile .NE. ' ' ) THEN
      CALL EXF_INIT_FLD(
     I     'AtmosP', bling_apressFile, bling_apressMask,
     I     bling_apressPeriod, bling_inscal_apress,
     I     bling_apressConst/Pa2Atm,
     O     AtmosP, apress0, apress1,
# ifdef USE_EXF_INTERPOLATION
     I     bling_apress_lon0, bling_apress_lon_inc,
     I     bling_apress_lat0, bling_apress_lat_inc,
     I     bling_apress_nlon, bling_apress_nlat, xC, yC,
     I     bling_apress_interpMethod,
# endif
     I     myThid )
       ENDIF
      ENDIF
#endif /* ALLOW_EXF */

#endif /* ALLOW_BLING */
      RETURN
      END
