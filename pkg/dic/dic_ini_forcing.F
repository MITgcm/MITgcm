#include "DIC_OPTIONS.h"

CBOP
C !ROUTINE: DIC_INI_FORCING

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_INI_FORCING( myThid )

C !DESCRIPTION:
c initializes forcing fields to zero
c (or to reasonable values)

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
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
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIC

c !LOCAL VARIABLES: ====================================================
       INTEGER bi, bj, i, j
       INTEGER intimeP, intime0, intime1
       _RL aWght, bWght, myLocTime
#ifdef DIC_CALCITE_SAT
      INTEGER k
#endif

      IF ( .NOT. dic_useEXF ) THEN
C--   Initialise forcing arrays in common blocks
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-    Initialise DIC_VARS.h forcing-field arrays:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           wind      (i,j,bi,bj) = 0. _d 0
           fIce      (i,j,bi,bj) = 0. _d 0
           AtmosP    (i,j,bi,bj) = 0. _d 0
           silicaSurf(i,j,bi,bj) = 0. _d 0
#ifdef DIC_BIOTIC
           par       (i,j,bi,bj) = 0. _d 0
           CHL       (i,j,bi,bj) = 0. _d 0
           InputFe   (i,j,bi,bj) = 0. _d 0
#endif
         ENDDO
        ENDDO
#ifdef DIC_CALCITE_SAT
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           silicaDeep(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
#endif

C-    Initialise DIC_LOAD.h arrays:
        DIC_ldRec(bi,bj) = 0
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           dicwind0(i,j,bi,bj) = 0. _d 0
           dicwind1(i,j,bi,bj) = 0. _d 0
           atmosp0 (i,j,bi,bj) = 0. _d 0
           atmosp1 (i,j,bi,bj) = 0. _d 0
           silicaSurf0(i,j,bi,bj) = 0. _d 0
           silicaSurf1(i,j,bi,bj) = 0. _d 0
           ice0    (i,j,bi,bj) = 0. _d 0
           ice1    (i,j,bi,bj) = 0. _d 0
#ifdef READ_PAR
           par0    (i,j,bi,bj) = 0. _d 0
           par1    (i,j,bi,bj) = 0. _d 0
#endif
#ifdef ALLOW_FE
           feinput0(i,j,bi,bj) = 0. _d 0
           feinput1(i,j,bi,bj) = 0. _d 0
#endif
#ifdef LIGHT_CHL
           chlinput(i,j,bi,bj) = 0. _d 0
#endif
         ENDDO
        ENDDO
#ifdef DIC_CALCITE_SAT
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           silicaDeep0(i,j,k,bi,bj) = 0. _d 0
           silicaDeep1(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
#endif
C-   end bi,bj loops:
       ENDDO
      ENDDO

C ======================================================================

C--   Set reasonable values to those that need at least something
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            wind(i,j,bi,bj)       = 5. _d 0*maskC(i,j,1,bi,bj)
            AtmosP(i,j,bi,bj)     = 1. _d 0*maskC(i,j,1,bi,bj)
            silicaSurf(i,j,bi,bj) = 7.6838 _d -3*maskC(i,j,1,bi,bj)
            fIce(i,j,bi,bj)       = 0. _d 0
#ifdef READ_PAR
            par(i,j,bi,bj)        = 100. _d 0*maskC(i,j,1,bi,bj)
#endif
#ifdef LIGHT_CHL
C If the chlorophyll climatology is not provided, use this default value.
            CHL(i,j,bi,bj)        = 1. _d -2*maskC(i,j,1,bi,bj)
#endif
#ifdef ALLOW_FE
            InputFe(i,j,bi,bj)    = 1. _d -11*maskC(i,j,1,bi,bj)
#endif
          ENDDO
         ENDDO
#ifdef DIC_CALCITE_SAT
         IF ( useCalciteSaturation ) THEN
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              silicaDeep(i,j,k,bi,bj) = 3. _d -2*maskC(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
#endif
C-    end bi,bj loops
       ENDDO
      ENDDO

C ======================================================================

C--   Load constant forcing-file (used if DIC_forcingCycle=0 )
c     IF ( DIC_forcingCycle .LE. zeroRL ) THEN
       IF ( DIC_windFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( DIC_windFile, wind,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( wind, myThid )
       ENDIF
       IF ( DIC_atmospFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( DIC_atmospFile, AtmosP,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( AtmosP, myThid )
       ENDIF
       IF ( DIC_silicaFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( DIC_silicaFile, silicaSurf,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( silicaSurf, myThid )
       ENDIF
       IF ( DIC_iceFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( DIC_iceFile, fIce,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( fIce, myThid )
       ENDIF
#ifdef DIC_BIOTIC
c#ifdef READ_PAR
       IF ( DIC_parFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( DIC_parFile, par,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( par, myThid )
       ENDIF
c#endif
c#ifdef LIGHT_CHL
C--   Load chlorophyll climatology data, unit for chlorophyll : mg/m3
       IF ( DIC_chlaFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( DIC_chlaFile, CHL,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( CHL, myThid )
       ENDIF
c#endif
c#ifdef ALLOW_FE
       IF ( DIC_ironFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( DIC_ironFile, InputFe,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( InputFe, myThid )
       ENDIF
c#endif
#endif /* DIC_BIOTIC */
#ifdef DIC_CALCITE_SAT
       IF ( useCalciteSaturation .AND.
     &      DIC_silicaDeepFile .NE. ' '  ) THEN
         CALL READ_REC_XYZ_RL( DIC_silicaDeepFile, silicaDeep,
     &                        1, nIter0, myThid )
         _EXCH_XYZ_RL( silicaDeep, myThid )
       ENDIF
#endif
c     ENDIF

C ======================================================================

      IF ( DIC_forcingCycle .GT. zeroRL ) THEN
C--   Load some initial forcing from file

C-    Read in surface silica field (used to compute initial surf. pH)
C     Note: For this purpose, might be accurate enough to use first record
C           in silica file (already loaded) and skip all this block.

C-    Get reccord number and weight for time interpolation:
       CALL GET_PERIODIC_INTERVAL(
     O                   intimeP, intime0, intime1, bWght, aWght,
     I                   DIC_forcingCycle, DIC_forcingPeriod,
     I                   deltaTClock, startTime, myThid )

       _BARRIER
       _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &   ' DIC_INI_FORCING, it=', nIter0,
     &   ' : Reading new data, i0,i1=', intime0, intime1
       _END_MASTER(myThid)

       IF ( DIC_silicaFile .NE. ' '  ) THEN
C-    If file provided for surface silicate, read it in
         CALL READ_REC_XY_RS( DIC_silicaFile,silicaSurf0,intime0,
     &        nIter0,myThid )
         CALL READ_REC_XY_RS( DIC_silicaFile,silicaSurf1,intime1,
     &        nIter0,myThid )
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              silicaSurf(i,j,bi,bj) = bWght*silicaSurf0(i,j,bi,bj)
     &                              + aWght*silicaSurf1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
#ifndef ALLOW_AUTODIFF
#ifdef DIC_CALCITE_SAT
       ELSEIF ( DIC_silicaDeepFile .NE. ' '  ) THEN
C-    If no surface silicate file but deep (3d) silicate provided, use top level
         k = 1
         CALL READ_REC_XYZ_RS( DIC_silicaDeepFile, silicaDeep0,
     &                         intime0, nIter0, myThid )
         CALL READ_REC_XYZ_RS( DIC_silicaDeepFile, silicaDeep1,
     &                         intime1, nIter0, myThid )
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              silicaSurf(i,j,bi,bj) = bWght*silicaDeep0(i,j,k,bi,bj)
     &                              + aWght*silicaDeep1(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
#endif /* DIC_CALCITE_SAT */
#endif /* ndef ALLOW_AUTODIFF */
       ENDIF
       _EXCH_XY_RL( silicaSurf, myThid )

C end if DIC_forcingCycle > 0
      ENDIF
C     if not dic_useEXF
      ENDIF

#ifdef ALLOW_EXF
      IF ( dic_useEXF ) THEN

       IF ( dic_SilicaFile .NE. ' ' ) THEN
C     This would be the exf-way to initialise a forcing field: just use
C     a constant value. But, because in pkg/dic
C     silica is already used before the first timestep (to initialize 
C     pH/pCO2) and before dic_fields_load is called, silica needs 
C     to have the correct value with respect to the current timestep 
C     (nIter0) and myTime (startTime + deltaTClock*nIter0). Therefore, 
C     we do not call exf_init_fld but exf_set_fld instead.
c        CALL EXF_INIT_FLD (
c     &     'dicSilicaSurf', dic_SilicaSurfFile, dic_SilicaSurfMask,
c     &     dic_SilicaSurfPeriod, dic_inscal_SilicaSurf, dic_SilicaSurf_const,
c     &     SilicaSurf, SilicaSurf0, SilicaSurf1,
c#ifdef USE_EXF_INTERPOLATION
c     &     SilicaSurf_lon0, SilicaSurf_lon_inc,
c     &     SilicaSurf_lat0, SilicaSurf_lat_inc,
c     &     SilicaSurf_nlon, SilicaSurf_nlat, xC, yC,
c     &     SilicaSurf_interpMethod,
c#endif
c     &     mythid )
        myLocTime = startTime + deltaTClock*nIter0
        CALL EXF_SET_FLD(
     &     'dicSilicaSurf', dic_SilicaFile, dic_SilicaSurfMask,
     &     dic_SilicaSurfStartTime, dic_SilicaSurfPeriod, 
     &     dic_SilicaSurfRepCycle,dic_inscal_SilicaSurf,
     &     dic_SilicaSurf_exfremo_intercept, 
     &     dic_SilicaSurf_exfremo_slope,
     &     SilicaSurf, SilicaSurf0, SilicaSurf1,
# ifdef USE_EXF_INTERPOLATION
     &     SilicaSurf_lon0, SilicaSurf_lon_inc,
     &     SilicaSurf_lat0, SilicaSurf_lat_inc,
     &     SilicaSurf_nlon, SilicaSurf_nlat, xC, yC,
     &     SilicaSurf_interpMethod,
# endif
     &     myLocTime, nIter0, mythid )
       ENDIF

#ifdef DIC_3D_SILICA
       myLocTime = startTime + deltaTClock*nIter0
       IF ( dic_SilicaDeepFile .NE. ' ' ) THEN
         CALL EXF_SET_FLD3D(
     &      'dicSilicaDeep', dic_SilicaDeepFile, dic_SilicaDeepMask,
     &      dic_SilicaDeepStartTime, dic_SilicaDeepPeriod, 
     &      dic_SilicaDeepRepCycle,
     &      dic_inscal_SilicaDeep,
     &      dic_SilicaDeep_exfremo_intercept, 
     &      dic_SilicaDeep_exfremo_slope,
     &      SilicaDeep, SilicaDeep0, SilicaDeep1,
# ifdef USE_EXF_INTERPOLATION
     &      SilicaDeep_lon0, SilicaDeep_lon_inc,
     &      SilicaDeep_lat0, SilicaDeep_lat_inc,
     &      SilicaDeep_nlon, SilicaDeep_nlat, xC, yC,
     &      SilicaDeep_interpMethod,
     &      SilicaDeep_nzin, SilicaDeep_dzin,
# endif
     &      myLocTime, nIter0, mythid )
       ENDIF
#endif /* DIC_3D_SILICA */

C    Do the same for SST and SSS, which are also used before 
C    the first timestep to initialize pH/pCO2
       IF ( dic_SSTFile .NE. ' ' ) THEN
        myLocTime = startTime + deltaTClock*nIter0
        CALL EXF_SET_FLD(
     &     'dicSST', dic_SSTFile, dic_SSTMask,
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
     &     myLocTime, nIter0, mythid )
       ENDIF
       
       IF ( dic_SSSFile .NE. ' ' ) THEN
        myLocTime = startTime + deltaTClock*nIter0
        CALL EXF_SET_FLD(
     &     'dicSSS', dic_SSSFile, dic_SSSMask,
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
     &     myLocTime, nIter0, mythid )
       ENDIF

       IF ( dic_WindFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'dicWind', dic_WindFile, dic_WindMask,
     &     dic_WindPeriod, dic_inscal_Wind, dic_wind_const,
     &     Wind, dicWind0, dicWind1,
# ifdef USE_EXF_INTERPOLATION
     &     wind_lon0, wind_lon_inc,
     &     wind_lat0, wind_lat_inc,
     &     wind_nlon, wind_nlat, xC, yC,
     &     wind_interpMethod,
# endif
     &     mythid )
       ENDIF

#ifndef USE_PLOAD
       IF ( dic_atmospFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'dicAtmosP', dic_atmospFile, dic_atmospMask,
     &     dic_atmospPeriod, dic_inscal_atmosp, dic_atmosp_const,
     &     AtmosP, AtmosP0, AtmosP1,
# ifdef USE_EXF_INTERPOLATION
     &     atmosp_lon0, atmosp_lon_inc,
     &     atmosp_lat0, atmosp_lat_inc,
     &     atmosp_nlon, atmosp_nlat, xC, yC,
     &     atmosp_interpMethod,
# endif
     &     mythid )
       ENDIF
#endif /* USE_PLOAD */

       IF ( dic_iceFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'dicIce', dic_iceFile, dic_iceMask,
     &     dic_icePeriod, dic_inscal_ice, dic_ice_const,
     &     fIce, Ice0, Ice1,
# ifdef USE_EXF_INTERPOLATION
     &     ice_lon0, ice_lon_inc,
     &     ice_lat0, ice_lat_inc,
     &     ice_nlon, ice_nlat, xC, yC,
     &     ice_interpMethod,
# endif
     &     mythid )
       ENDIF

#ifdef READ_PAR
       IF ( dic_parFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'dicPAR', dic_parFile, dic_parMask,
     &     dic_parPeriod, dic_inscal_par, dic_par_const,
     &     PAR, PAR0, PAR1,
# ifdef USE_EXF_INTERPOLATION
     &     par_lon0, par_lon_inc,
     &     par_lat0, par_lat_inc,
     &     par_nlon, par_nlat, xC, yC,
     &     par_interpMethod,
# endif
     &     mythid )
       ENDIF
#endif /* READ_PAR */

#ifdef ALLOW_FE
       IF ( dic_ironFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'dicIron', dic_ironFile, dic_ironMask,
     &     dic_ironPeriod, dic_inscal_iron, dic_iron_const,
     &     inputFe, feinput0, feinput1,
# ifdef USE_EXF_INTERPOLATION
     &     iron_lon0, iron_lon_inc,
     &     iron_lat0, iron_lat_inc,
     &     iron_nlon, iron_nlat, xC, yC,
     &     iron_interpMethod,
# endif
     &     mythid )
       ENDIF
#endif /* ALLOW_FE */

       IF ( dic_atmospCO2File .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'dicapCO2', dic_atmospCO2File, dic_atmospCO2Mask,
     &     dic_atmospCO2Period, dic_inscal_atmospCO2, 
     &     dic_atmospCO2_const,
     &     atmospCO2, atmospCO20, atmospCO21,
# ifdef USE_EXF_INTERPOLATION
     &     atmospCO2_lon0, atmospCO2_lon_inc,
     &     atmospCO2_lat0, atmospCO2_lat_inc,
     &     atmospCO2_nlon, atmospCO2_nlat, xC, yC,
     &     atmospCO2_interpMethod,
# endif
     &     mythid )
       ENDIF

#ifdef LIGHT_CHL
       IF ( dic_chlaFile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'dicCHL', dic_chlaFile, dic_chlMask,
     &     dic_chlPeriod, dic_inscal_chl, dic_chl_const,
     &     dicchl, dicchl0, dicchl1,
# ifdef USE_EXF_INTERPOLATION
     &     chl_lon0, chl_lon_inc,
     &     chl_lat0, chl_lat_inc,
     &     chl_nlon, chl_nlat, xC, yC,
     &     chl_interpMethod,
# endif
     &     mythid )
       ENDIF
#endif /* LIGHT_CHL */
C     dic_useEXF
      ENDIF
# endif /* ALLOW_EXF */

C If there is no SST file given in data.dic, get fields from
C  the main model so we can initialize pH/pCO2 correctly.
      IF ( dic_SSTFile .EQ. ' ' ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            dicSST(i,j,bi,bj) = theta(i,j,1,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

C If there is no SSS file given in data.dic, get fields from
C  the main model so we can initialize pH/pCO2 correctly.
      IF ( dic_SSSFile .EQ. ' ' ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            dicSSS(i,j,bi,bj) = salt(i,j,1,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_DIC */
      RETURN
      END
