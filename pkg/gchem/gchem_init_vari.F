#include "GCHEM_OPTIONS.h"
#ifdef ALLOW_DARWIN
# include "DARWIN_OPTIONS.h"
#endif

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_INIT_VARI(myThid )

C !DESCRIPTION:
C calls subroutines that initialize any time dependent variables
C for any tracer experiment
C!! remove exf_init_fld from bling ini forcing

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
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          gchemapCO2 (i,j,bi,bj) = gchem_apco2_const*maskC(i,j,1,bi,bj)
          gchemWind  (i,j,bi,bj) = gchem_wind_const *maskC(i,j,1,bi,bj)
          gchemAtmosP(i,j,bi,bj) = gchem_apres_const*maskC(i,j,1,bi,bj)
          gchemPAR   (i,j,bi,bj) = gchem_par_const  *maskC(i,j,1,bi,bj)
          gchemFe    (i,j,bi,bj) = gchem_Fe_const   *maskC(i,j,1,bi,bj)
          gchemSi    (i,j,bi,bj) = gchem_Si_const   *maskC(i,j,1,bi,bj)
          gchemIce   (i,j,bi,bj) = 0. _d 0          * maskC(i,j,1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( .NOT. gchem_useEXF ) THEN
         CALL LEF_ZERO( gchemSi0,myThid )
         CALL LEF_ZERO( gchemSi1,myThid )

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

        IF ( gchem_FeFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RS( gchem_FeFile,gchemFe0,intime0,
     &         nIter0,myThid )
          CALL READ_REC_XY_RS( gchem_FeFile,gchemFe1,intime1,
     &         nIter0,myThid )
        ENDIF

        _EXCH_XY_RS(gchemWind0, myThid )
        _EXCH_XY_RS(gchemWind1, myThid )
        _EXCH_XY_RS(gchemSi0, myThid )
        _EXCH_XY_RS(gchemSi1, myThid )
        _EXCH_XY_RS(gchemFe0, myThid )
        _EXCH_XY_RS(gchemFe1, myThid )

        IF ( gchem_WindFile .NE. ' '  ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemWind(i,j,bi,bj)= bWght*gchemWind0(i,j,bi,bj)
     &                        + aWght*gchemWind1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF

        IF ( gchem_SiFile .NE. ' '  ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemSi(i,j,bi,bj)= bWght*gchemSi0(i,j,bi,bj)
     &                        + aWght*gchemSi1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF

        IF ( gchem_FeFile .NE. ' '  ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gchemFe(i,j,bi,bj)= bWght*gchemFe0(i,j,bi,bj)
     &                        + aWght*gchemFe1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
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

C     gchem_useEXF
      ENDIF
#endif /* ALLOW_EXF */

      _EXCH_XY_RL( gchemSi, myThid )
      _EXCH_XY_RL( gchemPAR, myThid )
      _EXCH_XY_RL( gchemFe, myThid )
      _EXCH_XY_RL( gchemapCO2, myThid )
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
