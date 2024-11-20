#include "GCHEM_OPTIONS.h"
C!! needed?
#ifdef ALLOW_BLING
# include "BLING_OPTIONS.h"
#endif
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
#ifdef ALLOW_EXF
#include "EXF_INTERP_SIZE.h"
#endif /* ALLOW_EXF */
#include "GRID.h"
#ifdef ALLOW_AUTODIFF
# include "DYNVARS.h"
#endif /* ALLOW_AUTODIFF */
#include "GCHEM.h"
#include "GCHEM_SIZE.h"
#include "GCHEM_FIELDS.h"
#include "GCHEM_EXF.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

C !LOCAL VARIABLES: ====================================================
C  i,j,k,bi,bj          :: loop indices
C  jTr                  :: ptracer number
      INTEGER bi,bj
      INTEGER i,j
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

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C     First call requires that we initialize everything to zero for safety
        GCHEM_ldRec(bi,bj) = 0
C     set reasonable values fields that need at least something
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          gchemapCO2 (i,j,bi,bj) = GCHEM_pCO2   * maskC(i,j,1,bi,bj)
          gchemWind  (i,j,bi,bj) = 5. _d 0      * maskC(i,j,1,bi,bj)
          gchemAtmosP(i,j,bi,bj) = 1.01325 _d 5 * maskC(i,j,1,bi,bj)
          gchemIce   (i,j,bi,bj) = 0. _d 0
          gchemFe    (i,j,bi,bj) = 1. _d -11    * maskC(i,j,1,bi,bj)
          gchemSi    (i,j,bi,bj) = 7.6838 _d -3 * maskC(i,j,1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( .NOT. useEXF ) THEN
       IF ( gchem_silicaFile .NE. ' '  ) THEN
        CALL READ_FLD_XY_RS( gchem_silicaFile,' ',gchemSi,0,myThid )
       ENDIF
       IF ( gchem_PARfile .NE. ' '  ) THEN
        CALL READ_FLD_XY_RS( gchem_PARfile,' ',gchemPAR,0,myThid )
       ENDIF
       IF ( gchem_ironFile .NE. ' '  ) THEN
        CALL READ_FLD_XY_RS( gchem_ironFile,' ',gchemFe,0,myThid )
       ENDIF
       IF ( gchem_apCO2file .NE. ' '  ) THEN
        CALL READ_FLD_XY_RS( gchem_apCO2file,' ',gchemapCO2,0,myThid )
       ENDIF
      ENDIF

#ifdef ALLOW_EXF
      IF ( useEXF ) THEN

       IF ( gchem_silicafile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'silica', gchem_silicafile, silicamask,
     &     silicaperiod, gchem_inscal_silica, silicaconst,
     &     gchemSi, gchemSi0, gchemSi1,
#ifdef USE_EXF_INTERPOLATION
     &     silica_lon0, silica_lon_inc,
     &     silica_lat0, silica_lat_inc,
     &     silica_nlon, silica_nlat, xC, yC,
     &     silica_interpMethod,
#endif
     &     mythid )
       ENDIF

       IF ( gchem_PARfile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'PAR', gchem_PARfile, PARmask,
     &     PARperiod, gchem_inscal_PAR, PARconst,
     &     gchemPAR, gchemPAR0, gchemPAR1,
#ifdef USE_EXF_INTERPOLATION
     &     PAR_lon0, PAR_lon_inc,
     &     PAR_lat0, PAR_lat_inc,
     &     PAR_nlon, PAR_nlat, xC, yC,
     &     PAR_interpMethod,
#endif
     &     mythid )
       ENDIF

       IF ( gchem_ironfile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'iron', gchem_ironfile, ironmask,
     &     ironperiod, gchem_inscal_iron, ironconst,
     &     gchemFe, gchemIron0, gchemIron1,
#ifdef USE_EXF_INTERPOLATION
     &     iron_lon0, iron_lon_inc,
     &     iron_lat0, iron_lat_inc,
     &     iron_nlon, iron_nlat, xC, yC,
     &     iron_interpMethod,
#endif
     &     mythid )
       ENDIF

       IF ( gchem_apCO2file .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'apCO2', gchem_apCO2file, apCO2mask,
     &     apCO2period, gchem_inscal_apCO2, apCO2const,
     &     gchemapCO2, gchemApCO20, gchemApCO21,
#ifdef USE_EXF_INTERPOLATION
     &     apCO2_lon0, apCO2_lon_inc,
     &     apCO2_lat0, apCO2_lat_inc,
     &     apCO2_nlon, apCO2_nlat, xC, yC,
     &     apCO2_interpMethod,
#endif
     &     mythid )
       ENDIF

C     useEXF
      ENDIF
#endif /* ALLOW_EXF */

      _EXCH_XY_RL( gchemSi, myThid )
      _EXCH_XY_RL( gchemPAR, myThid )
      _EXCH_XY_RL( gchemFe, myThid )
      _EXCH_XY_RL( gchemapCO2, myThid )

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
