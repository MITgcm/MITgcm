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
#ifdef ALLOW_AUTODIFF
# include "GRID.h"
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
#ifdef GCHEM_ADD2TR_TENDENCY
      INTEGER i,j,k
      INTEGER bi,bj
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

C-- Initialize Geo-Chemistry forcing fields

#ifdef ALLOW_EXF

      IF ( useEXF ) THEN

       IF ( exf_silicafile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'silica', exf_silicafile, silicamask,
     &     silicaperiod, gchem_inscal_silica, silicaconst,
     &     gchemSi, gSi0, gSi1,
#ifdef USE_EXF_INTERPOLATION
     &     silica_lon0, silica_lon_inc,
     &     silica_lat0, silica_lat_inc,
     &     silica_nlon, silica_nlat, xC, yC,
     &     silica_interpMethod,
#endif
     &     mythid )
       ENDIF

       IF ( exf_PARfile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'PAR', exf_PARfile, PARmask,
     &     PARperiod, gchem_inscal_PAR, PARconst,
     &     gchemPAR, gPAR0, gPAR1,
#ifdef USE_EXF_INTERPOLATION
     &     PAR_lon0, PAR_lon_inc,
     &     PAR_lat0, PAR_lat_inc,
     &     PAR_nlon, PAR_nlat, xC, yC,
     &     PAR_interpMethod,
#endif
     &     mythid )
       ENDIF

       IF ( exf_ironfile .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'iron', exf_ironfile, ironmask,
     &     ironperiod, gchem_inscal_iron, ironconst,
     &     gchemFe, giron0, giron1,
#ifdef USE_EXF_INTERPOLATION
     &     iron_lon0, iron_lon_inc,
     &     iron_lat0, iron_lat_inc,
     &     iron_nlon, iron_nlat, xC, yC,
     &     iron_interpMethod,
#endif
     &     mythid )
       ENDIF

       IF ( exf_apCO2file .NE. ' ' ) THEN
        CALL EXF_INIT_FLD (
     &     'apCO2', exf_apCO2file, apCO2mask,
     &     apCO2period, gchem_inscal_apCO2, apCO2const,
     &     gchemapCO2, apCO20, apCO21,
#ifdef USE_EXF_INTERPOLATION
     &     apCO2_lon0, apCO2_lon_inc,
     &     apCO2_lat0, apCO2_lat_inc,
     &     apCO2_nlon, apCO2_nlat, xC, yC,
     &     apCO2_interpMethod,
#endif
     &     mythid )
       ENDIF

       _EXCH_XY_RL( gchemSi, myThid )
       _EXCH_XY_RL( gchemPAR, myThid )
       _EXCH_XY_RL( gchemFe, myThid )
       _EXCH_XY_RL( gchemapCO2, myThid )

C     useEXF
      ENDIF
#endif /* ALLOW_EXF */

      RETURN
      END
