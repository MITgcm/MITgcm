#include "GCHEM_OPTIONS.h"
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

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF
# include "GRID.h"
# include "DYNVARS.h"
#endif /* ALLOW_AUTODIFF */
#include "GCHEM.h"
#include "GCHEM_SIZE.h"
#include "GCHEM_FIELDS.h"

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

      RETURN
      END
