C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/Attic/aim_aim2dyn_exchanges.F,v 1.4 2007/04/04 02:47:17 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"
#ifdef ALLOW_THSICE
# include "THSICE_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: AIM_AIM2DYN_EXCHANGES
C     !INTERFACE:
      SUBROUTINE AIM_AIM2DYN_EXCHANGES(
     I               myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE AIM_AIM2DYN_EXCHANGES
C     | o Do overlap exchanges for AIM related quantities.
C     |   AIM does not compute into overlaps so exchanges
C     |   are needed for AIM quantities.
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"

#include "AIM2DYN.h"

#ifdef ALLOW_THSICE
# include "THSICE_PARAMS.h"
# include "THSICE_VARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myTime    :: Current time in simulation (s)
C     myIter    :: Current iteration number
C     myThid    :: My Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_AIM
C     !LOCAL VARIABLES:
#ifdef COMPONENT_MODULE
      INTEGER bi,bj
#endif

      _EXCH_XY_R8( aim_drag, myThid )

#ifdef ALLOW_THSICE
      IF (useThSIce) THEN
       CALL EXCH_XY_RL( iceMask, myThid )
       IF ( useSEAICE .OR. thSIceAdvScheme.GT.0 ) THEN
        CALL EXCH_XY_RL( iceHeight, myThid )
        CALL EXCH_XY_RL( snowHeight, myThid )
        CALL EXCH_XY_RL( Qice1, myThid )
        CALL EXCH_XY_RL( Qice2, myThid )
#ifdef ATMOSPHERIC_LOADING
        IF ( useRealFreshWaterFlux )
     &  CALL EXCH_XY_RS( sIceLoad, myThid )
#endif
       ENDIF
      ENDIF
#endif

#ifdef COMPONENT_MODULE
      IF ( useCoupler ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          CALL ATM_STORE_TAUX( bi,bj, myTime, myIter, myThid )
          CALL ATM_STORE_TAUY( bi,bj, myTime, myIter, myThid )
        ENDDO
       ENDDO
      ENDIF
#endif /* COMPONENT_MODULE */

#endif /* ALLOW_AIM */

      RETURN
      END
