C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_calc.F,v 1.13 2012/09/18 23:02:10 gforget Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"
#ifdef ALLOW_GMREDI
#include "GMREDI_OPTIONS.h"
#endif

CBOP 0
C !ROUTINE: LAYERS_CALC

C !INTERFACE:
      SUBROUTINE LAYERS_CALC(
     I                  myTime, myIter, myThid )

C !DESCRIPTION:
C ===================================================================
C     Calculate the transport in isopycnal layers.
C     This was the meat of the LAYERS package, which
C     has been moved to S/R LAYERS_FLUXCALC.F
C ===================================================================

C !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif

C !INPUT PARAMETERS:
C     myTime :: Current time in simulation
C     myIter :: Current iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_LAYERS

C !LOCAL VARIABLES:
C     bi, bj   :: tile indices
C     i,j      :: horizontal indices
C     k        :: vertical index for model grid

      INTEGER bi, bj
#ifdef LAYERS_PRHO_REF
      INTEGER i, j, k
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (LAYER_nb .EQ. 1) THEN
       CALL LAYERS_FLUXCALC( uVel,vVel,theta,layers_G,
     &  layers_UFlux,layers_VFlux,layers_HU,layers_HV,myThid)
      ELSEIF (LAYER_nb .EQ. 2) THEN
       CALL LAYERS_FLUXCALC( uVel,vVel,salt,layers_G,
     &  layers_UFlux,layers_VFlux,layers_HU,layers_HV,myThid)
      ENDIF

#ifdef LAYERS_PRHO_REF
C     For LAYER_nb = 3, calculate the potential density referenced to
C     the model level given by layers_kref.
      IF (LAYER_nb .EQ. 3) THEN
       DO k = 1,Nr
        CALL FIND_RHO_2D( 1-OLx, sNx+OLx, 1-OLy, sNy+OLy,
     &     layers_kref,
     &     theta(1-OLx,1-OLy,k,bi,bj),
     &     salt(1-OLx,1-OLy,k,bi,bj),
     &     prho(1-OLx,1-OLy,k,bi,bj),
     &     k, bi, bj, myThid )
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
           prho(i,j,k,bi,bj) = rhoConst + prho(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ENDDO
       CALL LAYERS_FLUXCALC( uVel,vVel,prho,layers_G,
     &  layers_UFlux,layers_VFlux,layers_HU,layers_HV,myThid)
      ENDIF
#endif


#ifdef ALLOW_TIMEAVE
C--   Time-average
      IF ( layers_taveFreq.GT.0. ) THEN
C --- The tile loops
      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef LAYERS_UFLUX
         CALL TIMEAVE_CUMULATE( layers_UFlux_T, layers_UFlux, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
#ifdef LAYERS_THICKNESS
         CALL TIMEAVE_CUMULATE( layers_HU_T, layers_HU, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */
#ifdef LAYERS_VFLUX
         CALL TIMEAVE_CUMULATE( layers_VFlux_T, layers_VFlux, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
#ifdef LAYERS_THICKNESS
         CALL TIMEAVE_CUMULATE( layers_HV_T, layers_HV, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

#ifdef LAYERS_PRHO_REF
      IF (LAYER_nb .EQ. 3) THEN
         CALL TIMEAVE_CUMULATE( prho_tave, prho, Nr,
     &                          deltaTclock, bi, bj, myThid )
      ENDIF
#endif /* LAYERS_PRHO_REF */

         layers_TimeAve(bi,bj)=layers_TimeAve(bi,bj)+deltaTclock

C --- End bi,bj loop
      ENDDO
      ENDDO
      ENDIF
#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_LAYERS */

      RETURN
      END
