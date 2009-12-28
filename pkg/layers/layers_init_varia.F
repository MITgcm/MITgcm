C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_init_varia.F,v 1.2 2009/12/28 02:39:11 jmc Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE LAYERS_INIT_VARIA( myThid )

C ===================================================================
C     Initialize LAYERS variables.
C ===================================================================

      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

C  INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid

#ifdef ALLOW_LAYERS

C     === Local variables ===
      INTEGER i,j,kg,bi,bj

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C     Loop through 3D variables:
        DO kg=1,Nlayers
         DO j=1-Oly,sNy+OLy
          DO i=1-Olx,sNx+Olx

#ifdef LAYERS_UFLUX
           layers_UFlux(i,j,kg,bi,bj) = 0. _d 0
#ifdef LAYERS_THICKNESS
           layers_HU(i,j,kg,bi,bj) = 0. _d 0
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
           layers_VFlux(i,j,kg,bi,bj) = 0. _d 0
#ifdef LAYERS_THICKNESS
           layers_HV(i,j,kg,bi,bj) = 0. _d 0
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_TIMEAVE
C     Initialize averages to zero
        layers_TimeAve(bi,bj) = 0. _d 0

#ifdef LAYERS_UFLUX
        CALL TIMEAVE_RESET(layers_UFlux_T,Nlayers,bi,bj,myThid)
#ifdef LAYERS_THICKNESS
        CALL TIMEAVE_RESET(layers_HU_T,Nlayers,bi,bj,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
        CALL TIMEAVE_RESET(layers_VFlux_T,Nlayers,bi,bj,myThid)
#ifdef LAYERS_THICKNESS
        CALL TIMEAVE_RESET(layers_HV_T,Nlayers,bi,bj,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

#endif /* ALLOW_TIMEAVE */

C- end bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_LAYERS */

      RETURN
      END
