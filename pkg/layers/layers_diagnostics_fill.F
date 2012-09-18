C $Header: /u/gcmpack/MITgcm/pkg/layers/Attic/layers_diagnostics_fill.F,v 1.1 2012/09/18 21:30:35 gforget Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE LAYERS_DIAGNOSTICS_FILL( myThid )

C ===================================================================
C     Initialize LAYERS variables that are kept fixed during the run.
C ===================================================================

      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_LAYERS
# include "LAYERS_SIZE.h"
# include "LAYERS.h"
#endif

C  INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid

#ifdef ALLOW_LAYERS
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

#ifdef LAYERS_UFLUX
         CALL DIAGNOSTICS_FILL( layers_UFlux,'LaUH    ',
     &                          0,Nlayers, 0, 1, 1, myThid )
# ifdef LAYERS_THICKNESS
         CALL DIAGNOSTICS_FILL( layers_HU,'LaHw    ',
     &                          0,Nlayers, 0, 1, 1, myThid )
# endif
#endif

#ifdef LAYERS_UFLUX
         CALL DIAGNOSTICS_FILL( layers_VFlux,'LaVH    ',
     &                          0,Nlayers, 0, 1, 1, myThid )
# ifdef LAYERS_THICKNESS
         CALL DIAGNOSTICS_FILL( layers_HV,'LaHs    ',
     &                          0,Nlayers, 0, 1, 1, myThid )
# endif
#endif

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_LAYERS */
     
      RETURN
      END
