C $Header: /u/gcmpack/MITgcm/pkg/layers/Attic/layers_diagnostics_fill.F,v 1.2 2012/09/19 18:48:18 gforget Exp $
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
      CHARACTER*8    diagName
      INTEGER iLa
      
#ifdef ALLOW_LAYERS
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

      DO iLa=1,layers_maxNum
      IF ( layers_num(iLa).NE.0 ) THEN      

#ifdef LAYERS_UFLUX
         WRITE(diagName,'(A4,I1,A3)') 'LaUH',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_UFlux(1-OLx,1-OLy,1,1,1,iLa),
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
# ifdef LAYERS_THICKNESS
         WRITE(diagName,'(A4,I1,A3)') 'LaHw',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_HU(1-OLx,1-OLy,1,1,1,iLa),
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
# endif
#endif

#ifdef LAYERS_UFLUX
         WRITE(diagName,'(A4,I1,A3)') 'LaVH',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_VFlux(1-OLx,1-OLy,1,1,1,iLa),
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
# ifdef LAYERS_THICKNESS
         WRITE(diagName,'(A4,I1,A3)') 'LaHs',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_HV(1-OLx,1-OLy,1,1,1,iLa),
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
# endif
#endif

      ENDIF
      ENDDO

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_LAYERS */
     
      RETURN
      END
