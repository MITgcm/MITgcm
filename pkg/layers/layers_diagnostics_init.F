C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_diagnostics_init.F,v 1.3 2012/09/19 22:46:56 gforget Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE LAYERS_DIAGNOSTICS_INIT( myThid )

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
#ifdef ALLOW_DIAGNOSTICS
# include "DIAGNOSTICS_SIZE.h"
# include "DIAGNOSTICS.h"
#endif

C  INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid

      INTEGER        iLa
      INTEGER        diagNum
      INTEGER        diagMate
      CHARACTER*8    diagName
      CHARACTER*16   diagCode
      CHARACTER*16   diagUnits
      CHARACTER*(80) diagTitle

#ifdef ALLOW_LAYERS
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

      diagNum=ndiagt

      DO iLa=1,layers_maxNum
      IF ( layers_num(iLa).NE.0 ) THEN      

#ifdef LAYERS_UFLUX
      WRITE(diagName,'(A4,I1,A3)') 'LaUH',iLa,layers_name(iLa)
      diagTitle = 'Layer Integrated U (UH in m2/s)'
      diagUnits = 'm2/s             '
      diagCode  = 'UU      MX      '
#ifdef LAYERS_VFLUX
      diagMate  = diagNum + 2
#else
      diagMate  = 0
#endif
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )
#endif

#ifdef LAYERS_VFLUX
      WRITE(diagName,'(A4,I1,A3)') 'LaVH',iLa,layers_name(iLa)
      diagTitle = 'Layer Integrated V (VH m2/s)'
      diagUnits = 'm2/s             '
      diagCode  = 'VV      MX      '
#ifdef LAYERS_UFLUX
      diagMate  = diagNum
#else
      diagMate  = 0
#endif
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )
#endif

#ifdef LAYERS_THICKNESS
#ifdef LAYERS_UFLUX
      WRITE(diagName,'(A4,I1,A3)') 'LaHw',iLa,layers_name(iLa)
      diagTitle = 'Layer Thickness at U points (m)'
      diagUnits = 'm             '
      diagCode  = 'UU      MX      '
#ifdef LAYERS_VFLUX
      diagMate  = diagNum + 2
#else
      diagMate  = 0
#endif
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )
#endif

#ifdef LAYERS_VFLUX
      WRITE(diagName,'(A4,I1,A3)') 'LaHs',iLa,layers_name(iLa)
      diagTitle = 'Layer Thickness at V points (m)'
      diagUnits = 'm             '
      diagCode  = 'VV      MX      '
#ifdef LAYERS_UFLUX
      diagMate  = diagNum
#else
      diagMate  = 0
#endif
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )
#endif
#endif

      ENDIF
      ENDDO

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_LAYERS */

      RETURN
      END
