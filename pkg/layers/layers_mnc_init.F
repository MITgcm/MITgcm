C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_mnc_init.F,v 1.1 2009/09/16 21:25:47 rpa Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE LAYERS_MNC_INIT( myThid )

C     Initialize MNC lookup tables for LAYERS.

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_LAYERS
#ifdef ALLOW_MNC

      IF (useMNC) THEN

C--     Need to figure out how to create new grids. We need
C           'U_xy_Hn__G__t'
C           'V_xy_Hn__G__t'

        CALL MNC_CW_ADD_VNAME('UFlux','U_xy_Hn__G__t', 4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('UFlux','units','m^2/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('UFlux','description',
     &       'Eastward Velocity Integrated Over Isopycnal Layer',
     &       myThid)

        CALL MNC_CW_ADD_VNAME('VFlux','V_xy_Hn__G__t', 4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('VFlux','units','m^2/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('VFlux','description',
     &       'Northward Velocity Integrated Over Isopycnal Layer',
     &       myThid)


      ENDIF

#endif /* ALLOW_MNC */
#endif /* ALLOW_GMREDI */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
