C $Header: /u/gcmpack/MITgcm/pkg/my82/my82_init_fixed.F,v 1.1 2008/04/14 09:42:03 mlosch Exp $
C $Name:  $

#include "MY82_OPTIONS.h"

CBOP
C     !ROUTINE: MY82_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE MY82_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE MY82_INIT_FIXED
C     | o Routine to initialize MY82 variables 
C     |   that are kept fixed during the run.
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES :
C     === Local variables ===

#ifdef ALLOW_MNC
      IF (useMNC) THEN
C       Define grid types for Mellor-Yamada (MY) variables
        CALL MNC_CW_ADD_VNAME('MYviscAr', 'Cen_xy_Hn__C__t',
     &       4,5, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('MYviscAr','units','m^2/s', 
     &       myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('MYviscAr','long_name',
     &       'MY_vertical_eddy_viscosity_coefficient', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('MYviscAr',
     &       'coordinates','XC YC RC iter', myThid)
        
        CALL MNC_CW_ADD_VNAME('MYdiffKr', 'Cen_xy_Hn__C__t', 
     &       4,5, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('MYdiffKr','units','m^2/s', 
     &       myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('MYdiffKr','long_name',
     &       'MY_tracer_vertical_diffusion_coefficient', 
     &       myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('MYdiffKr',
     &       'coordinates','XC YC RC iter', myThid)
        
        CALL MNC_CW_ADD_VNAME('MYhbl', 'Cen_xy_Hn__-__t', 
     &       3,4, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('MYhbl','units','m', 
     &       myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('MYhbl','long_name',
     &       'MY_boundary_layer_depth', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('MYhbl',
     &       'coordinates','XC YC iter', myThid)
      ENDIF
#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL MY82_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
