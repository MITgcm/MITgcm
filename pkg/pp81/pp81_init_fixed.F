C $Header: /u/gcmpack/MITgcm/pkg/pp81/pp81_init_fixed.F,v 1.1 2011/10/26 09:29:36 mlosch Exp $
C $Name:  $

#include "PP81_OPTIONS.h"

CBOP
C     !ROUTINE: PP81_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE PP81_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PP81_INIT_FIXED
C     | o Routine to initialize PP81 variables 
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

CML#ifdef ALLOW_MNC
CML      IF (useMNC) THEN
CMLC       Define grid types for Mellor-Yamada (MY) variables
CML        CALL MNC_CW_ADD_VNAME('PPviscAr', 'Cen_xy_Hn__C__t',
CML     &       4,5, myThid)
CML        CALL MNC_CW_ADD_VATTR_TEXT('PPviscAr','units','m^2/s', 
CML     &       myThid)
CML        CALL MNC_CW_ADD_VATTR_TEXT('PPviscAr','long_name',
CML     &       'MY_vertical_eddy_viscosity_coefficient', myThid)
CML        CALL MNC_CW_ADD_VATTR_TEXT('PPviscAr',
CML     &       'coordinates','XC YC RC iter', myThid)
CML        
CML        CALL MNC_CW_ADD_VNAME('PPdiffKr', 'Cen_xy_Hn__C__t', 
CML     &       4,5, myThid)
CML        CALL MNC_CW_ADD_VATTR_TEXT('PPdiffKr','units','m^2/s', 
CML     &       myThid)
CML        CALL MNC_CW_ADD_VATTR_TEXT('PPdiffKr','long_name',
CML     &       'MY_tracer_vertical_diffusion_coefficient', 
CML     &       myThid)
CML        CALL MNC_CW_ADD_VATTR_TEXT('PPdiffKr',
CML     &       'coordinates','XC YC RC iter', myThid)
CML#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL PP81_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
