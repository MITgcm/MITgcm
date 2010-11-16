C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/Attic/gad_valid_advscheme.F,v 1.1 2010/11/16 17:39:13 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_VALID_ADVSCHEME

C     !INTERFACE:
      LOGICAL FUNCTION GAD_VALID_ADVSCHEME( advScheme )

C     !DESCRIPTION:
C     *==========================================================*
C     | LOGICAL FUNCTION GAD\_VALID\_ADVSCHEME
C     | o Checks for valid advection scheme number
C     *==========================================================*

      !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GAD.h"

C     !INPUT PARAMETERS:
C     advScheme  :: advection scheme number
      INTEGER advScheme

C     !LOCAL VARIABLES:
CEOP

      GAD_VALID_ADVSCHEME = .FALSE.
      IF ( advScheme.GE.1 .AND. advScheme.LE.GAD_Scheme_MaxNum ) THEN
        GAD_VALID_ADVSCHEME =
     &       advScheme .EQ. ENUM_UPWIND_1RST
     &  .OR. advScheme .EQ. ENUM_CENTERED_2ND
     &  .OR. advScheme .EQ. ENUM_UPWIND_3RD
     &  .OR. advScheme .EQ. ENUM_CENTERED_4TH
     &  .OR. advScheme .EQ. ENUM_DST2
     &  .OR. advScheme .EQ. ENUM_FLUX_LIMIT
     &  .OR. advScheme .EQ. ENUM_DST3
     &  .OR. advScheme .EQ. ENUM_DST3_FLUX_LIMIT
     &  .OR. advScheme .EQ. ENUM_OS7MP
     &  .OR. advScheme .EQ. ENUM_SOM_PRATHER
     &  .OR. advScheme .EQ. ENUM_SOM_LIMITER
      ENDIF

      RETURN
      END
