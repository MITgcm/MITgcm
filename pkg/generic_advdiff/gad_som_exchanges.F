C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_som_exchanges.F,v 1.3 2008/05/09 21:43:16 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_SOM_EXCHANGES
C     !INTERFACE:
      SUBROUTINE GAD_SOM_EXCHANGES( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GAD_SOM_EXCHANGES
C     | o Apply exchanges to update overlaps
C     |   for 2nd.Order Moment fields
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GAD.h"
#include "GAD_SOM_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef GAD_ALLOW_TS_SOM_ADV
C     !LOCAL VARIABLES:
C     === Local variables ===

C--   Apply exchanges to Temp. 2nd.O.Moments:
      IF ( tempSOM_Advection ) THEN
       CALL GAD_EXCH_SOM( som_T, Nr, myThid )
      ENDIF

C--   Apply exchanges to Salin. 2nd.O.Moments:
      IF ( saltSOM_Advection ) THEN
       CALL GAD_EXCH_SOM( som_S, Nr, myThid )
      ENDIF

#endif /* GAD_ALLOW_TS_SOM_ADV */

      RETURN
      END
