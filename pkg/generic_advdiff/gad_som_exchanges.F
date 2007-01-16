C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_som_exchanges.F,v 1.1 2007/01/16 04:38:34 jmc Exp $
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

#ifdef GAD_ALLOW_SOM_ADVECT
C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER n

C--   Apply exchanges to Temp. 2nd.O.Moments:
      IF ( tempSOM_Advection ) THEN
       DO n=1,nSOM
        CALL EXCH_3D_RL( som_T(1-OLx,1-OLy,1,1,1,n), Nr, myThid )
       ENDDO
      ENDIF

C--   Apply exchanges to Salin. 2nd.O.Moments:
      IF ( saltSOM_Advection ) THEN
       DO n=1,nSOM
        CALL EXCH_3D_RL( som_S(1-OLx,1-OLy,1,1,1,n), Nr, myThid )
       ENDDO
      ENDIF

#endif /* GAD_ALLOW_SOM_ADVECT */

      RETURN
      END
