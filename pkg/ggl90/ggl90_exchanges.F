C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_exchanges.F,v 1.2 2009/04/28 18:15:33 jmc Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

CBOP
C !ROUTINE: GGL90_EXCHANGES

C !INTERFACE: ==========================================================
      SUBROUTINE GGL90_EXCHANGES( myThid )

C !DESCRIPTION:
C     Exchange data to update overlaps for GGL90TKE

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GGL90.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_GGL90
C !LOCAL VARIABLES: ====================================================
CEOP

#ifdef ALLOW_GGL90_HORIZDIFF
      IF (GGL90isON .AND. GGL90diffTKEh .GT. 0. _d 0) THEN
C Exchange overlaps
       _EXCH_XYZ_RL(GGL90TKE,myThid)
      ENDIF
#endif /* ALLOW_GGL90_HORIZDIFF */
#endif /* ALLOW_GGL90 */

      RETURN
      END
