C $Header: /u/gcmpack/MITgcm/pkg/ebm/ebm_ini_vars.F,v 1.4 2011/08/28 21:47:10 jmc Exp $
C $Name:  $

#include "EBM_OPTIONS.h"

CBOP 0
C !ROUTINE: EBM_INI_VARS

C !INTERFACE:
      SUBROUTINE EBM_INI_VARS( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R INITIALISE VARIABLES FOR EBM
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "DYNVARS.h"
c#include "GRID.h"
c#include "FFIELDS.h"
#ifdef ALLOW_EBM
# include "EBM.h"
#endif

C     !INPUT PARAMETERS:
C     myThid  :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_EBM
C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j, k :: Loop counters
      INTEGER bi,bj,i,j

C--   initially all forcing fields are zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          Run    (i,j,bi,bj) = 0.
          winPert(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EBM_LOAD_CLIMATOLOGY( myThid )

c     CALL EBM_DRIVER( startTime, niter0, myThid )

c     print *, TmlS - 273.15, TtS - 273.15
c     print *, TmlN - 273.15, TtN - 273.15

#endif /* ALLOW_EBM */

      RETURN
      END
