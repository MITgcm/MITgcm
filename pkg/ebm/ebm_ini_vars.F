C $Header: /u/gcmpack/MITgcm/pkg/ebm/ebm_ini_vars.F,v 1.2 2004/05/19 20:38:53 heimbach Exp $
C $Name:  $

#include "EBM_OPTIONS.h"

      SUBROUTINE EBM_INI_VARS( myThid )
C     |==========================================================|
C     | S/R INITIALISE VARIABLES FOR EBM                         |
C     |==========================================================|

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"
#ifdef ALLOW_EBM
# include "EBM.h"
#endif

C     === Routine arguments ===
C     myThid - Instance number for this innvocation 
      INTEGER myThid

CEndOfInterface
C     == Local variables ==

#ifdef ALLOW_EBM

C     I, J, K - Loop counters
      INTEGER bi,bj,i,j,k
      INTEGER iMin, iMax, jMin, jMax

C--   initially all forcing fields are zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          Run   (i,j,bi,bj) = 0.
          winPert(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _EXCH_XY_R4(Run,   myThid )
      _EXCH_XY_R4(winPert,  myThid )

      CALL EBM_LOAD_CLIMATOLOGY( myThid )

      CALL EBM_DRIVER( startTime, niter0, myThid )

      print *, TmlS - 273.15, TtS - 273.15
      print *, TmlN - 273.15, TtN - 273.15

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        iMin =   1-olx
        iMax = sNx+olx
        jMin =   1-oly
        jMax = sNy+oly

       CALL EBM_FORCING_SURF(
     I       bi, bj, iMin, iMax, jMin, jMax,
     I       startTime, niter0, myThid )
        ENDDO
       ENDDO

#endif /* ALLOW_EBM */

      RETURN
      END



