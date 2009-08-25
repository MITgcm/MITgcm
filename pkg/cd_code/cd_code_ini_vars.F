C $Header: /u/gcmpack/MITgcm/pkg/cd_code/cd_code_ini_vars.F,v 1.9 2009/08/25 21:33:27 jmc Exp $
C $Name:  $

#include "CD_CODE_OPTIONS.h"

      SUBROUTINE CD_CODE_INI_VARS( myThid )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CD_CODE_VARS.h"

C     == Routine arguments ==
C     myThid :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER I, J, K
CEOP

#ifdef ALLOW_CD_CODE

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-      3D arrays
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           uNM1(I,J,K,bi,bj)=0. _d 0
           vNM1(I,J,K,bi,bj)=0. _d 0
           uVeld(I,J,K,bi,bj)=0. _d 0
           vVeld(I,J,K,bi,bj)=0. _d 0
          ENDDO
         ENDDO
        ENDDO

C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          etaNm1(I,J,bi,bj)=0. _d 0
         ENDDO
        ENDDO

       ENDDO
      ENDDO

      IF ( nIter0.NE.0 .OR. pickupSuff.NE.' ' ) THEN
        CALL CD_CODE_READ_PICKUP( nIter0, myThid )
      ENDIF

#endif /*  ALLOW_CD_CODE  */

      RETURN
      END
