C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_gw.F,v 1.1 2001/03/12 20:38:36 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_GW ( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_GW                                        |
C     | o Set model initial gW tendency terms                    |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GW.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_GW 
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_NONHYDROSTATIC

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I,  J, K

C     Set initial tendency terms
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           gW   (I,J,K,bi,bj) = 0. _d 0
           gWnm1(I,J,K,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
