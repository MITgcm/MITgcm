C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_hfac.F,v 1.1 2006/03/22 22:48:13 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_HFAC
C     !INTERFACE:
      SUBROUTINE INI_HFAC( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_HFAC                                  
C     | o Initialise hfacs and masks to zero
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_HFAC
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I, J, K
CEOP

C 
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           hFacC(I,J,K,bi,bj) = 0. _d 0
           hFacS(I,J,K,bi,bj) = 0. _d 0
           hFacW(I,J,K,bi,bj) = 0. _d 0
           maskC(I,J,K,bi,bj) = 0. _d 0
           maskS(I,J,K,bi,bj) = 0. _d 0
           maskW(I,J,K,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
C - end bi,bj loops.
       ENDDO
      ENDDO

      RETURN
      END
