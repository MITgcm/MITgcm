C $Header: /u/gcmpack/MITgcm/model/src/ini_dynvars.F,v 1.6 2001/11/08 20:57:51 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_DYNVARS
C     !INTERFACE:
      SUBROUTINE INI_DYNVARS( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_DYNVARS                                    
C     | o Initialise to zero all DYNVARS.h arrays                  
C     *==========================================================*
C     | Sets all the State variables to zero.
C     | Sets all the Gs (arrays used for multi-level time-stepping)
C     | to zero.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "GW.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_UVEL 
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I,  J, K
CEOP

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-      3D arrays
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx

           uVel(I,J,K,bi,bj)=0. _d 0
           vVel(I,J,K,bi,bj)=0. _d 0
           wVel(I,J,K,bi,bj)=0. _d 0
           theta(I,J,K,bi,bj)=0. _d 0
           salt(I,J,K,bi,bj)=0. _d 0

           gU(I,J,K,bi,bj)=0. _d 0
           gV(I,J,K,bi,bj)=0. _d 0
           gT(I,J,K,bi,bj)=0. _d 0
           gS(I,J,K,bi,bj)=0. _d 0
           gUnm1(I,J,K,bi,bj)=0. _d 0
           gVnm1(I,J,K,bi,bj)=0. _d 0
           gTnm1(I,J,K,bi,bj)=0. _d 0
           gSnm1(I,J,K,bi,bj)=0. _d 0

#ifdef INCLUDE_CD_CODE
           guCD(I,J,K,bi,bj)=0. _d 0
           gvCD(I,J,K,bi,bj)=0. _d 0
           uNM1(I,J,K,bi,bj)=0. _d 0
           vNM1(I,J,K,bi,bj)=0. _d 0
           uVeld(I,J,K,bi,bj)=0. _d 0
           vVeld(I,J,K,bi,bj)=0. _d 0
#endif

#ifdef ALLOW_NONHYDROSTATIC
           gW(I,J,K,bi,bj)=0. _d 0
           gWnm1(I,J,K,bi,bj)=0. _d 0
#endif

          ENDDO
         ENDDO
        ENDDO

C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          etaN(I,J,bi,bj)=0. _d 0
          etaH(I,J,bi,bj)=0. _d 0
#ifdef INCLUDE_CD_CODE
          etaNm1(I,J,bi,bj)=0. _d 0
#endif
         ENDDO
        ENDDO

       ENDDO
      ENDDO

      RETURN
      END
