C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_tr1.F,v 1.2 2001/09/26 18:09:16 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_TR1
C     !INTERFACE:
      SUBROUTINE INI_TR1( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_TR1                                        
C     | o Set initial tracer 1 distribution.                      
C     *==========================================================*
C     | Passive tracers 1-N can be initialised so as to           
C     | dye fluid.                                                
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "TR1.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_TR1
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     iC, jC - Center of domain
C     iD, jD - Disitance from domain center.
C     rad    - Radius of initial patch
C     rD     - Radial displacement of point I,J
C     iG, jG - Global coordinate index
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER iC, jC, iD, jD
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER I, J, K, localWarnings
      _RL     rad, rD
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      _BARRIER

C--   Initialise tracer to inline.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           IF ( K .EQ. 1 .AND. hFacC(I,J,K,bi,bj) .NE. 0 ) THEN
            tr1(I,J,K,bi,bj) = 1. _d 0
           ELSE
            tr1(I,J,K,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     Set initial tendency terms
      localWarnings=0
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           gtr1   (I,J,K,bi,bj) = 0. _d 0
           gtr1NM1(I,J,K,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
      _EXCH_XYZ_R8(tr1 , myThid )
      _EXCH_XYZ_R8(gtr1 , myThid )
      _EXCH_XYZ_R8(gtr1NM1 , myThid )

      CALL PLOT_FIELD_XYZRL( tr1, 'Initial TR1' , 
     &                       Nr, 1, myThid )

      RETURN
      END
