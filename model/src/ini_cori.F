C $Header: /u/gcmpack/MITgcm/model/src/ini_cori.F,v 1.2 1998/04/24 02:05:41 cnh Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_CORI( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_CORI                                      |
C     | o Initialise coriolis term.                              |
C     \==========================================================/

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_CORI
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     iG, jG - Global coordinate index
C     bi,bj  - Loop counters
C     I,J,K
C     facGrid - Factor for grid to meter conversion
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER  I,  J, K
      _RL facGrid

C--   Initialise beta plane f.
      facGrid = 1. _d 0
      IF ( usingSphericalPolarGrid ) facGrid = deg2rad*rSphere
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nz
         DO J=1,sNy
          DO I=1,sNx
           fCori(i,j,bi,bj)=f0+beta*yC(i,j,bi,bj)*facGrid
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
      _EXCH_XY_R4(fCori , myThid )

      RETURN
      END
