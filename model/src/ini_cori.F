C $Header: /u/gcmpack/MITgcm/model/src/ini_cori.F,v 1.8 2000/03/27 22:25:44 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_CORI( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_CORI                                      |
C     | o Initialise coriolis term.                              |
C     \==========================================================/
      IMPLICIT NONE

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
C     bi,bj  - Loop counters
C     I,J,K
C     facGrid - Factor for grid to meter conversion
      INTEGER bi, bj
      INTEGER  I,  J, K
      _RL facGrid

C--   Initialise coriolis parameter
      IF     ( useConstantF ) THEN
C      o Constant F case
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
           DO I=1,sNx
            fCori(i,j,bi,bj)=f0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSEIF ( useBetaPlaneF ) THEN
C      o Beta plane case
       facGrid = 1. _d 0
       IF ( usingSphericalPolarGrid ) facGrid = deg2rad*rSphere
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
           DO I=1,sNx
            fCori(i,j,bi,bj)=f0+beta*_yC(i,j,bi,bj)*facGrid
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSEIF ( useSphereF ) THEN
C      o Spherical case
C        Note in this case we assume yC is in degrees.
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
           DO I=1,sNx
            fCori(i,j,bi,bj)=
     &       2. _d 0*omega*sin(_yC(i,j,bi,bj)*deg2rad)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
C      o Special custom form
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
           DO I=1,sNx
            fCori(i,j,bi,bj)=0.
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
C
      _EXCH_XY_R4(fCori , myThid )

      RETURN
      END
