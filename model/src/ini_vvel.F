C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_vvel.F,v 1.5 1998/08/22 17:51:09 cnh Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_VVEL( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_VVEL                                      |
C     | o Set model initial meridional velocity field.           |
C     |==========================================================|
C     | There are several options for setting the initial        |
C     | meridional velocity field.                               |
C     |  1. Inline code                                          |
C     |  2. Three-dimensional data from a file. For example from |
C     |     a checkpoint file from a previous integration.       |
C     | In addition to setting the velocity field we also        |
C     | set the initial velocity tendency term here.             |
C     \==========================================================/

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_VVEL 
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER iC, jC, iD, jD
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER  I,  J, K
      REAL rad, rD

C--   Initialise velocity field to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           vVel (I,J,K,bi,bj) = 0. _d 0
#ifdef ALLOW_CD
           vVeld(I,J,K,bi,bj) = 0. _d 0
           vNM1 (I,J,K,bi,bj) = 0. _d 0
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     Set initial tendency terms
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           gv   (I,J,K,bi,bj) = 0. _d 0
           gvNM1(I,J,K,bi,bj) = 0. _d 0
#ifdef ALLOW_CD
           gvCD (I,J,K,bi,bj) = 0. _d 0
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
      _EXCH_XYZ_R8(vVel  , myThid )
      _EXCH_XYZ_R8(gv    , myThid )
      _EXCH_XYZ_R8(gvNM1 , myThid )
#ifdef ALLOW_CD
      _EXCH_XYZ_R8(vVeld , myThid )
      _EXCH_XYZ_R8(vNM1  , myThid )
      _EXCH_XYZ_R8(gvCD  , myThid )
#endif
      RETURN
      END
