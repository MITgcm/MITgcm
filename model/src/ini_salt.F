C $Header: /u/gcmpack/MITgcm/model/src/ini_salt.F,v 1.12 2001/02/04 14:38:47 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_SALT ( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_SALT                                      |
C     | o Set model initial salinity field.                      |
C     |==========================================================|
C     | There are several options for setting the initial        |
C     | temperature file                                         |
C     |  1. Inline code                                          |
C     |  2. Vertical profile ( uniform S in X and Y )            |
C     |  3. Three-dimensional data from a file. For example from |
C     |     Levitus or from a checkpoint file from a previous    |
C     |     integration.                                         |
C     | In addition to setting the salinity field we also        |
C     | set the initial salinity tendency term here.             |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_SALT 
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I,  J, K

      _BARRIER

      IF ( hydrogSaltFile .EQ. ' ' ) THEN
C--    Initialise salinity field to the vertical reference profile
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
           DO I=1,sNx
            salt(I,J,K,bi,bj) = sRef(K)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XYZ_RL( hydrogSaltFile, ' ', salt, 0, myThid )
       _END_MASTER(myThid)
      ENDIF
C     Set initial tendency terms
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           gs   (I,J,K,bi,bj) = 0. _d 0
           gsNM1(I,J,K,bi,bj) = 0. _d 0
           IF (hFacC(I,J,K,bi,bj).EQ.0) salt(I,J,K,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
      _EXCH_XYZ_R8(salt  , myThid )
      _EXCH_XYZ_R8(gs , myThid )
      _EXCH_XYZ_R8(gsNM1 , myThid )

      CALL PLOT_FIELD_XYZRL( salt, 'Initial Salinity' , Nr, 1, myThid )

      RETURN
      END
