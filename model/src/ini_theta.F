C $Header: /u/gcmpack/MITgcm/model/src/ini_theta.F,v 1.7 1998/08/22 17:51:08 cnh Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_THETA( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_THETA                                     |
C     | o Set model initial temperature field.                   |
C     |==========================================================|
C     | There are several options for setting the initial        |
C     | temperature file                                         |
C     |  1. Inline code                                          |
C     |  2. Vertical profile ( uniform T in X and Y )            |
C     |  3. Three-dimensional data from a file. For example from |
C     |     Levitus or from a checkpoint file from a previous    |
C     |     integration.                                         |
C     | In addition to setting the temperature field we also     |
C     | set the initial temperature tendency term here.          |
C     \==========================================================/

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_THETA
      INTEGER myThid
CEndOfInterface

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
      INTEGER  I,  J, K
      REAL rad, rD

      _BARRIER

      IF ( hydrogThetaFile .EQ. ' ' ) THEN
C--    Example 1
C--    Initialise temperature field to a circular patch.
       iC  = Nx/2
       jC  = Ny/2
       rad = MIN(Ny/8,Nx/8)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
           DO I=1,sNx
            theta(I,J,K,bi,bj) = 0. _d 0
            iG = myXGlobalLo-1+(bi-1)*sNx+I
            jG = myYGlobalLo-1+(bj-1)*sNy+J
            iD = iG-iC
            jD = jG-jC
            rD = SQRT(FLOAT(iD*iD+jD*jD))
            IF ( rD .LT. rad ) theta(I,J,K,bi,bj) = 1. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C--    Example 2
C--    Initialise temperature field to the vertical reference profile
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
           DO I=1,sNx
            theta(I,J,K,bi,bj) = tRef(K)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XYZ_RL( hydrogThetaFile, ' ', theta, 0, myThid )
       _END_MASTER(myThid)
      ENDIF
C     Set initial tendency terms
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           gt   (I,J,K,bi,bj) = 0. _d 0
           gtNM1(I,J,K,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
      _EXCH_XYZ_R8(theta , myThid )
      _EXCH_XYZ_R8(gt , myThid )
      _EXCH_XYZ_R8(gtNM1 , myThid )

      CALL PLOT_FIELD_XYZRL( theta, 'Initial Temperature' , Nr, 1, myThid )

      RETURN
      END
