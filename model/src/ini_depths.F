C $Header: /u/gcmpack/MITgcm/model/src/ini_depths.F,v 1.10 1998/07/15 22:20:02 adcroft Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_DEPTHS( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_DEPTHS                                    |
C     | o Initialise map of model depths                         |
C     |==========================================================|
C     | The depths of the bottom of the model is specified in    |
C     | terms of an XY map with one depth for each column of     |
C     | grid cells. Depths do not have to coincide with the      |
C     | model levels. The model's lopping algorithm makes it     |
C     | possible to represent arbitrary depths.                  |
C     | The mode depths map also influences the models topology  |
C     | By default the model domain wraps around in X and Y.     |
C     | This default doubly periodic topology is "supressed"     |
C     | if a depth map is defined which closes off all wrap      |
C     | around flow.                                             |
C     \==========================================================/

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_DEPTHS
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     iG, jG - Global coordinate index
C     bi,bj  - Loop counters
C     I,J,K
C     phi    - total depth of model
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER  I,  J, K
      _RL phi

      _BARRIER
      IF ( bathyFile .EQ. ' '  ) THEN
C      Set up a flat bottom box with doubly periodic topology.
C      H is the basic variable from which other terms are derived. It
C      is the term that would be set from an external file for a 
C      realistic problem.
       phi = zFace(nZ+1)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           iG = myXGlobalLo-1+(bi-1)*sNx+I
           jG = myYGlobalLo-1+(bj-1)*sNy+J
C          Default depth of full domain
           H(i,j,bi,bj) = phi
C          Test for eastern edge
           IF ( iG .EQ. nX ) H(i,j,bi,bj) = 0.
C          Test for northern edge
           IF ( jG .EQ. nY ) H(i,j,bi,bj) = 0.
C          Island
           IF ( iG .EQ. 1 .AND. 
     &          jG .EQ. 24 ) H(i,j,bi,bj) = 0.75*phi
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XY_RS( bathyFile, ' ', H, 0, myThid )
       _END_MASTER(myThid)
C--    Reverse sign if necessary (We assume H>0)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           H(i,j,bi,bj)=-H(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      _EXCH_XY_R4(    H, myThid )
C
      CALL PLOT_FIELD_XYRS( H, 'Model depths' , 1, myThid )
C
      RETURN
      END
