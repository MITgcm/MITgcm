C $Header: /u/gcmpack/MITgcm/model/src/ini_depths.F,v 1.22 2001/02/04 14:38:47 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_DEPTHS( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_DEPTHS                                    |
C     | o Initialise map of model depths                         |
C     |==========================================================|
C     | The depths of the bottom of the model is specified in    |
C     | terms of an XY map with one depth for each column of     |
C     | grid cells. Depths do not have to coincide with the      |
C     | model levels. The model lopping algorithm makes it       |
C     | possible to represent arbitrary depths.                  |
C     | The mode depths map also influences the models topology  |
C     | By default the model domain wraps around in X and Y.     |
C     | This default doubly periodic topology is "supressed"     |
C     | if a depth map is defined which closes off all wrap      |
C     | around flow.                                             |
C     \==========================================================/
      IMPLICIT NONE

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
C     Hdefault - default r-coordinate of the lower boundary (=ground)
C                (=minus(Total_depth) in the ocean model)
C                (=Total Pressure at Sea Level in the atmos model)
C     oldPrec - Temporary used in controlling binary input dataset precision
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER  I, J, K
      _RL Hdefault

      _BARRIER
      IF ( bathyFile .EQ. ' '  ) THEN
C      Set up a flat bottom box with doubly periodic topology.
C      H is the basic variable from which other terms are derived. It
C      is the term that would be set from an external file for a 
C      realistic problem.
       Hdefault = rF(1)
       IF (.NOT. groundAtK1) THEN
        DO K = 1, Nr
         Hdefault = Hdefault - rkfac*drF(K)
        ENDDO
       ENDIF
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           iG = myXGlobalLo-1+(bi-1)*sNx+I
           jG = myYGlobalLo-1+(bj-1)*sNy+J
C          Default depth of full domain
           H(i,j,bi,bj) = Hdefault
C          Test for eastern edge
C          IF ( iG .EQ. nX ) H(i,j,bi,bj) = 0.
C          Test for northern edge
C          IF ( jG .EQ. nY ) H(i,j,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       _BEGIN_MASTER( myThid )
C Read the bathymetry using the mid-level I/O pacakage read_write_rec
C The 0 is the "iteration" argument. The 1 is the record number.
       CALL READ_REC_XY_RS( bathyFile, H, 1, 0, myThid )
C Read the bathymetry using the mid-level I/O pacakage read_write_fld
C The 0 is the "iteration" argument. The ' ' is an empty suffix
C      CALL READ_FLD_XY_RS( bathyFile, ' ', H, 0, myThid )
C Read the bathymetry using the low-level I/O package
C      CALL MDSREADFIELD( bathyFile, readBinaryPrec,
C    &                    'RS', 1, H, 1, myThid )
       _END_MASTER(myThid)
      ENDIF

      _EXCH_XY_R4(    H, myThid )

      IF (usingSphericalPolarGrid) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           IF (abs(yC(I,J,bi,bj)).GE.90.) H(I,J,bi,bj)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
C
      CALL PLOT_FIELD_XYRS(H,'Model depths (ini_depths)',1,myThid)
C
      RETURN
      END
