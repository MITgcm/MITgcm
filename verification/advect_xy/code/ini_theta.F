#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_THETA
C     !INTERFACE:
      SUBROUTINE INI_THETA( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_THETA
C     | o Set model initial temperature field.
C     *==========================================================*
C     | There are several options for setting the initial
C     | temperature file
C     |  1. Inline code
C     |  2. Vertical profile ( uniform T in X and Y )
C     |  3. Three-dimensional data from a file. For example from
C     |     Levitus or from a checkpoint file from a previous
C     |     integration.
C     | In addition to setting the temperature field we also
C     | set the initial temperature tendency term here.
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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Number of this instance of INI_THETA
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  :: Tile indices
C     i,j,k  :: Loop counters
C     rD     :: Radial displacement of point i,j
      INTEGER bi, bj
      INTEGER i, j, k, localWarnings
      _RL     Tfreezing
      _RL     rD
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C--   Initialise temperature field to the vertical reference profile
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           theta(i,j,k,bi,bj) = tRef(k)
          ENDDO
         ENDDO
C=== Set-up specific modification starts here ===
C--    Initialise temperature field to a circular patch.
         DO j=1,sNy
          DO i=1,sNx
            rD = SQRT( ( xC(i,j,bi,bj) - 40. _d 3 )**2
     &                +( yC(i,j,bi,bj) - 40. _d 3 )**2
     &                +( rC(k) + 50. _d 3 )**2
     &               )
            theta(i,j,k,bi,bj) = EXP( -0.5 _d 0*( rD/20. _d 3 )**2 )
          ENDDO
         ENDDO
C=== Set-up specific modification ends here  ===
        ENDDO
       ENDDO
      ENDDO
C=== Set-up specific modification starts here ===
      _EXCH_XYZ_RL( theta, myThid )
C=== Set-up specific modification ends here  ===

      IF ( hydrogThetaFile .NE. ' ' ) THEN
        CALL READ_FLD_XYZ_RL( hydrogThetaFile, ' ', theta, 0, myThid )
        _EXCH_XYZ_RL(theta,myThid)
      ENDIF

C--   Apply mask and test consistency
      localWarnings=0
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         IF ( maskIniTemp ) THEN
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            IF (maskC(i,j,k,bi,bj).EQ.0.) theta(i,j,k,bi,bj) = 0.
           ENDDO
          ENDDO
         ENDIF
         IF ( tRef(k).NE.0. ) THEN
          DO j=1,sNy
           DO i=1,sNx
            IF (  maskC(i,j,k,bi,bj).NE.0.
     &      .AND. theta(i,j,k,bi,bj).EQ.0. ) THEN
              localWarnings=localWarnings+1
            ENDIF
           ENDDO
          ENDDO
         ENDIF
        ENDDO
       ENDDO
      ENDDO
      IF (localWarnings.NE.0) THEN
       IF ( checkIniTemp ) THEN
        WRITE(msgBuf,'(A,I10,A)')
     &   ' INI_THETA: found', localWarnings,
     &   ' wet grid-pts with theta=0 identically.'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A,A)')
     &  ' If this is intentional, you need to',
     &  ' set checkIniTemp=.false. in "data", namelist PARM05'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R INI_THETA'
       ELSE
        WRITE(msgBuf,'(A,I10,A)')
     &   '** WARNINGS ** INI_THETA: found', localWarnings,
     &   ' wet grid-pts with theta=0 identically.'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDIF

C--   Check that there are no values of temperature below freezing point.
      Tfreezing=-1.9 _d 0
      IF ( allowFreezing ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            IF (theta(i,j,k,bi,bj) .LT. Tfreezing) THEN
               theta(i,j,k,bi,bj) = Tfreezing
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      IF (debugMode) THEN
        CALL PLOT_FIELD_XYZRL( theta, 'Initial Temperature',
     &                         Nr, 1, myThid )
      ENDIF

      RETURN
      END
