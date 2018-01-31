#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_SALT
C     !INTERFACE:
      SUBROUTINE INI_SALT( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_SALT
C     | o Set model initial salinity field.
C     *==========================================================*
C     | There are several options for setting the initial
C     | temperature file
C     |  1. Inline code
C     |  2. Vertical profile ( uniform S in X and Y )
C     |  3. Three-dimensional data from a file. For example from
C     |     Levitus or from a checkpoint file from a previous
C     |     integration.
C     | In addition to setting the salinity field we also
C     | set the initial salinity tendency term here.
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
C     myThid :: Number of this instance of INI_SALT
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  :: Tile indices
C     i,j,k  :: Loop counters
C     rD     :: Radial displacement of point i,j
      INTEGER bi, bj
      INTEGER i, j, k, localWarnings
      _RL     rD
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C--   Initialise salinity field to the vertical reference profile
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           salt(i,j,k,bi,bj) = sRef(k)
          ENDDO
         ENDDO
C=== Set-up specific modification starts here ===
C--    Initialise salinity field to a circular patch.
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            rD = SQRT( ( xC(i,j,bi,bj) - 40. _d 3 )**2
     &                +( yC(i,j,bi,bj) - 40. _d 3 )**2
     &                +( rC(k) + 50. _d 3 )**2
     &               )
            IF ( rD.LE.60. _d 3 ) salt(i,j,k,bi,bj) = sRef(k)+1. _d 0
          ENDDO
         ENDDO
C=== Set-up specific modification ends here  ===
        ENDDO
       ENDDO
      ENDDO
C=== Set-up specific modification starts here ===
      _EXCH_XYZ_RL( salt, myThid )
C=== Set-up specific modification ends here  ===

      IF ( hydrogSaltFile .NE. ' ' ) THEN
        CALL READ_FLD_XYZ_RL( hydrogSaltFile, ' ', salt, 0, myThid )
        _EXCH_XYZ_RL( salt, myThid )
      ENDIF

C--   Apply mask and test consistency
      localWarnings=0
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         IF ( maskIniSalt ) THEN
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            IF (maskC(i,j,k,bi,bj).EQ.0.) salt(i,j,k,bi,bj) = 0.
           ENDDO
          ENDDO
         ENDIF
         IF ( sRef(k).NE.0. ) THEN
          DO j=1,sNy
           DO i=1,sNx
            IF ( maskC(i,j,k,bi,bj).NE.0.
     &      .AND. salt(i,j,k,bi,bj).EQ.0. ) THEN
              localWarnings=localWarnings+1
            ENDIF
           ENDDO
          ENDDO
         ENDIF
        ENDDO
       ENDDO
      ENDDO
      IF ( localWarnings.NE.0 ) THEN
       IF ( checkIniSalt ) THEN
        WRITE(msgBuf,'(A,I10,A)')
     &   ' INI_SALT: found', localWarnings,
     &   ' wet grid-pts with salt=0 identically.'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A,A)')
     &  ' If this is intentional, you need to',
     &  ' set checkIniSalt=.false. in "data", namelist PARM05'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R INI_SALT'
       ELSE
        WRITE(msgBuf,'(A,I10,A)')
     &   '** WARNINGS ** INI_SALT: found', localWarnings,
     &   ' wet grid-pts with salt=0 identically.'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDIF

      IF ( debugLevel.GE.debLevC ) THEN
        CALL PLOT_FIELD_XYZRL( salt, 'Initial Salinity',
     &                         Nr, 1, myThid )
      ENDIF

      RETURN
      END
