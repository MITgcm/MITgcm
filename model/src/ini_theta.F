C $Header: /u/gcmpack/MITgcm/model/src/ini_theta.F,v 1.30 2014/08/26 06:15:06 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
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
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Number of this instance of INI_THETA
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  :: Tile indices
C     i,j,k  :: Loop counters
      INTEGER bi, bj
      INTEGER i, j, k, localWarnings
      _RL     Tfreezing
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C--   Initialise temperature field to the vertical reference profile
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           theta(i,j,k,bi,bj) = tRef(k)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( hydrogThetaFile .NE. ' ' ) THEN
#ifdef ALLOW_MNC
        IF ( useMNC.AND.mnc_read_theta ) THEN
          CALL MNC_FILE_CLOSE_ALL_MATCHING(hydrogThetaFile, myThid)
          CALL MNC_CW_SET_UDIM(hydrogThetaFile, 1, myThid)
          CALL MNC_CW_SET_CITER(hydrogThetaFile, 2, -1, -1, -1, myThid)
          CALL MNC_CW_SET_UDIM(hydrogThetaFile, 1, myThid)
          CALL MNC_CW_RL_R('D',hydrogThetaFile,0,0,'Temp',theta,myThid)
          CALL MNC_FILE_CLOSE_ALL_MATCHING(hydrogThetaFile, myThid)
        ELSE
#endif /*  ALLOW_MNC  */
          CALL READ_FLD_XYZ_RL( hydrogThetaFile, ' ', theta, 0, myThid )
#ifdef ALLOW_MNC
        ENDIF
#endif /*  ALLOW_MNC  */
        _EXCH_XYZ_RL(theta,myThid)
      ENDIF

C--   Apply mask and test consistency
      localWarnings=0
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         IF ( maskIniTemp ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
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
     &  ' set checkIniTemp=.FALSE. in "data", namelist PARM05'
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
      IF ( checkIniTemp .AND. allowFreezing ) THEN
       Tfreezing=-1.9 _d 0
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            IF (theta(i,j,k,bi,bj) .LT. Tfreezing) THEN
               theta(i,j,k,bi,bj) = Tfreezing
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
c     ELSEIF ( allowFreezing ) THEN
c      CALL FREEZE_SURFACE( startTime, nIter0, myThid )
      ENDIF

      IF ( debugLevel.GE.debLevC ) THEN
        CALL PLOT_FIELD_XYZRL( theta, 'Initial Temperature',
     &                         Nr, 1, myThid )
      ENDIF

      RETURN
      END
