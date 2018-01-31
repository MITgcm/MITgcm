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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_THETA
      INTEGER myThid

C     == Functions ==
c     real*8  PORT_RAND
c     real*8  seed

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER I, J, K, localWarnings
      _RL     term1,term2,thetaLim,thetaEq
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      J = 99+myBxLo(myThid)+nPx*myByLo(myThid)
c     CALL SRAND( J )
c     seed = j

      IF ( hydrogThetaFile .EQ. ' ' ) THEN
C--    Initialise temperature field to Held & Saurez equilibrium theta
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K=1,Nr
          thetaLim = 200. _d 0/((rC(K)/atm_po)**atm_kappa)
          DO J=1,sNy
           DO I=1,sNx
            term1=60. _d 0*(sin(yC(I,J,bi,bj)*deg2rad)**2)
            term2=10. _d 0*log((rC(K)/atm_po))
     &              *(cos(yC(I,J,bi,bj)*deg2rad)**2)
            thetaEq=315. _d 0-term1-term2
            theta(I,J,K,bi,bj) = MAX( thetaLim, thetaEq )
c    &                          + 0.01*(RAND()-0.5)
c    &                          + 0.01*(PORT_RAND(seed)-0.5)
c           theta(I,J,K,bi,bj) = tRef(K)
           ENDDO
          ENDDO
         ENDDO
#ifdef ALLOW_ZONAL_FILT
C--   Zonal FFT filter initial conditions
         IF (useZONAL_FILT) THEN
          CALL ZONAL_FILTER(
     U                       theta(1-OLx,1-OLy,1,bi,bj),
     I                       hFacC(1-OLx,1-OLy,1,bi,bj),
     I                       1, sNy, Nr, bi, bj, 1, myThid )
         ENDIF
#endif /* ALLOW_ZONAL_FILT */
        ENDDO
       ENDDO
      ELSE
       CALL READ_FLD_XYZ_RL( hydrogThetaFile, ' ', theta, 0, myThid )
      ENDIF
C--   Apply mask and test consistency
      localWarnings=0
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           IF (maskC(I,J,K,bi,bj).EQ.0.) theta(I,J,K,bi,bj) = 0.
          ENDDO
         ENDDO
         IF ( tRef(k).NE.0. ) THEN
          DO J=1,sNy
           DO I=1,sNx
            IF (  maskC(I,J,K,bi,bj).NE.0.
     &      .AND. theta(I,J,K,bi,bj).EQ.0. ) THEN
              localWarnings=localWarnings+1
            ENDIF
           ENDDO
          ENDDO
         ENDIF
        ENDDO
       ENDDO
      ENDDO
      IF (localWarnings.NE.0) THEN
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_THETA: theta = 0 identically. If this is intentional',
     &  'you will need to edit ini_theta.F to avoid this safety check'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_THETA'
      ENDIF

      _EXCH_XYZ_RL(theta , myThid )

      IF (debugMode) THEN
        CALL PLOT_FIELD_XYZRL( theta, 'Initial Temperature' ,
     &                         Nr, 1, myThid )
      ENDIF

      RETURN
      END
