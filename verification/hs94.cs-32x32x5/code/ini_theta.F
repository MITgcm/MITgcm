C $Header: /u/gcmpack/MITgcm/verification/hs94.cs-32x32x5/code/ini_theta.F,v 1.4 2001/07/06 22:13:37 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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
      IMPLICIT NONE

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

C     == Functions ==
      Real*8  PORT_RAND

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER I, J, K
      _RL     term1,term2,thetaLim,thetaEq

      _BARRIER

      J = 99+myBxLo(myThid)+nPx*myByLo(myThid)
c     CALL SRAND( J )

      IF ( hydrogThetaFile .EQ. ' ' ) THEN
C--    Initialise temperature field to Held & Suarez equilibrium theta
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
c    &                          + 0.01*(PORT_RAND()-0.5)
c           theta(I,J,K,bi,bj) = tRef(K)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#ifdef ALLOW_ZONAL_FILT
C--   Zonal FFT filter initial conditions
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
           CALL ZONAL_FILTER(
     U      theta, hFacC,
     I      1, sNy, k, k, bi, bj, 1, myThid)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#endif /* INCLUDE_LAT_CIRC_FFT_FILTER_CODE */
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
           IF (hFacC(I,J,K,bi,bj).EQ.0.) theta(I,J,K,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C


      _EXCH_XYZ_R8(theta , myThid )
      _EXCH_XYZ_R8(gt , myThid )
      _EXCH_XYZ_R8(gtNM1 , myThid )

      CALL PLOT_FIELD_XYZRL( theta, 'Initial Temperature' , 
     &                       Nr, 1, myThid )

      RETURN
      END
