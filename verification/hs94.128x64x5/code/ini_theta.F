C $Header: /u/gcmpack/MITgcm/verification/hs94.128x64x5/code/Attic/ini_theta.F,v 1.3 2001/02/04 14:38:52 cnh Exp $
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
      _RL     thKappa

      _BARRIER

      J = 99+myBxLo(myThid)+nPx*myByLo(myThid)
c     CALL SRAND( J )

      IF ( hydrogThetaFile .EQ. ' ' ) THEN
C--    Initialise temperature field to Held & Saurez equilibrium theta
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K=1,Nr
          Ro_SeaLevel=1.E5
          thKappa = 2./7.
          thetaLim = 200. / ((rC(K)/Ro_SeaLevel)**thKappa)
          DO J=1,sNy
           term1=60.*(sin(yC(1,J,bi,bj)*deg2rad)**2)
           term2=10.*log((rC(K)/Ro_SeaLevel))
     &              *(cos(yC(1,J,bi,bj)*deg2rad)**2)
           thetaEq=315.-term1-term2
           DO I=1,sNx
            theta(I,J,K,bi,bj) = MAX( thetaLim, thetaEq )
c    &                          + 0.01*(RAND()-0.5)
     &                          + 0.01*(PORT_RAND()-0.5)
C           theta(I,J,K,bi,bj) = tRef(K)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
#ifdef ALLOW_ZONAL_FILT
C--   Zonal FFT filter initial conditions
           CALL ZONAL_FILTER(
     U      theta, hFacC,
     I      1, sNy, k, k, bi, bj, 1, myThid)
#endif /* INCLUDE_LAT_CIRC_FFT_FILTER_CODE */
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
