C $Header: /u/gcmpack/MITgcm/model/src/tracers_iigw_correction.F,v 1.2 2006/06/07 01:55:13 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TRACERS_CORRECTION_IIGW
C     !INTERFACE:
      SUBROUTINE TRACERS_IIGW_CORRECTION(
     I                   bi, bj, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R TRACERS_IIGW_CORRECTION
C     | o apply correction term to Tracers due to Implicit
C     |   treatment of Internal Gravity Waves:
C-    |    -DeltaT.(w^{n+1}-w^{n})*d.Tr_{ref}/dz
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi,bj   :: tile indices
C     myTime  :: current time in simulation
C     myIter  :: current iteration number in simulation
C     myThid  :: my Thread Id number
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC
C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j,k           :: Loop counters
      INTEGER i,j,k
      INTEGER kp1
      _RL     dTr_k, dTrp1
      _RL     dW_k(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL     dWp1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
CEOP

C-    Initialise dW = w^{n+1} - w^{n} to zero:
      DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          dWp1(i,j) = 0. _d 0
        ENDDO
      ENDDO

C--   Start vertical loop
      DO k=1,Nr

C-    Compute dW @ interface k & k+1
       kp1 = MIN(k+1,Nr)
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          dW_k(i,j) = dWp1(i,j)
          dWp1(i,j) = ( wVel(i,j,kp1,bi,bj)
     &                  - gW(i,j,kp1,bi,bj) )*maskC(i,j,k,bi,bj)
        ENDDO
       ENDDO

C-     Add Impl.IGW correction to Pot.Temperature:
       dTr_k = 0. _d 0
       IF ( k.GT.1 ) dTr_k = (tRef(k) - tRef(k-1))*rkSign
       dTrp1 = (tRef(kp1) - tRef(k))*rkSign
       IF ( tempAdvection .AND.
     &     (dTr_k.NE.0. _d 0 .OR. dTrp1.NE.0. _d 0) ) THEN
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
          theta(i,j,k,bi,bj) = theta(i,j,k,bi,bj)
     &       -dTtracerLev(k)*0.5 _d 0
     &                      *( dTr_k*dW_k(i,j) + dTrp1*dWp1(i,j) )
     &                      *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
          ENDDO
         ENDDO
       ENDIF

C-     Add Impl.IGW correction to Salinity/Water vapor:
       dTr_k = 0. _d 0
       IF ( k.GT.1 ) dTr_k = (sRef(k) - sRef(k-1))*rkSign
       dTrp1 = (sRef(kp1) - sRef(k))*rkSign
       IF ( saltAdvection .AND.
     &     (dTr_k.NE.0. _d 0 .OR. dTrp1.NE.0. _d 0) ) THEN
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            salt(i,j,k,bi,bj) = salt(i,j,k,bi,bj)
     &       -dTtracerLev(k)*0.5 _d 0
     &                      *( dTr_k*dW_k(i,j) + dTrp1*dWp1(i,j) )
     &                      *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
          ENDDO
         ENDDO
       ENDIF

C-    End of k loop
      ENDDO

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
