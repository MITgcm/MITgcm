C $Header: /u/gcmpack/MITgcm/model/src/integr_continuity.F,v 1.13 2005/10/17 15:29:54 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INTEGR_CONTINUITY
C     !INTERFACE:
      SUBROUTINE INTEGR_CONTINUITY(
     I                             bi, bj, uFld, vFld,
     I                             myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INTEGR_CONTINUITY                                 
C     | o Integrate the continuity Eq : compute vertical velocity 
C     |   and free surface "r-anomaly" (etaN) to satisfy  
C     |   exactly the convervation of the Total Volume            
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     uFld    :: Zonal velocity ( m/s )
C     vFld    :: Meridional velocity ( m/s )
C     bi,bj   :: tile index
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER bi,bj
      LOGICAL UpdateEtaN_EtaH
      _RL uFld(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL vFld(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy) 

C     !LOCAL VARIABLES:
C     Local variables in common block

C     Local variables
C     i,j,k  :: Loop counters
C     uTrans :: Volume transports ( uVel.xA )
C     vTrans :: Volume transports ( vVel.yA )
C     hDivFlow :: Div. Barotropic Flow [transport unit m3/s]
      INTEGER i,j,k
      _RL uTrans(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL vTrans(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL hDivFlow(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL wSurf, facEmP
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef EXACT_CONSERV
      IF (exactConserv) THEN

C--   Compute the Divergence of The Barotropic Flow :

C-    Initialise 
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
         hDivFlow(i,j)      = 0. _d 0
         utrans(i,j)        = 0. _d 0
         vtrans(i,j)        = 0. _d 0
       ENDDO
      ENDDO

      DO k=1,Nr
       
C-    Calculate velocity field "volume transports" through tracer cell faces
        DO j=1,sNy+1
         DO i=1,sNx+1
          uTrans(i,j) = uFld(i,j,k,bi,bj)*_dyG(i,j,bi,bj)
     &                 *drF(k)*_hFacW(i,j,k,bi,bj)
          vTrans(i,j) = vFld(i,j,k,bi,bj)*_dxG(i,j,bi,bj)
     &                 *drF(k)*_hFacS(i,j,k,bi,bj)
         ENDDO
        ENDDO

C-    Integrate vertically the Horizontal Divergence 
        DO j=1,sNy
         DO i=1,sNx
           hDivFlow(i,j) = hDivFlow(i,j)
     &       +maskC(i,j,k,bi,bj)*( uTrans(i+1,j)-uTrans(i,j)
     &                            +vTrans(i,j+1)-vTrans(i,j) )
         ENDDO
        ENDDO

C-    End DO k=1,Nr
      ENDDO

C------------------------------------
       facEmP = 0.
       IF (useRealFreshWaterFlux) facEmP = convertEmP2rUnit
       IF ( myTime.EQ.startTime .AND. myTime.NE.baseTime
     &                          .AND. useRealFreshWaterFlux) THEN

C     needs previous time-step value of E-P-R, that has not been loaded
C     and was not in old pickup-file ; try to use etaN & etaH instead.
         IF ( usePickupBeforeC54 ) THEN
          DO j=1,sNy
           DO i=1,sNx
            dEtaHdt(i,j,bi,bj) = (etaN(i,j,bi,bj)-etaH(i,j,bi,bj))
     &                         / (implicDiv2Dflow*deltaTfreesurf)
           ENDDO
          ENDDO
         ENDIF

         DO j=1,sNy
          DO i=1,sNx
            PmEpR(i,j,bi,bj) = dEtaHdt(i,j,bi,bj)
     &                       + hDivFlow(i,j)*recip_rA(i,j,bi,bj)
            PmEpR(i,j,bi,bj) = PmEpR(i,j,bi,bj)/convertEmP2rUnit
          ENDDO
         ENDDO
       ELSEIF ( myTime.EQ.startTime ) THEN
         DO j=1,sNy
          DO i=1,sNx
            PmEpR(i,j,bi,bj) = 0. _d 0
            dEtaHdt(i,j,bi,bj) = -hDivFlow(i,j)*recip_rA(i,j,bi,bj)
          ENDDO
         ENDDO      
       ELSE
         DO j=1,sNy
          DO i=1,sNx
            PmEpR(i,j,bi,bj) = -EmPmR(i,j,bi,bj)
            dEtaHdt(i,j,bi,bj) = -hDivFlow(i,j)*recip_rA(i,j,bi,bj)
     &                           -facEmP*EmPmR(i,j,bi,bj)
          ENDDO
         ENDDO
       ENDIF
C------------------------------------

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (exactConserv .AND. myTime.NE.startTime) THEN
C--   Update etaN at the end of the time step : 
C     Incorporate the Implicit part of -Divergence(Barotropic_Flow)

       IF (implicDiv2Dflow.EQ. 0. _d 0) THEN
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
           etaN(i,j,bi,bj) = etaH(i,j,bi,bj) 
         ENDDO
        ENDDO
       ELSE
        DO j=1,sNy
         DO i=1,sNx
           etaN(i,j,bi,bj) = etaH(i,j,bi,bj)
     &      + implicDiv2Dflow*dEtaHdt(i,j,bi,bj)*deltaTfreesurf
         ENDDO
        ENDDO
       ENDIF

#ifdef ALLOW_OBCS
       IF ( useOBCS )
     &    CALL OBCS_APPLY_ETA( bi, bj, etaN, myThid )
#endif /* ALLOW_OBCS */            

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef NONLIN_FRSURF
      IF (select_rStar .NE. 0) THEN
        DO j=1,sNy
         DO i=1,sNx
           rStarDhCDt(i,j,bi,bj) =
     &            dEtaHdt(i,j,bi,bj)*recip_Rcol(i,j,bi,bj)
         ENDDO
        ENDDO      
      ENDIF
#endif /* NONLIN_FRSURF */
#endif /* EXACT_CONSERV */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO k=Nr,1,-1
C--    Integrate continuity vertically for vertical velocity

       CALL INTEGRATE_FOR_W(
     I                       bi, bj, k, uFld, vFld,
     O                       wVel,
     I                       myThid )
 
#ifdef EXACT_CONSERV
       IF ( k.EQ.Nr .AND. myTime.NE.baseTime .AND.
     &      useRealFreshWaterFlux .AND. usingPCoords ) THEN

          DO j=1,sNy
           DO i=1,sNx
             wVel(i,j,k,bi,bj) = wVel(i,j,k,bi,bj) 
     &         +convertEmP2rUnit*PmEpR(i,j,bi,bj)*maskC(i,j,k,bi,bj)
           ENDDO
          ENDDO

       ENDIF
#endif /* EXACT_CONSERV */

#ifdef ALLOW_OBCS
#ifdef ALLOW_NONHYDROSTATIC
C--    Apply OBC to W if in N-H mode
       IF (useOBCS.AND.nonHydrostatic) THEN
          CALL OBCS_APPLY_W( bi, bj, k, wVel, myThid )
        ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
#endif /* ALLOW_OBCS */            

C-    End DO k=Nr,1,-1
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
