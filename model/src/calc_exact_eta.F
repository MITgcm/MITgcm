C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_exact_eta.F,v 1.5 2001/09/26 18:09:13 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_EXACT_ETA
C     !INTERFACE:
      SUBROUTINE CALC_EXACT_ETA( UpdateEtaN_EtaH, 
     I                           bi,bj, uFld,vFld,
     I                           myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_EXACT_ETA                                 
C     | o Compute again the surface "r-anomaly" (eta) to satisfy  
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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C    UpdateEtaN_EtaH :: flag to distinguishe if this S/R is called
C           at the end of a time step (TRUE) to update EtaN,
C        or at the beginning of the time sptep (FALSE) to update EtaH
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
#ifdef EXACT_CONSERV
C     Local variables in common block
C     hDivFlow :: Div. Barotropic Flow at current time [transport unit m3/s]
      COMMON /EXACT_ETA_LOCAL/ hDivFlow
      _RL hDivFlow(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)

C     Local variables
C     i,j,k  :: Loop counters
C     uTrans :: Volume transports ( uVel.xA )
C     vTrans :: Volume transports ( vVel.yA )
      INTEGER i,j,k
      _RL uTrans(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL vTrans(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( UpdateEtaN_EtaH .OR. myTime.EQ.StartTime ) THEN

C--   Compute the Divergence of The Barotropic Flow :

C-    Initialise 
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
         hDivFlow(i,j,bi,bj) = 0.
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
           hDivFlow(i,j,bi,bj) = hDivFlow(i,j,bi,bj)
     &       +maskC(i,j,k,bi,bj)*( uTrans(i+1,j)-uTrans(i,j)
     &                            +vTrans(i,j+1)-vTrans(i,j) )
         ENDDO
        ENDDO

C-    End DO k=1,Nr
      ENDDO

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( UpdateEtaN_EtaH ) THEN

C--   Update etaN at the end of the time step : 
C     Incorporate the Implicit part of -Divergence(Barotropic_Flow)
       IF (implicDiv2Dflow .NE. 0. _d 0 ) THEN
        DO j=1,sNy
         DO i=1,sNx
           etaN(i,j,bi,bj) = etaH(i,j,bi,bj)
     &      - implicDiv2Dflow*hDivFlow(i,j,bi,bj)
     &                       *recip_rA(i,j,bi,bj)*DeltaTmom
         ENDDO
        ENDDO
       ELSE
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
           etaN(i,j,bi,bj) = etaH(i,j,bi,bj) 
         ENDDO
        ENDDO
       ENDIF

      ELSE

C--   Update etaH at the beginning of the time step : 
C     Incorporate the Explicit part of -Divergence(Barotropic_Flow)
       IF (implicDiv2Dflow .NE. 1. _d 0 ) THEN
        DO j=1,sNy
         DO i=1,sNx
           etaH(i,j,bi,bj) = etaN(i,j,bi,bj) 
     &      - (1. - implicDiv2Dflow)*hDivFlow(i,j,bi,bj)
     &                              *recip_rA(i,j,bi,bj)*DeltaTmom
         ENDDO
        ENDDO
       ELSE
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
           etaH(i,j,bi,bj) = etaN(i,j,bi,bj) 
         ENDDO
        ENDDO
       ENDIF

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* EXACT_CONSERV */

      RETURN
      END
