C $Header: /u/gcmpack/MITgcm/model/src/diags_rho.F,v 1.5 2011/11/10 21:03:35 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C--  File diags_rho.F: density & density advection diagnostics
C--   Contents
C--   o DIAGS_RHO_L
C--   o DIAGS_RHO_G

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DIAGS_RHO_L
C     !INTERFACE:
      SUBROUTINE DIAGS_RHO_L(
     I                        diagRho, k, bi, bj,
     I                        rho3d, rhoKm1, wFld,
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R DIAGS_RHO_L
C     | o Density vertical advective term diagnostics
C     *==========================================================*
C     | works with local arrays, and called inside k,bi,bj loops
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     diagRho  :: select which diags to fill
C     k        :: level index
C     bi, bj   :: tile indices
C     rho3d    :: in-situ density anomaly
C     rhoKm1   :: density of water @ level above (k-1), evaluated at pressure level k
C     wFld     :: vertical velocity
C     myTime   :: Current time
C     myIter   :: Iteration number
C     myThid   :: my Thread Id number
      INTEGER diagRho
      INTEGER k, bi, bj
      _RL rho3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL rhoKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j      :: Loop counters
C     tmpFld   :: temporary working array
      INTEGER i,j
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( k.GE.2 .AND. MOD(diagRho,8).GE.4 ) THEN
C--   Diagnose Vertical velocity times vertical difference
C     of potential density reference at level below (i.e. level k)
        DO j=1,sNy
         DO i=1,sNx
           tmpFld(i,j) = wFld(i,j,k,bi,bj)
     &                 *( rho3d(i,j,k) - rhoKm1(i,j) )*rkSign
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmpFld,'WdRHO_P ',k,1,2,bi,bj,myThid)
        IF ( k.EQ.2 ) CALL DIAGNOSTICS_COUNT('WdRHO_P ',bi,bj,myThid)
      ENDIF

      IF ( k.GE.2 .AND. diagRho.GE.8 ) THEN
C--   Diagnose Vertical velocity times vertical difference
C     of density at fixed Temp & Salt (from level above, i.e. level k-1)
        DO j=1,sNy
         DO i=1,sNx
           tmpFld(i,j) = wFld(i,j,k,bi,bj)
     &                 *( rhoKm1(i,j) - rho3d(i,j,k-1) )*rkSign
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmpFld,'WdRHOdP ',k,1,2,bi,bj,myThid)
        IF ( k.EQ.2 ) CALL DIAGNOSTICS_COUNT('WdRHOdP ',bi,bj,myThid)
      ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DIAGS_RHO_G
C     !INTERFACE:
      SUBROUTINE DIAGS_RHO_G(
     I                        rho3d, uFld, vFld, wFld,
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R DIAGS_RHO_G
C     | o Density & Density advective Flux diagnostics
C     *==========================================================*
C     | works with global arrays; k,bi,bj loops are done here
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     rho3d    :: in-situ density anomaly
C     uFld     :: zonal velocity
C     vFld     :: meridional velocity
C     wFld     :: vertical velocity
C     myTime   :: Current time
C     myIter   :: Iteration number
C     myThid   :: my Thread Id number
      _RL rho3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL uFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !FUNCTIONS:
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j      :: Loop counters
C     k, bi,bj :: level & tile indices
C     tmpFld   :: temporary working array
      INTEGER i,j
      INTEGER k, bi,bj
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpFac

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      CALL DIAGNOSTICS_FILL( rho3d, 'RHOAnoma',
     &                               0, Nr, 0, 1, 1, myThid )
      tmpFac = 1. _d 0
      CALL DIAGNOSTICS_SCALE_FILL( rho3d, tmpFac, 2,
     &                   'RHOANOSQ', 0, Nr, 0, 1, 1, myThid )

      IF ( DIAGNOSTICS_IS_ON('URHOMASS',myThid) ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,Nr
          DO j=1,sNy
           DO i=1,sNx+1
             tmpFld(i,j) = uFld(i,j,k,bi,bj)*_hFacW(i,j,k,bi,bj)
     &                   *(rho3d(i-1,j,k,bi,bj)+rho3d(i,j,k,bi,bj))
     &                   *0.5 _d 0
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(tmpFld,'URHOMASS',k,1,2,bi,bj,myThid)
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('VRHOMASS',myThid) ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,Nr
          DO j=1,sNy+1
           DO i=1,sNx
             tmpFld(i,j) = vFld(i,j,k,bi,bj)*_hFacS(i,j,k,bi,bj)
     &                   *(rho3d(i,j-1,k,bi,bj)+rho3d(i,j,k,bi,bj))
     &                   *0.5 _d 0
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(tmpFld,'VRHOMASS',k,1,2,bi,bj,myThid)
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('WRHOMASS',myThid) ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,Nr
          IF ( k.EQ.1 ) THEN
           DO j=1,sNy
            DO i=1,sNx
             tmpFld(i,j) = wFld(i,j,k,bi,bj)*rho3d(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ELSE
           DO j=1,sNy
            DO i=1,sNx
             tmpFld(i,j) = wFld(i,j,k,bi,bj)
     &                   *(rho3d(i,j,k-1,bi,bj)+rho3d(i,j,k,bi,bj))
     &                   *0.5 _d 0
            ENDDO
           ENDDO
          ENDIF
          CALL DIAGNOSTICS_FILL(tmpFld,'WRHOMASS',k,1,2,bi,bj,myThid)
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
