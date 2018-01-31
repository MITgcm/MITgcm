#include "MONITOR_OPTIONS.h"

C--  File mon_calc_advcfl.F: Routines to compute Tracer Advective CFL
C--   Contents
C--   o MON_CALC_ADVCFL_TILE
C--   o MON_CALC_ADVCFL_GLOB

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_CALC_ADVCFL_TILE

C     !INTERFACE:
      SUBROUTINE MON_CALC_ADVCFL_TILE(
     I               myNr, bi, bj,
     I               uFld, vFld, wFld, dT_lev,
     O               maxCFL,
     I               myIter, myThid )

C     Calculate Maximum advective CFL in 3 direction (x,y,z)
C     for current tile bi,bj

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "SURFACE.h"
c#include "MONITOR.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myNr    :: number of levels
C     bi, bj  :: tile indices
C     uFld    :: zonal velocity
C     vFld    :: merid velocity
C     wFld    :: vert. velocity
C     dT_lev  :: tracer time-step
C     maxCFL  :: maximum advective CFL in 3 directions
C     myIter  :: my Iteration number
C     myThid  :: my Thread Id number
      INTEGER myNr, bi, bj
      _RL uFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr)
      _RL vFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr)
      _RL wFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr)
      _RL dT_lev(myNr)
      _RL maxCFL(3)
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i,j,k
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTransKp1, tmpVal, recVol_dT

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      maxCFL(1) = 0.
      maxCFL(2) = 0.
      maxCFL(3) = 0.

      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          rTrans(i,j) = 0.
        ENDDO
      ENDDO

      DO k=myNr,1,-1

C--   compute horiz. transport and time-step divided by volume
C     (without level thickness "drF" since it cancels)
        DO j=1,sNy+1
         DO i=1,sNx+1
           uTrans(i,j) = uFld(i,j,k)*dyG(i,j,bi,bj)*deepFacC(k)
     &                         *hFacW(i,j,k,bi,bj)
           vTrans(i,j) = vFld(i,j,k)*dxG(i,j,bi,bj)*deepFacC(k)
     &                         *hFacS(i,j,k,bi,bj)
         ENDDO
        ENDDO
        DO j=1,sNy
         DO i=1,sNx
           recVol_dT = dT_lev(k)
     &             *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &                   *recip_hFacC(i,j,k,bi,bj)
           tmpVal = (
     &          + MAX( uTrans(i+1,j), zeroRL )
     &          - MIN( uTrans( i ,j), zeroRL )
     &              )*recVol_dT
           maxCFL(1) = MAX( maxCFL(1), tmpVal )
           tmpVal = (
     &          + MAX( vTrans(i,j+1), zeroRL )
     &          - MIN( vTrans(i, j ), zeroRL )
     &              )*recVol_dT
           maxCFL(2) = MAX( maxCFL(2), tmpVal )
         ENDDO
        ENDDO

C--   compute vert. transport and time-step divided by volume
C     (without grid-cell area "rA" since it cancels)
        DO j=1,sNy
         DO i=1,sNx
           rTransKp1 = rTrans(i,j)
           rTrans(i,j) = wFld(i,j,k)
     &                 *deepFac2F(k)*rhoFacF(k)
           recVol_dT = dT_lev(k)
     &             *recip_deepFac2C(k)*recip_rhoFacC(k)
     &             *recip_drF(k)*recip_hFacC(i,j,k,bi,bj)
           tmpVal = (
     &          + MAX( rTrans(i,j), zeroRL )
     &          - MIN( rTransKp1, zeroRL )
     &              )*recVol_dT
           maxCFL(3) = MAX( maxCFL(3), tmpVal )
         ENDDO
        ENDDO

      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_CALC_ADVCFL_GLOB

C     !INTERFACE:
      SUBROUTINE MON_CALC_ADVCFL_GLOB(
     I               maxCFL, myIter, myThid )

C     !DESCRIPTION:
C     Calculate Maximum advective CFL in 3 direction (x,y,z)
C     in global domain (from tile-max value)

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MONITOR.h"

C     !INPUT PARAMETERS:
C     maxCFL  :: maximum advective CFL (per tile) in 3 directions
C     myIter  :: Current iteration number in simulation
C     myThid  :: my Thread Id number
      _RL maxCFL(3,nSx,nSy)
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi, bj
      _RL uCFL, vCFL, wCFL

      uCFL = 0.
      vCFL = 0.
      wCFL = 0.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         uCFL = MAX( uCFL, maxCFL(1,bi,bj) )
         vCFL = MAX( vCFL, maxCFL(2,bi,bj) )
         wCFL = MAX( wCFL, maxCFL(3,bi,bj) )
       ENDDO
      ENDDO
      _GLOBAL_MAX_RL( uCFL, myThid )
      _GLOBAL_MAX_RL( vCFL, myThid )
      _GLOBAL_MAX_RL( wCFL, myThid )

C-    store values in common bloc
      _BEGIN_MASTER(myThid)
       mon_trAdvCFL(1) = uCFL
       mon_trAdvCFL(2) = vCFL
       mon_trAdvCFL(3) = wCFL
      _END_MASTER(myThid)

      RETURN
      END
