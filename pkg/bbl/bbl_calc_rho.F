C $Header: /u/gcmpack/MITgcm/pkg/bbl/bbl_calc_rho.F,v 1.4 2012/04/03 16:46:58 jmc Exp $
C $Name:  $

#include "BBL_OPTIONS.h"

CBOP
C     !ROUTINE: BBL_CALC_RHO
C     !INTERFACE:
      SUBROUTINE BBL_CALC_RHO(
     I                tFld, sFld,
     O                rhoLoc,
     I                k, bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE BBL_CALC_RHO
C     | o Calculates [rho(S,T,z)-rhoConst] of a 2-D slice
C     |     filling land-points with BBL density.
C     |   pkg/bbl requires in-situ bbl density for depths equal to
C     |     and deeper than the bbl. To reduce computation and
C     |     storage requirement, these densities are stored in the
C     |     dry grid boxes of rhoInSitu:
C     |   Top cell to kLowC computes rhoLoc at level k based on
C     |     tFld(k) and sFld(k). This is identical to FIND_RHO_2D.
C     |   kLowC+1 to Nr computes rhoLoc at level k-1 based on
C     |     bbl_theta and bbl_salt.
C     |   There is one level missing, bbl density at depth Nr,
C     |     which is intead stored in bbl_rho_nr.
C     *==========================================================*

C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BBL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     tFld      :: Pot.Temperature (3-D array)
C     sFld      :: Salinity (3-D array)
C     rhoLoc    :: In-situ density [kg/m3] (2-D array) computed at z=rC ;
C     k         :: current vertical index
C     bi,bj     :: Tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current time-step number
C     myThid    :: my Thread Id number
      _RL     tFld     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     sFld     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     rhoLoc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL     rBBL     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER i,j,kl

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Compute rhoLoc at level k based on tFld(k) and sFld(k).
      CALL FIND_RHO_2D(
     I     1-OLx, sNx+OLx, 1-OLy, sNy+OLy, k,
     I     tFld(1-OLx,1-OLy,k,bi,bj),
     I     sFld(1-OLx,1-OLy,k,bi,bj),
     O     rhoLoc(1-OLx,1-OLy,k,bi,bj),
     I     k, bi, bj, myThid )

C-    Compute rBBL at level k-1 based on bbl_theta and bbl_salt.
      kl = MAX(k-1,1)
      CALL FIND_RHO_2D(
     I     1-OLx, sNx+OLx, 1-OLy, sNy+OLy, kl,
     I     bbl_theta(1-OLx,1-OLy,bi,bj),
     I     bbl_salt(1-OLx,1-OLy,bi,bj),
     O     rBBL,
     I     kl, bi, bj, myThid )

C-    For k > kLowC replace rhoLoc with rBBL
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        IF ( k .GT. kLowC(i,j,bi,bj) )
     &       rhoLoc(i,j,k,bi,bj) = rBBL(i,j)
       ENDDO
      ENDDO

C-    Compute bbl_rho_nr at level Nr based on bbl_theta and bbl_salt.
      IF ( k .EQ. Nr ) THEN
       CALL FIND_RHO_2D(
     I      1-OLx, sNx+OLx, 1-OLy, sNy+OLy, Nr,
     I      bbl_theta(1-OLx,1-OLy,bi,bj),
     I      bbl_salt(1-OLx,1-OLy,bi,bj),
     O      bbl_rho_nr(1-OLx,1-OLy,bi,bj),
     I      Nr, bi, bj, myThid )
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
