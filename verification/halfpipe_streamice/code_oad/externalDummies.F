C $Header: /u/gcmpack/MITgcm/verification/halfpipe_streamice/code_oad/Attic/externalDummies.F,v 1.1 2013/12/18 19:58:40 utke Exp $
C $Name:  $
C ###########################################################
      SUBROUTINE EXCH1_RL(
     U                 array,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EXCH.h"
      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RL     array( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid
C dummy self dependence (nontrivial so mfef90 doesn't kill it)
      array(1,1,1,1,1)=2*array(1,1,1,1,1)
      end subroutine
C ###########################################################
C      SUBROUTINE EXCH1_RS(
C     U                 array,
C     I                 myOLw, myOLe, myOLs, myOLn, myNz,
C     I                 exchWidthX, exchWidthY,
C     I                 cornerMode, myThid )
C
C      IMPLICIT NONE
C#include "SIZE.h"
C#include "EEPARAMS.h"
C#include "EXCH.h"
C      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
C      _RS     array( 1-myOLw:sNx+myOLe,
C     &               1-myOLs:sNy+myOLn,
C     &               myNz, nSx, nSy )
C      INTEGER exchWidthX
C      INTEGER exchWidthY
C      INTEGER cornerMode
C      INTEGER myThid
C      end subroutine
C ###########################################################
C      SUBROUTINE GLOBAL_MAX_R8(
C     U                       maxphi,
C     I                       myThid )
C      IMPLICIT NONE
C#include "SIZE.h"
C#include "EEPARAMS.h"
C#include "EESUPPORT.h"
C#include "EXCH.h"
C      Real*8 maxPhi
C      INTEGER myThid
C      maxPhi=2*maxPhi
C      end subroutine
C ###########################################################
C      SUBROUTINE GLOBAL_SUM_R8(
C     U                       sumphi,
C     I                       myThid )
C      IMPLICIT NONE
C#include "SIZE.h"
C#include "EEPARAMS.h"
C#include "EESUPPORT.h"
C#include "EXCH.h"
C      Real*8 sumPhi
C      INTEGER myThid
CC dummy self dependence (nontrivial so mfef90 doesn't kill it)
C      sumPhi=2*sumPhi
C      end subroutine
C ###########################################################
      SUBROUTINE GLOBAL_SUM_TILE_RL(
     U                       phiTile,
     U                       sumphi,
     I                       myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"
      _RL phiTile(nSx,nSy)
      _RL sumPhi
      INTEGER myThid
C dummy self dependence (nontrivial so mfef90 doesn't kill it)
      sumPhi=2*phiTile(1,1)
      end subroutine
C ###########################################################
      SUBROUTINE STREAMICE_CG_SOLVE( 
     U                               cg_Uin,     ! x-velocities
     U                               cg_Vin,     ! y-velocities
     I                               cg_Bu,      ! force in x dir
     I                               cg_Bv,      ! force in y dir
     I                               A_uu,       ! section of matrix that multiplies u and projects on u
     I                               A_uv,       ! section of matrix that multiplies v and projects on u
     I                               A_vu,       ! section of matrix that multiplies u and projects on v
     I                               A_vv,       ! section of matrix that multiplies v and projects on v
     I                               tolerance, 
     O                               iters,
     I                               myThid )
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"
      INTEGER myThid
      INTEGER iters
      _RL tolerance
      _RL cg_Uin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL 
     & A_uu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_uv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)

      cg_Uin(1,1,1,1) = A_uu(1,1,1,1,1,1) + A_uv(1,1,1,1,1,1) + 
     & A_vu(1,1,1,1,1,1) + A_vv(1,1,1,1,1,1) + cg_Bu(1,1,1,1)  

      cg_Vin(1,1,1,1) = A_uu(1,1,1,1,1,1) + A_uv(1,1,1,1,1,1) + 
     & A_vu(1,1,1,1,1,1) + A_vv(1,1,1,1,1,1) + cg_Bv(1,1,1,1)  

      end subroutine
C ###########################################################
