#include "OPENAD_OPTIONS.h"
#ifdef ALLOW_STREAMICE
#include "STREAMICE_OPTIONS.h"
#endif

C ###########################################################
      SUBROUTINE DUMMY_IN_STEPPING( myTime, myIter, myThid )

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C dummy self dependence (nontrivial so mfef90 doesn't kill it)
      theta(1,1,1,1,1)=2*theta(1,1,1,1,1)
      end subroutine
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
#ifndef ALLOW_STREAMICE
      SUBROUTINE CG2D(
     I                cg2d_b,
     U                cg2d_x,
     O                firstResidual,
     O                minResidualSq,
     O                lastResidual,
     U                numIters,
     O                nIterMin,
     I                myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CG2D.h"
      Real*8  cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8  cg2d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8  firstResidual
      Real*8  minResidualSq
      Real*8  lastResidual
      INTEGER numIters
      INTEGER nIterMin
      INTEGER myThid
C dummy self dependence (nontrivial so mfef90 doesn't kill it)
      cg2d_x(1,1,1,1)=2*cg2d_b(1,1,1,1)
      end subroutine
#endif
C ###########################################################
#ifdef ALLOW_STREAMICE
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
     I                               maxiter,
     I                               myThid )
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"
      INTEGER myThid
      INTEGER iters
      INTEGER maxiter
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
#endif /* ALLOW_STREAMICE */
C ###########################################################
#ifdef ALLOW_STREAMICE
       SUBROUTINE STREAMICE_INVERT_SURF_FORTHICK (
     O            H,
     I            s,
     I            R,
     I            delta,
     I            myThid)

#include "SIZE.h"
#include "GRID.h"
#include "SET_GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"

      _RL H(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL S(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL R(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL DELTA
      INTEGER myThid
#ifdef STREAMICE_ALLOW_DEPTH_CONTROL
      H(1,1,1,1) = s(1,1,1,1) +
     &  R(1,1,1,1)
#endif

      end subroutine
#endif /* ALLOW_STREAMICE */
C ###########################################################
#ifdef ALLOW_STREAMICE
       SUBROUTINE STREAMICE_SMOOTH_ADJOINT_FIELD (
     O            X,
     I            myThid)

#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"

      _RL X(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

      INTEGER i, j, bi, bj, k
      _RL q_int1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL q_int2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx

          q_int1(i,j,bi,bj) = 0.0
          q_int2(i,j,bi,bj) = 0.0
          X(i,j,bi,bj) = X(i,j,bi,bj) * 1.0
          k=0
         ENDDO
        ENDDO
       ENDDO
      ENDDO


      end subroutine
#endif /* ALLOW_STREAMICE */
C ###########################################################
      subroutine active_read_xyz(
     I                           active_var_file,
     O                           active_var,
     I                           iRec,
     I                           doglobalread,
     I                           lAdInit,
     I                           myOptimIter,
     I                           myThid,
     I                           dummy
     &                         )
! xxx template ad_template.active_read_xyz.f90
        implicit none
!     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL     dummy
#ifdef ALLOW_OPENAD_ACTIVE_READ_XYZ
      active_var = dummy + active_var
      dummy = active_var(1,1,1,1,1) + dummy
#endif /* ALLOW_OPENAD_ACTIVE_READ_XYZ */
      end subroutine
C ###########################################################
      subroutine active_read_xy(
     I                           active_var_file,
     O                           active_var,
     I                           iRec,
     I                           doglobalread,
     I                           lAdInit,
     I                           myOptimIter,
     I                           myThid,
     I                           dummy
     &                         )
! xxx template ad_template.active_read_xy.f90
        implicit none
!     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL     dummy
#ifdef ALLOW_OPENAD_ACTIVE_READ_XY
      active_var = dummy + active_var
      dummy = active_var(1,1,1,1) + dummy
#endif /* ALLOW_OPENAD_ACTIVE_READ_XY */
      end subroutine
C ###########################################################
      subroutine active_write_xy(
     I                           active_var_file,
     I                           active_var,
     I                           iRec,
     I                           myOptimIter,
     I                           myThid,
     I                           dummy
     &                         )
! xxx template ad_template.active_write_xy.f90
        implicit none
!     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      _RL     dummy
#ifdef ALLOW_OPENAD_ACTIVE_WRITE
      active_var = dummy + active_var
      dummy = active_var(1,1,1,1) + dummy
#endif /* ALLOW_OPENAD_ACTIVE_WRITE */
      end subroutine
C ###########################################################
