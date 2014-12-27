C $Header: /u/gcmpack/MITgcm/pkg/openad/externalDummies.F,v 1.4 2014/12/27 20:37:34 heimbach Exp $
C $Name:  $

#include "OPENAD_OPTIONS.h"

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
! xxx template oad_template.active_read_xy.f90
        implicit none
!     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
      CHARACTER*(80) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL     dummy
#ifdef ALLOW_OPENAD_ACTIVE_FILE
      active_var = dummy + active_var
      dummy = active_var(1,1,1,1) + dummy
#endif /* ALLOW_OPENAD_ACTIVE_FILE */
      end subroutine
