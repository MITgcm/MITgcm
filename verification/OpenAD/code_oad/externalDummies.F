C ###########################################################
C      SUBROUTINE EXCH_UV_XY_RS(
C     U                       Uphi, Vphi,
C     I                       withSigns, myThid )
C      IMPLICIT NONE
C#include "SIZE.h"
C#include "EEPARAMS.h"
C#include "EESUPPORT.h"
C#include "EXCH.h"
C
C      _RS Uphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C      _RS Vphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C      _RS temp
C      LOGICAL withSigns
C      INTEGER myThid
C      temp=Uphi(1,1,1,1)
C      Uphi(1,1,1,1)=Vphi(1,1,1,1)
C      Vphi(1,1,1,1)=temp
C      end subroutine
C ###########################################################
      SUBROUTINE EXCH_UV_XYZ_RL(
     U                       Uphi, Vphi,
     I                       withSigns, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"
      _RL Uphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Vphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL temp
      LOGICAL withSigns
      INTEGER myThid
      temp=Uphi(1,1,1,1,1)
      Uphi(1,1,1,1,1)=Vphi(1,1,1,1,1)
      Vphi(1,1,1,1,1)=temp
      end subroutine
C ###########################################################
C      SUBROUTINE EXCH_UV_XYZ_RS(
C     U                       Uphi, Vphi,
C     I                       withSigns, myThid )
C      IMPLICIT NONE
C#include "SIZE.h"
C#include "EEPARAMS.h"
C#include "EESUPPORT.h"
C#include "EXCH.h"
C      _RS Uphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
C      _RS Vphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
C      _RL temp
C      LOGICAL withSigns
C      INTEGER myThid
C      temp=Uphi(1,1,1,1,1)
C      Uphi(1,1,1,1,1)=Vphi(1,1,1,1,1)
C      Vphi(1,1,1,1,1)=temp
C      end subroutine
C ###########################################################
      SUBROUTINE EXCH_XY_RL(
     U                       phi,
     I                       myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"
      _RL phi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid
      phi(1,1,1,1)=2*phi(1,1,1,1)
      end subroutine
C ###########################################################
      SUBROUTINE EXCH_XYZ_RL(
     U                       phi,
     I                       myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"
      _RL phi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid
      phi(1,1,1,1,1)=2*phi(1,1,1,1,1)
      end subroutine
CC ###########################################################
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
      SUBROUTINE GLOBAL_SUM_R8(
     U                       sumphi,
     I                       myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"
      Real*8 sumPhi
      INTEGER myThid
      sumPhi=2*sumPhi
      end subroutine
C ###########################################################
      SUBROUTINE CG2D(
     I                cg2d_b,
     U                cg2d_x,
     O                firstResidual,
     O                lastResidual,
     U                numIters,
     I                myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CG2D.h"
      Real*8  cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8  cg2d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8  firstResidual
      Real*8  lastResidual
      INTEGER numIters
      INTEGER myThid
      cg2d_x(1,1,1,1)=cg2d_b(1,1,1,1)
      end subroutine
