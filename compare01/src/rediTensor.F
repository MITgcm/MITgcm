#undef DEBUG_TESTS
#define _SMAX 1.d-2
#define _SMALL 1.d-9

#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"

#define ICHAN 0
#define GRONIL 9.8d0*999.8d0
 
! ==============================================================================
      subroutine RediTensor(Temp,Salt,Kredi,K13,K23,K31,K32,K33,Ustar,Vstar,Wstar
!                           |---in--| |------------------out--------------------|
     &         ,nIter,DumpFlag)
      implicit none
! Global
#include "SIZE.h"
#include "MASKS.h"
! Arguments
      real Temp(_I3(Nz,Nx,Ny))	! Locally referenced pot. temperature
      real Salt(_I3(Nz,Nx,Ny))	! Locally referenced pot. temperature
      real Kredi(_I3(Nz,Nx,Ny))	! Redi diffusion coefficient
      real K13(_I3(Nz,Nx,Ny))	! Redi tensor component
      real K23(_I3(Nz,Nx,Ny))	! Redi tensor component
      real K31(_I3(Nz,Nx,Ny))	! Redi tensor component
      real K32(_I3(Nz,Nx,Ny))	! Redi tensor component
      real K33(_I3(Nz,Nx,Ny))	! Redi tensor component
      real Ustar(_I3(Nz,Nx,Ny))	! Eddy transport velocity U*
      real Vstar(_I3(Nz,Nx,Ny))	! Eddy transport velocity V*
      real Wstar(_I3(Nz,Nx,Ny))	! Eddy transport velocity W*
      integer nIter
      logical DumpFlag
! Local
      real rSmax
      real SigX(_I3(Nz,Nx,Ny))		! d/dx Sigma (@ U points)
      real SigY(_I3(Nz,Nx,Ny))		! d/dy Sigma (@ V points)
      real SigZ(_I3(Nz,Nx,Ny))		! d/dz Sigma (@ W points)
      real SigZBz(_I3(Nz,Nx,Ny))	! d/dz Sigma (@ T/S points)
      real Kgm(_I3(Nz,Nx,Ny))	! Eddy induced transport coefficient
      character*(10) suffix
      integer iErr
! ------------------------------------------------------------------------------

      rSmax=1.d0/(_SMAX)

      call gradSigma(Temp,Salt,SigX,SigY,SigZ)
!                    |--in---| |----out-----|

! Interpolate d/dz Sigma to T/S points
      SigZBz=0.5*(SigZ+CSHIFT(SigZ,DIM=_I3Z,SHIFT=+1))*PMASK
      SigZBz(_I3(Nz,:,:))=SigZ(_I3(Nz-1,:,:))

      call KtensorUface(SigX,SigY,SigZBz,rSmax,K13,K33)
!                       |---------in---------| |-out-|

      call KtensorVface(SigX,SigY,SigZBz,rSmax,K33,K23)
!                       |---------in---------| |-out-|

      call KtensorWface(SigX,SigY,SigZ,rSmax,K31,K32,K33)
!                       |--------in--------| |---out---|

      call RediCoefficient(SigX,SigY,SigZ,Kredi)
!                          |-----in-----| |out|

      call GMcoefficient(SigX,SigX,SigZ,Kgm)
!                        |-----in-----| |out|

      call GMtransport(K13,K23,Kgm,Ustar,Vstar,Wstar)
!                      |----in---| |------out------|

      IF(DumpFlag)THEN
        iErr = 0.
        WRITE(suffix,'(I10.10)') nIter
        CALL WRITE_DUMP_3D( Kgm, Nx, Ny, Nz,   'Kgm.', suffix, iErr)
        CALL WRITE_DUMP_3D( Kredi, Nx, Ny, Nz, 'Kredi.', suffix, iErr)
        CALL WRITE_DUMP_3D( K13, Nx, Ny, Nz, 'K13.', suffix, iErr)
        CALL WRITE_DUMP_3D( K23, Nx, Ny, Nz, 'K23.', suffix, iErr)
        CALL WRITE_DUMP_3D( K31, Nx, Ny, Nz, 'K31.', suffix, iErr)
        CALL WRITE_DUMP_3D( K32, Nx, Ny, Nz, 'K32.', suffix, iErr)
        CALL WRITE_DUMP_3D( K33, Nx, Ny, Nz, 'K33.', suffix, iErr)
        CALL WRITE_DUMP_3D( SigX, Nx, Ny, Nz, 'SigX.', suffix, iErr)
        CALL WRITE_DUMP_3D( SigY, Nx, Ny, Nz, 'SigY.', suffix, iErr)
        CALL WRITE_DUMP_3D( SigZ, Nx, Ny, Nz, 'SigZ.', suffix, iErr)
        CALL WRITE_DUMP_3D( Ustar, Nx, Ny, Nz, 'Ustar.',  suffix, iErr)
        CALL WRITE_DUMP_3D( Vstar, Nx, Ny, Nz, 'Vstar.',  suffix, iErr)
        CALL WRITE_DUMP_3D( Wstar, Nx, Ny, Nz, 'Wstar.',  suffix, iErr)
      ENDIF


! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================

! ==============================================================================
      subroutine gradSigma(Temp,Salt,SigX,SigY,SigZ)
!                          |--in---| |----out-----|
      implicit none
! Global
#include "SIZE.h"
#include "OPERATORS.h"
#include "MASKS.h"
! Arguments
      real Temp(_I3(Nz,Nx,Ny))	! Locally referenced pot. temperature
      real Salt(_I3(Nz,Nx,Ny))	! Locally referenced pot. temperature
      real SigX(_I3(Nz,Nx,Ny))	! d/dx Sigma
      real SigY(_I3(Nz,Nx,Ny))	! d/dy Sigma
      real SigZ(_I3(Nz,Nx,Ny))	! d/dz Sigma
! Local
      real sigma(_I3(Nz,Nx,Ny))	! Locally referenced pot. density
      real sigUp(_I3(Nz,Nx,Ny))	! Pot. density referenced to level below
      integer K
! ------------------------------------------------------------------------------

      DO K=1,Nz
       CALL UPDATE_RHO(Temp,Salt,K,'POLY3','LOCAL',sigma)
!                      |-----------in------------| |out|
       sigma(_I3(K,:,:))=sigma(_I3(K,:,:))*PMASK(_I3(K,:,:))
       CALL UPDATE_RHO(Temp,Salt,K,'POLY3','BELOW',sigUp)
!                      |-----------in------------| |out|
! Lateral gradients of potential density
       SigX(_I3(K,:,:))=rDXatU*UMASK(_I3(K,:,:))*
     &  (sigma(_I3(K,:,:))-CSHIFT(sigma(_I3(K,:,:)),DIM=1,SHIFT=-1))
       SigY(_I3(K,:,:))=rDYatV*VMASK(_I3(K,:,:))*
     &  (sigma(_I3(K,:,:))-CSHIFT(sigma(_I3(K,:,:)),DIM=2,SHIFT=-1))

      ENDDO
      sigUp=sigUp*PMASK

! Static stability at W points (K=1 is at surface)
      DO K=2,Nz
       SigZ(_I3(K,:,:))=rDZatW(K)*(GRONIL)*WMASK(_I3(K,:,:))*
     &  (sigUp(_I3(K-1,:,:))-sigma(_I3(K,:,:)))
      ENDDO
      SigZ(_I3(1,:,:))=SigZ(_I3(2,:,:))
! Extrapolate downward from above at W=0 surfaces
      SigZ=WMASK*SigZ+(1.-WMASK)*CSHIFT(SigZ,DIM=_I3Z,SHIFT=-1)
! Extrapolate upward from below at Z=0
      SigZ(_I3(1,:,:))=SigZ(_I3(2,:,:))

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================

! ==============================================================================
      subroutine KtensorUface(SigX,SigY,SigZ,rSmax,Sx,Sy)
!                             |-----in-----| |out|
      implicit none
! Global
#include "SIZE.h"
#include "MASKS.h"
! Arguments
      real SigX(_I3(Nz,Nx,Ny))	! d/dx Sigma  (@ U points)
      real SigY(_I3(Nz,Nx,Ny))	! d/dy Sigma  (@ V points)
      real SigZ(_I3(Nz,Nx,Ny))	! d/dz Sigma  (interpd to T/S points)
      real rSmax		! reciprocal maximum allowed slope
      real Sx(_I3(Nz,Nx,Ny))	! Sx = - d/dx Sigma / d/dz Sigma
      real Sy(_I3(Nz,Nx,Ny))	! Sy = - d/dy Sigma / d/dz Sigma
! Local
      real tmp(_I3(Nz,Nx,Ny))
      real tmp2(_I3(Nz,Nx,Ny))
! ------------------------------------------------------------------------------

! Interpolate d/dy Sigma and d/dz Sigma to U points
      Sy=0.5d0*(SigY+CSHIFT(SigY,DIM=_I3Y,SHIFT=+1))*PMASK	! Interp to T
      Sy=0.5d0*(Sy+CSHIFT(Sy,DIM=_I3X,SHIFT=-1))*UMASK		! Interp to U
      tmp=0.5d0*(SigZ+CSHIFT(SigZ,DIM=_I3X,SHIFT=-1))		! d/dz sigma @ U

! Limit maximum slope
!     tmp=1.d0/max(tmp,-_SMALL-sqrt(SigX*SigX+Sy*Sy)*rSmax)	! check
      tmp2=-_SMALL-sqrt(SigX*SigX+Sy*Sy)*rSmax
      where (tmp.gt.tmp2) tmp=tmp2				! check
      tmp=1.d0/tmp

      Sx=-SigX*tmp
      Sy=-Sy*tmp

#ifdef DEBUG_TESTS
      write(ICHAN,*) 'KtensorUface: |Sx|  =',maxval(abs(Sx))
      write(ICHAN,*) 'KtensorUface: |Sy|  =',maxval(abs(Sy))
      write(ICHAN,*) 'KtensorUface: |S|^2 =',sqrt(maxval(Sx*Sx+Sy*Sy))
#endif

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================
 
! ==============================================================================
      subroutine KtensorVface(SigX,SigY,SigZ,rSmax,Sx,Sy)
!                             |-----in-----| |out|
      implicit none
! Global
#include "SIZE.h"
#include "MASKS.h"
! Arguments
      real SigX(_I3(Nz,Nx,Ny))	! d/dx Sigma  (@ U points)
      real SigY(_I3(Nz,Nx,Ny))	! d/dy Sigma  (@ V points)
      real SigZ(_I3(Nz,Nx,Ny))	! d/dz Sigma  (interpd to T/S points)
      real rSmax		! reciprocal maximum allowed slope
      real Sx(_I3(Nz,Nx,Ny))	! Sx = - d/dx Sigma / d/dz Sigma
      real Sy(_I3(Nz,Nx,Ny))	! Sy = - d/dy Sigma / d/dz Sigma
! Local
      real tmp(_I3(Nz,Nx,Ny))
      real tmp2(_I3(Nz,Nx,Ny))
! ------------------------------------------------------------------------------

! Interpolate d/dx Sigma and d/dz Sigma to V points
      Sx=0.5d0*(SigX+CSHIFT(SigX,DIM=_I3X,SHIFT=+1))*PMASK	! Interp to T
      Sx=0.5d0*(Sx+CSHIFT(Sx,DIM=_I3Y,SHIFT=-1))*VMASK		! Interp to V
      tmp=0.5d0*(SigZ+CSHIFT(SigZ,DIM=_I3Y,SHIFT=-1))		! d/dz sigma @ V

! Limit maximum slope
!     tmp=1.d0/max(tmp,-_SMALL-sqrt(Sx*Sx+SigY*SigY)*rSmax)	! check
      tmp2=-_SMALL-sqrt(Sx*Sx+SigY*SigY)*rSmax
      where (tmp.gt.tmp2) tmp=tmp2				! check
      tmp=1.d0/tmp

      Sx=-Sx*tmp
      Sy=-SigY*tmp

#ifdef DEBUG_TESTS
      write(ICHAN,*) 'KtensorVface: |Sx|  =',maxval(abs(Sx))
      write(ICHAN,*) 'KtensorVface: |Sy|  =',maxval(abs(Sy))
      write(ICHAN,*) 'KtensorVface: |S|^2 =',sqrt(maxval(Sx*Sx+Sy*Sy))
#endif

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================
 
! ==============================================================================
      subroutine KtensorWface(SigX,SigY,SigZ,rSmax,Sx,Sy,Sxy2)
!                             |--------in--------| |--out---|
      implicit none
! Global
#include "SIZE.h"
#include "MASKS.h"
! Arguments
      real SigX(_I3(Nz,Nx,Ny))	! d/dx Sigma  (@ U points)
      real SigY(_I3(Nz,Nx,Ny))	! d/dy Sigma  (@ V points)
      real SigZ(_I3(Nz,Nx,Ny))	! d/dz Sigma  (@ W points)
      real rSmax		! reciprocal of maximum allowed slope
      real Sx(_I3(Nz,Nx,Ny))	! Sx = - d/dx Sigma / d/dz Sigma
      real Sy(_I3(Nz,Nx,Ny))	! Sy = - d/dy Sigma / d/dz Sigma
      real Sxy2(_I3(Nz,Nx,Ny))	! Sxy2 = (grad_h Sigma)^2 / (d/dz Sigma)^2
! Local
      real tmp(_I3(Nz,Nx,Ny))
      real tmp2(_I3(Nz,Nx,Ny))
! ------------------------------------------------------------------------------

! Interpolate d/dx Sigma and d/dy Sigma to W points
      Sx=0.5d0*(SigX+CSHIFT(SigX,DIM=_I3X,SHIFT=+1))*PMASK	! Interp to T
      Sx=0.5d0*(Sx+CSHIFT(Sx,DIM=_I3Z,SHIFT=-1))*WMASK		! Interp to W
      Sy=0.5d0*(SigY+CSHIFT(SigY,DIM=_I3Y,SHIFT=+1))*PMASK	! Interp to T
      Sy=0.5d0*(Sy+CSHIFT(Sy,DIM=_I3Z,SHIFT=-1))*WMASK		! Interp to W
      tmp=SigZ							! d/dz sigma @ W

! Limit maximum slope
      Sxy2=sqrt(Sx*Sx+Sy*Sy)
!     tmp=1.d0/max(SigZ,-_SMALL-Sxy2*rSmax)			! check
      tmp2=-_SMALL-Sxy2*rSmax
      where (tmp.gt.tmp2) tmp=tmp2				! check
      tmp=1.d0/tmp

      Sx=-Sx*tmp
      Sy=-Sy*tmp
      Sxy2=Sx*Sx+Sy*Sy

#ifdef DEBUG_TESTS
      write(ICHAN,*) 'KtensorWface: |Sx|  =',maxval(abs(Sx))
      write(ICHAN,*) 'KtensorWface: |Sy|  =',maxval(abs(Sy))
      write(ICHAN,*) 'KtensorWface: |Sxy2|=',maxval(Sxy2),minval(Sxy2)
      write(ICHAN,*) 'KtensorWface: |S|^2 =',sqrt(maxval(Sx*Sx+Sy*Sy))
#endif

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================

! ==============================================================================
      subroutine RediCoefficient(SigX,SigY,SigZ,Kredi)
!                                |-----in-----| |out|
      implicit none
! Global
#include "SIZE.h"
#include "MASKS.h"
! Arguments
      real SigX(_I3(Nz,Nx,Ny))	! d/dx Sigma  (@ U points)
      real SigY(_I3(Nz,Nx,Ny))	! d/dy Sigma  (@ V points)
      real SigZ(_I3(Nz,Nx,Ny))	! d/dz Sigma  (@ W points)
      real Kredi(_I3(Nz,Nx,Ny))	! Redi diffusion coefficient (@ T points)
! Local
! ------------------------------------------------------------------------------

      Kredi=1.0d3*PMASK

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================

! ==============================================================================
      subroutine GMcoefficient(SigX,SigY,SigZ,Kgm)
!                              |-----in-----| |out|
      implicit none
! Global
#include "SIZE.h"
#include "MASKS.h"
! Arguments
      real SigX(_I3(Nz,Nx,Ny))	! d/dx Sigma  (@ U points)
      real SigY(_I3(Nz,Nx,Ny))	! d/dy Sigma  (@ V points)
      real SigZ(_I3(Nz,Nx,Ny))	! d/dz Sigma  (@ W points)
      real Kgm(_I3(Nz,Nx,Ny))	! Eddy induced transport coef. (@ W points)
! Local
! ------------------------------------------------------------------------------

      Kgm=1.0d3*PMASK

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================

! ==============================================================================
      subroutine GMtransport(Sx,Sy,Kgm,Ustar,Vstar,Wstar)
!                            |--in---| |------out------|
      implicit none
! Global
#include "SIZE.h"
#include "OPERATORS.h"
#include "MASKS.h"
! Arguments
      real Sx(_I3(Nz,Nx,Ny))	! K13 = - d/dx Sigma / d/dz Sigma (@ U points)
      real Sy(_I3(Nz,Nx,Ny))	! K23 = - d/dy Sigma / d/dz Sigma (@ V points)
      real Kgm(_I3(Nz,Nx,Ny))	! Eddy induced transport coef. (@ W points)
      real Ustar(_I3(Nz,Nx,Ny))	! U* = -d/dz (Kgm*Sx)
      real Vstar(_I3(Nz,Nx,Ny))	! V* = -d/dz (Kgm*Sy)
      real Wstar(_I3(Nz,Nx,Ny))	! W* = d/dx (Kgm*Sx) + d/dy (Kgm*Sy)
! Local
      real Fx(_I3(Nz,Nx,Ny))
      real Fy(_I3(Nz,Nx,Ny))
      integer K,Kp1
! ------------------------------------------------------------------------------

! *****
! *****  MUST change this to not be barXZ(Sx) but barXZ(sigX)/ sigZ
! *****

      Fx=WMASK*CSHIFT(WMASK,DIM=_I3X,SHIFT=-1)
     &  *0.5*(Kgm+CSHIFT(Kgm,DIM=_I3X,SHIFT=-1))
     &  *0.5*(Sx+CSHIFT(Sx,DIM=_I3Z,SHIFT=-1))

      Fy=WMASK*CSHIFT(WMASK,DIM=_I3Y,SHIFT=-1)
     &  *0.5*(Kgm+CSHIFT(Kgm,DIM=_I3Y,SHIFT=-1))
     &  *0.5*(Sy+CSHIFT(Sy,DIM=_I3Z,SHIFT=-1))

      do K=1,Nz
       Kp1=mod(K,Nz)+1
       Ustar(_I3(K,:,:))=-(GRONIL)*rDZatP(K)*UMASK(_I3(K,:,:))*
     &   (Fx(_I3(K,:,:))-Fx(_I3(Kp1,:,:)))
       Vstar(_I3(K,:,:))=-(GRONIL)*rDZatP(K)*VMASK(_I3(K,:,:))*
     &   (Fy(_I3(K,:,:))-Fy(_I3(Kp1,:,:)))
      enddo

!     Wstar=pDdxOp*(CSHIFT(Fx,DIM=_I3X,SHIFT=+1)-Fx)
!    &     +pDdyOp*(CSHIFT(Fy,DIM=_I3Y,SHIFT=+1)-Fy)

      call UPDATE_W(
     I               Ustar, Vstar, Fx,
     O               Wstar )

#ifdef DEBUG_TESTS
      write(ICHAN,*) 'GMtransport: |U*|  =',maxval(abs(Ustar))
      write(ICHAN,*) 'GMtransport: |V*|  =',maxval(abs(Vstar))
      write(ICHAN,*) 'GMtransport: |W*|  =',maxval(abs(Wstar))
#endif

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================
