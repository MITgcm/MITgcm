#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"

#undef DEBUG_TESTS
#define _SMAX 1.d-3
#define _SMALL 1.d-12
#define _SLP_FAC 1.0d0

#define ICHAN 0
#define _GRAV_G 9.81d0
#define _RHO_NIL 999.8d0
#define _GRONIL _GRAV_G*_RHO_NIL

! ==============================================================================
      subroutine RediTensor(Temp,Salt,Kredigm,K31,K32,K33,nIter,DumpFlag)
!                           |---in--| |-------out-------|
      implicit none
! Global
#include "SIZE.h"
#include "MASKS.h"
! Arguments
      real Temp(_I3(Nz,Nx,Ny))  ! Locally referenced pot. temperature
      real Salt(_I3(Nz,Nx,Ny))  ! Locally referenced pot. temperature
      real Kredigm(_I3(Nz,Nx,Ny))       ! Redi diffusion coefficient
      real K31(_I3(Nz,Nx,Ny))   ! Redi tensor component
      real K32(_I3(Nz,Nx,Ny))   ! Redi tensor component
      real K33(_I3(Nz,Nx,Ny))   ! Redi tensor component
      integer nIter
      logical DumpFlag
! Local
      real rSmax
      real SigX(_I3(Nz,Nx,Ny))          ! d/dx Sigma (@ U points)
      real SigY(_I3(Nz,Nx,Ny))          ! d/dy Sigma (@ V points)
      real SigZ(_I3(Nz,Nx,Ny))          ! d/dz Sigma (@ W points)
      real SigZBz(_I3(Nz,Nx,Ny))        ! d/dz Sigma (@ T/S points)
      character*(10) suffix
      integer iErr
! ------------------------------------------------------------------------------

      rSmax=1.d0/(_SMAX)

      call gradSigma(Temp,Salt,SigX,SigY,SigZ)
!                    |--in---| |----out-----|

! Interpolate d/dz Sigma to T/S points
      SigZBz=0.5*(SigZ+CSHIFT(SigZ,DIM=_I3Z,SHIFT=+1))*PMASK
      SigZBz(_I3(Nz,:,:))=SigZ(_I3(Nz-1,:,:))

CC      call KtensorUface(SigX,SigY,SigZBz,rSmax,K13,K33)
CC!                       |---------in---------| |-out-|

CC      call KtensorVface(SigX,SigY,SigZBz,rSmax,K33,K23)
CC!                       |---------in---------| |-out-|

      call KtensorWface(SigX,SigY,SigZ,rSmax,K31,K32,K33)
!                       |--------in--------| |---out---|

      call GMRediCoefficient(SigX,SigY,SigZ,SigZBz,Kredigm,nIter,DumpFlag)
!                            |---------in--------| |-out-|

      IF(DumpFlag)THEN
        iErr = 0.
        WRITE(suffix,'(I10.10)') nIter
        CALL WRITE_DUMP_3D( Kredigm, Nx, Ny, Nz, 'Kredi.', suffix, iErr)
        CALL WRITE_DUMP_3D( K31, Nx, Ny, Nz, 'K31.', suffix, iErr)
        CALL WRITE_DUMP_3D( K32, Nx, Ny, Nz, 'K32.', suffix, iErr)
        CALL WRITE_DUMP_3D( K33, Nx, Ny, Nz, 'K33.', suffix, iErr)
!       CALL WRITE_DUMP_3D( SigX, Nx, Ny, Nz, 'SigX.', suffix, iErr)
!       CALL WRITE_DUMP_3D( SigY, Nx, Ny, Nz, 'SigY.', suffix, iErr)
!       CALL WRITE_DUMP_3D( SigZ, Nx, Ny, Nz, 'SigZ.', suffix, iErr)
      ENDIF

#ifdef DEBUG_TESTS
      write(ICHAN,*) 'KtensorWface: |Sigx|  =',maxval(abs(SigX))
      write(ICHAN,*) 'KtensorWface: |Sigy|  =',maxval(abs(SigY))
#endif

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
      real Temp(_I3(Nz,Nx,Ny))  ! Locally referenced pot. temperature
      real Salt(_I3(Nz,Nx,Ny))  ! Locally referenced pot. temperature
      real SigX(_I3(Nz,Nx,Ny))  ! d/dx Sigma
      real SigY(_I3(Nz,Nx,Ny))  ! d/dy Sigma
      real SigZ(_I3(Nz,Nx,Ny))  ! d/dz Sigma
! Local
      real sigma(_I3(Nz,Nx,Ny)) ! Locally referenced pot. density
      real sigUp(_I3(Nz,Nx,Ny)) ! Pot. density referenced to level below
      integer K
! ------------------------------------------------------------------------------

      DO K=1,Nz
       CALL UPDATE_RHO(Temp,Salt,K,'LINEAR','LOCAL',sigma)
!                      |-----------in------------| |out|
       sigma(_I3(K,:,:))=sigma(_I3(K,:,:))*PMASK(_I3(K,:,:))
       CALL UPDATE_RHO(Temp,Salt,K,'LINEAR','BELOW',sigUp)
!                      |-----------in------------| |out|
! Lateral gradients of potential density
       SigX(_I3(K,:,:))=_SLP_FAC*rDXatU*UMASK(_I3(K,:,:))*
     &  (sigma(_I3(K,:,:))-CSHIFT(sigma(_I3(K,:,:)),DIM=1,SHIFT=-1))
       SigY(_I3(K,:,:))=_SLP_FAC*rDYatV*VMASK(_I3(K,:,:))*
     &  (sigma(_I3(K,:,:))-CSHIFT(sigma(_I3(K,:,:)),DIM=2,SHIFT=-1))

      ENDDO
      sigUp=sigUp*PMASK

! Static stability at W points (K=1 is at surface)
      DO K=2,Nz
       SigZ(_I3(K,:,:))=rDZatW(K)*(_GRONIL)*WMASK(_I3(K,:,:))*
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
      real SigX(_I3(Nz,Nx,Ny))  ! d/dx Sigma  (@ U points)
      real SigY(_I3(Nz,Nx,Ny))  ! d/dy Sigma  (@ V points)
      real SigZ(_I3(Nz,Nx,Ny))  ! d/dz Sigma  (interpd to T/S points)
      real rSmax                ! reciprocal maximum allowed slope
      real Sx(_I3(Nz,Nx,Ny))    ! Sx = - d/dx Sigma / d/dz Sigma
      real Sy(_I3(Nz,Nx,Ny))    ! Sy = - d/dy Sigma / d/dz Sigma
! Local
      real tmp(_I3(Nz,Nx,Ny))
      real tmp2(_I3(Nz,Nx,Ny))
! ------------------------------------------------------------------------------

! Interpolate d/dy Sigma and d/dz Sigma to U points
      Sy=0.5d0*(SigY+CSHIFT(SigY,DIM=_I3Y,SHIFT=+1))*PMASK      ! Interp to T
      Sy=0.5d0*(Sy+CSHIFT(Sy,DIM=_I3X,SHIFT=-1))*UMASK          ! Interp to U
      tmp=0.5d0*(SigZ+CSHIFT(SigZ,DIM=_I3X,SHIFT=-1))           ! d/dz sigma @ U

! Limit maximum slope
!     tmp=1.d0/max(tmp,-_SMALL-sqrt(SigX*SigX+Sy*Sy)*rSmax)     ! check
      tmp2=-_SMALL-sqrt(SigX*SigX+Sy*Sy)*rSmax
      where (tmp.gt.tmp2) tmp=tmp2                              ! check
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
      real SigX(_I3(Nz,Nx,Ny))  ! d/dx Sigma  (@ U points)
      real SigY(_I3(Nz,Nx,Ny))  ! d/dy Sigma  (@ V points)
      real SigZ(_I3(Nz,Nx,Ny))  ! d/dz Sigma  (interpd to T/S points)
      real rSmax                ! reciprocal maximum allowed slope
      real Sx(_I3(Nz,Nx,Ny))    ! Sx = - d/dx Sigma / d/dz Sigma
      real Sy(_I3(Nz,Nx,Ny))    ! Sy = - d/dy Sigma / d/dz Sigma
! Local
      real tmp(_I3(Nz,Nx,Ny))
      real tmp2(_I3(Nz,Nx,Ny))
! ------------------------------------------------------------------------------

! Interpolate d/dx Sigma and d/dz Sigma to V points
      Sx=0.5d0*(SigX+CSHIFT(SigX,DIM=_I3X,SHIFT=+1))*PMASK      ! Interp to T
      Sx=0.5d0*(Sx+CSHIFT(Sx,DIM=_I3Y,SHIFT=-1))*VMASK          ! Interp to V
      tmp=0.5d0*(SigZ+CSHIFT(SigZ,DIM=_I3Y,SHIFT=-1))           ! d/dz sigma @ V

! Limit maximum slope
!     tmp=1.d0/max(tmp,-_SMALL-sqrt(Sx*Sx+SigY*SigY)*rSmax)     ! check
      tmp2=-_SMALL-sqrt(Sx*Sx+SigY*SigY)*rSmax
      where (tmp.gt.tmp2) tmp=tmp2                              ! check
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
      real SigX(_I3(Nz,Nx,Ny))  ! d/dx Sigma  (@ U points)
      real SigY(_I3(Nz,Nx,Ny))  ! d/dy Sigma  (@ V points)
      real SigZ(_I3(Nz,Nx,Ny))  ! d/dz Sigma  (@ W points)
      real rSmax                ! reciprocal of maximum allowed slope
      real Sx(_I3(Nz,Nx,Ny))    ! Sx = - d/dx Sigma / d/dz Sigma
      real Sy(_I3(Nz,Nx,Ny))    ! Sy = - d/dy Sigma / d/dz Sigma
      real Sxy2(_I3(Nz,Nx,Ny))  ! Sxy2 = (grad_h Sigma)^2 / (d/dz Sigma)^2
! Local
      real tmp(_I3(Nz,Nx,Ny))
      real tmp2(_I3(Nz,Nx,Ny))
! ------------------------------------------------------------------------------

! Interpolate d/dx Sigma and d/dy Sigma to W points
      Sx=0.5d0*(SigX+CSHIFT(SigX,DIM=_I3X,SHIFT=+1))*PMASK      ! Interp to T
      Sx=0.5d0*(Sx+CSHIFT(Sx,DIM=_I3Z,SHIFT=-1))*WMASK          ! Interp to W
      Sy=0.5d0*(SigY+CSHIFT(SigY,DIM=_I3Y,SHIFT=+1))*PMASK      ! Interp to T
      Sy=0.5d0*(Sy+CSHIFT(Sy,DIM=_I3Z,SHIFT=-1))*WMASK          ! Interp to W
      tmp=SigZ                                                  ! d/dz sigma @ W

! Limit maximum slope
      Sxy2=sqrt(Sx*Sx+Sy*Sy)
!     tmp=1.d0/max(SigZ,-_SMALL-Sxy2*rSmax)                     ! check
      tmp2=-_SMALL-Sxy2*rSmax
      where (tmp.gt.tmp2) tmp=tmp2                              ! check
      tmp=1.d0/tmp

      Sx=-2.*Sx*tmp
      Sy=-2.*Sy*tmp
      Sxy2=0.25*(Sx*Sx+Sy*Sy)

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
      subroutine GMRediCoefficient(SigX,SigY,SigZ,SigZBz,Kredigm,nIter_arg,DumpFlag)
!                                  |---------in--------| |-out-|
      implicit none
! Global
#include "SIZE.h"
#include "PARAMS.h"
#include "MASKS.h"
! Arguments
      real SigX(_I3(Nz,Nx,Ny))  ! d/dx Sigma  (@ U points)
      real SigY(_I3(Nz,Nx,Ny))  ! d/dy Sigma  (@ V points)
      real SigZ(_I3(Nz,Nx,Ny))  ! d/dz Sigma  (@ W points)
      real SigZBz(_I3(Nz,Nx,Ny))! d/dz Sigma  (@ T points)
      real Kredigm(_I3(Nz,Nx,Ny)) ! Redi/GM eddy coefficient (@ T points)
      integer nIter_arg
      logical DumpFlag
! Local
      real rSmax,sqtgro,gRro
      real Sx(_I3(Nz,Nx,Ny))    ! d/dx Sigma  (@ T points)
      real Sy(_I3(Nz,Nx,Ny))    ! d/dy Sigma  (@ T points)
      real tmp(_I3(Nz,Nx,Ny))
      real tmp2(_I3(Nz,Nx,Ny))
      real M2(_I3(Nz,Nx,Ny))    ! M2 = g/rho0 |grad_h Sigma|
      real FrRi(Nx,Ny)
      real totRiDep(Nx,Ny),depRiZ(Nz)
      integer iErr,K,I,J
      character*(10) suffix
! ------------------------------------------------------------------------------

      rSmax=1.d0/(_SMAX)
      gRro=_GRAV_G/_RHO_NIL
      sqtgro=sqrt(gRro)

      Sx=0.5d0*(SigX+CSHIFT(SigX,DIM=_I3X,SHIFT=+1))*PMASK      ! Interp to T
      Sy=0.5d0*(SigY+CSHIFT(SigY,DIM=_I3Y,SHIFT=+1))*PMASK      ! Interp to T
      M2=sqrt(Sx*Sx+Sy*Sy)                                      ! M^2 * RONIL/G
      tmp=SigZBz
!             ^^  found error here in version 1 *********************

      tmp2=-_SMALL-M2*rSmax
      where (tmp.gt.tmp2) tmp=tmp2                              ! slope check

      M2=gRro*M2                                                ! M^2
      tmp=-gRro*tmp                                             ! N^2
!v1   tmp2=M2/sqrt(tmp)                                         ! |f|/sqrt(Ri)

      tmp2=M2/tmp               ! v2                            ! M^2/N^2
      tmp=-gRro*SigZBz          ! v2                            ! N^2 (no limit)
      tmp2=tmp2*sqrt(abs(tmp))  ! v2                            ! |f|/sqrt(Ri)

      depRiZ(1)=0.5*DELPS(1)
      do K=2,Nz
       depRiZ(K)=depRiZ(K-1)+0.5*(DELPS(K-1)+DELPS(K))
      enddo
      depRiZ=depRiZ/(_GRONIL)

      totRiDep=0.
      FrRi=0.
      do K=1,11                 ! Avg over top ???? metres
       totRiDep=totRiDep+DELPS(K)*PMASK(_I3(K,:,:))
       FrRi=FrRi+DELPS(K)*tmp2(_I3(K,:,:))              ! |f|/sqrt(Ri)
      enddo
      where (totRiDep.ne.0.) totRiDep=1./totRiDep
      FrRi=FrRi*totRiDep                                ! avg |f|/sqrt(Ri)

      do K=1,Nz
       do J=1,Ny
        do I=1,Nx
         Kredigm(_I3(K,I,J))=PMASK(_I3(K,I,J))*(
     &             0000.0d0                             ! Back-ground value
!    &   +0.02d0*400.d3*50.d3*FrRi(I,J)         ! Richardson # depend.
!    &   +1000.d0*exp(-((9.*0.05*(float(J)-20.5))**2))  ! Equatorial band
     &            )
!    &   *(0.1+0.9*exp(-depRiZ(K)/500.))                ! Depth depend.
        enddo
       enddo
      enddo
!dbg  where (Kredigm.gt.2500.) Kredigm=2500.            ! Stability cut-off

      IF(DumpFlag)THEN
        iErr = 0.
        WRITE(suffix,'(I10.10)') nIter_arg
        CALL WRITE_DUMP_3D( M2, Nx, Ny, Nz, 'rRiM2.', suffix, iErr)
        CALL WRITE_DUMP_3D( tmp2, Nx, Ny, Nz, 'rRi3.', suffix, iErr)
        CALL WRITE_DUMP_2D( FrRi, Nx, Ny, 'rRi.', suffix, iErr)
      ENDIF

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================
