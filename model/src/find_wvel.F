C $Header: /u/gcmpack/MITgcm/model/src/Attic/find_wvel.F,v 1.5 1998/05/28 03:34:52 cnh Exp $

#include "CPP_EEOPTIONS.h"

! ==============================================================================
      subroutine FIND_WVEL( bi, bj, iMin, iMax, jMin, jMax, Wvel )
      implicit none
! Common
#include "SIZE.h"
#include "DYNVARS.h"
#include "GRID.h"
! Arguments
      integer bi,bj,iMin,iMax,jMin,jMax
      REAL Wvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz+1)
! Local
      integer i,j,k
      REAL Utrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      REAL Vtrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      REAL Wtrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
! ------------------------------------------------------------------------------

! Always have W=0 at bottom
      do j=1-OLy,sNy+OLy
       do i=1-OLx,sNx+OLx
        Wvel(i,j,Nz+1)=0.
        Wtrans(i,j)=0.
        Utrans(i,J)=0.
        Vtrans(i,j)=0.
       enddo
      enddo

! Bottom to top
      do k=Nz,1,-1

! Volume transport thru' Western faces
       do j=jMin,jMax
        do i=iMin,iMax
         Utrans(i,j)=_dyF(i,j,bi,bj)*dzF(k)*Uvel(i,j,k,bi,bj)
        enddo
       enddo

! Volume transport thru' Southern faces
       do j=jMin,jMax
        do i=iMin,iMax
         Vtrans(i,j)=_dxF(i,j,bi,bj)*dzF(k)*Vvel(i,j,k,bi,bj)
        enddo
       enddo

! Volume transport thru' Upper faces
       do j=jMin,jMax
        do i=iMin,iMax
         Wtrans(i,j)=Wtrans(i,j)-
     &     (Utrans(i+1,j)-Utrans(i,j)+Vtrans(i,j+1)-Vtrans(i,j))
        enddo
       enddo

! Wvel = Wtrans/area
       do j=jMin,jMax
        do i=iMin,iMax
         Wvel(i,j,k)=Wtrans(i,j)
     &          *_rdxF(i,j,bi,bj)*_rdyF(i,j,bi,bj)	! should be 1/zA(...)
        enddo
       enddo

      enddo

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================
