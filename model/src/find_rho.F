C $Header: /u/gcmpack/MITgcm/model/src/find_rho.F,v 1.3 1998/04/24 02:05:40 cnh Exp $

#include "CPP_EEOPTIONS.h"

! ==============================================================================
      subroutine FIND_RHO( bi, bj, iMin, iMax, jMin, jMax, myThid )
      implicit none
! Arguments
      integer bi,bj,iMin,iMax,jMin,jMax
      integer myThid
! Common
#include "SIZE.h"
#include "DYNVARS.h"
#include "PARAMS.h"
! Local
      INTEGER i,j,k
! ------------------------------------------------------------------------------

      do k=1,Nz
       do j=jMin,jMax
        do i=iMin,iMax
         rho(i,j,k,bi,bj)=rhonil*(
     &     sBeta*( salt(i,j,k,bi,bj)-sRef(k))
     &   -tAlpha*(theta(i,j,k,bi,bj)-tRef(k))
     &    )
        enddo
       enddo
      enddo

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================
