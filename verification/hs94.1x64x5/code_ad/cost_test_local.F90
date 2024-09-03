#include "PACKAGES_CONFIG.h"
! Copied from default CPP_EEMACROS.h
! To avoid this, we need to find a way to include CPP_EEMACROS.h
#define _RL Real*8

!BOP
!     !ROUTINE: COST_TEST_LOCAL
!     !INTERFACE:
subroutine cost_test_local ( &
     sNx, sNy, nSx, nSy, OLx, OLy, Nr, &
     myBxLo, myBxHi, myByLo, myByHi, &
     myXGlobalLo, myYGlobalLo, &
     theta, &
     objf_test, &
     myThid )
  !     *==========================================================*
  !     | SUBROUTINE COST_TEST_LOCAL
  !     | o the subroutine computes the sum of temperatures
  !     |   in a band 30 < j < 40
  !     *==========================================================*
  !
  !     !USES:
  implicit none

  !   !INPUT/OUTPUT PARAMETERS:
  ! sNx,sNy,nSx,nSy,OLx,OLy,Nr  :: array boundaries (set it SIZE.h)
  ! myByLo,myByHi,myBxLo,myBxHi :: bi/bj loop boundaries for myThid
  ! myXGlobalLo, myYGlobalLo    :: smallest indices of global fields
  ! myThid        :: Thread number for this instance of the routine.
  ! theta         :: temperature
  ! objf_test     :: objective/cost function contribution defined in cost.h
  ! This requires TAF_FORTRAN_VERS > F77
  integer, intent(in) :: sNx, sNy, nSx, nSy, OLx, OLy, Nr, &
       myByLo, myByHi, myBxLo, myBxHi, myXGlobalLo, myYGlobalLo, myThid
  _RL, intent(in)     :: theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
  _RL, intent(inout)  :: objf_test(nSx,nSy)

#ifdef ALLOW_COST
  !   !LOCAL VARIABLES:
  ! loop indices
  integer :: i, j, k, ig, jg
  integer :: bi, bj
!EOP

  do bj=myByLo,myByHi
   do bi=myBxLo,myBxHi
    objf_test(bi,bj)= 0. _d 0
   enddo
  enddo

  k = 1

  ! Calculate cost function on tile of this instance
  do bj=myByLo,myByHi
   do bi=myBxLo,myBxHi
    do j=1,sNy
     jg = myYGlobalLo-1+(bj-1)*sNy+j
     do i=1,sNx
      ig = myXGlobalLo-1+(bi-1)*sNx+i
      if ((jg .gt.30) .and. (jg .lt.40)) then
       objf_test(bi,bj) = objf_test(bi,bj) + theta(i,j,k,bi,bj)
       write(*,'(a,F10.1,3(x,i4),a,4(x,i4))') &
            'objf_test  ', objf_test(bi,bj), ig, jg, k, ' TILE ', i, j, bi, bj
      endif
     enddo
    enddo

    objf_test(bi,bj) = objf_test(bi,bj) / 9. _d 0

   enddo
  enddo
#endif /* ALLOW_COST */

  return
  ! This requires TAF_FORTRAN_VERS > F77
end subroutine cost_test_local
