!=======================================================================
      subroutine mitcplr_sortranks( nranks, ranks )
      implicit none
! Arguments
      integer nranks
      integer ranks(nranks)
! Local
      integer j,k,itmp
!     ------------------------------------------------------------------

      do j=1,nranks-1
       do k=j+1,nranks
        if ( ranks(k) .lt. ranks(j) ) then
         itmp=ranks(k)
         ranks(k)=ranks(j)
         ranks(j)=itmp
        endif
       enddo
      enddo

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
