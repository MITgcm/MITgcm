      subroutine diagnostics_initialise (myThid)
C***********************************************************************
C
C  PURPOSE
C     Set Pointer Locations for Diagnostics in diag_list
C
C  INPUT:
C   myThid ..... Process(or) number
C
C***********************************************************************
      implicit none

#include "EEPARAMS.h"
#include "CPP_OPTIONS.h"
#include "SIZE.h"

#ifdef ALLOW_FIZHI
#include "fizhi_SIZE.h"
#else
       integer Nrphys
       parameter (Nrphys=1)
#endif

#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer myThid

      integer ndiagcount
      integer m,mm,n
      logical found
      integer i,j,bi,bj

C **********************************************************************
C **** Calculate pointers for diagnostics set to non-zero frequency ****
C **********************************************************************

      call diagnostics_init_vals (myThid)

      ndiagcount = 0
      do n=1,nlists
       do m=1,nfields(n)

        found = .false.
        do mm=1,ndiagt   ! Search all possible model diagnostics
         if( flds(m,n).eq.cdiag(mm) ) then
          call setdiag (myThid,mm,ndiagcount)
          found = .true.
         endif
        enddo
        if( .not.found .and. myThid.eq.1 ) then
         print *, 'Note:  ',flds(m,n),' is not a Diagnostic.'
        endif

       enddo
      enddo

C **********************************************************************
C **** Zero out the qdiag array which accumulates during integration****
C **********************************************************************
      do bj=myByLo(myThid), myByHi(myThid)
      do bi=myBxLo(myThid), myBxHi(myThid)
       do n = 1,numdiags
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,n,bi,bj) = 0.
        enddo
        enddo
       enddo
      enddo
      enddo

      return
      end
