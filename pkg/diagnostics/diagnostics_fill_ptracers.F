       subroutine diagnostics_fill_ptracers(myThid)
       implicit none
#include "PACKAGES_CONFIG.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "CPP_OPTIONS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS.h"
#endif

      integer myThid 
      _RL dummy(1-OLx:sNx+Olx,1-Oly:sNy+Oly,Nr,Nsx,Nsy)
      integer i,j,K,N,bi,bj
      character *8 diagname
#ifdef ALLOW_PTRACERS
      diagname = '        '

      do N = 1,PTRACERS_numInUse

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = ptracers(i,j,K,bi,bj,N)
          enddo
          enddo
         enddo
        enddo
        enddo
        diagname = '        '
        write(diagname,(a4,i1))'TRAC',N
        call fill_diagnostics(myThid,diagname,0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*hFacW(i,j,K,bi,bj) *
     .       0.5*(ptracers(i,j,K,bi,bj,N)+ptracers(i-1,j,K,bi,bj,N))
          enddo
          enddo
         enddo
        enddo
        enddo
        diagname = '        '
        write(diagname,(a5,i1))'UTRAC',N
        call fill_diagnostics(myThid,diagname,0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = vVel(i,j,K,bi,bj)*hFacS(i,j,K,bi,bj) *
     .       0.5*(ptracers(i,j,K,bi,bj,N)+ptracers(i,j-1,K,bi,bj,N))
          enddo
          enddo
         enddo
        enddo
        enddo
        diagname = '        '
        write(diagname,(a5,i1))'VTRAC',N
        call fill_diagnostics(myThid,diagname,0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = 0.
          enddo
          enddo
         enddo
        enddo
        enddo
        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=2,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = wVel(i,j,K,bi,bj) *
     .       0.5*(ptracers(i,j,K,bi,bj,N)+ptracers(i,j,K-1,bi,bj,N))
          enddo
          enddo
         enddo
        enddo
        enddo
        diagname = '        '
        write(diagname,(a5,i1))'WTRAC',N
        call fill_diagnostics(myThid,diagname,0,Nr,0,1,1,dummy)

      enddo
#endif

      return
      end
