       subroutine diagnostics_fill_ptracers(myThid)
       implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "CPP_OPTIONS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "PTRACERS.h"

      integer myThid 
      _RL dummy(1-OLx:sNx+Olx,1-Oly:sNy+Oly,Nr,Nsx,Nsy)
      integer i,j,K,N,bi,bj
      character *8 diagname
      diagname = '        '

      do N = 1,PTRACERS_numInUse

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1-OLy,sNy+Oly
          do i = 1-OLx,sNx+Olx
           dummy(i,j,K,bi,bj) = ptracers(i,j,K,bi,bj,N) * 
     .                      hFacC(i,j,K,bi,bj)*drF(K)
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
          do j = 1-OLy,sNy+Oly
          do i = 1-OLx,sNx+Olx
           dummy(i,j,K,bi,bj) = ptracers(i,j,K,bi,bj,N) * 
     .                      uVel(i,j,K,bi,bj)*hFacW(i,j,K,bi,bj)
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
          do j = 1-OLy,sNy+Oly
          do i = 1-OLx,sNx+Olx
           dummy(i,j,K,bi,bj) = ptracers(i,j,K,bi,bj,N) * 
     .                      vVel(i,j,K,bi,bj)*hFacS(i,j,K,bi,bj)
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
          do j = 1-OLy,sNy+Oly
          do i = 1-OLx,sNx+Olx
           dummy(i,j,K,bi,bj) = ptracers(i,j,K,bi,bj,N) * 
     .                      wVel(i,j,K,bi,bj)*hFacC(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        diagname = '        '
        write(diagname,(a5,i1))'WTRAC',N
        call fill_diagnostics(myThid,diagname,0,Nr,0,1,1,dummy)

      enddo

      return
      end
