C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_fill_diagnostics.F,v 1.1 2004/06/14 22:37:49 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

       subroutine ptracers_fill_diagnostics(myThid)
       implicit none

#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "PTRACERS.h"

      integer myThid 
#ifdef ALLOW_DIAGNOSTICS
      _RL dummy(1-OLx:sNx+Olx,1-Oly:sNy+Oly,Nr,Nsx,Nsy)
      integer i,j,K,N,bi,bj
      character *8 diagname
      INTEGER km1

      diagname = '        '

      do N = 1,PTRACERS_numInUse

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = ptracer(i,j,K,bi,bj,N)
          enddo
          enddo
         enddo
        enddo
        enddo
        diagname = '        '
        write(diagname,'(a4,i1)') 'TRAC',N
        call fill_diagnostics(myThid,diagname,0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*hFacW(i,j,K,bi,bj) *
     .       0.5*(ptracer(i,j,K,bi,bj,N)+ptracer(i-1,j,K,bi,bj,N))
          enddo
          enddo
         enddo
        enddo
        enddo
        diagname = '        '
        write(diagname,'(a5,i1)') 'UTRAC',N
        call fill_diagnostics(myThid,diagname,0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = vVel(i,j,K,bi,bj)*hFacS(i,j,K,bi,bj) *
     .       0.5*(ptracer(i,j,K,bi,bj,N)+ptracer(i,j-1,K,bi,bj,N))
          enddo
          enddo
         enddo
        enddo
        enddo
        diagname = '        '
        write(diagname,'(a5,i1)') 'VTRAC',N
        call fill_diagnostics(myThid,diagname,0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          km1 = MAX(K-1,1)
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = wVel(i,j,K,bi,bj) *
     .       0.5*(ptracer(i,j,K,bi,bj,N)+ptracer(i,j,km1,bi,bj,N))
          enddo
          enddo
         enddo
        enddo
        enddo
        diagname = '        '
        write(diagname,'(a5,i1)') 'WTRAC',N
        call fill_diagnostics(myThid,diagname,0,Nr,0,1,1,dummy)

      enddo
#endif /* ALLOW_DIAGNOSTICS */

      return
      end
