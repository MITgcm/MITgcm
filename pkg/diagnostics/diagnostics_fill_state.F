       subroutine diagnostics_fill_state(myThid)
       implicit none
#include "PACKAGES_CONFIG.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "CPP_OPTIONS.h"
#include "GRID.h"
#include "DYNVARS.h"
# ifdef ALLOW_PTRACERS
#  include "PTRACERS.h"
# endif

      integer myThid 
      _RL dummy(1-OLx:sNx+Olx,1-Oly:sNy+Oly,Nr,Nsx,Nsy)
      integer i,j,K,bi,bj

        call fill_diagnostics(myThid,'ETAN    ',0,1,0,1,1,etaN)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,1,bi,bj) = etaN(i,j,bi,bj)*etaN(i,j,bi,bj)
          enddo
          enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'ETANSQ  ',0,1,0,1,1,dummy)

        call fill_diagnostics(myThid,'UVEL    ',0,Nr,0,1,1,uVel)
        call fill_diagnostics(myThid,'VVEL    ',0,Nr,0,1,1,vVel)
        call fill_diagnostics(myThid,'WVEL    ',0,Nr,0,1,1,wVel)
        call fill_diagnostics(myThid,'THETA   ',0,Nr,0,1,1,theta)
        call fill_diagnostics(myThid,'SALT    ',0,Nr,0,1,1,salt)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*uVel(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'UVELSQ  ',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = vVel(i,j,K,bi,bj)*vVel(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'VVELSQ  ',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = wVel(i,j,K,bi,bj)*wVel(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'WVELSQ  ',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = theta(i,j,K,bi,bj)*theta(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'THETASQ ',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = salt(i,j,K,bi,bj)*salt(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'SALTSQ  ',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*vVel(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'UVELVVEL',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*theta(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'UVELTH  ',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = vVel(i,j,K,bi,bj)*theta(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'VVELTH  ',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=2,Nr
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
           dummy(i,j,K,bi,bj) = wVel(i,j,K,bi,bj)*0.5*
     .                 (theta(i,j,K,bi,bj)+theta(i,j,K-1,bi,bj))
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'WVELTH  ',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*salt(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'UVELSLT ',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = vVel(i,j,K,bi,bj)*salt(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'VVELSLT ',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = wVel(i,j,K,bi,bj)*0.5*
     .                 (salt(i,j,K,bi,bj)+salt(i,j,K-1,bi,bj))
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'WVELSLT ',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*hFacW(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'UVELMASS',0,Nr,0,1,1,salt)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = vVel(i,j,K,bi,bj)*hFacS(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'VVELMASS',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*
     .         0.5*(theta(i,j,K,bi,bj)+theta(i-1,j,K,bi,bj))
     .                                * hFacW(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'UTHMASS ',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = vVel(i,j,K,bi,bj)*
     .         0.5*(theta(i,j,K,bi,bj)+theta(i,j-1,K,bi,bj))
     .                                * hFacW(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'VTHMASS ',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*
     .         0.5*(salt(i,j,K,bi,bj)+salt(i-1,j,K,bi,bj))
     .                                * hFacW(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'USLTMASS',0,Nr,0,1,1,dummy)

        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do K=1,Nr
          do j = 1,sNy
          do i = 1,sNx
           dummy(i,j,K,bi,bj) = vVel(i,j,K,bi,bj)*
     .         0.5*(salt(i,j,K,bi,bj)+salt(i,j-1,K,bi,bj))
     .                                * hFacW(i,j,K,bi,bj)
          enddo
          enddo
         enddo
        enddo
        enddo
        call fill_diagnostics(myThid,'VSLTMASS',0,Nr,0,1,1,dummy)


      return
      end
