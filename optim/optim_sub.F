

      subroutine optim_sub(
     I                 nn
     &               )

c     ==================================================================
c     SUBROUTINE optim_sub
c     ==================================================================
c
c     o Initialization of optimization run.
c
c     started: Christian Eckert eckert@mit.edu 15-Feb-2000
c
c     changed: Christian Eckert eckert@mit.edu 10-Mar-2000
c
c              - Added ECCO layout.
c
c     changed:  Patrick Heimbach heimbach@mit.edu 19-Jun-2000
c               - finished, revised and debugged
c
c     ==================================================================
c     SUBROUTINE optim_sub
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "ctrl.h"
#include "optim.h"

c     == routine arguments ==

      integer nn

c     == local variables ==

      _RL   objf

#if defined (DYNAMIC)
      _RL   xx(nn)
      _RL   adxx(nn)
      _RL   dd(nn)
      _RL   gold(nn)
      _RL   xdiff(nn)
#elif defined (USE_POINTER) || (MAX_INDEPEND == 0)
      _RL   xx
      _RL   adxx
      _RL   dd(1)
      _RL   gold(1)
      _RL   xdiff(1)
      pointer (pxx,xx(1))
      pointer (padxx,adxx(1))
      pointer (pdd,dd)
      pointer (pgold,gold)
      pointer (pxdiff,xdiff)
#else
      integer nmax
      parameter( nmax = MAX_INDEPEND )
      _RL   xx(nmax)
      _RL   adxx(nmax)
      _RL   dd(nmax)
      _RL   gold(nmax)
      _RL   xdiff(nmax)
#endif

c--   Allocate memory for the control variables and the gradient vector.
#if defined(DYNAMIC)
#elif defined(USE_POINTER) || (MAX_INDEPEND == 0)
      call myalloc( pxx  ,  nn*REAL_BYTE )
      call myalloc( padxx,  nn*REAL_BYTE )
      call myalloc( pdd,    nn*REAL_BYTE )
      call myalloc( pgold,  nn*REAL_BYTE )
      call myalloc( pxdiff, nn*REAL_BYTE )
#endif

      integer ifail
      integer itmax
      logical loffline

c     == external ==

      external simul
      external lsline

c     == end of interface ==

c--   Initialisize the model and set a first guess of the control
c--   variables.
      call optim_initmod( nn, xx )

#if defined (DYNAMIC)
#elif defined(USE_POINTER) || (MAX_INDEPEND == 0)
#else
      if (nn .gt. nmax) then
        print*,' OPTIMUM: Not enough space.'
        print*,'          nmax = ',nmax
        print*,'            nn = ',nn
        print*
        print*,'          Set MAX_INDEPEND in Makefile .ge. ',nn
        print*
        stop   ' ... stopped in OPTIMUM.'
      endif
#endif

      print*, ' OPTIMUM: Calling lsopt for iteration: ',optimcycle
      print*, ' OPTIMUM: with nn, REAL_BYTE = ', nn, REAL_BYTE

      loffline = .true.
      itmax    = numiter

c--   Large scale optimization --> Gilbert & Lemarechal.
      call lsopt_top( nn, xx, objf, adxx
     &              , simul, lsline
     &              , epsx, fmin, epsg
     &              , iprint
     &              , itmax, nfunc, nupdate
     &              , dd, gold, xdiff
     &              , loffline
     &              , ifail )

      return
      end
