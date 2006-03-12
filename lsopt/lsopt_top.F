
      subroutine lsopt_top( nn, xx, ff, gg, simul, optline
     $                  , epsx, fmin, epsg
     $                  , iprint, itmax, nfunc, nupdate
     $                  , dd, gold, xdiff
     $                  , loffline
     $                  , ifail )

c     ==================================================================
c     SUBROUTINE lsopt_top
c     ==================================================================
c
c     o uses a set of control variables, their adjoint variables,
c       and a cost function value
c       to compute an improved set of controls with respect to the
c       cost function via a
c       variable-storage Quasi-Newton method
c
c     o Reference: J.C. Gilbert & C. Lemarechal
c                  Some numerical experiments with variable-storage
c                  quasi-Newton algorithms
c                  Mathematical Programming 45 (1989), pp. 407-435
c
c     o started: ??? not reproducible
c
c     o changed: Patrick Heimbach, MIT/EAPS
c                
c     o Version: 2.0, 24-Feb-2000: 
c                    (Version 1.0 is considered as version
c                     starting from which changes were made).
c                   - severe changes in structure including various
c                     bug-fixes to enable multi-optimization runs;
c                   - routine lsoptw incorporated into lsoptv
c                   - optimization iteration loop restructured
c                   - complete restructuring of handling
c                     cold start cases
c                   - number of 3 control flags for error handling
c                     (indic, moderl, ifail) reduced to 1 (ifail)
c                     and homogenized with routine lsline
c
c     o Version: 2.1, 29-Feb-2000:
c                   - handling of case ifail = 6 changed;
c                     leads to stop of optimization
c                     (similar to case ifail = 8)
c                   - logical lphprint included
c
c     ==================================================================
c     SUBROUTINE lsopt_top
c     ==================================================================

      implicit none

ccc#include <blas1.h>

c-----------------------------------------
c declare arguments
c-----------------------------------------
      integer nn, iprint, itmax, nfunc, nupdate, ifail

      double precision  xx(nn), ff, gg(nn), epsx, fmin, epsg
      double precision  dd(nn), gold(nn), xdiff(nn)

cph(
      integer phniter0, phniterold
      double precision phff
      COMMON /PH_OPTI/ phniter0, phniterold, phff
cph)

      external simul, optline

c-----------------------------------------
C declare local variables
c-----------------------------------------
      logical cold, lphprint, loffline
      parameter (lphprint = .true.)

      integer mm, mupd, jmin, jmax, indic, isize, REAL_BYTE
      integer i, iiter, ifunc

      double precision   fupd
      double precision   r1, tmin, tmax, tact, gnorm, gnorm0, eps1
      double precision   fold, ys
      double precision   dotdg

      external DDOT, DNRM2, DSCAL
      double precision     DDOT, DNRM2

c-----------------------------------------
c parameters
c-----------------------------------------

      double precision rmin
      parameter( rmin = 1.e-20 )

      character*(*) iform
      parameter(iform='(i5,2x,1pe8.1,1x,i5,4x,1pe11.4,3(2x,1pe8.1))' )

c     ==================================================================
c
c-----------------------------------------
c initialization
c-----------------------------------------
      cold  = .true.
      fupd  = 1.0e+10
      indic = 0
      tmin  = 0.
      tmax  = 1.0e+10
      tact  = 1.0
cph(
      phniterold = 0
cph)
      iiter = 0
      eps1  = 1.0
      ifunc = 0
      ifail = 0
      gnorm0 = 1.

c-----------------------------------------
c initialization for dd and dds
c-----------------------------------------

      jmin  = 1
      jmax  = 0

      mm    = nn
      mupd  = nupdate

      REAL_BYTE = 4
      isize = REAL_BYTE

c-----------------------------------------
c print information
c-----------------------------------------
      if (iprint .ge. 1) then
         print '(2x,a)',
     $         '==============================================='
         print '(2x,a)',
     $         '===         O P T I M I Z A T I O N         ==='
         print '(2x,a)',
     $         '==============================================='
         print '(a,i9)'
     $        , '  number of control variables.......', nn
         print '(a,e9.2)'
     $        , '  precision on x....................', epsx
         print '(a,e9.2)'
     $        , '  precision on g....................', epsg
         print '(a,e9.2)'
     $        , '  expected optimal function value...', fmin
         print '(a,i9)'
     $        , '  maximal number of iterations......', itmax
         print '(a,i9)'
     $        , '  maximal number of simulations.....', nfunc
         print '(a,i9)'
     $        , '  information level.................', iprint
         print '(a,i9)'
     $        , '  number of updates.................', nupdate
         print '(a,i9)'
     $        , '  size of used memory...............', 3*nn
      endif

c-----------------------------------------
c check arguments
c-----------------------------------------

      if (nn .le. 0) then
         if (iprint.ge.1) then
            print '(a,i6)'  , '  ERROR : n     = ', nn
         endif
         ifail = 1
         goto 999
      endif

      if (itmax .lt. 0) then
         if (iprint.ge.1) then
            print '(a,i6)'  , '  ERROR : itmax = ', itmax
         endif
         ifail = 1
         goto 999
      endif

      if (nfunc .le. 0) then
         if (iprint.ge.10) then
            print '(a,i6)'  , '  ERROR : nfunc  = ', nfunc
         endif
         ifail = 1
         goto 999
      endif

      if (epsx .le. 0.) then
         if (iprint.ge.1) then
            print '(a,e9.2)', '  ERROR : epsx = ', epsx
         endif
         ifail = 1
         goto 999
      endif

      if (epsg .le. 0.) then
         if (iprint.ge.1) then
            print '(a,e9.2)', '  ERROR : epsg  = ', epsg
         endif
         ifail = 1
         goto 999
      endif

      if (epsg .gt. 1.) then
         if (iprint.ge.1) then
            print '(a,e9.2)', '  ERROR : epsg  = ', epsg
         endif
         ifail = 1
         goto 999
      endif

cph(
      print *, 'pathei: vor instore '
cph)
      call instore( mm, fupd, gnorm0, isize, mupd, jmin, jmax, cold,
     &              ifail )

cph(
      phff = fupd
cph)

c-----------------------------------------
c check warm start parameters
c-----------------------------------------
      if (ifail .ne. 0) then
         if (iprint.ge.1) then
            print '(a)', ' ERROR : reading restart file'
         end if
         ifail = 2
         goto 999
      end if

      if (isize .ne. REAL_BYTE) then
         if (iprint.ge.1) then
            print '(a)', ' ERROR : uncompatible floating point format'
         end if
         ifail = 2
         goto 999
      end if

      if (mupd .lt. 1) then
         if (iprint .ge. 1) then
            print '(a)', ' ERROR : m is set too small in instore'
         endif
         ifail = 2
         goto 999
      endif

c-----------------------------------------
c cold start or warm restart ?
c-----------------------------------------

      if (cold) then
c--- start if cold start ---
         if (lphprint) then
            print '(a)', 'pathei-lsopt: cold start'
         end if

         print *, 'pathei-lsopt vor simul', nn
         print *, 'pathei-lsopt xx(1), gg(1) ', xx(1), gg(1)

         call simul( indic, nn, xx, ff, gg )

         print *, 'pathei: nach simul:   nn, ff = ', nn, ff
         print *, 'pathei: nach simul:   xx(1), gg(1) = ', xx(1), gg(1)

         do i = 1, nn
            xdiff(i) = 1.
         end do

cph(
      print *, 'pathei: vor dostore '
cph)
         call dostore( nn, xx, .true., 1 )
         call dostore( nn, gg, .true., 2 )
         call dostore( nn, xdiff, .true., 3 )
               
cph(
      print *, 'pathei: vor lswri '
cph)
cph         call lswri( isize, iiter, nn, xx, gg, lphprint )

cph(
      print *, 'pathei: vor gnorm0 '
cph)
         gnorm0 = DNRM2( nn, gg, 1 )
cph(
      print *, 'pathei: gnorm0 = ', gnorm0
cph)
         if (gnorm0 .lt. rmin) then
            ifail = 3
           goto 1000
         endif

cph(
         phff = ff
cph)

         if (lphprint)
     &        print *, 'pathei-lsopt: cold; first call simul: ff = ', 
     &        phff

c--- end if cold start ---
      else
c--- start if warm start ---
         if (mm .ne. nn) then
            if (iprint.ge.1) then
               print '(a,i6)'
     $              , '  ERROR : inconsistent nn = ', mm
            endif
            ifail = 2
            goto 999
         endif
         if (mupd .ne. nupdate) then
            if (iprint.ge.1) then
               print '(a,i6)'
     $              , '  ERROR : inconsistent nupdate = ', mupd
            endif
            ifail = 2
            goto 999
         endif
         if (jmin .lt. 1) then
            if (iprint.ge.1) then
               print '(a,i6)'
     $              , '  ERROR : inconsistent jmin = ', jmin
            endif
            ifail = 2
            goto 999
         endif
         if (jmin .gt. mupd) then
            if (iprint.ge.1) then
               print '(a,i6)'
     $              , '  ERROR : inconsistent jmin = ', jmin
            endif
            ifail = 2
            goto 999
         endif
         if (jmax .gt. mupd) then
            if (iprint.ge.1) then
               print '(a,i6)'
     $              , '  ERROR : inconsistent jmax = ', jmax
            endif
            ifail = 2
            goto 999
         endif
         
         if (lphprint) then
            print *, 'pathei-lsopt: warm start; read via dostore'
            print *
         endif

         call dostore( nn, xx, .false., 1 )
         call dostore( nn, gg, .false., 2 )
         ff = fupd

cph(
         phff = ff
cph)

         if (lphprint)
     &        print *, 'pathei-lsopt: warm; first dostore read: ff = ',
     &        ff

c--- end if warm start ---
      endif

      if (iprint .ge. 1) then
         print '(2a)', '  Itn      Step  Nfun      Objective    '
     $        , 'Norm G    Norm X   Norm (X(k-1)-X(k))'
      end if

c-----------------------------------------
c print information line
c-----------------------------------------
         if (cold) then
            print iform, iiter, tact, ifunc, ff, gnorm0
     $           , DNRM2( nn, xx, 1 ), 0.

Cml            write(94,'(i5,2x,1pe11.4,4(2x,1pe8.1))')
Cml     &           iiter, ff, gnorm0, tact, 
Cml     &           DNRM2( nn, xx, 1 ), 0.

            if ( itmax .EQ. 0 ) then
               ifail = 10
               goto 1000
            end if
         end if

c=======================================================================
c begin of loop
c compute x(k+1) out of x(k) + t*d, where  t > 0.
c=======================================================================

      do iiter = 1, itmax

         if (lphprint) then
            print *, 'pathei-lsopt: ++++++++++++++++++++++++'
            print *, 'pathei-lsopt: entering iiter =', iiter
         end if

c-----------------------------------------
c store old values
c-----------------------------------------
      do i = 1, nn
         gold(i) = gg(i)
      end do

      fold = ff
cph(
      phniter0 = iiter
      phff = ff
cph)

c-----------------------------------------
c compute new dd and xx
c-----------------------------------------
c
      call lsupdxx(
     &     nn, ifail, lphprint
     &     , jmin, jmax, nupdate
     &     , ff, fmin, fold, gnorm0, dotdg
     &     , gg, dd, xx, xdiff
     &     , tmin, tmax, tact, epsx
     &     )

c-----------------------------------------
c check whether new direction is a descent one
c-----------------------------------------
      if ( ifail .eq. 4) goto 1000

c-----------------------------------------
c optline returns new values of x, f, and g
c-----------------------------------------
c
      call optline( 
     &     simul
     &     , nn, ifail, lphprint
     &     , ifunc, nfunc
     &     , ff, dotdg
     &     , tmin, tmax, tact, epsx
     &     , dd, gg, xx, xdiff
     &     )

      if (lphprint) print *, 'pathei-lsopt: ',
     &     'back from optline; ifail = ', ifail
      if (lphprint) print *, 'pathei-lsopt: ',
     &     'dostore 1,2 at iiter ', iiter

      call dostore( nn, xx, .true., 1 )
      call dostore( nn, gg, .true., 2 )
cph(
cph      call lswri( isize, iiter, nn, xx, gg, lphprint )
cph)

      gnorm = DNRM2( nn, gg, 1 )

c-----------------------------------------
c print information line
c-----------------------------------------
      print iform, iiter, tact, ifunc, ff, gnorm
     $     , DNRM2( nn, xx, 1 ), tact*DNRM2( nn, dd, 1 )

Cml      write(94,'(i5,2x,1pe11.4,4(2x,1pe8.1))')
Cml     &     iiter, ff, gnorm, tact,
Cml     &     DNRM2( nn, xx, 1 ), tact*DNRM2( nn, dd, 1 )

c-----------------------------------------
c check output mode of ifail
c-----------------------------------------
      if (ifail .eq. 7
     &     .or. ifail .eq. 8
     &     .or. ifail .eq. 9) goto 1000

c-----------------------------------------
c maximal number of simulation reached
c no goto in order to update Hessian
c-----------------------------------------
         if (ifail .eq. 6) ifail = 0

c-----------------------------------------
c NOTE: stopping tests are now done
c after having updated the matrix,
c so that update information can be stored
c in case of a later warm restart
c-----------------------------------------
c-----------------------------------------
c  compute new s, y
c-----------------------------------------
         do i = 1, nn
            xdiff(i) = tact*dd(i)
         end do

c-----------------------------------------
c compute ys
c-----------------------------------------
         do i = 1, nn
            gold(i) = gg(i)-gold(i)
         end do

         ys = DDOT( nn, gold, 1, xdiff, 1 )
         if (ys .le. 0.) then
            ifail = 4
            print *, 'pathei-lsopt: ys < 0; ifail = ', ifail
            goto 1000
         endif
cph(
cph-----------------------------------------
cph at this point it is clear that xdiff 
cph provides a true optimized solution;
cph i.e. take new gradient gg to compute new dd
cph-----------------------------------------
cph)

c-----------------------------------------
c update pointers for hessupd
c-----------------------------------------
      if (nupdate .gt. 0) then

         if (jmax .eq. 0) then
            jmax = jmax+1
            if (lphprint) 
     &           print *, 'pathei-lsopt: ',
     &           'first pointer update after cold start; ',
     &           'iiter, jmin, jmax = ', iiter, jmin, jmax
         else
            jmax = jmax+1
            if (jmax .gt. nupdate) jmax = jmax-nupdate

            if (jmin .eq. jmax) then
               if (lphprint) 
     &              print *, 'pathei-lsopt: pointers updated for ',
     &              ' iiter, jmin, jmax = ', iiter, jmin, jmax
               jmin = jmin+1
               if (jmin .gt. nupdate) jmin = jmin-nupdate
            end if
         end if

c-----------------------------------------
c compute sbar, ybar store rec = min 4,5
c-----------------------------------------
         r1 = sqrt(1./ys)
         call DSCAL( nn, r1, xdiff, 1 )
         call DSCAL( nn, r1, gold, 1 )

      if (lphprint) 
     &        print *, 'pathei-lsopt: dostore at iiter, jmin, jmax ', 
     &        iiter, jmin, jmax

         call dostore( nn, gold, .true., 2*jmax+2 )
         call dostore( nn, xdiff, .true., 2*jmax+3 )

c-----------------------------------------
c compute the diagonal preconditioner
c use dd as temporary array
c-----------------------------------------
         call dgscale( nn, gold, xdiff, dd, rmin )

      endif

c-----------------------------------------
c test convergence and stopping criteria
c-----------------------------------------
      eps1 = gnorm / gnorm0

      if (eps1 .lt. epsg) then

         ifail = 11
         goto 1000
      endif

c=======================================================================
c return of loop
c=======================================================================
      
      end do

      iiter = iiter - 1
      ifail = 5

c-----------------------------------------
c loop exit
c-----------------------------------------
 1000 continue
      nfunc = ifunc
      epsg = eps1

c-----------------------------------------------------------------------
c save data for warm start
c-----------------------------------------------------------------------

      call outstore( nn, ff, gnorm0, nupdate, jmin, jmax )

c-----------------------------------------------------------------------
c compute dd(i+1), xx(i+1) based on latest available gg(i), xx(i)
c for offline version
c-----------------------------------------------------------------------

      if (loffline) then

         call lsupdxx(
     &        nn, ifail, lphprint
     &        , jmin, jmax, nupdate
     &        , ff, fmin, fold, gnorm0, dotdg
     &        , gg, dd, xx, xdiff
     &        , tmin, tmax, tact, epsx
     &        )

c        Save xx(i+1) to file for offline version.
         call optim_write_control( nn, xdiff )

      end if

c-----------------------------------------
c print final information
c-----------------------------------------
      if (iprint .ge. 5) then
         print *
         print '(a,i9)'
     $        , '  number of iterations..............', iiter
         print '(a,i9)'
     $        , '  number of simultations............', nfunc
         print '(a,e9.2)'
     $        , '  relative precision on g...........', epsg
      end if

      if (iprint.ge.10) then
         print *
         print '(a,e15.8)'
     $        , '  cost function...............', ff
         print '(a,e15.8)'
     $        , '  norm of x...................', DNRM2( nn, xx, 1 )
         print '(a,e15.8)'
     $        , '  norm of g...................', DNRM2( nn, gg, 1 )
      end if

c-----------------------------------------
c print error message
c-----------------------------------------
 999  continue

      if (ifail .ne. 0) then
         if (iprint .ge. 5) then
            print *
            print '(a)', '  optimization stopped because :'
            if (ifail .lt. 0) then
               print '(2x,a8,I3,a)', 'ifail = ', ifail
     $              , '    user request during simul'
            else if (ifail .eq. 0) then
               print '(2x,a8,I3,a)', 'ifail = ', ifail
     $              , '    optimal solution found'
            else if (ifail .eq. 1) then
               print '(2x,a8,I3,a)', 'ifail = ', ifail
     $              , '    an input argument is wrong'
            else if (ifail .eq. 2) then
               print '(2x,a8,I3,a)', 'ifail = ', ifail
     $              , '    warm start file is corrupted'
            else if (ifail .eq. 3) then
               print '(2x,a8,I3,a)', 'ifail = ', ifail
     $              , '    the initial gradient is too small'
            else if (ifail .eq. 4) then
               print '(2x,a8,I3,a)', 'ifail = ', ifail
     $              , '    the search direction is not a descent one'
            else if (ifail .eq. 5) then
               print '(2x,a8,I3,a)', 'ifail = ', ifail
     $              , '    maximal number of iterations reached'
            else if (ifail .eq. 6) then
               print '(2x,a8,I3,a)', 'ifail = ', ifail
     $              , '    maximal number of simulations reached'
            else if (ifail .eq. 7) then
               print '(2x,a8,I3,a)', 'ifail = ', ifail
     $              , '    the linesearch failed'
            else if (ifail .eq. 8) then
               print '(2x,a8,I3,a)', 'ifail = ', ifail
     $              , '    the function could not be improved'
            else if (ifail .eq. 9) then
               print '(2x,a8,I3,a)', 'ifail = ', ifail
     $              , '    optline parameters wrong'
            else if (ifail .eq. 10) then
               print '(2x,a8,I3,a)', 'ifail = ', ifail
     $              , '    cold start, no optimization done'
            else if (ifail .eq. 11) then
               print '(2x,a8,I3,a)', 'ifail = ', ifail
     $              , '    convergence achieved within precision'
            end if
            print *
         else if (iprint .ge. 1) then
            print *
            print '(a,i9)'
     $           , '  after optimization ifail..........', ifail
            print *
         end if
      end if

c-----------------------------------------
c end of subroutine
c-----------------------------------------
      return

      end
