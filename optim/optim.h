

c     ==================================================================
c     HEADER OPTIMIZATION
c     ==================================================================
c
c     o Header for the large scale optimization.
c
c       This header file contains those parameters that are monitoring
c       the state of the off-line optimization procedure. One
c       optimization cycle is considered to consist of a forward/adjoint
c       model run followed by a cost function minimization that then
c       yields a new guess for the control vector:
c
c       optimcycle i:   xx(i) --> fc(i) adxx(i) --> xx(i+1)
c
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  24-Feb-2000
c
c
c     ==================================================================
c     HEADER OPTIMIZATION
c     ==================================================================

c     Optimization version:
c     =====================
c
c     lsoptversion - Version of the large scale optimization.

      character*(5)  lsoptversion
      parameter    ( lsoptversion = '2.1.0' )

      character*(5)  offlineversion
      parameter    ( offlineversion = '0.1.1' )

c     The off-line optimization part:
c     ===============================
c
c     Starting an optimization experiment implies setting optimcycle to
c     zero. During cycle number I the model is called first, then the
c     offline optimization is run. The latter provides a new guess for
c     the vector of control variables for the next cycle I+1.
c
c     optimcycle - cycle number of the off-line optimization.

      common /optiparm_i/
     &                    optimcycle,
     &                    nvars,
     &                    numiter,
     &                    nfunc,
     &                    iprint,
     &                    nupdate
      integer optimcycle
      integer nvars
      integer numiter
      integer nfunc
      integer iprint
      integer nupdate

      common /optiparm_r/
     &                    fmin,
     &                    epsf,
     &                    epsx,
     &                    epsg,
     &                    eps
      _RL epsf
      _RL epsx
      _RL fmin
      _RL epsg
      _RL eps

      common /optiparm_l/
     &                    nondimcontrol
      logical nondimcontrol

      common /optiparm_c/
     &                    copt
      character*( 3) copt

c     ==================================================================
c     END OF HEADER OPTIMIZATION
c     ==================================================================


