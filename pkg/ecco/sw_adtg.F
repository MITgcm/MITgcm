C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/sw_adtg.F,v 1.2 2007/10/09 00:02:51 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      _RL function SW_ADTG  (S,T,P)

c     ==================================================================
c     SUBROUTINE SW_ADTG
c     ==================================================================
c
c     o Calculates adiabatic temperature gradient as per UNESCO 1983 routines.
c
c     started:
c
c              Armin Koehl akoehl@ucsd.edu
c
c     ==================================================================
c     SUBROUTINE SW_ADTG
c     ==================================================================

      implicit none
      _RL a0,a1,a2,a3,b0,b1,c0,c1,c2,c3,d0,d1,e0,e1,e2
      _RL S,T,P
      _RL sref

      sref = 35. _d 0
      a0 =  3.5803 _d -5
      a1 = +8.5258 _d -6
      a2 = -6.836 _d -8
      a3 =  6.6228 _d -10

      b0 = +1.8932 _d -6
      b1 = -4.2393 _d -8

      c0 = +1.8741 _d -8
      c1 = -6.7795 _d -10
      c2 = +8.733 _d -12
      c3 = -5.4481 _d -14

      d0 = -1.1351 _d -10
      d1 =  2.7759 _d -12

      e0 = -4.6206 _d -13
      e1 = +1.8676 _d -14
      e2 = -2.1687 _d -16

      SW_ADTG =      a0 + (a1 + (a2 + a3*T)*T)*T
     &     + (b0 + b1*T)*(S-sref)
     &     + ( (c0 + (c1 + (c2 + c3*T)*T)*T) + (d0 + d1*T)*(S-sref) )*P
     &     + (  e0 + (e1 + e2*T)*T )*P*P
      end
