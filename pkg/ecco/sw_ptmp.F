C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/sw_ptmp.F,v 1.2 2007/10/09 00:02:51 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"


      _RL function SW_PTMP  (S,T,P,PR)

c     ==================================================================
c     SUBROUTINE SW_PTMP
c     ==================================================================
c
c     o   Calculates potential temperature as per UNESCO 1983 report.
c
c     started:
c
c              Armin Koehl akoehl@ucsd.edu
c
c     ==================================================================
c     SUBROUTINE SW_PTMP
c     ==================================================================
C     S  = salinity    [psu      (PSS-78) ]
C     T  = temperature [degree C (IPTS-68)]
C     P  = pressure    [db]
C     PR = Reference pressure  [db]

      implicit none

c     routine arguments
      _RL S,T,P,PR

c     local arguments
      _RL del_P ,del_th, th, q
      _RL onehalf, two, three
      parameter ( onehalf = 0.5 _d 0, two = 2. _d 0, three = 3. _d 0 )

c     externals
      _RL sw_adtg
      external sw_adtg
c theta1
      del_P  = PR - P
      del_th = del_P*sw_adtg(S,T,P)
      th     = T + onehalf*del_th
      q      = del_th
c theta2
      del_th = del_P*sw_adtg(S,th,P+onehalf*del_P)

      th     = th + (1 - 1/sqrt(two))*(del_th - q)
      q      = (two-sqrt(two))*del_th + (-two+three/sqrt(two))*q

c theta3
      del_th = del_P*sw_adtg(S,th,P+onehalf*del_P)
      th     = th + (1 + 1/sqrt(two))*(del_th - q)
      q      = (two + sqrt(two))*del_th + (-two-three/sqrt(two))*q

c theta4
      del_th = del_P*sw_adtg(S,th,P+del_P)
      SW_PTMP     = th + (del_th - two*q)/(two*three)
      return
      end
