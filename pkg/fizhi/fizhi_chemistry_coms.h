C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_chemistry_coms.h,v 1.6 2004/07/13 21:11:08 molod Exp $
C $Name:  $

c Chemistry Variables Dimensions
c -------------------------------
      integer nlatsoz, nlevsoz, ntimesoz, nlatsq, nlevsq, ntimesq,
     .                              nlevsn2o,nlevsmeth
      parameter (nlatsoz = 37, nlevsoz = 34, ntimesoz = 12)
      parameter (nlatsq = 18, nlevsq = 20, ntimesq = 12)
      parameter (nlevsn2o = Nrphys)
      parameter (nlevsmeth = Nrphys)
      common /chemistry_grid/ latsoz, levsoz, latsq, levsq
      _RL latsoz(nlatsoz), levsoz(nlevsoz), latsq(nlatsq) 
      _RL levsq(nlevsq)

c "Chemistry State" Variables
c -------------------------
      common /chemistry_vars/ 
     .   co2, cfc11, cfc12, cfc22, ozone, stratq, n2o, methane
      _RL co2
      _RL cfc11
      _RL cfc12
      _RL cfc22
      _RL ozone(nlatsoz,nlevsoz,ntimesoz)
      _RL stratq(nlatsq,nlevsq,ntimesq)
      _RL n2o(nlevsn2o)
      _RL methane(nlevsmeth)

c Chemistry Exports
c -------------------
      common /chem_exports/ o3, qstr 
      _RL o3(sNx,sNy,Nrphys,Nsx,Nsy)
      _RL qstr(sNx,sNy,Nrphys,Nsx,Nsy)
