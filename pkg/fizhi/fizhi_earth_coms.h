C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_earth_coms.h,v 1.6 2004/07/13 21:11:08 molod Exp $
C $Name:  $

c Solid-Earth State Variables
c ---------------------------
      common /earth_state/ phis_var, tilefrac, surftype 
      _RL phis_var(sNx,sNy,Nsx,Nsy)
      _RL tilefrac(sNx,sNy,maxtyp,Nsx,Nsy)
      integer surftype(sNx,sNy,maxtyp,Nsx,Nsy)

c Solid_Earth Couplings
c ---------------------
      common /earth_exports/ 
     .   chfr, alai, agrn, 
     .   albvisdr, albvisdf, albnirdr, albnirdf, emiss,
     .   landtype, tgz, nchpland, ityp
      integer nchpland
      integer ityp(nchp)
      _RL chfr(nchp)
      _RL alai(nchp)
      _RL agrn(nchp)
      _RL albvisdr(sNx,sNy,Nsx,Nsy)
      _RL albvisdf(sNx,sNy,Nsx,Nsy)
      _RL albnirdr(sNx,sNy,Nsx,Nsy)
      _RL albnirdf(sNx,sNy,Nsx,Nsy)
      _RL emiss(sNx,sNy,10,Nsx,Nsy)
      _RL landtype(sNx,sNy,Nsx,Nsy)
      _RL tgz(sNx,sNy,Nsx,Nsy)
