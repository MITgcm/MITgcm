C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_earth_coms.h,v 1.10 2004/08/12 15:21:22 molod Exp $
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
     .   landtype, tgz, nchptot, nchpland, ityp
      integer nchptot(Nsx,Nsy),nchpland(Nsx,Nsy)
      integer ityp(nchp,Nsx,Nsy)
      integer landtype(sNx,sNy,Nsx,Nsy)
      _RL chfr(nchp,Nsx,Nsy)
      _RL alai(nchp,Nsx,Nsy)
      _RL agrn(nchp,Nsx,Nsy)
      _RL albvisdr(sNx,sNy,Nsx,Nsy)
      _RL albvisdf(sNx,sNy,Nsx,Nsy)
      _RL albnirdr(sNx,sNy,Nsx,Nsy)
      _RL albnirdf(sNx,sNy,Nsx,Nsy)
      _RL emiss(sNx,sNy,10,Nsx,Nsy)
      _RL tgz(sNx,sNy,Nsx,Nsy)
