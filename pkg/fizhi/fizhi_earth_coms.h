C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_earth_coms.h,v 1.11 2004/08/27 15:08:02 molod Exp $
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
     .   tgz, landtype, nchptot, nchpland, ityp
      _RL chfr(nchp,Nsx,Nsy)
      _RL alai(nchp,Nsx,Nsy)
      _RL agrn(nchp,Nsx,Nsy)
      _RL albvisdr(sNx,sNy,Nsx,Nsy)
      _RL albvisdf(sNx,sNy,Nsx,Nsy)
      _RL albnirdr(sNx,sNy,Nsx,Nsy)
      _RL albnirdf(sNx,sNy,Nsx,Nsy)
      _RL emiss(sNx,sNy,10,Nsx,Nsy)
      _RL tgz(sNx,sNy,Nsx,Nsy)
      integer landtype(sNx,sNy,Nsx,Nsy)
      integer nchptot(Nsx,Nsy),nchpland(Nsx,Nsy)
      integer ityp(nchp,Nsx,Nsy)
