C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_earth_coms.h,v 1.3 2004/06/15 21:18:18 molod Exp $
C $Name:  $

c Solid-Earth State Variables
c ---------------------------
      common /earth_state/ phis_var, tilefrac, surftype 
      _RL phis_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tilefrac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,maxtyp,nSx,nSy)
      integer surftype(1-OLx:sNx+OLx,1-OLy:sNy+OLy,maxtyp,nSx,nSy)

c Solid_Earth Couplings
c ---------------------
      common /earth_exports/ 
     .   nchpland, ityp, chfr, alai, agrn, 
     .   albvisdr, albvisdf, albnirdr, albnirdf, emiss,
     .   landtype
      integer nchpland
      integer ityp(nchp)
      _RL chfr(nchp)
      _RL alai(nchp)
      _RL agrn(nchp)
      _RL albvisdr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL albvisdf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL albnirdr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL albnirdf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL emiss(1-OLx:sNx+OLx,1-OLy:sNy+OLy,10,nSx,nSy)
      _RL landtype(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
