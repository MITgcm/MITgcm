C $Header: /u/gcmpack/MITgcm/pkg/flt/Attic/flt_bilinear.F,v 1.1 2001/09/13 17:43:55 adcroft Exp $
C $Name:  $

#include "FLT_CPPOPTIONS.h"

      subroutine  flt_bilinear(
     I                         xp,
     I                         yp,
     O                         uu,
     I                         kp,
     I                         u,
     I                         nu,
     I                         bi,
     I                         bj
     &                        )

c     ==================================================================
c     SUBROUTINE flt_bilinear
c     ==================================================================
c
c     o Bilinear scheme to find u of particle at given xp,yp location
c
c     ==================================================================
c     SUBROUTINE flt_bilinear
c     ==================================================================

c     == global variables ==

#include "SIZE.h"

c     == routine arguments ==

      _RL xp, yp
      _RL uu
      integer nu, kp, bi, bj
      _RL  u (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

c     == local variables ==

      INTEGER nnx, nny, nfx, nfy, nfxp, nfyp
      _RL dx, dy, ddx, ddy 
      integer ip
      _RL xx, yy, phi, scalex, scaley
      _RL u11, u12, u22, u21

c     == end of interface ==

      nnx = int(xp)
      nny = int(yp)
      dx = xp - float(nnx)
      dy = yp - float(nny)
c
c to chose the u box in which the particle is found
c nu=1 for T, S
c nu=2 for u
c nu=3 for v
c nu=4 for w
c
      if (nu.eq.1.or.nu.eq.4) then
	nfx = nnx
	nfy = nny
	ddx = dx
	ddy = dy
      endif
c
      if (nu.eq.2) then
	if (dx.le.0.5) then
	   nfx = nnx
	   ddx = dx + 0.5
	else
	   nfx = nnx + 1
	   ddx = dx - 0.5
	endif
	nfy = nny
	ddy = dy
      endif
c
      if (nu.eq.3) then
	if (dy.le.0.5) then
	  nfy = nny
	  ddy = dy + 0.5
	else
	  nfy = nny + 1
	  ddy = dy - 0.5
	endif
	nfx = nnx
	ddx = dx
      endif
c
c
cab change start
c was correct only for global?
c     if(nfx.gt.nx) nfx=nfx-nx
      if(nfx.gt.nx) nfx=nx
cab change end
      if(nfy.gt.ny) nfy=ny 
      nfxp = nfx + 1
      nfyp = nfy + 1
cab change start
c     if (nfx.eq.nx) nfxp = 1
      if (nfx.eq.nx) nfxp = nfx
cab change end
      if (nfy.eq.ny) nfyp = nfy

      if (nu.lt.4) then
        u11 = u(nfx,nfy,kp,bi,bj)
        u21 = u(nfxp,nfy,kp,bi,bj)
        u22 = u(nfxp,nfyp,kp,bi,bj)
        u12 = u(nfx,nfyp,kp,bi,bj)
      endif
      if (nu.eq.4) then
        u11 = u(nfx,nfy,kp,bi,bj)+u(nfx,nfy,kp-1,bi,bj)
        u21 = u(nfxp,nfy,kp,bi,bj)+u(nfxp,nfy,kp-1,bi,bj)
        u22 = u(nfxp,nfyp,kp,bi,bj)+u(nfxp,nfyp,kp-1,bi,bj)
        u12 = u(nfx,nfyp,kp,bi,bj)+u(nfx,nfyp,kp-1,bi,bj)
      endif
c
c
c bilinear interpolation (from numerical recipes)
      uu = (1-ddx)*(1-ddy)*u11 + ddx*(1-ddy)*u21 + ddx*ddy*u22
     .     + (1-ddx)*ddy*u12
c
c
      return
      end


      subroutine  flt_bilinear2d(
     I                           xp,
     I                           yp,
     O                           uu,
     I                           u,
     I                           nu,
     I                           bi,
     I                           bj
     &                          )

c     ==================================================================
c     SUBROUTINE flt_bilinear2d
c     ==================================================================
c
c     o Bilinear scheme to find u of particle at given xp,yp location
c     o For 2D fields
c
c     started: Arne Biastoch abiastoch@ucsd.edu 13-Jan-2000
c              (adopted from subroutine bilinear)
c
c     ==================================================================
c     SUBROUTINE flt_bilinear2d
c     ==================================================================

c     == global variables ==

#include "SIZE.h"

c     == routine arguments ==

      _RL xp, yp
      _RL uu
      integer nu, bi, bj
      _RL  u (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

c     == local variables ==

      INTEGER nnx, nny, nfx, nfy, nfxp, nfyp
      _RL dx, dy, ddx, ddy 
      integer ip
      _RL xx, yy, phi, scalex, scaley
      _RL u11, u12, u22, u21

c     == end of interface ==

      nnx = int(xp)
      nny = int(yp)
      dx = xp - float(nnx)
      dy = yp - float(nny)
c
c to chose the u box in which the particle is found
c nu=1 for T, S
c nu=2 for u
c nu=3 for v
c nu=4 for w
c
      if (nu.eq.1.or.nu.eq.4) then
	nfx = nnx
	nfy = nny
	ddx = dx
	ddy = dy
      endif
c
      if (nu.eq.2) then
	if (dx.le.0.5) then
	   nfx = nnx
	   ddx = dx + 0.5
	else
	   nfx = nnx + 1
	   ddx = dx - 0.5
	endif
	nfy = nny
	ddy = dy
      endif
c
      if (nu.eq.3) then
	if (dy.le.0.5) then
	  nfy = nny
	  ddy = dy + 0.5
	else
	  nfy = nny + 1
	  ddy = dy - 0.5
	endif
	nfx = nnx
	ddx = dx
      endif
c
cab change start
c was correct only for global?
c     if(nfx.gt.nx) nfx=nfx-nx
      if(nfx.gt.nx) nfx=nx
cab change end
      if(nfy.gt.ny) nfy=ny 
      nfxp = nfx + 1
      nfyp = nfy + 1
cab change start
c     if (nfx.eq.nx) nfxp = 1
      if (nfx.eq.nx) nfxp = nfx
cab change end
      if (nfy.eq.ny) nfyp = nfy

      if (nu.lt.4) then
        u11 = u(nfx,nfy,bi,bj)
        u21 = u(nfxp,nfy,bi,bj)
        u22 = u(nfxp,nfyp,bi,bj)
        u12 = u(nfx,nfyp,bi,bj)
      endif
      if (nu.eq.4) then
        u11 = u(nfx,nfy,bi,bj)+u(nfx,nfy,bi,bj)
        u21 = u(nfxp,nfy,bi,bj)+u(nfxp,nfy,bi,bj)
        u22 = u(nfxp,nfyp,bi,bj)+u(nfxp,nfyp,bi,bj)
        u12 = u(nfx,nfyp,bi,bj)+u(nfx,nfyp,bi,bj)
      endif
c
c
c bilinear interpolation (from numerical recipes)
      uu = (1-ddx)*(1-ddy)*u11 + ddx*(1-ddy)*u21 + ddx*ddy*u22
     .     + (1-ddx)*ddy*u12
c
c
      return
      end

