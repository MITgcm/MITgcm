C $Header: /u/gcmpack/MITgcm/pkg/flt/Attic/flt_bilinear.F,v 1.3 2008/12/03 01:34:51 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

      subroutine flt_bilinear (
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

      IF ( kp.LT.1 .OR. kp.GT.Nr ) THEN
c           WRITE(msgbuf,'(A,I8)')
c    &        ' FLT_BILINEAR: illegal value for kp=',kp
c           CALL PRINT_ERROR( msgbuf, myThid )
            STOP 'ABNORMAL END: S/R FLT_BILINEAR'
      ENDIF
      nnx = int(xp)
      nny = int(yp)
      dx = xp - float(nnx)
      dy = yp - float(nny)
c
c to choose the u box in which the particle is found
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
caw This may be incorrect.
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

      subroutine  flt_trilinear(
     I                         xp,
     I                         yp,
     I                         zp,
     O                         uu,
     I                         u,
     I                         nu,
     I                         bi,
     I                         bj
     &                        )

c     ==================================================================
c     SUBROUTINE flt_trilinear
c     ==================================================================
c
c     o Trilinear scheme to find u of particle at a given xp,yp,zp 
c       location. This routine is a straight forward generalization of the 
c       bilinear interpolation scheme.
c
c     started: 2004.05.28 Antti Westerlund (antti.westerlund@fimr.fi)
c              and Sergio Jaramillo (sju@eos.ubc.ca).
c              (adopted from subroutine bilinear by Arne Biastoch) 
c
c     ==================================================================
c     SUBROUTINE flt_trilinear
c     ==================================================================

c     == global variables ==

#include "SIZE.h"

c     == routine arguments ==

      _RL xp, yp, zp
      _RL uu
      integer nu, bi, bj
      _RL  u (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

c     == local variables ==

      INTEGER nnx, nny, nnz, nfx, nfy, nfz, nfxp, nfyp, nfzp
      _RL dx, dy, dz, ddx, ddy, ddz 
      integer ip
      _RL xx, yy, zz, phi, scalex, scaley, scalez
      _RL u111, u121, u221, u211, u112, u122, u222, u212

c     == end of interface ==

c Round xp,yp,zp down to find a grid point.
      nnx = int(xp)
      nny = int(yp)
      nnz = int(zp)

c Find out the distance from the gridpoint.
      dx = xp - float(nnx)
      dy = yp - float(nny)
      dz = zp - float(nnz)
c
c to choose the u box in which the particle is found
c nu=1 for T, S
c nu=2 for u
c nu=3 for v
c nu=4 for w
c
c Velocities are face quantities and must therefore be treated differently
c
c if the variable is T,S
      if (nu.eq.1) then
        nfx = nnx
        ddx = dx
        nfy = nny
        ddy = dy
        nfz = nnz
        ddz = dz
      endif
c if the variable is u 
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
        nfz = nnz
        ddz = dz
      endif
c if the variable is v
      if (nu.eq.3) then
        nfx = nnx
        ddx = dx
        if (dy.le.0.5) then
          nfy = nny
          ddy = dy + 0.5
        else
          nfy = nny + 1
          ddy = dy - 0.5
        endif
        nfz = nnz
        ddz = dz
      endif
c if the variable is w
      if (nu.eq.4) then
        nfx = nnx
        ddx = dx
        nfy = nny
        ddy = dy
        if (dz.le.0.5) then
          nfz = nnz
          ddz = dz + 0.5
        else
          nfz = nnz + 1
          ddz = dz - 0.5
        endif
      endif
c
c if we are near or over the edge, limit nfx/y/z
      if(nfx.gt.nx) nfx=nx
      if(nfy.gt.ny) nfy=ny 
      if(nfz.gt.nr) nfz=nr 
      if(nfz.le.1) nfz=1
c We should possibly check something else too...
c
c the coordinates for the other grid points
      nfxp = nfx + 1
      nfyp = nfy + 1
      nfzp = nfz + 1
c if we are near the edge, also limit nf?p
      if (nfx.eq.nx) nfxp = nfx
      if (nfy.eq.ny) nfyp = nfy
      if (nfz.eq.nr) nfzp = nfz

c Values of the field at relevant grid points
      u111 = u(nfx,nfy,nfz,bi,bj)
      u211 = u(nfxp,nfy,nfz,bi,bj)
      u221 = u(nfxp,nfyp,nfz,bi,bj)
      u121 = u(nfx,nfyp,nfz,bi,bj)
      u112 = u(nfx,nfy,nfzp,bi,bj)
      u212 = u(nfxp,nfy,nfzp,bi,bj)
      u222 = u(nfxp,nfyp,nfzp,bi,bj)
      u122 = u(nfx,nfyp,nfzp,bi,bj)

c Trilinear interpolation, a straight forward generalization 
c of the bilinear interpolation scheme. 
      uu = (1-ddx)*(1-ddy)*(1-ddz)*u111 + ddx*(1-ddy)*(1-ddz)*u211 
     &   + ddx*ddy*(1-ddz)*u221         + (1-ddx)*ddy*(1-ddz)*u121 
     &   + (1-ddx)*(1-ddy)*ddz*u112     + ddx*(1-ddy)*ddz*u212 
     &   + ddx*ddy*ddz*u222             + (1-ddx)*ddy*ddz*u122
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
c to choose the u box in which the particle is found
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
caw This may be incorrect.
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

