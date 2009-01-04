C $Header: /u/gcmpack/MITgcm/pkg/flt/Attic/flt_bilinear.F,v 1.4 2009/01/04 00:58:23 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

C--   Contents
C--   o FLT_BILINEAR
C--   o FLT_TRILINEAR
C--   o FLT_BILINEAR2D

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE FLT_BILINEAR (
     I                         xp,
     I                         yp,
     O                         uu,
     I                         kp,
     I                         u,
     I                         nu,
     I                         bi,
     I                         bj
     &                        )

C     ==================================================================
C     SUBROUTINE flt_bilinear
C     ==================================================================
C
C     o Bilinear scheme to find u of particle at given xp,yp location
C
C     ==================================================================
C     SUBROUTINE flt_bilinear
C     ==================================================================

C     == global variables ==

#include "SIZE.h"

C     == routine arguments ==

      _RL xp, yp
      _RL uu
      INTEGER nu, kp, bi, bj
      _RL  u (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C     == local variables ==

      INTEGER nnx, nny, nfx, nfy, nfxp, nfyp
      _RL dx, dy, ddx, ddy
      _RL u11, u12, u22, u21

C     == end of interface ==

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

C to choose the u box in which the particle is found
C nu=1 for T, S
C nu=2 for u
C nu=3 for v
C nu=4 for w

      IF (nu.EQ.1.OR.nu.EQ.4) THEN
        nfx = nnx
        nfy = nny
        ddx = dx
        ddy = dy
      ENDIF

      IF (nu.EQ.2) THEN
        IF (dx.LE.0.5) THEN
           nfx = nnx
           ddx = dx + 0.5
        ELSE
           nfx = nnx + 1
           ddx = dx - 0.5
        ENDIF
        nfy = nny
        ddy = dy
      ENDIF

      IF (nu.EQ.3) THEN
        IF (dy.LE.0.5) THEN
          nfy = nny
          ddy = dy + 0.5
        ELSE
          nfy = nny + 1
          ddy = dy - 0.5
        ENDIF
        nfx = nnx
        ddx = dx
      ENDIF

Cab change start
C was correct only for global?
c     IF (nfx.GT.nx) nfx=nfx-nx
      IF (nfx.GT.nx) nfx=nx
Cab change end
      IF (nfy.GT.ny) nfy=ny
      nfxp = nfx + 1
      nfyp = nfy + 1
Cab change start
c     IF (nfx.EQ.nx) nfxp = 1
      IF (nfx.EQ.nx) nfxp = nfx
Cab change end
      IF (nfy.EQ.ny) nfyp = nfy

      IF (nu.LT.4) THEN
        u11 = u(nfx,nfy,kp,bi,bj)
        u21 = u(nfxp,nfy,kp,bi,bj)
        u22 = u(nfxp,nfyp,kp,bi,bj)
        u12 = u(nfx,nfyp,kp,bi,bj)
      ENDIF
      IF (nu.EQ.4) THEN
Caw This may be incorrect.
        u11 = u(nfx,nfy,kp,bi,bj)+u(nfx,nfy,kp-1,bi,bj)
        u21 = u(nfxp,nfy,kp,bi,bj)+u(nfxp,nfy,kp-1,bi,bj)
        u22 = u(nfxp,nfyp,kp,bi,bj)+u(nfxp,nfyp,kp-1,bi,bj)
        u12 = u(nfx,nfyp,kp,bi,bj)+u(nfx,nfyp,kp-1,bi,bj)
      ENDIF

C bilinear interpolation (from numerical recipes)
      uu = (1-ddx)*(1-ddy)*u11 + ddx*(1-ddy)*u21 + ddx*ddy*u22
     .     + (1-ddx)*ddy*u12

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE FLT_TRILINEAR(
     I                         xp,
     I                         yp,
     I                         zp,
     O                         uu,
     I                         u,
     I                         nu,
     I                         bi,
     I                         bj
     &                        )

C     ==================================================================
C     SUBROUTINE flt_trilinear
C     ==================================================================
C
C     o Trilinear scheme to find u of particle at a given xp,yp,zp
C       location. This routine is a straight forward generalization of the
C       bilinear interpolation scheme.
C
C     started: 2004.05.28 Antti Westerlund (antti.westerlund@fimr.fi)
C              and Sergio Jaramillo (sju@eos.ubc.ca).
C              (adopted from SUBROUTINE bilinear by Arne Biastoch)
C
C     ==================================================================
C     SUBROUTINE flt_trilinear
C     ==================================================================

C     == global variables ==

#include "SIZE.h"

C     == routine arguments ==

      _RL xp, yp, zp
      _RL uu
      INTEGER nu, bi, bj
      _RL  u (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C     == local variables ==

      INTEGER nnx, nny, nnz, nfx, nfy, nfz, nfxp, nfyp, nfzp
      _RL dx, dy, dz, ddx, ddy, ddz
      _RL u111, u121, u221, u211, u112, u122, u222, u212

C     == end of interface ==

C Round xp,yp,zp down to find a grid point.
      nnx = int(xp)
      nny = int(yp)
      nnz = int(zp)

C Find out the distance from the gridpoint.
      dx = xp - float(nnx)
      dy = yp - float(nny)
      dz = zp - float(nnz)

C to choose the u box in which the particle is found
C nu=1 for T, S
C nu=2 for u
C nu=3 for v
C nu=4 for w

C Velocities are face quantities and must therefore be treated differently

C If the variable is T,S
      IF (nu.EQ.1) THEN
        nfx = nnx
        ddx = dx
        nfy = nny
        ddy = dy
        nfz = nnz
        ddz = dz
      ENDIF
C If the variable is u
      IF (nu.EQ.2) THEN
        IF (dx.LE.0.5) THEN
           nfx = nnx
           ddx = dx + 0.5
        ELSE
           nfx = nnx + 1
           ddx = dx - 0.5
        ENDIF
        nfy = nny
        ddy = dy
        nfz = nnz
        ddz = dz
      ENDIF
C If the variable is v
      IF (nu.EQ.3) THEN
        nfx = nnx
        ddx = dx
        IF (dy.LE.0.5) THEN
          nfy = nny
          ddy = dy + 0.5
        ELSE
          nfy = nny + 1
          ddy = dy - 0.5
        ENDIF
        nfz = nnz
        ddz = dz
      ENDIF
C If the variable is w
      IF (nu.EQ.4) THEN
        nfx = nnx
        ddx = dx
        nfy = nny
        ddy = dy
        IF (dz.LE.0.5) THEN
          nfz = nnz
          ddz = dz + 0.5
        ELSE
          nfz = nnz + 1
          ddz = dz - 0.5
        ENDIF
      ENDIF

C If we are near or over the edge, limit nfx/y/z
      IF (nfx.GT.nx) nfx=nx
      IF (nfy.GT.ny) nfy=ny
      IF (nfz.GT.nr) nfz=nr
      IF (nfz.LE.1) nfz=1
C We should possibly check something else too...

C the coordinates for the other grid points
      nfxp = nfx + 1
      nfyp = nfy + 1
      nfzp = nfz + 1
C If we are near the edge, also limit nf?p
      IF (nfx.EQ.nx) nfxp = nfx
      IF (nfy.EQ.ny) nfyp = nfy
      IF (nfz.EQ.nr) nfzp = nfz

C Values of the field at relevant grid points
      u111 = u(nfx,nfy,nfz,bi,bj)
      u211 = u(nfxp,nfy,nfz,bi,bj)
      u221 = u(nfxp,nfyp,nfz,bi,bj)
      u121 = u(nfx,nfyp,nfz,bi,bj)
      u112 = u(nfx,nfy,nfzp,bi,bj)
      u212 = u(nfxp,nfy,nfzp,bi,bj)
      u222 = u(nfxp,nfyp,nfzp,bi,bj)
      u122 = u(nfx,nfyp,nfzp,bi,bj)

C Trilinear interpolation, a straight forward generalization
C of the bilinear interpolation scheme.
      uu = (1-ddx)*(1-ddy)*(1-ddz)*u111 + ddx*(1-ddy)*(1-ddz)*u211
     &   + ddx*ddy*(1-ddz)*u221         + (1-ddx)*ddy*(1-ddz)*u121
     &   + (1-ddx)*(1-ddy)*ddz*u112     + ddx*(1-ddy)*ddz*u212
     &   + ddx*ddy*ddz*u222             + (1-ddx)*ddy*ddz*u122

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE FLT_BILINEAR2D(
     I                           xp,
     I                           yp,
     O                           uu,
     I                           u,
     I                           nu,
     I                           bi,
     I                           bj
     &                          )

C     ==================================================================
C     SUBROUTINE flt_bilinear2d
C     ==================================================================
C
C     o Bilinear scheme to find u of particle at given xp,yp location
C     o For 2D fields
C
C     started: Arne Biastoch abiastoch@ucsd.edu 13-Jan-2000
C              (adopted from SUBROUTINE bilinear)
C
C     ==================================================================
C     SUBROUTINE flt_bilinear2d
C     ==================================================================

C     == global variables ==

#include "SIZE.h"

C     == routine arguments ==

      _RL xp, yp
      _RL uu
      INTEGER nu, bi, bj
      _RL  u (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     == local variables ==

      INTEGER nnx, nny, nfx, nfy, nfxp, nfyp
      _RL dx, dy, ddx, ddy
      _RL u11, u12, u22, u21

C     == end of interface ==

      nnx = int(xp)
      nny = int(yp)
      dx = xp - float(nnx)
      dy = yp - float(nny)

C to choose the u box in which the particle is found
C nu=1 for T, S
C nu=2 for u
C nu=3 for v
C nu=4 for w

      IF (nu.EQ.1.OR.nu.EQ.4) THEN
        nfx = nnx
        nfy = nny
        ddx = dx
        ddy = dy
      ENDIF

      IF (nu.EQ.2) THEN
        IF (dx.LE.0.5) THEN
           nfx = nnx
           ddx = dx + 0.5
        ELSE
           nfx = nnx + 1
           ddx = dx - 0.5
        ENDIF
        nfy = nny
        ddy = dy
      ENDIF

      IF (nu.EQ.3) THEN
        IF (dy.LE.0.5) THEN
          nfy = nny
          ddy = dy + 0.5
        ELSE
          nfy = nny + 1
          ddy = dy - 0.5
        ENDIF
        nfx = nnx
        ddx = dx
      ENDIF

Cab change start
C was correct only for global?
c     IF (nfx.GT.nx) nfx=nfx-nx
      IF (nfx.GT.nx) nfx=nx
Cab change end
      IF (nfy.GT.ny) nfy=ny
      nfxp = nfx + 1
      nfyp = nfy + 1
Cab change start
c     IF (nfx.EQ.nx) nfxp = 1
      IF (nfx.EQ.nx) nfxp = nfx
Cab change end
      IF (nfy.EQ.ny) nfyp = nfy

      IF (nu.LT.4) THEN
        u11 = u(nfx,nfy,bi,bj)
        u21 = u(nfxp,nfy,bi,bj)
        u22 = u(nfxp,nfyp,bi,bj)
        u12 = u(nfx,nfyp,bi,bj)
      ENDIF
      IF (nu.EQ.4) THEN
Caw This may be incorrect.
        u11 = u(nfx,nfy,bi,bj)+u(nfx,nfy,bi,bj)
        u21 = u(nfxp,nfy,bi,bj)+u(nfxp,nfy,bi,bj)
        u22 = u(nfxp,nfyp,bi,bj)+u(nfxp,nfyp,bi,bj)
        u12 = u(nfx,nfyp,bi,bj)+u(nfx,nfyp,bi,bj)
      ENDIF

C bilinear interpolation (from numerical recipes)
      uu = (1-ddx)*(1-ddy)*u11 + ddx*(1-ddy)*u21 + ddx*ddy*u22
     .     + (1-ddx)*ddy*u12

      RETURN
      END
