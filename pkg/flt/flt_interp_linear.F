C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_interp_linear.F,v 1.1 2009/02/01 21:10:51 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

C--   Contents
C--   o FLT_BILINEAR
C--   o FLT_TRILINEAR
C--   o FLT_BILINEAR2D

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE FLT_BILINEAR(
     I                         ix, jy,
     O                         uu,
     I                         var,
     I                         kl, nu, bi, bj, myThid )

C     ==================================================================
C     SUBROUTINE FLT_BILINEAR
C     ==================================================================
C     o Bilinear scheme to interpolate variable to particle position
C       given by its fractional (real) index ix,jy location
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"

C     == routine arguments ==
      _RL ix, jy
      _RL uu
      _RL var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER kl, nu, bi, bj, myThid

C     == local variables ==
      INTEGER i1, j1, i2, j2, klp
      _RL ddx, ddy
      _RL u11, u12, u22, u21

C     == end of interface ==

C--   to choose the u box in which the particle is found
C nu=0 for T, S
C nu=1 for u
C nu=2 for v
C nu=3 for Vorticity
C nu=4 for w

      IF ( kl.LT.1 .OR. kl.GT.Nr ) THEN
c           WRITE(msgbuf,'(A,I8)')
c    &        ' FLT_BILINEAR: illegal value for kl=',kl
c           CALL PRINT_ERROR( msgbuf, myThid )
            STOP 'ABNORMAL END: S/R FLT_BILINEAR'
      ENDIF

C--   find x-index according to grid-location of variable
      IF ( MOD(nu,2).EQ.0 ) THEN
        i1 = INT(ix)
        ddx = ix - DFLOAT(i1)
      ELSE
        i1 = NINT(ix)
        ddx = 0.5 _d 0 + ix - DFLOAT(i1)
      ENDIF
C--   find y-index according to grid-location of variable
      IF ( MOD(nu,4).LE.1 ) THEN
        j1 = INT(jy)
        ddy = jy - DFLOAT(j1)
      ELSE
        j1 = NINT(jy)
        ddy = 0.5 _d 0 + jy - DFLOAT(j1)
      ENDIF

C--   Set the higher index for interpolation
      i2 = i1 + 1
      j2 = j1 + 1

C--   No need to change start/end index : use array overlap if needed

C--   bilinear interpolation (from numerical recipes)
      IF (nu.LE.3) THEN
        uu =          ( (1.-ddx)*(1.-ddy)*var(i1,j1,kl,bi,bj)
     &                +     ddx * ddy    *var(i2,j2,kl,bi,bj) )
     &              + (     ddx *(1.-ddy)*var(i2,j1,kl,bi,bj)
     &                + (1.-ddx)* ddy    *var(i1,j2,kl,bi,bj) )
      ELSE
        klp = MIN(kl+1,Nr)
        u11 = ( var(i1,j1,kl,bi,bj)+var(i1,j1,klp,bi,bj) )*0.5 _d 0
        u21 = ( var(i2,j1,kl,bi,bj)+var(i2,j1,klp,bi,bj) )*0.5 _d 0
        u22 = ( var(i2,j2,kl,bi,bj)+var(i2,j2,klp,bi,bj) )*0.5 _d 0
        u12 = ( var(i1,j2,kl,bi,bj)+var(i1,j2,klp,bi,bj) )*0.5 _d 0
        uu =          ( (1.-ddx)*(1.-ddy)*u11
     &                +     ddx * ddy    *u22 )
     &              + (     ddx *(1.-ddy)*u21
     &                + (1.-ddx)* ddy    *u12 )
      ENDIF


      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE FLT_TRILINEAR(
     I                         ix, jy, kz,
     O                         uu,
     I                         var,
     I                         nu, bi, bj, myThid )

C     ==================================================================
C     SUBROUTINE FLT_TRILINEAR
C     ==================================================================
C     o Trilinear scheme to interpolate variable to particle position
C       given by its fractional (real) index ix,jy,kz location
C       This routine is a straight forward generalization of the
C       bilinear interpolation scheme.
C
C     started: 2004.05.28 Antti Westerlund (antti.westerlund@fimr.fi)
C              and Sergio Jaramillo (sju@eos.ubc.ca).
C              (adopted from SUBROUTINE bilinear by Arne Biastoch)
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"

C     == routine arguments ==
      _RL ix, jy, kz
      _RL uu
      _RL var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER nu, bi, bj, myThid

C     == local variables ==
      INTEGER i1, j1, k1, i2, j2, k2
      _RL ddx, ddy, ddz

C     == end of interface ==

C to choose the u box in which the particle is found
C nu=0 for T, S
C nu=1 for u
C nu=2 for v
C nu=3 for Vorticity
C nu=4 for w

C--   find x-index according to grid-location of variable
      IF ( MOD(nu,2).EQ.0 ) THEN
        i1 = INT(ix)
        ddx = ix - DFLOAT(i1)
      ELSE
        i1 = NINT(ix)
        ddx = 0.5 _d 0 + ix - DFLOAT(i1)
      ENDIF
C--   find y-index according to grid-location of variable
      IF ( MOD(nu,4).LE.1 ) THEN
        j1 = INT(jy)
        ddy = jy - DFLOAT(j1)
      ELSE
        j1 = NINT(jy)
        ddy = 0.5 _d 0 + jy - DFLOAT(j1)
      ENDIF
C--   find z-index according to grid-location of variable
      IF ( nu.LE.3 ) THEN
        k1 = INT(kz)
        ddz = kz - DFLOAT(k1)
      ELSE
        k1 = NINT(kz)
        ddz = 0.5 _d 0 + kz - DFLOAT(k1)
      ENDIF

C--   Set the higher index for interpolation
      i2 = i1 + 1
      j2 = j1 + 1
      k2 = k1 + 1

C--   No need to change start/end horizontal index : use array overlap if needed
C--   Need to adjust start/end vertical index :
      k1 = MIN( MAX( k1, 1 ), Nr )
      k2 = MIN( MAX( k2, 1 ), Nr )

C     Trilinear interpolation, a straight forward generalization
C     of the bilinear interpolation scheme.
      uu = (1.-ddz)*( ( (1.-ddx)*(1.-ddy)*var(i1,j1,k1,bi,bj)
     &                +     ddx * ddy    *var(i2,j2,k1,bi,bj) )
     &              + (     ddx *(1.-ddy)*var(i2,j1,k1,bi,bj)
     &                + (1.-ddx)* ddy    *var(i1,j2,k1,bi,bj) ) )
     &   +   ddz   *( ( (1.-ddx)*(1.-ddy)*var(i1,j1,k2,bi,bj)
     &                +     ddx * ddy    *var(i2,j2,k2,bi,bj) )
     &              + (     ddx*(1.-ddy) *var(i2,j1,k2,bi,bj)
     &                + (1.-ddx)* ddy    *var(i1,j2,k2,bi,bj) ) )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE FLT_BILINEAR2D(
     I                           ix, jy,
     O                           uu,
     I                           var,
     I                           nu, bi, bj, myThid )

C     ==================================================================
C     SUBROUTINE FLT_BILINEAR2D
C     ==================================================================
C     o Bilinear scheme to interpolate 2-D variable to particle position
C       given by its fractional (real) index ix,jy location
C
C     started: Arne Biastoch abiastoch@ucsd.edu 13-Jan-2000
C              (adopted from SUBROUTINE bilinear)
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"

C     == routine arguments ==
      _RL ix, jy
      _RL uu
      _RL var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER nu, bi, bj, myThid

C     == local variables ==
      INTEGER i1, j1, i2, j2
      _RL ddx, ddy

C     == end of interface ==

C to choose the u box in which the particle is found
C nu=0 for T, S
C nu=1 for u
C nu=2 for v
C nu=3 for Vorticity
C nu=4 for w

C--   find x-index according to grid-location of variable
      IF ( MOD(nu,2).EQ.0 ) THEN
        i1 = INT(ix)
        ddx = ix - DFLOAT(i1)
      ELSE
        i1 = NINT(ix)
        ddx = 0.5 _d 0 + ix - DFLOAT(i1)
      ENDIF
C--   find y-index according to grid-location of variable
      IF ( MOD(nu,4).LE.1 ) THEN
        j1 = INT(jy)
        ddy = jy - DFLOAT(j1)
      ELSE
        j1 = NINT(jy)
        ddy = 0.5 _d 0 + jy - DFLOAT(j1)
      ENDIF

C--   Set the higher index for interpolation
      i2 = i1 + 1
      j2 = j1 + 1

C--   No need to change start/end index : use array overlap if needed

C bilinear interpolation (from numerical recipes)
      uu =            ( (1.-ddx)*(1.-ddy)*var(i1,j1,bi,bj)
     &                +     ddx * ddy    *var(i2,j2,bi,bj) )
     &              + (     ddx *(1.-ddy)*var(i2,j1,bi,bj)
     &                + (1.-ddx)* ddy    *var(i1,j2,bi,bj) )

      RETURN
      END
