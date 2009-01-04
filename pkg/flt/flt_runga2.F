C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_runga2.F,v 1.9 2009/01/04 00:58:23 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

      SUBROUTINE FLT_RUNGA2 (
     I                        myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE flt_runga2
C     ==================================================================
C
C     o This routine steps floats forward with second order Runge-Kutta
C
C     started: Arne Biastoch
C
C     changed: 2004.06.10 Antti Westerlund (antti.westerlund@helsinki.fi)
C              and Sergio Jaramillo (sju@eos.ubc.ca)
C
C     ==================================================================
C     SUBROUTINE flt_runga2
C     ==================================================================

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FLT.h"

C     == routine arguments ==

      _RL myTime
      INTEGER myIter, myThid

C     == local variables ==
      INTEGER bi, bj
      _RL global2local_i
      _RL global2local_j
c     _RL global2local_k

      INTEGER ip, kp, iG, jG
      _RL uu, vv, u1, v1
#ifdef ALLOW_3D_FLT
      _RL ww, w1, zt, zz, scalez
#endif
      _RL xx, yy, xt, yt
      _RL scalex, scaley
#ifdef USE_FLT_ALT_NOISE
      Real*8 PORT_RAND_NORM
#else
      Real*8 PORT_RAND
#undef _USE_INTEGERS
#ifdef _USE_INTEGERS
      INTEGER seed
      seed = -1
#else
      Real*8 seed
      seed = -1.d0
#endif
#endif

C     == end of interface ==

      DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
            DO ip=1,npart_tile(bi,bj)

C     If float has died move to level 0

               IF (
     & (tend(ip,bi,bj).NE.-1. .AND. myTime.GT. tend(ip,bi,bj)))
     & THEN
                  kpart(ip,bi,bj) = 0.
               ELSE
C     Start integration between tstart and tend (individual for each float)
                  IF (
     & (tstart(ip,bi,bj).EQ.-1. .OR. myTime.GE.tstart(ip,bi,bj))
     &  .AND.
     & (  tend(ip,bi,bj).EQ.-1. .OR. myTime.LE.  tend(ip,bi,bj))
     & .AND.
     & (   iup(ip,bi,bj).NE. -3.)
     & ) THEN

C     Convert to local indices

C Note: global2local_i and global2local_j use delX and delY.
C This may be a problem, especially IF you are using a curvilinear
C grid. More information below.
                     xx=global2local_i(xpart(ip,bi,bj),bi,bj,mythid)
                     yy=global2local_j(ypart(ip,bi,bj),bi,bj,mythid)
                     kp=INT(kpart(ip,bi,bj))

                     scalex=recip_dxF(INT(xx),INT(yy),bi,bj)
                     scaley=recip_dyF(INT(xx),INT(yy),bi,bj)
                     iG = myXGlobalLo + (bi-1)*sNx
                     jG = myYGlobalLo + (bj-1)*sNy


#ifdef ALLOW_3D_FLT
                     IF (iup(ip,bi,bj).EQ.-1.) THEN
c                        zz=global2local_k(kpart(ip,bi,bj),bi,bj,mythid)

C recip_drF is in units 1/r (so IF r is in m this is in 1/m)
                        scalez=recip_drF(kp)
C We should not do any special conversions for zz, since flt_trilinear
C expects it to be just a normal kpart type variable.
                        zz=kpart(ip,bi,bj)
                        CALL flt_trilinear(xx,yy,zz,uu,uVel,2,bi,bj)
                        CALL flt_trilinear(xx,yy,zz,vv,vVel,3,bi,bj)
                        CALL flt_trilinear(zz,yy,zz,ww,wVel,4,bi,bj)
                        zt=zz+0.5*deltaTmom*ww*scalez
                     ELSE
#endif
                        CALL flt_bilinear(xx,yy,uu,kp,uVel,2,bi,bj)
                        CALL flt_bilinear(xx,yy,vv,kp,vVel,3,bi,bj)
#ifdef ALLOW_3D_FLT
                     ENDIF
#endif

#ifdef USE_FLT_ALT_NOISE
C When using this alternative scheme the noise probably should not be added twice.
#else
                     IF (iup(ip,bi,bj).NE.-2.) THEN
                        uu = uu + uu*(PORT_RAND(seed)-0.5)*flt_noise
                        vv = vv + vv*(PORT_RAND(seed)-0.5)*flt_noise
#ifdef ALLOW_3D_FLT
#ifdef ALLOW_FLT_3D_NOISE
                        IF (iup(ip,bi,bj).EQ.-1.) THEN
                           ww = ww + ww*(PORT_RAND(seed)-0.5)*flt_noise
                        ENDIF
#endif
#endif
                     ENDIF
#endif

C xx and xt are in indices. Therefore it is necessary to multiply
C with a grid scale factor.

                     xt=xx+0.5*deltaTmom*uu*scalex
                     yt=yy+0.5*deltaTmom*vv*scaley

C     Second step

#ifdef ALLOW_3D_FLT
                     IF (iup(ip,bi,bj).EQ.-1.) THEN
                        CALL flt_trilinear(xt,yt,zt,u1,uVel,2,bi,bj)
                        CALL flt_trilinear(xt,yt,zt,v1,vVel,3,bi,bj)
                        CALL flt_trilinear(xt,yt,zt,w1,wVel,4,bi,bj)
                     ELSE
#endif
                        CALL flt_bilinear(xt,yt,u1,kp,uVel,2,bi,bj)
                        CALL flt_bilinear(xt,yt,v1,kp,vVel,3,bi,bj)
#ifdef ALLOW_3D_FLT
                     ENDIF
#endif

                     IF (iup(ip,bi,bj).NE.-2.) THEN
#ifdef USE_FLT_ALT_NOISE
                        u1 = u1 + port_rand_norm()*flt_noise
                        v1 = v1 + port_rand_norm()*flt_noise
#ifdef ALLOW_3D_FLT
#ifdef ALLOW_FLT_3D_NOISE
                        IF (iup(ip,bi,bj).EQ.-1.) THEN
                           w1 = w1 + port_rand_norm()*flt_noise
                        ENDIF
#endif
#endif

#else
                        u1 = u1 + u1*(PORT_RAND(seed)-0.5)*flt_noise
                        v1 = v1 + v1*(PORT_RAND(seed)-0.5)*flt_noise
#ifdef ALLOW_3D_FLT
#ifdef ALLOW_FLT_3D_NOISE
                        IF (iup(ip,bi,bj).EQ.-1.) THEN
                           w1 = w1 + w1*(PORT_RAND(seed)-0.5)*flt_noise
                        ENDIF
#endif
#endif

#endif
                     ENDIF

C xpart is in coordinates. Therefore it is necessary to multiply
C with a grid scale factor divided by the number grid points per
C geographical coordinate.
C
c This will only work if delX & delY are available.
c This may be a problem, especially IF you are using a curvilinear
c grid. In that case you have to replace them for the values of
c your grid, which can be troublesome.
                     xpart(ip,bi,bj) = xpart(ip,bi,bj)
     &                    + deltaTmom*u1*scalex*delX(iG)
                     ypart(ip,bi,bj) = ypart(ip,bi,bj)
     &                    + deltaTmom*v1*scaley*delY(jG)
#ifdef ALLOW_3D_FLT
                     IF (iup(ip,bi,bj).EQ.-1.) THEN
                        kpart(ip,bi,bj) = kpart(ip,bi,bj)
     &                                  + deltaTmom*w1*scalez
                     ENDIF
#endif

#ifdef ALLOW_3D_FLT
C If float is 3D, make sure that it remains in water
                     IF (iup(ip,bi,bj).EQ.-1.) THEN
C reflect on surface
                        IF (kpart(ip,bi,bj).LT.1.0)
     &                       kpart(ip,bi,bj)=1.0
     &                       +abs(1.0-kpart(ip,bi,bj))
C stop at bottom
                        IF (kpart(ip,bi,bj).GT.Nr)
     &                       kpart(ip,bi,bj)=Nr
                     ENDIF
#endif
                  ENDIF
               ENDIF

            ENDDO
         ENDDO
      ENDDO

      RETURN
      END
