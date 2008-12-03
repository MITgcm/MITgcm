C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_runga2.F,v 1.8 2008/12/03 01:32:35 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

      subroutine flt_runga2 (
     I                        myCurrentIter, 
     I                        myCurrentTime, 
     I                        myThid
     &                     )

c     ==================================================================
c     SUBROUTINE flt_runga2
c     ==================================================================
c
c     o This routine steps floats forward with second order Runge-Kutta
c
c     started: Arne Biastoch 
c
c     changed: 2004.06.10 Antti Westerlund (antti.westerlund@helsinki.fi) 
c              and Sergio Jaramillo (sju@eos.ubc.ca)
c
c     ==================================================================
c     SUBROUTINE flt_runga2
c     ==================================================================

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FLT.h"

c     == routine arguments ==

      INTEGER myCurrentIter, myThid
      _RL myCurrentTime
      INTEGER bi, bj
      _RL global2local_i
      _RL global2local_j
      _RL global2local_k
c     == local variables ==

      integer ip, kp, iG, jG
      _RL phi, uu, vv, u1, v1
#ifdef ALLOW_3D_FLT
      _RL ww, w1, zt, zz, scalez
#endif
      _RL xx, yy, xt, yt
      _RL scalex, scaley
      character*(max_len_mbuf) msgbuf
      _RL npart_dist
#ifdef USE_FLT_ALT_NOISE
      Real*8 PORT_RAND_NORM
#else
      Real*8 PORT_RAND
#undef _USE_INTEGERS
#ifdef _USE_INTEGERS
      integer seed
      seed = -1
#else
      Real*8 seed
      seed = -1.d0
#endif
#endif

c     == end of interface ==

      DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
            do ip=1,npart_tile(bi,bj)

c     If float has died move to level 0
c
               if(
     & (tend(ip,bi,bj).ne.-1. .and. myCurrentTime.gt. tend(ip,bi,bj)))
     & then
                  kpart(ip,bi,bj) = 0.
               else
c     Start integration between tstart and tend (individual for each float)
c
                  if(
     & (tstart(ip,bi,bj).eq.-1. .or. myCurrentTime.ge.tstart(ip,bi,bj))
     &  .and.
     & (  tend(ip,bi,bj).eq.-1. .or. myCurrentTime.le.  tend(ip,bi,bj))
     & .and.
     & (   iup(ip,bi,bj).ne. -3.)
     & ) then

c     Convert to local indices
c

C Note: global2local_i and global2local_j use delX and delY.
C This may be a problem, especially if you are using a curvilinear 
C grid. More information below.
                     xx=global2local_i(xpart(ip,bi,bj),bi,bj,mythid)
                     yy=global2local_j(ypart(ip,bi,bj),bi,bj,mythid)
                     kp=INT(kpart(ip,bi,bj))

                     scalex=recip_dxF(INT(xx),INT(yy),bi,bj)
                     scaley=recip_dyF(INT(xx),INT(yy),bi,bj)
                     iG = myXGlobalLo + (bi-1)*sNx
                     jG = myYGlobalLo + (bj-1)*sNy


#ifdef ALLOW_3D_FLT
                     if (iup(ip,bi,bj).eq.-1.) then
c                        zz=global2local_k(kpart(ip,bi,bj),bi,bj,mythid)

c recip_drF is in units 1/r (so if r is in m this is in 1/m) 
                        scalez=recip_drF(kp)
c We should not do any special conversions for zz, since flt_trilinear
c expects it to be just a normal kpart type variable.
                        zz=kpart(ip,bi,bj)
                        call flt_trilinear(xx,yy,zz,uu,uVel,2,bi,bj)
                        call flt_trilinear(xx,yy,zz,vv,vVel,3,bi,bj)
                        call flt_trilinear(zz,yy,zz,ww,wVel,4,bi,bj)
                        zt=zz+0.5*deltaTmom*ww*scalez
                     else
#endif
                        call flt_bilinear(xx,yy,uu,kp,uVel,2,bi,bj)
                        call flt_bilinear(xx,yy,vv,kp,vVel,3,bi,bj)
#ifdef ALLOW_3D_FLT
                     endif
#endif

#ifdef USE_FLT_ALT_NOISE
c When using this alternative scheme the noise probably should not be added twice.
#else
                     if (iup(ip,bi,bj).ne.-2.) then
                        uu = uu + uu*(PORT_RAND(seed)-0.5)*flt_noise
                        vv = vv + vv*(PORT_RAND(seed)-0.5)*flt_noise
#ifdef ALLOW_3D_FLT
#ifdef ALLOW_FLT_3D_NOISE
                        if (iup(ip,bi,bj).eq.-1.) then
                           ww = ww + ww*(PORT_RAND(seed)-0.5)*flt_noise
                        endif
#endif
#endif
                     endif
#endif

c xx and xt are in indices. Therefore it is necessary to multiply
c with a grid scale factor.
c
                     xt=xx+0.5*deltaTmom*uu*scalex
                     yt=yy+0.5*deltaTmom*vv*scaley

c     Second step
c
            
#ifdef ALLOW_3D_FLT
                     if (iup(ip,bi,bj).eq.-1.) then
                        call flt_trilinear(xt,yt,zt,u1,uVel,2,bi,bj)
                        call flt_trilinear(xt,yt,zt,v1,vVel,3,bi,bj)
                        call flt_trilinear(xt,yt,zt,w1,wVel,4,bi,bj)
                     else
#endif
                        call flt_bilinear(xt,yt,u1,kp,uVel,2,bi,bj)
                        call flt_bilinear(xt,yt,v1,kp,vVel,3,bi,bj)
#ifdef ALLOW_3D_FLT
                     endif
#endif

                     if (iup(ip,bi,bj).ne.-2.) then
#ifdef USE_FLT_ALT_NOISE
                        u1 = u1 + port_rand_norm()*flt_noise
                        v1 = v1 + port_rand_norm()*flt_noise
#ifdef ALLOW_3D_FLT
#ifdef ALLOW_FLT_3D_NOISE
                        if (iup(ip,bi,bj).eq.-1.) then
                           w1 = w1 + port_rand_norm()*flt_noise
                        endif
#endif
#endif

#else
                        u1 = u1 + u1*(PORT_RAND(seed)-0.5)*flt_noise
                        v1 = v1 + v1*(PORT_RAND(seed)-0.5)*flt_noise
#ifdef ALLOW_3D_FLT
#ifdef ALLOW_FLT_3D_NOISE
                        if (iup(ip,bi,bj).eq.-1.) then
                           w1 = w1 + w1*(PORT_RAND(seed)-0.5)*flt_noise
                        endif
#endif
#endif

#endif
                     endif

c xpart is in coordinates. Therefore it is necessary to multiply
c with a grid scale factor divided by the number grid points per
c geographical coordinate.
c
C This will only work if delX & delY are available.
C This may be a problem, especially if you are using a curvilinear 
C grid. In that case you have to replace them for the values of 
C your grid, which can be troublesome.
                     xpart(ip,bi,bj) = xpart(ip,bi,bj) 
     &                    + deltaTmom*u1*scalex*delX(iG)
                     ypart(ip,bi,bj) = ypart(ip,bi,bj) 
     &                    + deltaTmom*v1*scaley*delY(jG)
#ifdef ALLOW_3D_FLT
                     if (iup(ip,bi,bj).eq.-1.) then
                        kpart(ip,bi,bj) = kpart(ip,bi,bj) 
     &                                  + deltaTmom*w1*scalez
                     endif
#endif

#ifdef ALLOW_3D_FLT
c If float is 3D, make sure that it remains in water
                     if (iup(ip,bi,bj).eq.-1.) then
c reflect on surface
                        if(kpart(ip,bi,bj).lt.1.0) 
     &                       kpart(ip,bi,bj)=1.0
     &                       +abs(1.0-kpart(ip,bi,bj))
c stop at bottom
                        if(kpart(ip,bi,bj).gt.Nr) 
     &                       kpart(ip,bi,bj)=Nr
                     endif
#endif 
                  endif
               endif
            
            enddo
         ENDDO
      ENDDO
c
      return
      end
