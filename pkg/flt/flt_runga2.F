C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_runga2.F,v 1.1 2001/09/13 17:43:56 adcroft Exp $
C $Name:  $

#include "FLT_CPPOPTIONS.h"

      subroutine flt_runga2 (
     I                        myCurrentIter, 
     I                        myCurrentTime, 
     I                        myThid
     &                     )

c     ==================================================================
c     SUBROUTINE flt_runga2
c     ==================================================================
c
c     o This routine steps floats forward with second order Runga-Kutta
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
#ifdef ALLOW_3D_FLT
#include "GW.h"
#endif

c     == routine arguments ==

      INTEGER myCurrentIter, myThid
      _RL myCurrentTime
      INTEGER bi, bj
      _RL global2local_i
      _RL global2local_j

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
      Real*8 PORT_RAND

c     == end of interface ==

      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

      do ip=1,npart_tile(bi,bj)

c     If float has died move to level 0
c
         if(
     &  (tend(ip,bi,bj).ne.-1. .and. myCurrentTime.gt. tend(ip,bi,bj))
     &      ) then

            kpart(ip,bi,bj) = 0.

         else
c     Start integration between tstart and tend (individual for each float)
c
         if(
     &  (tstart(ip,bi,bj).eq.-1. .or. myCurrentTime.ge.tstart(ip,bi,bj))
     &   .and.
     &  (  tend(ip,bi,bj).eq.-1. .or. myCurrentTime.le.  tend(ip,bi,bj))
     &   .and.
     &  (   iup(ip,bi,bj).ne. -3.)
     &      ) then

c     Convert to local indices
c
            xx=global2local_i(xpart(ip,bi,bj),bi,bj,mythid)
            yy=global2local_j(ypart(ip,bi,bj),bi,bj,mythid)
            kp=INT(kpart(ip,bi,bj))

            scalex=recip_dxF(INT(xx),INT(yy),bi,bj)
            scaley=recip_dyF(INT(xx),INT(yy),bi,bj)
            iG = myXGlobalLo + (bi-1)*sNx
            jG = myYGlobalLo + (bj-1)*sNy


#ifdef ALLOW_3D_FLT
            if (iup(ip,bi,bj).eq.-1.) then
               scalez=recip_drF(kp)
               zt=global2local_j(kpart(ip,bi,bj),bi,bj,mythid)
               call flt_bilinear3D(xx,yy,uu,zp,uVel,2,bi,bj)
               call flt_bilinear3D(xx,yy,vv,zp,vVel,3,bi,bj)
               call flt_bilinear3D(zz,yy,ww,zp,wVel,4,bi,bj)
               zt=zz+0.5*deltaTmom*zz*scalez
            else
#endif
               call flt_bilinear(xx,yy,uu,kp,uVel,2,bi,bj)
               call flt_bilinear(xx,yy,vv,kp,vVel,3,bi,bj)
#ifdef ALLOW_3D_FLT
            endif
#endif

            if (iup(ip,bi,bj).ne.-2.) then
               uu = uu + uu*(PORT_RAND()-0.5)*flt_noise
               vv = vv + vv*(PORT_RAND()-0.5)*flt_noise
            endif

c xx and xt are in indices. Therefore it is necessary to multiply
c with a grid scale factor.
c
            xt=xx+0.5*deltaTmom*uu*scalex
            yt=yy+0.5*deltaTmom*vv*scaley

c     Second step
c
            
#ifdef ALLOW_3D_FLT
            if (iup(ip,bi,bj).eq.-1.) then
               call flt_bilinear3D(xt,yt,u1,zt,uVel,2,bi,bj)
               call flt_bilinear3D(xt,yt,v1,zt,vVel,3,bi,bj)
               call flt_bilinear3D(xx,yy,w1,zt,wVel,4,bi,bj)
               kpart(ip,bi,bj) = kpart(ip,bi,bj) + deltaTmom*w1*scalez
            else
#endif
            call flt_bilinear(xt,yt,u1,kp,uVel,2,bi,bj)
            call flt_bilinear(xt,yt,v1,kp,vVel,3,bi,bj)
#ifdef ALLOW_3D_FLT
            endif
#endif

            if (iup(ip,bi,bj).ne.-2.) then
               u1 = u1 + u1*(PORT_RAND()-0.5)*flt_noise
               v1 = v1 + v1*(PORT_RAND()-0.5)*flt_noise
            endif

c xpart is in coordinates. Therefore it is necessary to multiply
c with a grid scale factor divided by the number grid points per
c geographical coordinate.
c
            xpart(ip,bi,bj) = xpart(ip,bi,bj) 
     &                        + deltaTmom*u1*scalex*delX(iG)
            ypart(ip,bi,bj) = ypart(ip,bi,bj) 
     &                        + deltaTmom*v1*scaley*delY(jG)

         endif
      endif

      enddo

      ENDDO
      ENDDO
c
      return
      end
