C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_runga2.F,v 1.12 2009/02/01 21:10:51 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

      SUBROUTINE FLT_RUNGA2 (
     I                        myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE FLT_RUNGA2
C     ==================================================================
C     o This routine steps floats forward with second order Runge-Kutta
C
C     started: Arne Biastoch
C
C     changed: 2004.06.10 Antti Westerlund (antti.westerlund@helsinki.fi)
C              and Sergio Jaramillo (sju@eos.ubc.ca)
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FLT.h"

C     == routine arguments ==
      _RL myTime
      INTEGER myIter, myThid

C     == Functions ==
      _RL global2local_i
      _RL global2local_j
c     _RL global2local_k
      EXTERNAL global2local_i
      EXTERNAL global2local_j
c     EXTERNAL global2local_k

C     == local variables ==
      INTEGER bi, bj
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER ip
      INTEGER ic, jc, kc, iG, jG
      _RL uu, vv, u1, v1
#ifdef ALLOW_3D_FLT
      _RL ww, w1, zt, zz, scalez
      _RL kzlo, kzhi
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

#ifdef ALLOW_3D_FLT
      kzlo = 0.5 _d 0
      kzhi = 0.5 _d 0 + DFLOAT(Nr)
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO ip=1,npart_tile(bi,bj)

C     If float has died move to level 0

           IF ( tend(ip,bi,bj).NE.-1. .AND. myTime.GT.tend(ip,bi,bj)
     &        ) THEN
            kpart(ip,bi,bj) = 0.
           ELSE
C     Start integration between tstart and tend (individual for each float)
            IF ( (tstart(ip,bi,bj).EQ.-1..OR.myTime.GE.tstart(ip,bi,bj))
     &      .AND.(  tend(ip,bi,bj).EQ.-1..OR.myTime.LE.  tend(ip,bi,bj))
     &      .AND.(   iup(ip,bi,bj).NE.-3.)
     &         ) THEN

C     Convert to local indices

C Note: global2local_i and global2local_j use delX and delY.
C This may be a problem, especially IF you are using a curvilinear
C grid. More information below.
              xx=global2local_i(xpart(ip,bi,bj),bi,bj,mythid)
              yy=global2local_j(ypart(ip,bi,bj),bi,bj,mythid)
              ic=NINT(xx)
              jc=NINT(yy)
              kc=NINT(kpart(ip,bi,bj))

              scalex=recip_dxF(ic,jc,bi,bj)
              scaley=recip_dyF(ic,jc,bi,bj)
              iG = myXGlobalLo + (bi-1)*sNx + ic-1
              jG = myYGlobalLo + (bj-1)*sNy + jc-1

#ifdef ALLOW_3D_FLT
              IF (iup(ip,bi,bj).EQ.-1.) THEN
c               zz=global2local_k(kpart(ip,bi,bj),bi,bj,mythid)

C recip_drF is in units 1/r (so IF r is in m this is in 1/m)
                scalez=rkSign*recip_drF(kc)
C We should not do any special conversions for zz, since flt_trilinear
C expects it to be just a normal kpart type variable.
                zz=kpart(ip,bi,bj)
                CALL FLT_TRILINEAR(xx,yy,zz,uu,uVel,1,bi,bj,myThid)
                CALL FLT_TRILINEAR(xx,yy,zz,vv,vVel,2,bi,bj,myThid)
                CALL FLT_TRILINEAR(xx,yy,zz,ww,wVel,4,bi,bj,myThid)
              ELSE
#else  /* ALLOW_3D_FLT */
              IF ( .TRUE. ) THEN
#endif /* ALLOW_3D_FLT */
                CALL FLT_BILINEAR(xx,yy,uu,uVel,kc,1,bi,bj,myThid)
                CALL FLT_BILINEAR(xx,yy,vv,vVel,kc,2,bi,bj,myThid)
              ENDIF

C When using this alternative scheme the noise probably should not be added twice.
#ifndef USE_FLT_ALT_NOISE
              IF (iup(ip,bi,bj).NE.-2.) THEN
                uu = uu + uu*(PORT_RAND(seed)-0.5)*flt_noise
                vv = vv + vv*(PORT_RAND(seed)-0.5)*flt_noise
#ifdef ALLOW_3D_FLT
#ifdef ALLOW_FLT_3D_NOISE
                IF (iup(ip,bi,bj).EQ.-1.) THEN
                  ww = ww + ww*(PORT_RAND(seed)-0.5)*flt_noise
                ENDIF
#endif
#endif /* ALLOW_3D_FLT */
              ENDIF
#endif

C xx and xt are in indices. Therefore it is necessary to multiply
C with a grid scale factor.

              xt=xx+0.5*deltaTmom*uu*scalex
              yt=yy+0.5*deltaTmom*vv*scaley

C     Second step

#ifdef ALLOW_3D_FLT
              IF (iup(ip,bi,bj).EQ.-1.) THEN
                zt=zz+0.5*deltaTmom*ww*scalez
                CALL FLT_TRILINEAR(xt,yt,zt,u1,uVel,1,bi,bj,myThid)
                CALL FLT_TRILINEAR(xt,yt,zt,v1,vVel,2,bi,bj,myThid)
                CALL FLT_TRILINEAR(xt,yt,zt,w1,wVel,4,bi,bj,myThid)
              ELSE
#else  /* ALLOW_3D_FLT */
              IF ( .TRUE. ) THEN
#endif /* ALLOW_3D_FLT */
                CALL FLT_BILINEAR(xt,yt,u1,uVel,kc,1,bi,bj,myThid)
                CALL FLT_BILINEAR(xt,yt,v1,vVel,kc,2,bi,bj,myThid)
              ENDIF

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
#endif /* ALLOW_3D_FLT */

#else /* USE_FLT_ALT_NOISE */
                u1 = u1 + u1*(PORT_RAND(seed)-0.5)*flt_noise
                v1 = v1 + v1*(PORT_RAND(seed)-0.5)*flt_noise
#ifdef ALLOW_3D_FLT
#ifdef ALLOW_FLT_3D_NOISE
                IF (iup(ip,bi,bj).EQ.-1.) THEN
                  w1 = w1 + w1*(PORT_RAND(seed)-0.5)*flt_noise
                ENDIF
#endif
#endif /* ALLOW_3D_FLT */

#endif /* USE_FLT_ALT_NOISE */
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
     &             + deltaTmom*u1*scalex*delX(iG)
              ypart(ip,bi,bj) = ypart(ip,bi,bj)
     &             + deltaTmom*v1*scaley*delY(jG)
#ifdef ALLOW_3D_FLT
              IF (iup(ip,bi,bj).EQ.-1.) THEN
                kpart(ip,bi,bj) = kpart(ip,bi,bj)
     &                          + deltaTmom*w1*scalez
              ENDIF
#endif /* ALLOW_3D_FLT */

#ifdef ALLOW_3D_FLT
C If float is 3D, make sure that it remains in water
              IF (iup(ip,bi,bj).EQ.-1.) THEN
C reflect on surface
                IF (kpart(ip,bi,bj).LT.kzlo) kpart(ip,bi,bj)=kzlo
     &                                      +kzlo-kpart(ip,bi,bj)
C stop at bottom
                IF (kpart(ip,bi,bj).GT.kzhi) kpart(ip,bi,bj)=kzhi
              ENDIF
#endif /* ALLOW_3D_FLT */
            ENDIF
           ENDIF

C-    end ip loop
         ENDDO
C-    end bi,bj loops
       ENDDO
      ENDDO

      RETURN
      END
