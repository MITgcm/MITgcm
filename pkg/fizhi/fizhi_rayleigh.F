C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_rayleigh.F,v 1.2 2004/10/22 14:52:14 molod Exp $
C $Name:  $
                                                                                               
#include "FIZHI_OPTIONS.h"
      subroutine rayleigh(myid,pres,pkap,pekap,zsurf,u,v,t,s,im,jm,lm,
     .                                                bi,bj,rfu,rfv,rft)
C **********************************************************************
C
C  PURPOSE
C     To implement Rayleigh Friction
C
C  ARGUMENTS   DESCRIPTION
C
C  INPUT:
C     MYID .... PROCESS(OR) NUMBER
C     PRES .... MID-LEVEL PRESSURE IN MB
C     PKAP .... MID-LEVEL PRESSURE ** KAPPA
C     PEKAP ... EDGE-LEVEL PRESSURE ** KAPPA
C     ZSURF ... SURFACE ELEVATION IN M
C     U ....... U-WIND
C     V ....... V-WIND
C     TH ...... THETA (ACTUALLY REAL THETA * P0**KAPPA) IN K
C     S  ...... SPECIFIC HUMIDITY (KG/KG)
C     IM ...... NUMBER OF LONGITUDE POINTS
C     JM ...... NUMBER OF LATITUDE  POINTS
C     LM ...... NUMBER OF VERTICAL  LEVELS
C     BI ...... X-DIRECTION PROCESSOR INDEX
C     BJ ...... Y-DIRECTION PROCESSOR INDEX
C  OUTPUT:
C     RFU ..... U-WIND TENDENCY
C     RFV ..... V-WIND TENDENCY
C     RFT ..... THETA  TENDENCY
C
C **********************************************************************

      implicit none

#ifdef ALLOW_DIAGNOSTICS
#include "SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"
#endif

      integer myid,im,jm,lm,bi,bj
      _RL zsurf(im,jm),pres(im,jm,lm),pkap(im,jm,lm),pekap(im,jm,lm+1)
      _RL u(im,jm,lm),v(im,jm,lm),t(im,jm,lm),s(im,jm,lm)
      _RL rfu(im,jm,lm),rfv(im,jm,lm),rft(im,jm,lm)

      integer  i,j,L
      _RL rf(im,jm,lm)
      _RL z(im,jm,lm)
      _RL dz(im,jm,lm)
      _RL cpog, cpinv, virtcon, getcon

C **********************************************************************
C ****   APPLY RAYLEIGH FRICTION TO WIND (INCLUDE HEATING)           ***
C **********************************************************************

      cpog = getcon('CP')/getcon('GRAVITY')
      cpinv = 1.0/getcon('CP')
      virtcon = getcon('VIRTCON')

      do L=1,lm
       do j=1,jm
       do i=1,im
        dz(i,j,L) = cpog * (pekap(i,j,L+1)-pekap(i,j,L)) * t(i,j,L) *
     .                (1.+virtcon*s(i,j,L))
       enddo
       enddo
      enddo

      do L=1,lm
       do j=1,jm
       do i=1,im
        dz(i,j,L) = cpog * (pekap(i,j,L+1)-pekap(i,j,L)) * t(i,j,L) *
     .                (1.+virtcon*s(i,j,L))
       enddo
       enddo
      enddo

      do j=1,jm
      do i=1,im
       z(i,j,lm) = zsurf(i,j) +  0.5 * dz(i,j,lm)
      enddo
      enddo

      do L=lm-1,1,-1
       do j=1,jm
       do i=1,im
        z(i,j,L) = z(i,j,L+1) + 0.5 * (dz(i,j,L)+dz(i,j,L+1))
       enddo
       enddo
      enddo

      do L=1,lm
       do j=1,jm
       do i=1,im
        rf(i,j,L) = (2./3.)*(1+tanh((z(i,j,L)-80000.)/5000.))/86400.
        rfu(i,j,L) = - rf(i,j,L) * u(i,j,L)
        rfv(i,j,L) = - rf(i,j,L) * v(i,j,L)
        rft(i,j,L) = -(u(i,j,L)*rfu(i,j,L) + v(i,j,L)*rfv(i,j,L) )*cpinv
     .                        /pkap(i,j,L)
       enddo
       enddo
      enddo

#ifdef ALLOW_DIAGNOSTICS
      do L=1,lm
       if( irfu.ne.0 ) then
       do j=1,jm
       do i=1,im
        qdiag(i,j,irfu+L-1,bi,bj) = qdiag(i,j,irfu+L-1,bi,bj) + 
     .                  rfu(i,j,L)*86400
       enddo
       enddo
       endif

       if( irfv.ne.0 ) then
       do j=1,jm
       do i=1,im
        qdiag(i,j,irfv+L-1,bi,bj) = qdiag(i,j,irfv+L-1,bi,bj) + 
     .                  rfv(i,j,L)*86400
       enddo
       enddo
       endif

       if( irft.ne.0 ) then
       do j=1,jm
       do i=1,im
        qdiag(i,j,irft+L-1,bi,bj) = qdiag(i,j,irft+L-1,bi,bj) + 
     .                  rft(i,j,L)*86400
       enddo
       enddo
       endif
      enddo
#endif

#ifdef ALLOW_DIAGNOSTICS
      if( (bi.eq.1) .and. (bj.eq.1) ) then
       nrfu = nrfu + 1
       nrfv = nrfv + 1
       nrft = nrft + 1
      endif
#endif

      return
      end
