C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_rayleigh.F,v 1.1 2004/10/20 18:27:36 molod Exp $
C $Name:  $
                                                                                               
#include "FIZHI_OPTIONS.h"
      subroutine rayleigh(myid,pres,preskappa,psurf,u,v,im,jm,lm,bi,bj,
     .                                                      rfu,rfv,rft)
C **********************************************************************
C
C  PURPOSE
C     To implement Rayleigh Friction
C
C  ARGUMENTS   DESCRIPTION
C     MYID .... PROCESS(OR) NUMBER
C     PRES .... VALUE OF MID-LEVEL PRESSURE
C     PZ ...... VALUE OF SURFACE PRESSURE - PTOP
C     UZ ...... VALUE OF U-WIND IN MODEL FOR N-1 TIME STEP
C     VZ ...... VALUE OF V-WIND IN MODEL FOR N-1 TIME STEP
C     IM ...... NUMBER OF LONGITUDE POINTS IN MODEL
C     JM ...... NUMBER OF LATITUDE  POINTS IN MODEL
C     LM ...... NUMBER OF VERTICAL  LEVELS IN MODEL
C     BI ...... X-DIRECTION PROCESSOR INDEX
C     BJ ...... Y-DIRECTION PROCESSOR INDEX
C     RFU ..... VALUE OF U-WIND TENDENCY
C     RFV ..... VALUE OF V-WIND TENDENCY
C     RFT ..... VALUE OF THETA  TENDENCY
C
C **********************************************************************

      implicit none

#ifdef ALLOW_DIAGNOSTICS
#include "SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"
#endif

      integer myid,im,jm,lm,bi,bj

      real psurf(im,jm)
      real pres(im,jm,lm)
      real preskappa(im,jm,lm)
      real u(im,jm,lm)
      real v(im,jm,lm)
      real rfu(im,jm,lm)
      real rfv(im,jm,lm)
      real rft(im,jm,lm)

      integer  i,j,L
      real rf(im,jm,lm)
      real z(im,jm,lm)
      real z1(im,jm,lm)
      real z2(im,jm,lm)
      real cpinv, getcon

C **********************************************************************
C ****         APPLY RAYLEIGH FRICTION TO WIND TENDENCIES            ***
C **********************************************************************

      cpinv = 1.0/getcon('CP')

      do L=1,lm

      do j=1,jm
      do i=1,im

       z1(i,j,L) = -7e2*log(pres(i,j,1)/psurf(i,j))
       z2(i,j,L) = -7e3*log(pres(i,j,2)/psurf(i,j))
       z(i,j,L) = -7e3*log(pres(i,j,L)/psurf(i,j))
       rf(i,j,L) = 0.40*(1+tanh((z(i,j,L)-z2(i,j,L))/z1(i,j,L)))/86400

       rfu(i,j,L) = - rf(i,j,L) * u(i,j,L)
       rfv(i,j,L) = - rf(i,j,L) * v(i,j,L)
       rft(i,j,L) = -(u(i,j,L)*rfu(i,j,L) + v(i,j,L)*rfv(i,j,L) )*cpinv
     .                        /preskappa(i,j,L)

      enddo
      enddo

      if( irfu.ne.0 ) then
      do j=1,jm
      do i=1,im
      qdiag(i,j,irfu+L-1,bi,bj) = qdiag(i,j,irfu+L-1,bi,bj) + 
     .                 rfu(i,j,L)*86400
      enddo
      enddo
      endif

      if( irfv.ne.0 ) then
      do j=1,jm
      do i=1,im
      qdiag(i,j,irfv+L-1,bi,bj) = qdiag(i,j,irfv+L-1,bi,bj) + 
     .                 rfv(i,j,L)*86400
      enddo
      enddo
      endif

      if( irft.ne.0 ) then
      do j=1,jm
      do i=1,im
      qdiag(i,j,irft+L-1,bi,bj) = qdiag(i,j,irft+L-1,bi,bj) + 
     .                 rft(i,j,L)*86400
      enddo
      enddo
      endif

      enddo

      nrfu = nrfu + 1
      nrfv = nrfv + 1
      nrft = nrft + 1

      return
      end
