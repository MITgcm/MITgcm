C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_fillnegs.F,v 1.9 2009/04/02 20:54:03 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      SUBROUTINE QCHECK ( idim1,idim2,jdim1,jdim2,ldim,Nsx,Nsy,im1,im2,
     .     jm1,jm2,bi,bj,dp,qz)
C***********************************************************************
C  Purpose
C     Check Specific Humidity Field for Negative values
C
C  Argument Description
C     IDIM1 .... Start Zonal Dimension
C     IDIM2 .... End Zonal Dimension
C     JDIM1 .... Start Meridional Dimension
C     JDIM2 .... End Meridional Dimension
C     IM1 ...... Start Zonal Span
C     IM2 ...... End Zonal Span
C     JM1 ...... Start Meridional Span
C     JM2 ...... End Meridional Span
C     LDIM ..... Vertical   Dimension
C     DP ....... Delta Pressure
C     QZ ........Specific Humidity (g/g)
C***********************************************************************
      implicit none

      integer idim1,idim2,jdim1,jdim2,Ldim,im1,im2,jm1,jm2
      integer Nsx,Nsy,bi,bj
      _RL qz(idim1:idim2,jdim1:jdim2,ldim,Nsx,Nsy)
      _RL dp(idim1:idim2,jdim1:jdim2,ldim,Nsx,Nsy)

      integer i,j,L,LM1
      _RL ddsig

c Fill Negative Specific Humidities
c ---------------------------------
      DO L=2,Ldim
      LM1 = L-1
      do j=jm1,jm2
      do i=im1,im2
       ddsig = dp(i,j,LM1,bi,bj)/dp(i,j,L,bi,bj)
       if( qz(i,j,LM1,bi,bj).lt.0.0  _d 0) then
        qz(i,j,L,bi,bj  ) = qz(i,j,L,bi,bj) + qz(i,j,LM1,bi,bj)*ddsig
        qz(i,j,LM1,bi,bj) = 0.0 _d 0
       endif
      enddo
      enddo
      enddo

      do j=jm1,jm2
      do i=im1,im2
       if(qz(i,j,Ldim,bi,bj).lt.0.0  _d 0)qz(i,j,Ldim,bi,bj) = 0.0 _d 0
      enddo
      enddo

      return
      end

      subroutine tracer_fill ( pq,im,jm,lm,dlam,dphi,dp)
C***********************************************************************
C  PURPOSE
C     Fill negative tracer values using local borrowing
C
C  INPUT
C     pq ..... Mass-weighted (PI) Tracer
C     im ..... Zonal      Dimension
C     jm ..... Meridional Dimension
C     lm ..... Vertical   Dimension
C     dlam ... Zonal      Grid Increment
C     dphi ... Meridional Grid Increment
C     dp ..... Vertical   Grid Increment
C
C  Note:
C     If no immediate surrounding value is large enough to fill negative
C     value,
C     the sum of immediate surrounding positive values is tried.
C     If sum is not large enough, tracer is simply set to zero.
C
C***********************************************************************

      implicit none

c Input Variables
c ---------------
      integer im,jm,lm
      _RL    pq(im,jm,lm),dlam(im),dphi(jm),dp(im,jm,lm)

c Local Variables
c ---------------
      integer  i,j,l,im1,ip1,imax,m
      _RL     lam(im), phi(jm)
      _RL     array(6)
      _RL     pi,a,getcon,undef
      _RL     qmax,qval,sum,fact

      _RL        dxu(im,jm)
      _RL        dxv(im,jm)
      _RL        dxp(im,jm)
      _RL        dyv(im,jm)
      _RL        dyp(im,jm)

      _RL d2p(im,jm)

C *********************************************************
C ****                 Initialization                  ****
C *********************************************************

      pi = 4.0*atan(1.0)
      a  = getcon('EARTH RADIUS')

c Compute Longitudes
c ------------------
      lam(1) = -pi
      do i=2,im
      lam(i) = lam(i-1) + dlam(i-1)
      enddo

c Compute Latitudes
c -----------------
      phi(1) = -pi/2.
      do j=2,jm-1
      phi(j) = phi(j-1) + dphi(j-1)
      enddo
      phi(jm) =  pi/2.

c Compute DXU and DYV
c -------------------
      do j=2,jm-1
      do i=1,im
      dxu(i,j) = a*cos(phi(j))*dlam(i)
      enddo
      enddo

      do j=2,jm-2
      do i=1,im
      dyv(i,j) = a*dphi(j)
      enddo
      enddo
      do i=1,im
      dyv(i,1)    = a*(dphi(1)   +0.5*dphi(2)   )
      dyv(i,jm-1) = a*(dphi(jm-1)+0.5*dphi(jm-2))
      enddo

c Compute DXP and DXV
c -------------------
      do j=2,jm-1
      im1 =  im
      do i=1,im
      dxp(i,j) = ( dxu(i,j)+dxu(im1,j) )*0.5
      im1 = i
      enddo
      enddo

      do j=2,jm-2
      do i=1,im
      dxv(i,j) = ( dxp(i,j)+dxp(i,j+1) )*0.5
      enddo
      enddo

c Compute DYP
c -----------
      do j=3,jm-2
      do i=1,im
      dyp(i,j) = ( dyv(i,j)+dyv(i,j-1) )*0.5
      enddo
      enddo
      do i=1,im
      dyp(i,2)    = dyv(i,1)
      dyp(i,jm-1) = dyv(i,jm-1)
      enddo

c Compute Area Factor D2P
c -----------------------
      do j=3,jm-2
      do i=1,im
      d2p(i,j) = 0.5*( dxv(i,j)+dxv(i,j-1) )*dyp(i,j)
      enddo
      enddo
      do i=1,im
      d2p(i,2)    = dxv(i,2)   *dyp(i,2)
      d2p(i,jm-1) = dxv(i,jm-2)*dyp(i,jm-1)
      enddo

      undef = getcon('UNDEF')

C *********************************************************
C ****             Fill Negative Values                ****
C *********************************************************

      do l=1,lm
      do j=2,jm-1

      im1 = im-1
      i   = im
      do ip1=1,im

      if( pq(i,j,L).lt.0.0 ) then

      qval     = pq(i  ,j,L)*d2p(i  ,j)*dp(i,j,L)
      array(1) = pq(ip1,j,L)*d2p(ip1,j)*dp(i,j,L)
      array(2) = pq(im1,j,L)*d2p(im1,j)*dp(i,j,L)

      if( j.eq.jm-1 ) then
      array(3) = -undef
      else
      array(3) = pq(i,j+1,L)*d2p(i,j+1)*dp(i,j,L)
      endif
      if( j.eq.2    ) then
      array(4) = -undef
      else
      array(4) = pq(i,j-1,L)*d2p(i,j-1)*dp(i,j,L)
      endif
      if( L.eq.1    ) then
      array(5) = -undef
      else
      array(5) = pq(i,j,L-1)*d2p(i,j)*dp(i,j,L)
      endif
      if( L.eq.lm   ) then
      array(6) = -undef
      else
      array(6) = pq(i,j,L+1)*d2p(i,j)*dp(i,j,L)
      endif

      call maxval1 (array,6,-qval,qmax,imax)

      if( imax.eq.0 ) then
          sum = 0.0
          do m=1,6
          if( array(m).gt.0.0 ) sum = sum + array(m)
          enddo
             if( sum.gt.-qval ) then
               fact = 1.0 + qval/sum
               if( array(1).gt.0 ) pq(ip1,j,L) = pq(ip1,j,L) * fact
               if( array(2).gt.0 ) pq(im1,j,L) = pq(im1,j,L) * fact
               if( array(3).gt.0 ) pq(i,j+1,L) = pq(i,j+1,L) * fact
               if( array(4).gt.0 ) pq(i,j-1,L) = pq(i,j-1,L) * fact
               if( array(5).gt.0 ) pq(i,j,L-1) = pq(i,j,L-1) * fact
               if( array(6).gt.0 ) pq(i,j,L+1) = pq(i,j,L+1) * fact
                                   pq(i,j,L)   = 0.0
             else
               pq(i,j,L) = 0.0
             endif
      else
          if( imax.eq.1 ) pq(ip1,j,L) = pq(ip1,j,L) +
     .                                 pq(i,j,L)*d2p(i,j)/d2p(ip1,j)
          if( imax.eq.2 ) pq(im1,j,L) = pq(im1,j,L) +
     .                                 pq(i,j,L)*d2p(i,j)/d2p(im1,j)
          if( imax.eq.3 ) pq(i,j+1,L) = pq(i,j+1,L) +
     .                                 pq(i,j,L)*d2p(i,j)/d2p(i,j+1)
          if( imax.eq.4 ) pq(i,j-1,L) = pq(i,j-1,L) +
     .                                 pq(i,j,L)*d2p(i,j)/d2p(i,j-1)
          if( imax.eq.5 ) pq(i,j,L-1) = pq(i,j,L-1) +
     .                                 pq(i,j,L)*dp(i,j,L) /dp(i,j,L-1)
          if( imax.eq.6 ) pq(i,j,L+1) = pq(i,j,L+1) +
     .                                 pq(i,j,L)*dp(i,j,L) /dp(i,j,L+1)
                          pq(i,j,L)   = 0.0
      endif

      endif  ! End pq<0 Test

      im1 = i
      i   = ip1
      enddo
      enddo
      enddo

      return
      end

      subroutine maxval1 (q,im,qval,qmax,imax)
C***********************************************************************
C  PURPOSE
C     Find the location and value of the array element which is greater
C     than a prescribed value.
C
C  INPUT
C     q ...... Array Elements
C     im ..... Dimension of Array q
C     qval ... Prescribed Value
C
C  OUTPUT
C     qmax ... Largest Array element which is greater than qval
C     imax ... Location of Largest Array Element
C
C  Note:
C     If no array element is larger than qval, then imax = 0
C
C***********************************************************************
      implicit none
      integer  im, i, imax
      _RL   q(im), qmax, qval
      qmax = qval
      imax = 0
      do i=1,im
      if( q(i).gt.qmax ) then
      qmax = q(i)
      imax =   i
      endif
      enddo
      return
      end
