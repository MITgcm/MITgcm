      SUBROUTINE PQCHECK ( PQZ,PZ,DP,IM,JM,LM,delt)
C***********************************************************************
C  Purpose                                                              
C     Check Specific Humidity Field for Negative values                 
C
C  Argument Description
C     PQZ ........ (ps-ptop)*Specific Humidity (mb g/g)                                  
C     PZ ......... Pi = ps-ptop (mb)                                  
C     DP .......   Delta Pressure                                              
C     IM ......... Zonal      Dimension
C     JM ......... Meridional Dimension
C     LM ......... Vertical   Dimension
C     DELT ....... Timestep  (Seconds)
C
C***********************************************************************
                                                                        
      implicit none

      integer im,jm,lm
      real delt

      real     PQZ(IM,JM,LM), DP(IM,JM,LM)                        
      real      PZ(IM,JM)

      integer i,j,L,LM1
      real    getcon,grav,ddsig
      real    tmp1(im,jm)

      grav = getcon('GRAVITY')
 
c Fill Negative Specific Humidities
c ---------------------------------
      DO  L = 2,LM
      LM1   = L-1                                                     
      do j=1,jm
      do i=1,im
      DDSIG = DP(I,J,LM1)/DP(I,J,L)                                     
      IF( PQZ(I,j,LM1).LT.0.0 ) THEN                                
          PQZ(I,j,L  ) = PQZ(I,j,L) + PQZ(I,j,LM1)*DDSIG                   
          PQZ(I,j,LM1) = 0.0
      ENDIF
      ENDDO
      ENDDO
      ENDDO

      do j=1,jm
      do i=1,im
      IF( PQZ(I,j,LM).LT.0.0 ) PQZ(I,j,LM) = 0.0
      ENDDO
      ENDDO

      RETURN                                                            
      END                                                               

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
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      implicit none

c Input Variables
c ---------------
      integer im,jm,lm
      real    pq(im,jm,lm),dlam(im),dphi(jm),dp(im,jm,lm)

c Local Variables
c ---------------
      integer  i,j,l,im1,ip1,imax,m
      real     lam(im), phi(jm)
      real     array(6)
      real     pi,a,getcon,undef
      real     qmax,qval,sum,fact

      real        dxu(im,jm)
      real        dxv(im,jm)
      real        dxp(im,jm)
      real        dyv(im,jm)
      real        dyp(im,jm)

      real, allocatable, save :: d2p(:,:)

      logical first
      data    first /.true./

C *********************************************************
C ****                 Initialization                  ****
C *********************************************************

      if (first) then

      allocate ( d2p(im,jm) )
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

      first = .false.
      endif

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
      array(5) = pq(i,j,L-1)*d2p(i,j)*dp(i,j,L)1)
      endif
      if( L.eq.lm   ) then
      array(6) = -undef
      else
      array(6) = pq(i,j,L+1)*d2p(i,j)*dp(i,j,L)1)
      endif

      call maxval (array,6,-qval,qmax,imax)

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

      subroutine maxval (q,im,qval,qmax,imax)
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
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************
      implicit none
      integer  im, i, imax
      real   q(im), qmax, qval
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
