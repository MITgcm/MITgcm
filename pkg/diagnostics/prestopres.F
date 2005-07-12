      subroutine prestopres ( qprs,qinp,pkz,pksrf,ptop,p,im,jm,lm )
C***********************************************************************
C                                               
C PURPOSE
C   To interpolate an arbitrary quantity to Specified Pressure Levels
C
C INPUT                                         
C   QINP .. QINP (im,jm,lm) Arbitrary Input Quantity
C   PKZ ... PKZ  (im,jm,lm) Pressure to the Kappa at Input Levels
C   PKSRF . PKSRF(im,jm) Surface Pressure to the Kappa
C   PTOP .. Pressure at Input-Level-Edge (1) (top of model)
C   P ..... Output Pressure Level (mb)          
C   IM .... Longitude Dimension of Input
C   JM .... Latitude  Dimension of Input
C   LM .... Vertical  Dimension of Input
C
C OUTPUT                                        
C   QPRS .. QPRS (im,jm) Arbitrary Quantity at Pressure p
C
C NOTE
C   Quantity is interpolated Linear in P**Kappa.
C   Between  PTOP**Kappa and PKZ(1),  quantity is extrapolated.
C   Between PKSRF**Kappa and PKZ(LM), quantity is extrapolated.
C   Undefined Input quantities are not used.
C   Finally: This routine assumes that pressure levels are counted
C            top down -- ie, level 1 is the top, level lm is the bottom
C                                               
C***********************************************************************
C    
      implicit none
      integer  i,l,im,jm,lm

      real  qprs(im,jm)
      real  qinp(im,jm,lm)
      real   pkz(im,jm,lm)
      real pksrf(im,jm)

      real  ptop,p,undef,kappa,getcon
      real  pk,pkmin,pkmax,pktop,temp

      undef = getcon('UNDEF')
      kappa = getcon('KAPPA')
     
      pk    =    p**kappa
      if(ptop.ne.0.) then
       pktop = ptop**kappa
      else
       pktop = 0.
      endif

c Initialize to UNDEFINED
c -----------------------
      do i=1,im*jm
      qprs(i,1) = undef
      enddo

c Interpolate to Pressure Between Input Levels
c --------------------------------------------
      do L=1,lm-1
      pkmin = pkz(1,1,L)
      pkmax = pkz(1,1,L+1)

         do i=2,im*jm
         if( pkz(i,1,L)  .lt.pkmin ) pkmin = pkz(i,1,L)
         if( pkz(i,1,L+1).gt.pkmax ) pkmax = pkz(i,1,L+1)
         enddo

      if( pk.le.pkmax .and. pk.ge.pkmin ) then
         do i=1,im*jm
         if( pk.le.pkz(i,1,L+1) .and. pk.ge.pkz(i,1,L) ) then
         temp      = ( pkz(i,1,L)-pk ) / ( pkz(i,1,L)-pkz(i,1,L+1) )

                 if( qinp(i,1,L)  .ne.undef  .and. 
     .               qinp(i,1,L+1).ne.undef ) then
         qprs(i,1) = qinp(i,1,L+1)*temp + qinp(i,1,L)*(1.-temp)
            else if( qinp(i,1,L+1).ne.undef  .and. temp.ge.0.5 ) then
         qprs(i,1) = qinp(i,1,L+1)
            else if( qinp(i,1,L)  .ne.undef  .and. temp.le.0.5 ) then
         qprs(i,1) = qinp(i,1,L)
                 endif

         endif
         enddo
      endif

      enddo

      do i=1,im*jm
c Extrapolate to Pressure between Ptop and First Input Level
c ----------------------------------------------------------
      if( pk.le.pkz(i,1,1) .and. pk.ge.pktop ) then
      temp  = ( pkz(i,1,1)-pk ) / ( pkz(i,1,1)-pkz(i,1,2) )

              if( qinp(i,1,1).ne.undef  .and. 
     .            qinp(i,1,2).ne.undef ) then
      qprs(i,1) = qinp(i,1,2)*temp + qinp(i,1,1)*(1.-temp)
         else if( qinp(i,1,1).ne.undef ) then
      qprs(i,1) = qinp(i,1,1)
              endif

      endif

c Extrapolate to Pressure between Psurf and Lowest Input Level
c ------------------------------------------------------------
      if( pk.le.pksrf(i,1) .and. pk.ge.pkz(i,1,lm ) ) then
      temp      = ( pkz(i,1,lm)-pk ) / ( pkz(i,1,lm)-pkz(i,1,lm-1) )

              if( qinp(i,1,lm)  .ne.undef  .and. 
     .            qinp(i,1,lm-1).ne.undef ) then
      qprs(i,1) = qinp(i,1,lm-1)*temp + qinp(i,1,lm)*(1.-temp)
         else if( qinp(i,1,lm)  .ne.undef ) then
      qprs(i,1) = qinp(i,1,lm)
              endif

      endif
      enddo

      return
      end
