        program write_float

        double precision npart,xpart,ypart,kpart,kfloat,iup,itop
     &                  ,tstart,tend

        parameter(Nx=80,Ny=42)
        double precision depth(Nx,Ny),xc(Nx),yc(Ny),degX,degY
c
        integer kyear,kpres,kday
        real rtime,rlon,rlat
        integer narg
        logical flag

c
c if float should not be written use flag
c
      flag = .true.
      narg=iargc()
      if ( narg .gt. 0 ) flag = .false.
      print*, flag
      
      if (flag) then
c
c open float file
c
        ilen2=9*8
        open(1,file='float_pos.input',status='new',form='unformatted'
     &       ,access='direct',recl=ilen2)
      endif
c
c read bathymetry
c
        ilen=Nx*Ny*8
        open(2,file='topog.bump',status='old',form='unformatted'
     &,      access='direct',recl=ilen)
        read(2,rec=1) depth
        close(2)
c
c generate axes
c
        degX=5000.
        xc(1)=2500.
        do i=2,Nx
           xc(i)=xc(i-1)+degX
        enddo
c     
        degY=5000.
        yc(1)=2500.
        do j=2,Ny
           yc(j)=yc(j-1)+degY
        enddo

        print*,'xc(1), xc(Nx): ',xc(1), xc(Nx)
        print*,'yc(1), yc(Ny): ',yc(1), yc(Ny)
c     
c preset first line with dummies
                   npart  = 0.
                  tstart  = -1.
                   xpart  = 0.
                   ypart  = 0.
                   kpart  = 0.
                   kfloat = 0.
                   iup    = 0.
                   itop   = 0.
                   tend   = -1.
        if (flag)  write(1,rec=1)    npart,tstart,xpart,ypart,
     &                               kpart,kfloat,iup,itop,tend

 100    continue
c
        print*, '--------------------------------------------------'
        print*, '|         FLOAT CONFIGURATION                    |'
        print*, '--------------------------------------------------'
        print*, ' '
        print*, 'sets over whole basin: '
        ip=0
c
c target depth: level 5
c profiling:    5 days
c surface time: 12, 24  hours
c
        do j=20,30,2
           do i=20,50,2
              if (depth(i,j) .le. -2530.) then
                 ip=ip+1
                   npart  = REAL(ip)
                  tstart  = -1.
                   xpart  = xc(i)
                   ypart  = yc(j)
                   kpart  = 5.
                   kfloat = kpart
                   iup    = 432000.
                   itop   =  43200.
                   tend   = -1.
         if (flag) write(1,rec=ip+1) npart,tstart,xpart,ypart,
     &                               kpart,kfloat,iup,itop,tend
              endif
           enddo
        enddo
        write(6,200) 'ip = ',ip,' kpart,kfloat,iup,itop,tstart,tend: ',
     &                            kpart,kfloat,iup,itop,tstart,tend
 200    format(A,I6,A,2F3.0,4F8.0)
c
        do j=20,30,2
           do i=20,50,2
              if (depth(i,j) .le. -2530.) then
                 ip=ip+1
                   npart  = REAL(ip)
                  tstart  = -1.
                   xpart  = xc(i)
                   ypart  = yc(j)
                   kpart  = 5.
                   kfloat = kpart
                   iup    = 432000.
                   itop   =  86400.
                   tend   = -1.
         if (flag) write(1,rec=ip+1) npart,tstart,xpart,ypart,
     &                               kpart,kfloat,iup,itop,tend
              endif
           enddo
        enddo
        write(6,200) 'ip = ',ip,' kpart,kfloat,iup,itop,tstart,tend: ',
     &                            kpart,kfloat,iup,itop,tstart,tend
c
c
c target depth: level 5
c profiling:    1 day
c surface time: 12 hours
c integrating only day 2-5
c
        do j=20,30,2
           do i=20,50,2
              if (depth(i,j) .le. -2530.) then
                 ip=ip+1
                   npart  = REAL(ip)
                  tstart  = 172800.
                   xpart  = xc(i)
                   ypart  = yc(j)
                   kpart  = 0.
                   kfloat = 5.
                   iup    = 86400.
                   itop   = 43200.
                   tend   = 518400.
         if (flag) write(1,rec=ip+1) npart,tstart,xpart,ypart,
     &                               kpart,kfloat,iup,itop,tend
              endif
           enddo
        enddo
        write(6,200) 'ip = ',ip,' kpart,kfloat,iup,itop,tstart,tend: ',
     &                            kpart,kfloat,iup,itop,tstart,tend
c
c
c target depth: level 5
c no profiling
c integrating starting day 5
c
        do j=20,30,2
           do i=20,50,2
              if (depth(i,j) .le. -2530.) then
                 ip=ip+1
                   npart  = REAL(ip)
                  tstart  = 432000.
                   xpart  = xc(i)
                   ypart  = yc(j)
                   kpart  = 0.
                   kfloat = 5.
                   iup    = 0.
                   itop   = 0.
                   tend   = -1.
         if (flag) write(1,rec=ip+1) npart,tstart,xpart,ypart,
     &                               kpart,kfloat,iup,itop,tend
              endif
           enddo
        enddo
        write(6,200) 'ip = ',ip,' kpart,kfloat,iup,itop,tstart,tend: ',
     &                            kpart,kfloat,iup,itop,tstart,tend
c
c mooring
c
        do j=20,30,2
           i=20
                 ip=ip+1
                   npart  = REAL(ip)
                  tstart  = -1.
                   xpart  = xc(i)
                   ypart  = yc(j)
                   kpart  = 0.
                   kfloat = kpart
                   iup    = -3.
                   itop   = 0.
                   tend   = -1.
         if (flag) write(1,rec=ip+1) npart,tstart,xpart,ypart,
     &                               kpart,kfloat,iup,itop,tend
        enddo
        write(6,200) 'ip = ',ip,' kpart,kfloat,iup,itop,tstart,tend: ',
     &                            kpart,kfloat,iup,itop,tstart,tend
c

        print*, ' '
        print*, '--------------------------------------------------'
        print*, 'total number of floats: npart = ',ip
        print*, '--------------------------------------------------'

c write total number of floats in first line
                   npart  = DBLE(ip)
                  tstart  = -1.
                   xpart  = 0.
                   ypart  = 0.
                   kpart  = 0.
                   kfloat = DBLE(ip)
                   iup    = 0.
                   itop   = 0.
                   tend   = -1.
                   if (flag)
     & write(1,rec=1) npart,tstart,xpart,ypart,kpart,kfloat,iup,itop,tend



        close(1)

        end
