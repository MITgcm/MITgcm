      program cvprofiles
c
c=======================================================================
c     converts binary float profiles to netCDF
c
c     * must be compiled with a FORTRAN90 compiler and netcdf library
c       f90 cvprofiles.F /usr/local/lib/libnetcdf.a
c       f90 cvprofiles.F /net/ice/ecco/lib/libnetcdf.a (for the ECCO cluster)
c     * uses namelist data.profiles
c
c     Arne Biastoch, abiastoch@ucsd.edu, 11/16/2000
c
c=======================================================================
c
      integer stdin, stdout, stderr
      parameter (stdin = 5, stdout = 6, stderr = 6)
c
      parameter (maxcoord=3000, maxdepth=100)
      character iotext*80, expnam*60, stamp*32
c
c variables for filenames
      integer narg, npart
      character*6 cpart
      character*1 split
      integer factor(6)
      data factor / 1.e5, 1.e4, 1.e3, 1.e2, 1.e1, 1.e0 /
      character*(80) dataFName
      logical exst
c
c     parameter(spval=-1.0e+23)
      parameter(spval=-999.)

c number of variables per record
c     parameter(imax=10)
      integer narg
      logical preflag
c
c netCDF ids
c
      integer  iret, ncid, VARid
      integer  partdim,Timedim,Dep_tdim, Dep_wdim, Dep_wm1dim
      integer  partid, Timeid
      integer  xpartid, ypartid, kpartid
      integer  tempid, saltid, uvelid, vvelid, zetaid
      integer  Dep_tid, Dep_wid, Dep_wm1id 
c
c variable shapes, corner and edge lengths
c
      integer dims(4), corner(4), edges(4)
c
      character*1 inumber(4)
c
c attribute vectors
c
      integer  longval(1)
      real  floatval(2)
      character*1 charval(1)
      character name*24, unit*16, grid*2
      logical   usingSphericalPolarGrid
      logical iedge
c 
c data variables for NetCDF
c
      real, dimension(:), allocatable :: Dep_t, Dep_w, Dep_wm1
      real, dimension(:),   allocatable :: pnum,time
      real, dimension(:,:), allocatable :: xpart,ypart,kpart,zeta
      real, dimension(:,:,:), allocatable :: uvel,vvel,temp,salt
      double precision, dimension(:), allocatable ::  tmp
c
c these variables cannot be allocatable because they appear in the namelist
c
      real delZ(maxdepth)
c
c namelist contains
c
      data npart /10/
      character*50 outname2
      character*50 fName, outname
      data fName / 'float_profiles' /
      character*20 startDate
      data startDate / '01-JAN-1992:12:00:00' /
      data expnam /'Experimentname not set in data.profiles'/
      data usingSphericalPolarGrid /.true./
      namelist /dimensions/ expnam, startDate, usingSphericalPolarGrid
      namelist /floats/ fName
      namelist /coord/ Nr, delZ
c
c in most systems netcdf.inc should be side in /usr/local/include
c     include '/usr/local/include/netcdf.inc'
c     include '/users/guests2/ux451985/netcdf/include/netcdf.inc'
      include '/net/ice/ecco/include/netcdf.inc'

      ioun=11
      open(ioun,file='data.profiles',status='old',form='formatted')
      read  (unit=ioun, end=666, nml=dimensions)
      write (stdout,dimensions)
      close (ioun)
 666  continue
      open(ioun,file='data.profiles',status='old',form='formatted')
      read  (unit=ioun, end=777, nml=coord)
c     write (stdout,coord)
      close (ioun)
 777  continue
      open(ioun,file='data.profiles',status='old',form='formatted')
      read  (unit=ioun, end=999, nml=floats)
      write (stdout,floats)
      close (ioun)
 999  continue

c
c     big data set:
c     if the data set contains a big number of particles and timesteps
c     it has to be read in chunks. This takes longer but fits better
c     into the memory. The argument preflag is used to indicate a big 
c     data set.
c
      preflag = .false.
      narg=iargc()
      if ( narg .gt. 0 ) preflag = .true.

c
c strip names
c
      IL=ILNBLNK( fName )

c check existent files
c
      iGmax=1
      do m=1,100
         write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &             fName(1:IL),'.',iGmax,'.',1,'.data'
         inquire( file=dataFname, exist=exst )
         if (exst)  iGmax = iGmax + 1
      enddo

      jGmax=1
      do m=1,100
         write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &             fName(1:IL),'.',1,'.',jGmax,'.data'
         inquire( file=dataFname, exist=exst )
         if (exst)  jGmax = jGmax + 1
      enddo

      iGmax = iGmax - 1
      jGmax = jGmax - 1
      print*, 'There are ',iGmax,' x ',jGmax,' files'

c open first file and get dimensions (float number and time)
c     
      imax=(6+4*Nr)
      ilen=imax*8

      allocate (tmp(imax))
c
      write(dataFname(1:80),'(2a,a)')
     &          fName(1:IL),'.001.001.data'
       open(1,file=dataFname,status='old',form='unformatted'
     &      ,access='direct',recl=ilen)
c
       read(1,rec=1) tmp
       rcountstart = SNGL(tmp(2))
       rcountdelta = SNGL(tmp(4))
       icount      = INT(tmp(5))
       npart       = INT(tmp(6))
       close(1)
      print*, 'npart    = ',npart
      print*, 'timesteps= ',icount
      if (preflag) then
         print*, 'big data set --> read in chunks'
      endif


c-----------------------------------------------------------------------
c     allocate variables
c-----------------------------------------------------------------------
c
      allocate (pnum(npart))
      allocate (time(icount))
      allocate (Dep_t(Nr))
      allocate (Dep_w(Nr+1))
      allocate (Dep_wm1(Nr))
      allocate (xpart(npart,icount))
      allocate (ypart(npart,icount))
      allocate (kpart(npart,icount))
      allocate (temp(npart,Nr,icount))
      if (.not. preflag) then
         allocate (uvel(npart,Nr,icount))
         allocate (vvel(npart,Nr,icount))
         allocate (salt(npart,Nr,icount))
      endif
      allocate (zeta(npart,icount))

c initialize arrays
c
      do m=1,npart
         do n=1,icount
            xpart(m,n) = spval
            ypart(m,n) = spval
            kpart(m,n) = spval
            do k=1,Nr
             if (.not. preflag) uvel(m,k,n) = spval
             if (.not. preflag) vvel(m,k,n) = spval
             temp(m,k,n) = spval
             if (.not. preflag) salt(m,k,n) = spval
            enddo
            zeta(m,n) = spval
         enddo
      enddo
c
c
c test if depth axis is evenly spaced (in that case no edges have to
c be set)
c
      iedge=.false.
      do k=2,Nr
         if (delZ(k) .ne. delZ(k-1)) then
            iedge=.true.
            goto 20
         endif
      enddo
 20   continue
c
      Dep_w(1)=0.
      Dep_wm1(1)=0.
      do k=2,Nr+1
         Dep_w(k)=Dep_w(k-1)+delZ(k-1)
         if (k .ne. Nr+1) Dep_wm1(k) = Dep_w(k)
      enddo
c
      do k=1,Nr
         Dep_t(k)=(Dep_w(k)+Dep_w(k+1))*0.5
      enddo
c
c generate axes
c
      time(1)=rcountstart
      do m=2,icount
         time(m) = time(m-1)+rcountdelta
      enddo
      print*, 'time: ',time
c
      do ip=1,npart
         pnum(ip) = FLOAT(ip)
      enddo
c      print*, 'pnum: ',pnum
c
c
c-----------------------------------------------------------------------
c     open files and read input
c-----------------------------------------------------------------------
c
      itotalrecord = 0

      do iG=1,iGmax
         do jG=1,jGmax
c
            write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &                fName(1:IL),'.',iG,'.',jG,'.data'
            open(1,file=dataFname,status='old',form='unformatted'
     &           ,access='direct',recl=ilen)
c
            read(1,rec=1) tmp
            imaxrecord = INT(tmp(1))
            print*,'read ',dataFname,imaxrecord
            itotalrecord = itotalrecord + imaxrecord
c                goto 200

            do irec=2,imaxrecord+1

               read(1,rec=irec) tmp
               ip = INT(tmp(1))
               if (ip .gt. npart) then
                  print*,'ip out of order: ',ip,npart
                  stop
               endif
               np = INT((tmp(2)-rcountstart)/rcountdelta+1)


               if (usingSphericalPolarGrid) then
               xpart(ip,np)  = SNGL(tmp(3))
               ypart(ip,np)  = SNGL(tmp(4))
               else
               xpart(ip,np)  = SNGL(tmp(3))/1000.
               ypart(ip,np)  = SNGL(tmp(4))/1000.
               endif
               kpart(ip,np)  = SNGL(tmp(5))
               zeta(ip,np)   = SNGL(tmp(6))
               do k=1,Nr
                if (.not. preflag) uvel(ip,k,np)   = SNGL(tmp(6+k))
                if (.not. preflag) vvel(ip,k,np)   = SNGL(tmp(6+1*Nr+k))
                                   temp(ip,k,np)   = SNGL(tmp(6+2*Nr+k))
                if (.not. preflag) salt(ip,k,np)   = SNGL(tmp(6+3*Nr+k))
                  if (temp(ip,k,np) .eq. 0.) then
                    if (.not. preflag)  uvel(ip,k,np)   = spval
                    if (.not. preflag)  vvel(ip,k,np)   = spval
                                        temp(ip,k,np)   = spval
                    if (.not. preflag)  salt(ip,k,np)   = spval
                  endif
               enddo
c            print*, 'rec= ',irec,' npart= ',ip,' timestep= ',np
c     &      ,time(np),tmp
c     &     ,xpart(ip,np),ypart(ip,np),kpart(ip,np),uvel(ip,np,1),
c     &      vvel(ip,np,1),temp(ip,np,1),salt(ip,np,1),zeta(ip,np)
 100           continue
            enddo

            close(1)
 200            continue
         enddo
      enddo

      print*,icount,' x ',npart,' = ',icount*npart,' records expected,',
     & ' found ',itotalrecord,' float records'
      print*,'==> ',icount*npart-itotalrecord,' float records missing'
c
c-----------------------------------------------------------------------
c     define netCDF variables
c-----------------------------------------------------------------------
c
      write(stdout,*) ' Start Converting'
c
c enter define mode: NCCLOB=overwrite, NCNOCLOB=do not overwrite
c
      IL=ILNBLNK( fname )
      outname2=fname(1:IL)//'.cdf'
      write (stdout,*)
     &       ' ==>  Writing a profiles to file ',outname2(1:IL+4)
      ncid = nccre(outname2(1:IL+4), NCCLOB, iret)
      if (iret .ne. 0) write(stdout,*) 'Error: Open NetCDF file'
c 
c define dimensions
c
      partdim = ncddef(ncid, 'Particles', npart, iret)
      Dep_tdim  = ncddef(ncid, 'Depth_t',     Nr, iret)
      Dep_wm1dim= ncddef(ncid, 'Depth_wm1',   Nr, iret)
      Dep_wdim  = ncddef(ncid, 'Depth_w',   Nr+1, iret)
      Timedim = ncddef(ncid, 'Time', NCUNLIM, iret)
      if (iret .ne. 0) write(stdout,*) 'Error: define dimensions'
c
c define variables
c
      dims(1)  = partdim
      partid  = ncvdef (ncid,'Particles',NCFLOAT,1,dims,iret)
      dims(1)  = Dep_tdim
      Dep_tid  = ncvdef (ncid,'Depth_t',    NCFLOAT,1,dims,iret)
      dims(1)  = Dep_wdim
      Dep_wid  = ncvdef (ncid,'Depth_w',    NCFLOAT,1,dims,iret)
      dims(1)  = Dep_wm1dim
      Dep_wm1id= ncvdef (ncid,'Depth_wm1',  NCFLOAT,1,dims,iret)
      dims(1)  = Timedim
      Timeid   = ncvdef (ncid,'Time',   NCFLOAT,1,dims,iret)
      if (iret .ne. 0) write(stdout,*) 'Error: define axis ids'
c
      dims(1) = partdim
      dims(2) = Timedim
      xpartid = ncvdef (ncid,'xpart', NCFLOAT,2,dims,iret)
      ypartid = ncvdef (ncid,'ypart', NCFLOAT,2,dims,iret)
      kpartid = ncvdef (ncid,'kpart', NCFLOAT,2,dims,iret)
      zetaid  = ncvdef (ncid,'zeta',  NCFLOAT,2,dims,iret)
c
      dims(1) = partdim
      dims(2) = Dep_tdim
      dims(3) = Timedim
      uvelid  = ncvdef (ncid,'uvel',  NCFLOAT,3,dims,iret)
      vvelid  = ncvdef (ncid,'vvel',  NCFLOAT,3,dims,iret)
      tempid  = ncvdef (ncid,'temp',  NCFLOAT,3,dims,iret)
      saltid  = ncvdef (ncid,'salt',  NCFLOAT,3,dims,iret)
      if (iret .ne. 0) write(stdout,*) 'Error: define variable ids'
c
c-----------------------------------------------------------------------
c     assign attributes
c-----------------------------------------------------------------------
c
      charval(1) = ' '
c      
      name = 'Particles Number    '
c      unit = 'particle number  '
      call ncaptc(ncid, partid, 'long_name', NCCHAR, 24, name, iret) 
      call ncaptc(ncid, partid, 'units',     NCCHAR, 16, unit, iret) 
c
      name = 'Time'
      unit = 'seconds'
      call ncaptc(ncid, Timeid, 'long_name', NCCHAR, 24, name, iret) 
      call ncaptc(ncid, Timeid, 'units',     NCCHAR, 16, unit, iret) 
      call ncaptc(ncid, Timeid,'time_origin',NCCHAR, 20,startDate, iret)
      if (iret .ne. 0) write(stdout,*) 'Error: assign axis attributes'
c
c      
      name = 'Depth of T grid points  '
      unit = 'meters          '
      call ncaptc(ncid, Dep_tid, 'long_name', NCCHAR, 24, name, iret) 
      call ncaptc(ncid, Dep_tid, 'units',     NCCHAR, 16, unit, iret) 
      call ncaptc(ncid, Dep_tid, 'positive',  NCCHAR, 4, 'down',iret)     
c     call ncaptc(ncid, Dep_tid, 'point_spacing',NCCHAR,6,'uneven',iret)     
      if (iedge)
     & call ncaptc(ncid, Dep_tid, 'edges',NCCHAR, 7,'Depth_w',iret)     
c      
      name = 'Depth at top of T box'
      unit = 'meters          '
      call ncaptc(ncid, Dep_wm1id, 'long_name', NCCHAR, 24, name, iret) 
      call ncaptc(ncid, Dep_wm1id, 'units',     NCCHAR, 16, unit, iret) 
      call ncaptc(ncid, Dep_wm1id, 'positive',  NCCHAR, 4, 'down',iret)     
c
      name = 'Depth at bottom of T box'
      unit = 'meters          '
      call ncaptc(ncid, Dep_wid, 'long_name', NCCHAR, 24, name, iret) 
      call ncaptc(ncid, Dep_wid, 'units',     NCCHAR, 16, unit, iret) 
      call ncaptc(ncid, Dep_wid, 'positive',  NCCHAR, 4, 'down',iret)     
      floatval(1) = spval
c
      if (usingSphericalPolarGrid) then
         name = 'LONGITUDE '
         unit = 'degrees_W '
      else
         name = 'X_t '
         unit = 'kilometer '
      endif
      call ncaptc(ncid, xpartid, 'long_name', NCCHAR, 24, name, iret) 
      call ncaptc(ncid, xpartid, 'units',     NCCHAR, 16, unit, iret) 
      call ncapt (ncid, xpartid,'missing_value',NCFLOAT,1,floatval,iret)
      call ncapt (ncid, xpartid,'_FillValue', NCFLOAT, 1,floatval, iret)
c
      if (usingSphericalPolarGrid) then
         name = 'LATITUDE '
         unit = 'degrees_N '
      else
         name = 'Y_t '
         unit = 'kilometer '
      endif
      call ncaptc(ncid, ypartid, 'long_name', NCCHAR, 24, name, iret) 
      call ncaptc(ncid, ypartid, 'units',     NCCHAR, 16, unit, iret) 
      call ncapt (ncid, ypartid,'missing_value',NCFLOAT,1,floatval,iret)
      call ncapt (ncid, ypartid,'_FillValue', NCFLOAT, 1,floatval, iret)
c
      name = 'LEVEL '
      unit = 'LEVEL '
      call ncaptc(ncid, kpartid, 'long_name', NCCHAR, 24, name, iret) 
      call ncaptc(ncid, kpartid, 'units',     NCCHAR, 16, unit, iret) 
      call ncapt (ncid, kpartid,'missing_value',NCFLOAT,1,floatval,iret)
      call ncapt (ncid, kpartid,'_FillValue', NCFLOAT, 1,floatval, iret)
c
      name = 'POTENTIAL TEMPERATURE '
      unit = 'deg C '
      call ncaptc(ncid, tempid, 'long_name', NCCHAR, 24, name, iret) 
      call ncaptc(ncid, tempid, 'units',     NCCHAR, 16, unit, iret) 
      call ncapt (ncid, tempid, 'missing_value',NCFLOAT,1,floatval,iret)
      call ncapt (ncid, tempid, '_FillValue', NCFLOAT, 1,floatval, iret)
c
      name = 'SALINITY '
      unit = 'PSU '
      call ncaptc(ncid, saltid, 'long_name', NCCHAR, 24, name, iret) 
      call ncaptc(ncid, saltid, 'units',     NCCHAR, 16, unit, iret) 
      call ncapt (ncid, saltid, 'missing_value',NCFLOAT,1,floatval,iret)
      call ncapt (ncid, saltid, '_FillValue', NCFLOAT, 1,floatval, iret)
c
      name = 'U VELOCITY '
      unit = 'm/sec'
      call ncaptc(ncid, uvelid, 'long_name', NCCHAR, 24, name, iret) 
      call ncaptc(ncid, uvelid, 'units',     NCCHAR, 16, unit, iret) 
      call ncapt (ncid, uvelid, 'missing_value',NCFLOAT,1,floatval,iret)
      call ncapt (ncid, uvelid, '_FillValue', NCFLOAT, 1,floatval, iret)
c
      name = 'V VELOCITY '
      unit = 'm/sec'
      call ncaptc(ncid, vvelid, 'long_name', NCCHAR, 24, name, iret) 
      call ncaptc(ncid, vvelid, 'units',     NCCHAR, 16, unit, iret) 
      call ncapt (ncid, vvelid, 'missing_value',NCFLOAT,1,floatval,iret)
      call ncapt (ncid, vvelid, '_FillValue', NCFLOAT, 1,floatval, iret)
c
      name = 'SEA SURFACE HEIGHT '
      unit = 'm '
      call ncaptc(ncid, zetaid, 'long_name', NCCHAR, 24, name, iret) 
      call ncaptc(ncid, zetaid, 'units',     NCCHAR, 16, unit, iret) 
      call ncapt (ncid, zetaid,'missing_value',NCFLOAT, 1,floatval,iret)
      call ncapt (ncid, zetaid,'_FillValue', NCFLOAT, 1, floatval, iret)
c
      if (iret .ne. 0) write(stdout,*) 'Error: define variable attrib.'
c
      expname= ' '
      stamp = ' '
      call ncaptc(ncid, NCGLOBAL, 'title',   NCCHAR, 60, expnam, iret)
      call ncaptc(ncid, NCGLOBAL, 'history', NCCHAR, 32, stamp, iret)
c
c-----------------------------------------------------------------------
c     leave define mode
c-----------------------------------------------------------------------
c
      call ncendf(ncid, iret)
c
c
c-----------------------------------------------------------------------
c     put variables in netCDF file
c-----------------------------------------------------------------------
c
c store Particles
      corner(1) = 1
      edges(1) = npart
      call ncvpt(ncid, partid, corner, edges, pnum, iret)
c
c store Time      
      corner(1) = 1 
      edges(1) = icount
      call ncvpt(ncid, Timeid, corner, edges, Time, iret)
c
c store Depth_t
      corner(1) = 1
      edges(1) = Nr
      call ncvpt(ncid, Dep_tid, corner, edges, Dep_t, iret)
c store Depth_w
      corner(1) = 1
      edges(1) = Nr+1
      call ncvpt(ncid, Dep_wid, corner, edges, Dep_w, iret)
c store Depth_wm1
      corner(1) = 1
      edges(1) = Nr
      call ncvpt(ncid, Dep_wm1id, corner, edges, Dep_wm1, iret)
c store 2D values
      corner(1) = 1
      corner(2) = 1
      edges(1) = npart
      edges(2) = icount
      VARid=xpartid
      call ncvpt(ncid, VARid, corner, edges, xpart, iret)
      VARid=ypartid
      call ncvpt(ncid, VARid, corner, edges, ypart, iret)
      VARid=kpartid
      call ncvpt(ncid, VARid, corner, edges, kpart, iret)
      VARid=zetaid
      call ncvpt(ncid, VARid, corner, edges, zeta, iret)
c store values
      corner(1) = 1
      corner(2) = 1
      corner(3) = 1
      edges(1) = npart
      edges(2) = Nr
      edges(3) = icount
      VARid=tempid
      call ncvpt(ncid, VARid, corner, edges, temp, iret)

      if (preflag) then
c read in salt into temp array
      do iG=1,iGmax
         do jG=1,jGmax
c
            write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &                fName(1:IL),'.',iG,'.',jG,'.data'
            open(1,file=dataFname,status='old',form='unformatted'
     &           ,access='direct',recl=ilen)
c
            read(1,rec=1) tmp
            imaxrecord = INT(tmp(1))
            print*,'read salt from ',dataFname
            do irec=2,imaxrecord+1

               read(1,rec=irec) tmp
               ip = INT(tmp(1))
               np = INT((tmp(2)-rcountstart)/rcountdelta+1)
               do k=1,Nr
                  temp(ip,k,np)   = SNGL(tmp(6+3*Nr+k))
                  if (SNGL(tmp(6+2*Nr+k)) .eq. 0.) then
                     temp(ip,k,np)   = spval
                  endif
               enddo
            enddo
            close(1)
         enddo
      enddo
         VARid=saltid
         call ncvpt(ncid, VARid, corner, edges, temp, iret)

c read in u into temp array
      do iG=1,iGmax
         do jG=1,jGmax
c
            write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &                fName(1:IL),'.',iG,'.',jG,'.data'
            open(1,file=dataFname,status='old',form='unformatted'
     &           ,access='direct',recl=ilen)
c
            read(1,rec=1) tmp
            imaxrecord = INT(tmp(1))
            print*,'read uvel from ',dataFname
            do irec=2,imaxrecord+1

               read(1,rec=irec) tmp
               ip = INT(tmp(1))
               np = INT((tmp(2)-rcountstart)/rcountdelta+1)
               do k=1,Nr
                  temp(ip,k,np)   = SNGL(tmp(6+k))
                  if (SNGL(tmp(6+2*Nr+k)) .eq. 0.) then
                     temp(ip,k,np)   = spval
                  endif
               enddo
            enddo
            close(1)
         enddo
      enddo
         VARid=uvelid
         call ncvpt(ncid, VARid, corner, edges, temp, iret)

c read in v into temp array
      do iG=1,iGmax
         do jG=1,jGmax
c
            write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &                fName(1:IL),'.',iG,'.',jG,'.data'
            open(1,file=dataFname,status='old',form='unformatted'
     &           ,access='direct',recl=ilen)
c
            read(1,rec=1) tmp
            imaxrecord = INT(tmp(1))
            print*,'read vvel from ',dataFname
            do irec=2,imaxrecord+1

               read(1,rec=irec) tmp
               ip = INT(tmp(1))
               np = INT((tmp(2)-rcountstart)/rcountdelta+1)
               do k=1,Nr
                  temp(ip,k,np)   = SNGL(tmp(6+1*Nr+k))
                  if (SNGL(tmp(6+2*Nr+k)) .eq. 0.) then
                     temp(ip,k,np)   = spval
                  endif
               enddo
            enddo
            close(1)
         enddo
      enddo
         VARid=vvelid
         call ncvpt(ncid, VARid, corner, edges, temp, iret)

      else

         VARid=saltid
         call ncvpt(ncid, VARid, corner, edges, salt, iret)
         VARid=uvelid
         call ncvpt(ncid, VARid, corner, edges, uvel, iret)
         VARid=vvelid
         call ncvpt(ncid, VARid, corner, edges, vvel, iret)

      endif
c
      if (iret .ne. 0) write(stdout,*) 'Error: write variables'
      call ncclos (ncid, iret)
c
      write(stdout,*) ' End '
 
      end


      INTEGER FUNCTION ILNBLNK( string )
C     /==========================================================\
C     | FUNCTION ILNBLNK                                         |
C     | o Find last non-blank in character string.               |
C     \==========================================================/
      IMPLICIT NONE
      CHARACTER*(*) string
CEndOfInterface
      INTEGER L, LS
C
      LS      = LEN(string)
      ILNBLNK = LS
      DO 10 L = LS, 1, -1
        IF ( string(L:L) .EQ. ' ' ) GOTO 10
         ILNBLNK = L
         GOTO 11
   10 CONTINUE
   11 CONTINUE
C
      RETURN
      END
