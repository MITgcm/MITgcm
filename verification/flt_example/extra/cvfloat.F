      program cvfloat
c
c=======================================================================
c     converts binary float trajectories to netCDF
c
c     * must be compiled with a FORTRAN90 compiler and netcdf library
c       f90 cvfloat.F /usr/local/lib/libnetcdf.a
c       f90 cvfloat.F /net/ice/ecco/lib/libnetcdf.a (for the ECCO cluster)
c     * uses namelist data.float
c
c     Arne Biastoch, abiastoch@ucsd.edu, 11/16/2000
c
c=======================================================================
c
      integer stdin, stdout, stderr
      parameter (stdin = 5, stdout = 6, stderr = 6)
c
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
c
c number of variables per record
      parameter(imax=10)
c
      integer narg
      logical preflag
c
c netCDF ids
c
      integer  iret, ncid, VARid
      integer  partdim,Timedim
      integer  partid, Timeid
      integer  xpartid, ypartid, kpartid
      integer  tempid, saltid, uvelid, vvelid, zetaid
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
c 
c data variables for NetCDF
c
      real, dimension(:),   allocatable :: pnum,time
      real, dimension(:,:), allocatable :: xpart,ypart,kpart
     &                                    ,uvel,vvel,temp,salt,zeta
      double precision, dimension(:), allocatable ::  tmp
c
c namelist contains
c
      data npart /10/
      character*50 outname2
      character*50 fName, outname
      data fName / 'float_trajectories' /
      character*20 startDate
      data startDate / '01-JAN-1992:12:00:00' /
      data expnam /'Experimentname not set in data.float'/
      data usingSphericalPolarGrid /.true./
      namelist /dimensions/ expnam, startDate, usingSphericalPolarGrid
      namelist /floats/ fName

c
c in most systems netcdf.inc should be side in /usr/local/include
c     include '/usr/local/include/netcdf.inc'
c     include '/users/guests2/ux451985/netcdf/include/netcdf.inc'
      include '/net/ice/ecco/include/netcdf.inc'
c
c      call GETARG(1,cpart)
c      npart=0
c      do i=1,6
c         read(cpart(i:i),'(a1)') split
c         npart = npart + factor(i)*(ICHAR(split)-48)
c      enddo
c      print*, 'npart= ',npart
c      call GETARG(2,fName)

      ioun=11
      open(ioun,file='data.float',status='old',form='formatted')
      read  (unit=ioun, end=666, nml=dimensions)
      write (stdout,dimensions)
      close (ioun)
 666  continue
      open(ioun,file='data.float',status='old',form='formatted')
      read  (unit=ioun, end=999, nml=floats)
      write (stdout,floats)
      close (ioun)
 999  continue

c
c     preliminary use:
c     if floats should be viewed during a current model run the first
c     line of the file may not be updated correctly, i.e. there might
c     be more times than stated at the beginning. By giving a flag
c     only icount-1 timesteps are used
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
      ilen=10*8
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
         icount=icount-1
         print*, 'preliminary --> use timesteps= ',icount
      endif

c-----------------------------------------------------------------------
c     allocate variables
c-----------------------------------------------------------------------
c
      allocate (pnum(npart))
      allocate (time(icount))
      allocate (xpart(npart,icount))
      allocate (ypart(npart,icount))
      allocate (kpart(npart,icount))
      allocate (uvel(npart,icount))
      allocate (vvel(npart,icount))
      allocate (temp(npart,icount))
      allocate (salt(npart,icount))
      allocate (zeta(npart,icount))

c initialize arrays
c
      do m=1,npart
         do n=1,icount
            xpart(m,n) = spval
            ypart(m,n) = spval
            kpart(m,n) = spval
             uvel(m,n) = spval
             vvel(m,n) = spval
             temp(m,n) = spval
             salt(m,n) = spval
             zeta(m,n) = spval
         enddo
      enddo

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
c-----------------------------------------------------------------------
c     open files and read input
c-----------------------------------------------------------------------
c
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
            print*,'read ',dataFname
            itotalrecord = itotalrecord + imaxrecord

            do irec=2,imaxrecord+1
               
               read(1,rec=irec) tmp
               ip = INT(tmp(1))
               if (ip .gt. npart) then
                  print*,'ip out of order: ',ip,npart
                  stop
               endif
               np = INT((tmp(2)-rcountstart)/rcountdelta+1)

c this is only for prelimiray results. Use only icount-1 timesteps
               if (preflag .and. (np .gt. icount .or. np .lt. 1))
     &         goto 100

               if (usingSphericalPolarGrid) then
               xpart(ip,np)  = SNGL(tmp(3))
               ypart(ip,np)  = SNGL(tmp(4))
               else
               xpart(ip,np)  = SNGL(tmp(3))/1000.
               ypart(ip,np)  = SNGL(tmp(4))/1000.
               endif
               kpart(ip,np)  = SNGL(tmp(5))
               uvel(ip,np)   = SNGL(tmp(6))
               vvel(ip,np)   = SNGL(tmp(7))
               temp(ip,np)   = SNGL(tmp(8))
               salt(ip,np)   = SNGL(tmp(9))
               zeta(ip,np)   = SNGL(tmp(10))
c            if (ip .eq. 59)
c     &      print*, 'rec= ',irec,' npart= ',ip,' timestep= ',np,
c     &      time(np),
c     &      xpart(ip,np),ypart(ip,np),kpart(ip,np)
 100           continue
            enddo

            close(1)
         enddo
      enddo

      print*,icount,' x ',npart,' = ',icount*npart,' records expected,',
     & ' found ',itotalrecord,' float records'
      print*,'==> ',icount*npart-itotalrecord,' float records missing'
c
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
     &       ' ==>  Writing a trajectories to file ',outname2(1:IL+4)
      ncid = nccre(outname2(1:IL+4), NCCLOB, iret)
      if (iret .ne. 0) write(stdout,*) 'Error: Open NetCDF file'
c 
c define dimensions
c
      partdim = ncddef(ncid, 'Particles', npart, iret)
      Timedim = ncddef(ncid, 'Time', NCUNLIM, iret)
      if (iret .ne. 0) write(stdout,*) 'Error: define dimensions'
c
c define variables
c
      dims(1)  = partdim
      partid  = ncvdef (ncid,'Particles',NCFLOAT,1,dims,iret)
      dims(1)  = Timedim
      Timeid   = ncvdef (ncid,'Time',   NCFLOAT,1,dims,iret)
      if (iret .ne. 0) write(stdout,*) 'Error: define axis ids'
c
      dims(1) = partdim
      dims(2) = Timedim
      xpartid = ncvdef (ncid,'xpart', NCFLOAT,2,dims,iret)
      ypartid = ncvdef (ncid,'ypart', NCFLOAT,2,dims,iret)
      kpartid = ncvdef (ncid,'kpart', NCFLOAT,2,dims,iret)
      uvelid  = ncvdef (ncid,'uvel',  NCFLOAT,2,dims,iret)
      vvelid  = ncvdef (ncid,'vvel',  NCFLOAT,2,dims,iret)
      tempid  = ncvdef (ncid,'temp',  NCFLOAT,2,dims,iret)
      saltid  = ncvdef (ncid,'salt',  NCFLOAT,2,dims,iret)
      zetaid  = ncvdef (ncid,'zeta',  NCFLOAT,2,dims,iret)
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
c store values
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
      VARid=tempid
      call ncvpt(ncid, VARid, corner, edges, temp, iret)
      VARid=saltid
      call ncvpt(ncid, VARid, corner, edges, salt, iret)
      VARid=uvelid
      call ncvpt(ncid, VARid, corner, edges, uvel, iret)
      VARid=vvelid
      call ncvpt(ncid, VARid, corner, edges, vvel, iret)
      VARid=zetaid
      call ncvpt(ncid, VARid, corner, edges, zeta, iret)
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
