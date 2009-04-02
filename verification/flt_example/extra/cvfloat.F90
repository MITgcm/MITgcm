program cvfloat
   !
   !=======================================================================
   !
   !     converts binary float trajectories to netCDF
   !
   !=======================================================
   !
   !     * uses namelist data.float
   !
   !     Arne Biastoch, abiastoch@ucsd.edu, 11/16/2000
   !     Updated 03/16/2009 Christopher Wolfe, clwolfe@ucsd.edu
   !
   !=======================================================================
   !
   use netcdf
   implicit none
   integer, parameter :: STDIN = 5, STDOUT = 6, STDERR = 6
   
   character(80) :: iotext
   character(32) :: stamp
   integer :: ioun,iargc,m,n,ilen,irec,ip,np
   integer :: icount,itotalrecord,imaxrecord

   ! tile counters
   integer :: iGmax,jGmax,iG,jG

   ! variables for filenames
   integer :: narg, npart
   character(80) :: dataFName
   logical :: exst

   ! number of variables per record
   integer, parameter :: IMAX = 13
   !
   !      integer narg
   logical preflag
   !
   ! netCDF ids
   !
   integer :: ncid
   integer :: partdim,Timedim
   integer :: partid, Timeid
   integer :: xpartid, ypartid, zpartid
   integer :: ipartid, jpartid, kpartid
   integer  tempid, saltid, uvelid, vvelid, presid
   !
   ! attribute vectors
   !
   character(24) :: name*24
   character(16) :: unit*16
   ! 
   ! data variables for NetCDF
   !
   double precision :: rcountstart, rcountdelta
   double precision, dimension(:),   allocatable :: pnum,time
   double precision, dimension(:,:), allocatable :: xpart,ypart,zpart, &
                                                   ipart,jpart,kpart, &
                                                   pres,uvel,vvel,temp,salt
   double precision, dimension(:), allocatable ::  tmp
   !
   ! namelist contains
   !
   character(50) :: outname2, fName="float_trajectories", outname
   character(20) :: startDate="01-JAN-1992:12:00:00"
   character(60) :: expnam = "Experiment name not set"
   logical :: usingSphericalPolarGrid=.true.
   
   namelist /dimensions/ expnam, startDate, usingSphericalPolarGrid
   namelist /floats/ fName

   ioun=11
   open(ioun,file="data.float",status="old",form="formatted")
   read  (unit=ioun, end=666, nml=dimensions)
   write (STDOUT,dimensions)
   close (ioun)
666  continue
   open(ioun,file="data.float",status="old",form="formatted")
   read  (unit=ioun, end=999, nml=floats)
   write (STDOUT,floats)
   close (ioun)
999  continue

   !
   !     PRELIMINARY USE:
   !     IF FLOATS SHOULD BE VIEWED DURING A CURRENT MODEL RUN THE FIRST
   !     LINE OF THE FILE MAY NOT BE UPDATED CORRECTLY, I.E. THERE MIGHT
   !     BE MORE TIMES THAN STATED AT THE BEGINNING. BY GIVING A FLAG
   !     ONLY ICOUNT-1 TIMESTEPS ARE USED
   !
   preflag = .false.
   narg=iargc()
   if ( narg > 0 ) preflag = .true.
   !
   ! check existent files
   !
   iGmax=1
   do m=1,100
      write(dataFname(1:80),"(2a,i3.3,a,i3.3,a)") trim(fName),".",iGmax,".",1,".data"
      inquire( file=dataFname, exist=exst )
      if (exst)  iGmax = iGmax + 1
   enddo
   
   jGmax=1
   do m=1,100
      write(dataFname(1:80),"(2a,i3.3,a,i3.3,a)") trim(fName),".",1,".",jGmax,".data"
      inquire( file=dataFname, exist=exst )
      if (exst)  jGmax = jGmax + 1
   enddo
   
   iGmax = iGmax - 1
   jGmax = jGmax - 1
   print*, "There are ",iGmax," x ",jGmax," files"

   ! open first file and get dimensions (float number and time)
   ilen=IMAX*8
   allocate (tmp(IMAX))

   write(dataFname(1:80),"(2a,a)") trim(fName),".001.001.data"
   open(1,file=dataFname,status="old",form="unformatted",access="direct",recl=ilen)
   
   read(1,rec=1) tmp
!   print*,"tmp:", tmp
   
   rcountstart = tmp(2)
   rcountdelta = tmp(4)
   icount      = INT(tmp(5))
   npart       = INT(tmp(6))
   close(1)
   
   print*, "npart    = ",npart
   print*, "timesteps= ",icount
!    print*,"rcountstart=",rcountstart,"rcountdelta",rcountdelta
   if (preflag) then
      icount=icount-1
      print*, "preliminary --> use timesteps= ",icount
   endif

   !-----------------------------------------------------------------------
   !     allocate variables
   !-----------------------------------------------------------------------
   !
   allocate (pnum(npart))
   allocate (time(icount))
   allocate (xpart(npart,icount))
   allocate (ypart(npart,icount))
   allocate (zpart(npart,icount))
   allocate (ipart(npart,icount))
   allocate (jpart(npart,icount))
   allocate (kpart(npart,icount))
   allocate (uvel(npart,icount))
   allocate (vvel(npart,icount))
   allocate (temp(npart,icount))
   allocate (salt(npart,icount))
   allocate (pres(npart,icount))

   ! initialize arrays
   !
   do m=1,npart
      do n=1,icount
         xpart(m,n) = NF90_FILL_DOUBLE
         ypart(m,n) = NF90_FILL_DOUBLE
         zpart(m,n) = NF90_FILL_DOUBLE
         ipart(m,n) = NF90_FILL_DOUBLE
         jpart(m,n) = NF90_FILL_DOUBLE
         kpart(m,n) = NF90_FILL_DOUBLE
         uvel(m,n) = NF90_FILL_DOUBLE
         vvel(m,n) = NF90_FILL_DOUBLE
         temp(m,n) = NF90_FILL_DOUBLE
         salt(m,n) = NF90_FILL_DOUBLE
         pres(m,n) = NF90_FILL_DOUBLE
      enddo
   enddo

   ! generate axes
   !
   time(1)=rcountstart
   do m=2,icount
      time(m) = time(m-1)+rcountdelta
   enddo
   print*, "time: ",time(1:icount)
   
   do ip=1,npart
      pnum(ip) = FLOAT(ip)
   enddo
   !
   !-----------------------------------------------------------------------
   !     open files and read input
   !-----------------------------------------------------------------------
   !

   itotalrecord = 0
   
   do iG=1,iGmax
      do jG=1,jGmax
   
         write(dataFname(1:80),"(2a,i3.3,a,i3.3,a)") trim(fName),".",iG,".",jG,".data"
         open(1,file=dataFname,status="old",form="unformatted",access="direct",recl=ilen)
   
         read(1,rec=1) tmp
         imaxrecord = INT(tmp(1))
         print "(1x,2a)","read ",dataFname
         itotalrecord = itotalrecord + imaxrecord
   
         do irec=2,imaxrecord+1
   
            read(1,rec=irec) tmp
            ip = INT(tmp(1))
            if (ip > npart) then
               print*,"ip out of order: ",ip,npart
               stop
            endif
            
            ! Note: If the float initial conditions are also written to the data files the time interval
            ! between record 1 and 2 may be somewhat less than rcountdelta. The +0.9999 deals with
            ! this case without upsetting the rest of the indexing.
            np = INT((tmp(2)-rcountstart)/rcountdelta + 1 + 0.9999)
   
            ! this is only for prelimiray results. Use only icount-1 timesteps
            if (preflag .and. (np > icount .or. np < 1)) cycle
   
            if (usingSphericalPolarGrid) then
               xpart(ip,np)  = tmp(3)
               ypart(ip,np)  = tmp(4)
            else
               xpart(ip,np)  = tmp(3)
               ypart(ip,np)  = tmp(4)
            endif
            zpart(ip,np)  = tmp(5)
            ipart(ip,np)  = tmp(6)
            jpart(ip,np)  = tmp(7)
            kpart(ip,np)  = tmp(8)
            pres(ip,np)   = tmp(9)
            uvel(ip,np)   = tmp(10)
            vvel(ip,np)   = tmp(11)
            temp(ip,np)   = tmp(12)
            salt(ip,np)   = tmp(13)
         enddo
   
         close(1)
      enddo
   enddo

   print*,icount," x ",npart," = ",icount*npart," records expected,",& 
      " found ",itotalrecord," float records"
   print*,"==> ",icount*npart-itotalrecord," float records missing"
   !
   !
   !-----------------------------------------------------------------------
   !     define netCDF variables
   !-----------------------------------------------------------------------
   !
   write(STDOUT,*) " Start Converting"
   !
   ! enter define mode
   !
   outname2=trim(fname)//".cdf"
   write (STDOUT,*)" ==>  Writing a trajectories to file ",trim(outname2)
   call nc_check(nf90_create(trim(outname2),NF90_CLOBBER,ncid),__LINE__)
   ! 
   ! define dimensions
   !
   call nc_check(nf90_def_dim(ncid, "Particles", npart, partdim),__LINE__)
   call nc_check(nf90_def_dim(ncid, "Time",      NF90_UNLIMITED, Timedim),__LINE__)
   !
   ! define variables
   !
   call nc_check(nf90_def_var(ncid, "Particles",NF90_DOUBLE, (/partdim/), partid),__LINE__)
   call nc_check(nf90_def_var(ncid, "Time",     NF90_DOUBLE, (/Timedim/), Timeid),__LINE__)
   
   call nc_check(nf90_def_var(ncid, "x",        NF90_DOUBLE, (/partdim,Timedim/), xpartid),__LINE__)
   call nc_check(nf90_def_var(ncid, "y",        NF90_DOUBLE, (/partdim,Timedim/), ypartid),__LINE__)
   call nc_check(nf90_def_var(ncid, "z",        NF90_DOUBLE, (/partdim,Timedim/), zpartid),__LINE__)
   call nc_check(nf90_def_var(ncid, "i",        NF90_DOUBLE, (/partdim,Timedim/), ipartid),__LINE__)
   call nc_check(nf90_def_var(ncid, "j",        NF90_DOUBLE, (/partdim,Timedim/), jpartid),__LINE__)
   call nc_check(nf90_def_var(ncid, "k",        NF90_DOUBLE, (/partdim,Timedim/), kpartid),__LINE__)
   call nc_check(nf90_def_var(ncid, "pressure", NF90_DOUBLE, (/partdim,Timedim/), presid),__LINE__)
   call nc_check(nf90_def_var(ncid, "u",        NF90_DOUBLE, (/partdim,Timedim/), uvelid),__LINE__)
   call nc_check(nf90_def_var(ncid, "v",        NF90_DOUBLE, (/partdim,Timedim/), vvelid),__LINE__)
   call nc_check(nf90_def_var(ncid, "T",        NF90_DOUBLE, (/partdim,Timedim/), tempid),__LINE__)
   call nc_check(nf90_def_var(ncid, "S",        NF90_DOUBLE, (/partdim,Timedim/), saltid),__LINE__)
   !
   !-----------------------------------------------------------------------
   !     assign attributes
   !-----------------------------------------------------------------------
   !
   name = "Particle Number"
   unit = "particle number"
   call nc_check(nf90_put_att(ncid,partid,"long_name",name),__LINE__)
   call nc_check(nf90_put_att(ncid,partid,"units",    unit),__LINE__)
   
   name = "Time"
   unit = "seconds"
   call nc_check(nf90_put_att(ncid,Timeid,"long_name",  name),__LINE__)
   call nc_check(nf90_put_att(ncid,Timeid,"units",      unit),__LINE__)
   call nc_check(nf90_put_att(ncid,Timeid,"time_origin",startDate),__LINE__)
   
   if (usingSphericalPolarGrid) then
      name = "LONGITUDE "
      unit = "degrees_W "
   else
      name = "X_t "
      unit = "meter "
   endif
   call nc_check(nf90_put_att(ncid,xpartid,"long_name",  name),__LINE__)
   call nc_check(nf90_put_att(ncid,xpartid,"units",      unit),__LINE__)
   call nc_check(nf90_put_att(ncid,xpartid,"missing_value",NF90_FILL_DOUBLE),__LINE__)
   call nc_check(nf90_put_att(ncid,xpartid,"_FillValue",   NF90_FILL_DOUBLE),__LINE__)
   
   if (usingSphericalPolarGrid) then
      name = "LATITUDE "
      unit = "degrees_N "
   else
      name = "Y_t "
      unit = "meter "
   endif
   call nc_check(nf90_put_att(ncid,ypartid,"long_name",  name),__LINE__)
   call nc_check(nf90_put_att(ncid,ypartid,"units",      unit),__LINE__)
   call nc_check(nf90_put_att(ncid,ypartid,"missing_value",NF90_FILL_DOUBLE),__LINE__)
   call nc_check(nf90_put_att(ncid,ypartid,"_FillValue",   NF90_FILL_DOUBLE),__LINE__)
   
   name = "DEPTH "
   unit = "meter "
   call nc_check(nf90_put_att(ncid,zpartid,"long_name",  name),__LINE__)
   call nc_check(nf90_put_att(ncid,zpartid,"units",      unit),__LINE__)
   call nc_check(nf90_put_att(ncid,zpartid,"missing_value",NF90_FILL_DOUBLE),__LINE__)
   call nc_check(nf90_put_att(ncid,zpartid,"_FillValue",   NF90_FILL_DOUBLE),__LINE__)
   
   name = "iINDEX "
   unit = "index "
   call nc_check(nf90_put_att(ncid,ipartid,"long_name",  name),__LINE__)
   call nc_check(nf90_put_att(ncid,ipartid,"units",      unit),__LINE__)
   call nc_check(nf90_put_att(ncid,ipartid,"missing_value",NF90_FILL_DOUBLE),__LINE__)
   call nc_check(nf90_put_att(ncid,ipartid,"_FillValue",   NF90_FILL_DOUBLE),__LINE__)
   
   name = "jINDEX "
   unit = "index "
   call nc_check(nf90_put_att(ncid,jpartid,"long_name",  name),__LINE__)
   call nc_check(nf90_put_att(ncid,jpartid,"units",      unit),__LINE__)
   call nc_check(nf90_put_att(ncid,jpartid,"missing_value",NF90_FILL_DOUBLE),__LINE__)
   call nc_check(nf90_put_att(ncid,jpartid,"_FillValue",   NF90_FILL_DOUBLE),__LINE__)
   
   name = "LEVEL "
   unit = "level "
   call nc_check(nf90_put_att(ncid,kpartid,"long_name",  name),__LINE__)
   call nc_check(nf90_put_att(ncid,kpartid,"units",      unit),__LINE__)
   call nc_check(nf90_put_att(ncid,kpartid,"missing_value",NF90_FILL_DOUBLE),__LINE__)
   call nc_check(nf90_put_att(ncid,kpartid,"_FillValue",   NF90_FILL_DOUBLE),__LINE__)
   
   name = "POTENTIAL TEMPERATURE "
   unit = "deg C "
   call nc_check(nf90_put_att(ncid,tempid,"long_name",  name),__LINE__)
   call nc_check(nf90_put_att(ncid,tempid,"units",      unit),__LINE__)
   call nc_check(nf90_put_att(ncid,tempid,"missing_value",NF90_FILL_DOUBLE),__LINE__)
   call nc_check(nf90_put_att(ncid,tempid,"_FillValue",   NF90_FILL_DOUBLE),__LINE__)
   
   name = "SALINITY "
   unit = "PSU "
   call nc_check(nf90_put_att(ncid,saltid,"long_name",  name),__LINE__)
   call nc_check(nf90_put_att(ncid,saltid,"units",      unit),__LINE__)
   call nc_check(nf90_put_att(ncid,saltid,"missing_value",NF90_FILL_DOUBLE),__LINE__)
   call nc_check(nf90_put_att(ncid,saltid,"_FillValue",   NF90_FILL_DOUBLE),__LINE__)
   
   name = "U VELOCITY "
   unit = "m/sec"
   call nc_check(nf90_put_att(ncid,uvelid,"long_name",  name),__LINE__)
   call nc_check(nf90_put_att(ncid,uvelid,"units",      unit),__LINE__)
   call nc_check(nf90_put_att(ncid,uvelid,"missing_value",NF90_FILL_DOUBLE),__LINE__)
   call nc_check(nf90_put_att(ncid,uvelid,"_FillValue",   NF90_FILL_DOUBLE),__LINE__)
   
   name = "V VELOCITY "
   unit = "m/sec"
   call nc_check(nf90_put_att(ncid,vvelid,"long_name",  name),__LINE__)
   call nc_check(nf90_put_att(ncid,vvelid,"units",      unit),__LINE__)
   call nc_check(nf90_put_att(ncid,vvelid,"missing_value",NF90_FILL_DOUBLE),__LINE__)
   call nc_check(nf90_put_att(ncid,vvelid,"_FillValue",   NF90_FILL_DOUBLE),__LINE__)
   
   name = "PRESSURE "
   unit = "N/m^2 "
   call nc_check(nf90_put_att(ncid,presid,"long_name",  name),__LINE__)
   call nc_check(nf90_put_att(ncid,presid,"units",      unit),__LINE__)
   call nc_check(nf90_put_att(ncid,presid,"missing_value",NF90_FILL_DOUBLE),__LINE__)
   call nc_check(nf90_put_att(ncid,presid,"_FillValue",   NF90_FILL_DOUBLE),__LINE__)
   
   
!   expnam= " "
!   stamp = " "
   call nc_check(nf90_put_att(ncid,NF90_GLOBAL,"title",  trim(expnam)),__LINE__)
!   call nc_check(nf90_put_att(ncid,NF90_GLOBAL,"history",stamp),__LINE__)
   !
   !-----------------------------------------------------------------------
   !     leave define mode
   !-----------------------------------------------------------------------
   !
   call nc_check(nf90_enddef(ncid),__LINE__)
   !
   !
   !-----------------------------------------------------------------------
   !     put variables in netCDF file
   !-----------------------------------------------------------------------
   !
   ! store Particles
   call nc_check(nf90_put_var(ncid,partid,pnum),__LINE__)
   !
   ! store Time      
   call nc_check(nf90_put_var(ncid,Timeid,Time),__LINE__)
   !
   ! store values
   call nc_check(nf90_put_var(ncid,xpartid,xpart),__LINE__)
   call nc_check(nf90_put_var(ncid,ypartid,ypart),__LINE__)
   call nc_check(nf90_put_var(ncid,zpartid,zpart),__LINE__)
   call nc_check(nf90_put_var(ncid,ipartid,ipart),__LINE__)
   call nc_check(nf90_put_var(ncid,jpartid,jpart),__LINE__)
   call nc_check(nf90_put_var(ncid,kpartid,kpart),__LINE__)
   call nc_check(nf90_put_var(ncid,tempid, temp),__LINE__)
   call nc_check(nf90_put_var(ncid,saltid, salt),__LINE__)
   call nc_check(nf90_put_var(ncid,uvelid, uvel),__LINE__)
   call nc_check(nf90_put_var(ncid,vvelid, vvel),__LINE__)
   call nc_check(nf90_put_var(ncid,presid, pres),__LINE__)
   
   call nc_check(nf90_close(ncid),__LINE__)
   
   write(STDOUT,*) " End "
      
   !-----------------------------------------------------------------------
   !     internal subroutines
   !-----------------------------------------------------------------------
contains
   subroutine nc_check(status,lineno)
      integer, intent ( in) :: status
      integer, intent ( in) :: lineno
   
      if(status /= nf90_noerr) then
         print *, "Error at line number ",lineno,":"
         print *, trim(nf90_strerror(status))
         stop 2
      end if
   end subroutine nc_check
end program cvfloat
