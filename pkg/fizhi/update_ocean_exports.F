C $Header: /u/gcmpack/MITgcm/pkg/fizhi/update_ocean_exports.F,v 1.3 2004/06/08 16:42:54 molod Exp $
C $Name:  $

       subroutine update_ocean_exports (myTime, myIter, myThid)
c----------------------------------------------------------------------
c  Subroutine update_ocean_exports - 'Wrapper' routine to update
c        the fields related to the ocean's surface that are needed
c        by fizhi (sst and sea ice extent).
c
c Call:  getsst  (Return the current sst field-read dataset if needed)
c        getsice (Return the current sea ice field-read data if needed)
c-----------------------------------------------------------------------
       implicit none
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "GRID.h"
#include "fizhi_ocean_coms.h"
#include "EEPARAMS.h"
#include "chronos.h"

       integer myTime, myIter, myThid

       integer i, j, L, bi, bj
       integer im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2

       im1 = 1-OLx
       im2 = sNx+OLx
       jm1 = 1-OLy
       jm2 = sNy+OLy
       idim1 = 1
       idim2 = sNx
       jdim1 = 1
       jdim2 = sNy

       integer ksst,kice

c Declare Local Variables
c -----------------------
       real        sstmin 
       parameter ( sstmin = 273.16 )

       integer i,j,im,jm

C*********************************************************************
C****             Interpolate Data to Current Time                ****
C*********************************************************************

       call getsst (ksst,nymd,nhms,sst,im,jm)
       call getsice (kice,nymd,nhms,sice,im,jm)

c Check for Minimum Open-Water SST
c --------------------------------
       do j=1,jm
       do i=1,im
       if(sice(i,j).eq.0.0 .and. sst(i,j).lt.sstmin)sst(i,j) = sstmin
       enddo
       enddo

       return
       end

       subroutine getsice ( iunit,nymd,nhms,sice,im,jm )
C************************************************************************
C!GETSICE
C************************************************************************
C
C!ROUTINE:	GETSICE
C!PROGRAMMER:	Sharon Nebuda
C!DATE CODED:	May 8, 1996
C!DESCRIPTION:	GETSICE returns the sea ice depth.
C!		This routine is adaptable for any frequency
C!		data upto a daily frequency.  
C!		note: for diurnal data ndmax should be increased.
C
C!INPUT PARAMETERS:
C!		iunit   Unit number assigned to the sice data file
C!		nymd	YYMMDD of the current model timestep
C!		nhms	HHMMSS of the model time
C!            im     Number of x points
C!            jm     Number of y points
C!       lattice     Grid Decomposition defined by Dynamics
C
C!OUTPUT PARAMETERS:
C!		sice(im,jm)	Sea ice depth in meters
C
C!ROUTINES CALLED:
C
C!	bcdata		Reads the data for a given unit number
C!	bcheader	Reads the header info for a given unit number
C!     interp_time   Returns weights for linear interpolation
C
C--------------------------------------------------------------------------

      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice

c MPI Utilities
c -------------
      include 'mpif.h'
      integer  ierror

C  Parameter statements
      integer 	ndmax 	! Maximum number of dates in one year for the data
      parameter (ndmax = 370)

C  Variables passed to the routine:
      integer 	iunit	! Unit number assigned to the sea ice data file
      integer 	nymd	! YYMMDD of the current model timestep
      integer 	nhms	! HHMMSS of the model time
      integer        im     ! Number of x points
      integer        jm     ! Number of y points

C  Variables returned by the routine:
      real 	sice(im,jm)	! Sea ice depth in meters

C  Variables unique to the routine:
      character*8  cname	! Name of the data in the file header
      character*80 cdscrip	! Description of the data in the file header
      real	fac1		! Weighted value (fraction) of the data
				! before the model time
      real	fac2		! Weighted value (fraction) of the data
				! after the model time
      logical	first		! True for first time using the dates for the
				! BC data file.  Then read in the header file.
      logical	found		! If false, then the data surrounding the model
				! time was not found
      logical	error		! TRUE if problem with data
      integer 	i,j,n,nn	! DO loop counters
      integer 	iyear		! Year of model 
      integer 	iyearbc		! Year of boundary condition data 
      real 	lat0		! Starting lat of the bc data set (future use)
      real 	lon0		! Starting lon of the bc data set (future use)
      integer 	nd		! Counter for record number of data to read
      integer 	ndby3		! int(ndatebc/3) used for write statement
      integer 	imbc  		! IM read from the BC data
      integer 	jmbc		! JM read from the BC data
      integer	ndatebc		! Number of dates in the BC file
      integer 	nhmsbc(ndmax)	! HHMMSS of the data time (not needed currently)
      integer	nhmsbc1	! HHMMSS of the earlier data kept from last timestep
      integer	nhmsbc2 ! HHMMSS of the later data kept from last timestep
      integer 	nrec		! Record number of the header (set to 1)
      integer	nymdbc(ndmax)	! YYYYMMDD of each data
      integer	nymdbc1	! YYYYMMDD of the earlier data kept from last timestep
      integer	nymdbc2	! YYYYMMDD of the later data kept from last timestep
      integer 	nymdmod	 	! YYYYMMDD of the current model timestep
      real 	timebc1		! YYYYMMDD.HHMMSS of the earlier bc data 
      real 	timebc2		! YYYYMMDD.HHMMSS of the later bc data 
      real 	timemod		! YYYYMMDD.HHMMSS of the current timestep
      real 	undef		! Undefined value for missing data

C Define Allocatable Arrays
      real, allocatable, save :: sicebc1(:,:)    ! Sea ice 0=no 1=yes from the bc data
                                                 ! of the date before the model time
      real, allocatable, save :: sicebc2(:,:)    ! Sea ice 0=no 1=yes from bc data
                                                 ! of the date after the model time

C--------- Variable Initialization ---------------------------------

      data first /.true./
      data error /.false./

c  save header info
      save imbc, jmbc, lat0, lon0, ndatebc, undef, nymdbc, nhmsbc
      save nymdbc1, nymdbc2, nhmsbc1, nhmsbc2
      save first

c  this only works for between 1950-2050
      if (nymd .lt. 500101) then
        nymdmod = 20000000 + nymd
      else if (nymd .le. 991231) then
        nymdmod = 19000000 + nymd
      else
        nymdmod = nymd
      endif

c  initialize so that first time through they have values for the check
c  these vaules make the iyear .ne. iyearbc true anyways for
c  for the first time so first isnt checked below.

      if (first) then
      allocate ( sicebc1(lattice%imglobal,lattice%jmglobal) )    ! Allocate Memory for sicebc1
      allocate ( sicebc2(lattice%imglobal,lattice%jmglobal) )    ! Allocate Memory for sicebc2
        nymdbc(2) = 0
        nymdbc1   = 0
        nymdbc2   = 0
        nhmsbc1   = 0
        nhmsbc2   = 0
        first = .false.
      endif

C---------- Read in Header file ----------------------------------

      iyear   = nymdmod/10000
      iyearbc = nymdbc(2)/10000

      if( iyear.ne.iyearbc ) then
        if( lattice%myid.eq.0 ) then

        close(iunit)
        open (iunit, form='unformatted', access='direct',recl=lattice%imglobal*lattice%jmglobal*4)
        nrec = 1
        call bcheader (iunit, ndmax, nrec,
     .                 cname, cdscrip, imbc, jmbc, lat0, lon0, 
     .                 ndatebc, nymdbc, nhmsbc, undef, error)

C--------- Check data for Compatibility ------------------------------

C Check for correct data in boundary condition file
       if (.not.error .and. cname.ne.'SICE') then
            write(6,*) 'Wrong data in SICE boundary condition file => ',cname
            error = .true.
       endif

C Check Horizontal Resolution
       if (.not.error .and. imbc*jmbc.ne.lattice%imglobal*lattice%jmglobal) then
            write(6,*) 'Boundary Condition Resolution DOES NOT match Model Resolution!'
            write(6,*) 'Boundary Condition Resolution:  ',imbc*jmbc
            write(6,*) '             Model Resolution:  ',lattice%imglobal*lattice%jmglobal
            error = .true.
       endif

C Check Year 
       iyearbc = nymdbc(2)/10000
       if (.not.error .and. iyear.ne.iyearbc .and. iyearbc.ne.0) then
            write(6,*) 'Boundary Condition Year DOES NOT match REQUESTED Year!'
            write(6,*) 'Boundary Condition Year:  ', iyearbc
            write(6,*) '         Requested Year:  ', iyear
            error = .true.
       endif

       if (.not.error)   then		
C if climatology, fill dates for data with current model year
            if (iyearbc.eq.0) then		
            write(6,*)
            write(6,*) 'Climatological Dataset is being used.'  
            write(6,*) 'Current model year will be used to fill Header Dates'
              do n = 2, ndatebc-1
               nymdbc(n) = nymdbc(n) +(nymdmod/10000)*10000 
              enddo
C  For the first date subtract 1 year from the current model NYMD
              n = 1
              nymdbc(n) = nymdbc(n) +(nymdmod/10000-1)*10000 
C  For the last date add 1 year to the current model NYMD
              n = ndatebc
              nymdbc(n) = nymdbc(n) +(nymdmod/10000+1)*10000 
            endif

C  Write out header info
       write(6,*) ' Updated boundary condition data'
       write(6,*) ' ---------------------------------'
       write(6,*) ' Variable: ',cname
       write(6,*) ' Description: ',cdscrip
       write(6,*) ' Resolution: x= ',imbc,' y= ',jmbc,' Undefined value = ',undef
       write(6,*) ' Starting latitude = ',lat0,' longitude =',lon0
       write(6,*) ' Data valid at these times: '
       ndby3 = ndatebc/3
       do n = 1, ndby3*3,3
        write(6,1000) (nn,nymdbc(nn),nhmsbc(nn),nn=n,n+2)
 1000   format(3(2x,i3,':',i8,2x,i8))
       enddo
       write(6,1000) (nn,nymdbc(nn),nhmsbc(nn),nn=ndby3*3+1,ndatebc)
      endif  ! End error  Check

      endif  ! End MYID=0 Check

c Broadcast information to other PEs
c ----------------------------------
#if (mpi)
       call mpi_bcast ( error,1,mpi_logical,0,lattice%comm,ierror )
#endif
       if( error ) call my_exit (101)

#if (mpi)
       call mpi_bcast ( ndatebc,1     ,mpi_integer,0,lattice%comm,ierror )
       call mpi_bcast ( nymdbc,ndatebc,mpi_integer,0,lattice%comm,ierror )
       call mpi_bcast ( nhmsbc,ndatebc,mpi_integer,0,lattice%comm,ierror )
#endif

      endif  ! New Year Info Check

C---------- Read sice data if necessary -------------------------------

        found = .false.
        nd = 2

c  If model time is not within the times of saved sice data 
c  from previous call to getsice then read new data 

        timemod = float(nymdmod) + float(nhms)   /1000000
        timebc1 = float(nymdbc1) + float(nhmsbc1)/1000000
        timebc2 = float(nymdbc2) + float(nhmsbc2)/1000000
        if (timemod .lt. timebc1 .or. timemod .ge. timebc2) then

        do while (.not.found .and. nd .le. ndatebc)
          timebc2 = float(nymdbc(nd)) + float(nhmsbc(nd))/1000000
          if (timebc2 .gt. timemod) then	
            nymdbc1 = nymdbc(nd-1)
            nymdbc2 = nymdbc(nd)
            nhmsbc1 = nhmsbc(nd-1)
            nhmsbc2 = nhmsbc(nd)
            if ( lattice%myid.eq.0 ) call bcdata (iunit, imbc, jmbc, nd, nd+1, sicebc1, sicebc2)
#if (mpi)
            call mpi_bcast ( sicebc1,lattice%imglobal*lattice%jmglobal,mpi_double_precision,0,lattice%comm,ierror )
            call mpi_bcast ( sicebc2,lattice%imglobal*lattice%jmglobal,mpi_double_precision,0,lattice%comm,ierror )
#endif
            found = .true.
          else
            nd = nd + 1
          endif
        enddo

c  Otherwise the data from the last time in getsice surrounds the 
c  current model time.

        else
          found = .true.
        endif
 
          if (.not.found) then
            if( lattice%myid.eq.0 ) print *, 'STOP: Could not find SICE boundary condition dates surrounding the model time.' 
            call my_finalize
            call my_exit (101)
          endif

C---------- Interpolate sice data ------------------------------------

          call interp_time ( nymdmod,nhms, nymdbc1,nhmsbc1, nymdbc2,nhmsbc2, fac1,fac2 )

          do j = 1, jm
          do i = 1, im
            sice(i,j) = sicebc1( lattice%iglobal(i),lattice%jglobal(j) )*fac1 
     .                + sicebc2( lattice%iglobal(i),lattice%jglobal(j) )*fac2 
c average to 0 or 1
c -----------------
            if (sice(i,j) .ge. 0.5) then
                sice(i,j) = 1.
            else
                sice(i,j) = 0.
            endif
          enddo
          enddo

C---------- Fill sice with depth of ice ------------------------------------

          do j = 1, jm
          do i = 1, im
            if (sice(i,j) .eq. 1.) then		! sea ice present
                sice(i,j) = 3.
            endif
          enddo
          enddo

C---------------------------------------------------------------------------
 
      return
      end
      subroutine getsst ( iunit,nymd,nhms,sst,im,jm,lattice )
C************************************************************************
C!GETSST
C************************************************************************
C
C!ROUTINE:	GETSST
C!PROGRAMMER:	Sharon Nebuda
C!DATE CODED:	May 8, 1996
C!DESCRIPTION:	GETSST gets the SST data.
C!		This routine is adaptable for any frequency
C!		data upto a daily frequency.  
C!		note: for diurnal data ndmax should be increased.
C
C!INPUT PARAMETERS:
C!		iunit   Unit number assigned to the sst data file
C!		nymd	YYMMDD of the current model timestep
C!		nhms	HHMMSS of the model time
C!            im     Number of x points
C!            jm     Number of y points
C!       lattice     Grid Decomposition defined by Dynamics
C
C!OUTPUT PARAMETERS:
C!		sst(im,jm)	Sea surface temperature in Kelvin
C
C!ROUTINES CALLED:
C
C!	bcdata		Reads the data for a given unit number
C!	bcheader	Reads the header info for a given unit number
C!     interp_time   Returns weights for linear interpolation
C
C--------------------------------------------------------------------------

      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice

c MPI Utilities
c -------------
      include 'mpif.h'
      integer  ierror

C  Parameter statements
      integer 	ndmax 	! Maximum number of dates in one year for the data
      parameter (ndmax = 370)

C  Variables passed to the routine:
      integer 	iunit	! Unit number assigned to the SST data file
      integer 	nymd	! YYMMDD of the current model timestep
      integer 	nhms	! HHMMSS of the model time
      integer        im     ! Number of x points
      integer        jm     ! Number of y points

C  Variables returned by the routine:
      real 	sst(im,jm)	! Sea surface temperature in Kelvin

C  Variables unique to the routine:
      character*8  cname	! Name of the data in the file header
      character*80 cdscrip	! Description of the data in the file header
      real	fac1		! Weighted value (fraction) of the data
				! before the model time
      real	fac2		! Weighted value (fraction) of the data
				! after the model time
      logical	first		! True for first time using the dates for the
				! BC data file.  Then read in the header file.
      logical	found		! If false, then the data surrounding the model
				! time was not found
      logical error         ! TRUE if problem with data
      integer 	i,j,n,nn	! DO loop counters
      integer 	iyear		! Year of model 
      integer 	iyearbc		! Year of boundary condition data 
      real 	lat0		! Starting lat of the bc data set (future use)
      real 	lon0		! Starting lon of the bc data set (future use)
      integer 	nd		! Counter for record number of data to read
      integer 	ndby3		! int(ndatebc/3) used for write statement
      integer 	imbc  		! IM read from the BC data
      integer 	jmbc		! JM read from the BC data
      integer	ndatebc		! Number of dates in the BC file
      integer	nhmsbc(ndmax)	       ! HHMMSS of the data time (not needed currently)
      integer	nhmsbc1		! HHMMSS of the earlier data kept from last timestep
      integer	nhmsbc2 	       ! HHMMSS of the later data kept from last timestep
      integer nrec	              ! Record number of the header (set to 1)
      integer	nymdbc(ndmax)	       ! YYYYMMDD of each data
      integer	nymdbc1		! YYYYMMDD of the earlier data kept from last timestep
      integer	nymdbc2 	       ! YYYYMMDD of the later data kept from last timestep
      integer nymdmod	 	! YYYYMMDD of the current model timestep
      real 	timebc1		! YYYYMMDD.HHMMSS of the earlier bc data 
      real 	timebc2		! YYYYMMDD.HHMMSS of the later bc data 
      real 	timemod		! YYYYMMDD.HHMMSS of the current timestep
      real 	undef		! Undefined value for missing data

C Define Allocatable Arrays
      real, allocatable, save :: sstbc1(:,:)     ! Sea surface temperature (K) from bc data
                                                 ! of the date before the model time
      real, allocatable, save :: sstbc2(:,:)     ! Sea surface temperature (K) from bc data
                                                 ! of the date after the model time

C--------- Variable Initialization ---------------------------------

      data first /.true./
      data error /.false./

c  save header info
      save imbc, jmbc, lat0, lon0, ndatebc, undef, nymdbc, nhmsbc
      save nymdbc1, nymdbc2, nhmsbc1, nhmsbc2, sstbc1
      save first

c  this only works for between 1950-2050
      if (nymd .lt. 500101) then
        nymdmod = 20000000 + nymd
      else if (nymd .le. 991231) then
        nymdmod = 19000000 + nymd
      else
        nymdmod = nymd
      endif

c  initialize so that first time through they have values for the check
c  these vaules make the iyear .ne. iyearbc true anyways for
c  for the first time so first isnt checked below.
      if (first) then
      allocate ( sstbc1(lattice%imglobal,lattice%jmglobal) )  ! Allocate Memory for sstbc1
      allocate ( sstbc2(lattice%imglobal,lattice%jmglobal) )  ! Allocate Memory for sstbc2
        nymdbc(2) = 0
        nymdbc1   = 0
        nymdbc2   = 0
        nhmsbc1   = 0
        nhmsbc2   = 0
        first = .false.
      endif

C---------- Read in Header file ----------------------------------

      iyear   = nymdmod/10000
      iyearbc = nymdbc(2)/10000

      if( iyear.ne.iyearbc ) then
        if( lattice%myid.eq.0 ) then  

        close(iunit)
        open (iunit, form='unformatted', access='direct',recl=lattice%imglobal*lattice%jmglobal*4)
        nrec = 1
        call bcheader (iunit, ndmax, nrec,
     .                 cname, cdscrip, imbc, jmbc, lat0, lon0, 
     .                 ndatebc, nymdbc, nhmsbc, undef, error)

C--------- Check data for Compatibility

C Check for correct data in boundary condition file
       if (.not.error .and. cname.ne.'SST') then
            write(6,*) 'Wrong data in SST boundary condition file => ',cname
            error = .true.
       endif

C Check Horizontal Resolution
       if (.not.error .and. imbc*jmbc.ne.lattice%imglobal*lattice%jmglobal) then
            write(6,*) 'Boundary Condition Resolution DOES NOT match Model Resolution!'
            write(6,*) 'Boundary Condition Resolution:  ',imbc*jmbc
            write(6,*) '             Model Resolution:  ',lattice%imglobal*lattice%jmglobal
            error = .true.
       endif

C Check Year 
       iyearbc = nymdbc(2)/10000
       if (.not.error .and. iyear.ne.iyearbc .and. iyearbc.ne.0) then
            write(6,*) 'Boundary Condition Year DOES NOT match REQUESTED Year!'
            write(6,*) 'Boundary Condition Year:  ', iyearbc
            write(6,*) '         Requested Year:  ', iyear
            error = .true.
       endif

       if (.not.error)   then
C if climatology, fill dates for data with current model year
            if (iyearbc.eq.0) then		
            write(6,*)
            write(6,*) 'Climatological Dataset is being used.'  
            write(6,*) 'Current model year will be used to fill Header Dates'
              do n = 2, ndatebc-1
               nymdbc(n) = nymdbc(n) +(nymdmod/10000)*10000 
              enddo
C  For the first date subtract 1 year from the current model NYMD
              n = 1
              nymdbc(n) = nymdbc(n) +(nymdmod/10000-1)*10000 
C  For the last date add 1 year to the current model NYMD
              n = ndatebc
              nymdbc(n) = nymdbc(n) +(nymdmod/10000+1)*10000 
            endif

C  Write out header info
       write(6,*) ' Updated boundary condition data'
       write(6,*) ' ---------------------------------'
       write(6,*) ' Variable: ',cname
       write(6,*) ' Description: ',cdscrip
       write(6,*) ' Resolution: x= ',imbc,' y= ',jmbc,' Undefined value = ',undef
       write(6,*) ' Starting latitude = ',lat0,' longitude =',lon0
       write(6,*) ' Data valid at these times: '
       ndby3 = ndatebc/3
       do n = 1, ndby3*3,3
        write(6,1000) (nn,nymdbc(nn),nhmsbc(nn),nn=n,n+2)
 1000   format(3(2x,i3,':',i8,2x,i8))
       enddo
       write(6,1000) (nn,nymdbc(nn),nhmsbc(nn),nn=ndby3*3+1,ndatebc)
      endif  ! End error  Check

      endif  ! End MYID=0 Check

c Broadcast information to other PEs
c ----------------------------------
#if (mpi)
       call mpi_bcast ( error,1,mpi_logical,0,lattice%comm,ierror )
#endif
       if( error ) call my_exit (101)

#if (mpi)
       call mpi_bcast ( ndatebc,1     ,mpi_integer,0,lattice%comm,ierror )
       call mpi_bcast ( nymdbc,ndatebc,mpi_integer,0,lattice%comm,ierror )
       call mpi_bcast ( nhmsbc,ndatebc,mpi_integer,0,lattice%comm,ierror )
#endif

      endif  ! New Year Info Check

C---------- Read SST data if necessary -------------------------------

        found = .false.
        nd = 2

c  If model time is not within the times of saved sst data 
c  from previous call to getsst then read new data 

        timemod = float(nymdmod) + float(nhms)   /1000000
        timebc1 = float(nymdbc1) + float(nhmsbc1)/1000000
        timebc2 = float(nymdbc2) + float(nhmsbc2)/1000000
        if (timemod .lt. timebc1 .or. timemod .ge. timebc2) then

        do while (.not.found .and. nd .le. ndatebc)
          timebc2 = float(nymdbc(nd)) + float(nhmsbc(nd))/1000000
          if (timebc2 .gt. timemod) then	
            nymdbc1 = nymdbc(nd-1)
            nymdbc2 = nymdbc(nd)
            nhmsbc1 = nhmsbc(nd-1)
            nhmsbc2 = nhmsbc(nd)
            if ( lattice%myid.eq.0 ) call bcdata (iunit, imbc, jmbc, nd, nd+1, sstbc1, sstbc2)
#if (mpi)
            call mpi_bcast ( sstbc1,lattice%imglobal*lattice%jmglobal,mpi_double_precision,0,lattice%comm,ierror )
            call mpi_bcast ( sstbc2,lattice%imglobal*lattice%jmglobal,mpi_double_precision,0,lattice%comm,ierror )
#endif
            found = .true.
          else
            nd = nd + 1
          endif
        enddo

c  Otherwise the data from the last time in getsst surrounds the 
c  current model time.

        else
          found = .true.
        endif
 
        if (.not.found) then
          if( lattice%myid.eq.0 ) print *, 'STOP: Could not find SST boundary condition dates surrounding the model time.'
          call my_finalize
          call my_exit (101)
        endif

C---------- Interpolate SST data ------------------------------------

          call interp_time ( nymdmod,nhms, nymdbc1,nhmsbc1, nymdbc2,nhmsbc2, fac1,fac2 )

          do j = 1, jm
          do i = 1, im
            sst(i,j) = sstbc1( lattice%iglobal(i),lattice%jglobal(j) )*fac1 
     .               + sstbc2( lattice%iglobal(i),lattice%jglobal(j) )*fac2 
          enddo
          enddo
 
      return
      end

      subroutine bcdata (iunit, im, jm, nrec1, nrec2, field1, field2)
C************************************************************************
C!BCDATA
C************************************************************************
C
C!ROUTINE:	BCDATA
C!PROGRAMMER:	Sharon Nebuda
C!DATE CODED:	April 29, 1996
C!DESCRIPTION:	BCDATA reads the data from the file assigned to the
C!		passed unit number and returns data from the two times
C!		surrounding the current model time.  The two record 
C!		postitions are not assumed to be next to each other.
C
C!INPUT PARAMETERS:
C!	im	number of x points
C!	jm	number of y points
C!	nrec1	record number of the time before the model time
C!	nrec2	record number of the time after the model time
C
C!OUTPUT PARAMETERS:
C!	field1(im,jm)	data field before the model time
C!	field2(im,jm)	data field after the model time
C
C!REVISION HISTORY:	
C!	NEW
C
C!ROUTINES CALLED:
C
C!      none
C
C--------------------------------------------------------------------------


C--------------- Variable Declaration -------------------------------------
      implicit none

C  Variables passed to the routine:
      integer 	iunit	! Unit number assigned to the data file
      integer 	im	! Number of x points
      integer 	jm	! Number of y points
      integer 	nrec1	! Record number of the time before the model time
      integer 	nrec2	! Record number of the time after the model time

C  Variables returned by the routine:
      real  	field1(im,jm)	! Real*8 Data before the model time
      real  	field2(im,jm)	! Real*8 Data after  the model time

C  Variables unique to the routine:
      integer i,j		! DO loop counters
      real*4	f1(im,jm)	! Real*4 Data before the model time
      real*4	f2(im,jm)	! Real*4 Data after  the model time

C--------- Read file -----------------------------------------------

      read(iunit,rec=nrec1) f1
      read(iunit,rec=nrec2) f2

      do j=1,jm
      do i=1,im
      field1(i,j) = f1(i,j)
      field2(i,j) = f2(i,j)
      enddo
      enddo

      return
      end
      subroutine bcheader (iunit, ndmax, nrec,
     .                     cname, cdscrip, im, jm, lat0, lon0, ndatebc, 
     .                     nymdbc, nhmsbc, undef, error)
C************************************************************************
C!BCHEADER
C************************************************************************
C
C!ROUTINE:	BCHEADER
C!PROGRAMMER:	Sharon Nebuda
C!DATE CODED:	April 29, 1996
C!DESCRIPTION:	BCHEADER reads the header info from the file assigned to the
C!		passed unit number and returns the info back.  
C
C!INPUT PARAMETERS:
C!	iunit   unit number assigned to the data file
C!	ndmax	maximum number of date/times of the data
C!	nrec	record number of the header info (or assume 1??)
C
C!OUTPUT PARAMETERS:
C!      cname		name of the data in the file header
C!      cdscrip		description of the data in the file header
C!	im		number of x points
C!	jm		number of y points
C!	lat0		starting latitude for the data grid
C!	lon0		starting longitude for the data grid
C!	ndatebc		number of date/times of the data in the file
C!	nymdbc(ndmax)	array of dates for the data including century
C!	nhmsbc(ndmax)	array of times for the data
C!	undef		value for undefined values in the data
C!	error		logical TRUE if dataset problem
C
C!REVISION HISTORY:	
C!	NEW
C
C!ROUTINES CALLED:
C!      none
C
C--------------------------------------------------------------------------

C--------------- Variable Declaration -------------------------------------
      implicit none

C  Variables passed to the routine:
      integer 	iunit	! Unit number assigned to the data file
      integer 	ndmax	! Maximum number of dates for a given field
      integer 	nrec	! Record number of the header info (or assume 1??)

C  Variables returned by the routine:
      character*8  cname	! Name of the data in the file header
      character*80 cdscrip	! Description of the data in the file header
      integer 	im     ! Number of x points
      integer 	jm     ! Number of y points
      real         lat0     ! Starting latitude of the data
      real   	     lon0     ! Starting longitude of the data
      integer	ndatebc	! Number of date/times of the data in the file
      integer	nymdbc(ndmax)	! array of dates for the data including century
      integer	nhmsbc(ndmax)	! array of times for the data
      real 	undef		! value for undefined values in the data
      logical error         ! logical TRUE if dataset problem

C  Variables unique to the routine:
      integer 	i		! DO loop counters
      integer n
      integer*4            im_32  ! Number of x points
      integer*4 	      jm_32  ! Number of y points
      real*4             lat0_32  ! Starting latitude of the data
      real*4 	           lon0_32  ! Starting longitude of the data
      integer*4       ndatebc_32  ! Number of date/times of the data in the file
      integer*4 nymdbc_32(ndmax)  ! array of dates for the data including century
      integer*4 nhmsbc_32(ndmax)  ! array of times for the data
      real*4     undef_32         ! value for undefined values in the data

C--------- Read file -----------------------------------------------

      read(iunit,rec=nrec,err=500) cname, cdscrip, 
     .                     im_32, jm_32, lat0_32, lon0_32, 
     .                     ndatebc_32, undef_32,
     .                    (nymdbc_32(i), nhmsbc_32(i), i=1,ndatebc_32)

      im    =    im_32
      jm    =    jm_32
      lat0  =  lat0_32
      lon0  =  lon0_32
      undef = undef_32

      ndatebc = ndatebc_32
      do i=1,ndatebc
      nymdbc(i) = nymdbc_32(i)
      nhmsbc(i) = nhmsbc_32(i)
      enddo

      return
  500 continue
      print *, 'Error reading boundary condition from unit ',iunit
      error = .true.
      return
      end
