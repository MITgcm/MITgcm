C $Header: /u/gcmpack/MITgcm/pkg/fizhi/update_ocean_exports.F,v 1.16 2005/02/25 18:35:30 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
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
#include "SIZE.h"
#include "GRID.h"
#include "fizhi_ocean_coms.h"
#include "EEPARAMS.h"
#include "chronos.h"

       integer myIter, myThid
       _RL myTime

       integer i, j, bi, bj, biglobal, bjglobal
       integer im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2
       integer nSxglobal, nSyglobal
       integer ksst,kice
       _RL        sstmin 
       parameter ( sstmin = 273.16 )

       idim1 = 1-OLx
       idim2 = sNx+OLx
       jdim1 = 1-OLy
       jdim2 = sNy+OLy
       im1 = 1
       im2 = sNx
       jm1 = 1
       jm2 = sNy
       nSxglobal = nSx*nPx
       nSyglobal = nSy*nPy

       call mdsfindunit( ksst, myThid )
       call mdsfindunit( kice, myThid )
 
C***********************************************************************

       DO BJ = myByLo(myThid),myByHi(myThid)
       DO BI = myBxLo(myThid),myBxHi(myThid)

       biglobal=bi+(myXGlobalLo-1)/im2
       bjglobal=bj+(myYGlobalLo-1)/jm2

       call getsst(ksst,idim1,idim2,jdim1,jdim2,im1,im2,jm1,jm2,nSx,
     .  nSy,nSxglobal,nSyglobal,bi,bj,biglobal,bjglobal,nymd,nhms,sst)
       call getsice(kice,idim1,idim2,jdim1,jdim2,im1,im2,jm1,jm2,nSx,
     .  nSy,nSxglobal,nSyglobal,bi,bj,biglobal,bjglobal,nymd,nhms,sice)

c Check for Minimum Open-Water SST
c --------------------------------
       do j=jm1,jm2
       do i=im1,im2
       if(sice(i,j,bi,bj).eq.0.0 .and. sst(i,j,bi,bj).lt.sstmin)
     .                                          sst(i,j,bi,bj) = sstmin
       enddo
       enddo

       ENDDO
       ENDDO

       return
       end

       subroutine getsice(iunit,idim1,idim2,jdim1,jdim2,im1,im2,jm1,jm2,
     .     nSumx,nSumy,nPgx,nPgy,bi,bj,biglobal,bjglobal,nymd,nhms,sice)
C************************************************************************
C
C!ROUTINE:	GETSICE
C!DESCRIPTION:	GETSICE returns the sea ice depth.
C!		This routine is adaptable for any frequency
C!		data upto a daily frequency.  
C!		note: for diurnal data ndmax should be increased.
C
C!INPUT PARAMETERS:
C!	iunit     Unit number assigned to the sice data file
C!      idim1     Start dimension in x-direction
C!      idim2     End dimension in x-direction
C!      jdim1     Start dimension in y-direction
C!      jdim2     End dimension in y-direction
C!      im1       Begin of x-direction span for filling sice
C!      im2       End of x-direction span for filling sice
C!      jm1       Begin of y-direction span for filling sice
C!      jm2       End of y-direction span for filling sice
C!      nSumx     Number of processors in x-direction (local processor)
C!      nSumy     Number of processors in y-direction (local processor)
C!      nPgx      Number of processors in x-direction (global)
C!      nPgx      Number of processors in y-direction (global)
C!      bi        Processor number in x-direction (local to processor)
C!      bj        Processor number in y-direction (local to processor)
C!      biglobal  Processor number in x-direction (global)
C!      bjglobal  Processor number in y-direction (global)
C!	nymd	  YYMMDD of the current model timestep
C!	nhms	  HHMMSS of the model time
C
C!OUTPUT PARAMETERS:
C!	sice(idim1:idim2,jdim1:jdim2,nSumx,nSumy) Sea ice depth in meters
C
C!ROUTINES CALLED:
C
C!	bcdata	     Reads the data for a given unit number
C!	bcheader     Reads the header info for a given unit number
C!      interp_time  Returns weights for linear interpolation
C
C--------------------------------------------------------------------------

      implicit none
#include "SIZE.h"

      integer iunit,idim1,idim2,jdim1,jdim2,im1,im2,jm1,jm2,nSumx,nSumy
      integer nPgx,nPgy,bi,bj,biglobal,bjglobal,nymd,nhms

      _RL sice(idim1:idim2,jdim1:jdim2,nSumx,nSumy)

C Maximum number of dates in one year for the data
      integer 	ndmax
      parameter (ndmax = 370)

      character*8  cname
      character*80 cdscrip
      character*40 sicedata
      _RL fac1, fac2, lat0, lon0, timebc1, timebc2, timemod, undef
      logical first, found, error
      integer i,j,n,nn,iyear,iyearbc,nd,ndby3,imbc,jmbc,npxbc,npybc
      integer ndatebc,nhmsbc(ndmax), nhmsbc1, nhmsbc2,nrec
      integer nymdbc(ndmax),nymdbc1,nymdbc2,nymdmod

      _RL sicebc1(sNx,sNy,nSx*nPx,nSy*nPy)
      _RL sicebc2(sNx,sNy,nSx*nPx,nSy*nPy)

C--------- Variable Initialization ---------------------------------

      data first /.true./
      data error /.false./

c  save header info
      save imbc,jmbc,npxbc,npybc,lat0,lon0,ndatebc,undef,nymdbc,nhmsbc
      save nymdbc1, nymdbc2, nhmsbc1, nhmsbc2
      save first
      save sicebc1, sicebc2

c  this only works for between 1950-2050
      if (nymd .lt. 500101) then
        nymdmod = 20000000 + nymd
      else if (nymd .le. 991231) then
        nymdmod = 19000000 + nymd
      else
        nymdmod = nymd
      endif

      sicedata='sice19232.weekly.clim'

c  initialize so that first time through they have values for the check
c  these vaules make the iyear .ne. iyearbc true anyways for
c  for the first time so first isnt checked below.

      if (first) then
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

       close(iunit)
       open (iunit,file=sicedata,form='unformatted',access='direct',
     .                                         recl=im2*jm2*nPgx*nPgy*4)
       nrec = 1
       call bcheader (iunit, ndmax, nrec,
     .          cname, cdscrip, imbc, jmbc, npxbc, npybc, lat0, lon0, 
     .          ndatebc, nymdbc, nhmsbc, undef, error)

C--------- Check data for Compatibility ------------------------------

C Check for correct data in boundary condition file
       if (.not.error .and. cname.ne.'SICE') then
        write(6,*)'Wrong data in SICE boundary condition file => ',cname
        error = .true.
       endif

C Check Horizontal Resolution
       if(.not.error.and.imbc*jmbc*npxbc*npybc.ne.im2*jm2*npgx*npgy)then
        write(6,*) ' B.C. Resolution DOES NOT match Model Resolution!'
        write(6,*) ' B.C. Resolution:  ',imbc*jmbc*npxbc*npybc
        write(6,*) 'Model Resolution:  ',im2*jm2*npgx*npgy
        error = .true.
       endif

C Check Year 
       iyearbc = nymdbc(2)/10000
       if (.not.error .and. iyear.ne.iyearbc .and. iyearbc.ne.0) then
        write(6,*)'     B.C. Year DOES NOT match REQUESTED Year!'
        write(6,*)'     B.C. Year:  ', iyearbc
        write(6,*)'Requested Year:  ', iyear
        error = .true.
       endif

       if (.not.error)   then
C if climatology, fill dates for data with current model year
        if (iyearbc.eq.0) then		
         write(6,*)
         write(6,*) 'Climatological Dataset is being used.'  
         write(6,*) 'Current model year to be used to fill Header Dates'
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
        write(6,*) ' Resolution: x= ',imbc,' y= ',jmbc,
     .                                       ' Undefined value = ',undef
        write(6,*) ' Starting latitude = ',lat0,' longitude =',lon0
        write(6,*) ' Data valid at these times: '
        ndby3 = ndatebc/3
        do n = 1, ndby3*3,3
         write(6,1000) (nn,nymdbc(nn),nhmsbc(nn),nn=n,n+2)
 1000    format(3(2x,i3,':',i8,2x,i8))
        enddo
        write(6,1000) (nn,nymdbc(nn),nhmsbc(nn),nn=ndby3*3+1,ndatebc)
       endif  

      endif

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
         call bcdata (iunit,imbc,jmbc,nPgx,nPgy,nd,nd+1,sicebc1,sicebc2)
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
       print *, 'STOP: Could not find SICE dates for model time.' 
       call my_finalize
       call my_exit (101)
      endif

C---------- Interpolate sice data ------------------------------------

      call interp_time(nymdmod,nhms,nymdbc1,nhmsbc1,nymdbc2,nhmsbc2,
     .                                                       fac1,fac2)

      do j = jm1,jm2
      do i = im1,im2
       sice(i,j,bi,bj) = sicebc1(i,j,biglobal,bjglobal)*fac1 
     .                 + sicebc2(i,j,biglobal,bjglobal)*fac2 
c average to 0 or 1
c -----------------
       if (sice(i,j,bi,bj) .ge. 0.5) then
        sice(i,j,bi,bj) = 1.
       else
        sice(i,j,bi,bj) = 0.
       endif
      enddo
      enddo

C---------- Fill sice with depth of ice ------------------------------------
      do j = jm1,jm2
      do i = im1,im2
       if (sice(i,j,bi,bj) .eq. 1.) then		
        sice(i,j,bi,bj) = 3.
       endif
      enddo
      enddo
C---------------------------------------------------------------------------
 
      return
      end
      subroutine getsst(iunit,idim1,idim2,jdim1,jdim2,im1,im2,jm1,jm2,
     .      nSumx,nSumy,nPgx,nPgy,bi,bj,biglobal,bjglobal,nymd,nhms,sst)
C************************************************************************
C
C!ROUTINE:	GETSST
C!DESCRIPTION:	GETSST gets the SST data.
C!		This routine is adaptable for any frequency
C!		data upto a daily frequency.  
C!		note: for diurnal data ndmax should be increased.
C
C!INPUT PARAMETERS:
C!	iunit     Unit number assigned to the sice data file
C!      idim1     Start dimension in x-direction
C!      idim2     End dimension in x-direction
C!      jdim1     Start dimension in y-direction
C!      jdim2     End dimension in y-direction
C!      im1       Begin of x-direction span for filling sice
C!      im2       End of x-direction span for filling sice
C!      jm1       Begin of y-direction span for filling sice
C!      jm2       End of y-direction span for filling sice
C!      nSumx     Number of processors in x-direction (local processor)
C!      nSumy     Number of processors in y-direction (local processor)
C!      nPgx      Number of processors in x-direction (global)
C!      nPgy      Number of processors in y-direction (global)
C!      bi        Processor number in x-direction (local to processor)
C!      bj        Processor number in y-direction (local to processor)
C!      biglobal  Processor number in x-direction (global)
C!      bjglobal  Processor number in y-direction (global)
C!	nymd	  YYMMDD of the current model timestep
C!	nhms	  HHMMSS of the model time
C
C!OUTPUT PARAMETERS:
C!	sst(idim1:idim2,jdim1:jdim2,nSumx,nSumy) Sea surface temperature (K)
C
C!ROUTINES CALLED:
C
C!	bcdata		Reads the data for a given unit number
C!	bcheader	Reads the header info for a given unit number
C!     interp_time   Returns weights for linear interpolation
C
C--------------------------------------------------------------------------

      implicit none
#include "SIZE.h"

      integer iunit,idim1,idim2,jdim1,jdim2,im1,im2,jm1,jm2,nSumx,nSumy
      integer nPgx,nPgy,bi,bj,biglobal,bjglobal,nymd,nhms

      _RL sst(idim1:idim2,jdim1:jdim2,nSumx,nSumy)

C Maximum number of dates in one year for the data
      integer 	ndmax
      parameter (ndmax = 370)

      character*8  cname
      character*80 cdscrip
      character*40 sstdata
      _RL fac1, fac2, lat0, lon0, timebc1, timebc2, timemod, undef
      logical first, found, error
      integer i,j,n,nn,iyear,iyearbc,nd,ndby3,imbc,jmbc,npxbc,npybc
      integer ndatebc,nhmsbc(ndmax), nhmsbc1, nhmsbc2,nrec
      integer nymdbc(ndmax),nymdbc1,nymdbc2,nymdmod

      _RL sstbc1(sNx,sNy,nSx*nPx,nSy*nPy)
      _RL sstbc2(sNx,sNy,nSx*nPx,nSy*nPy)

C--------- Variable Initialization ---------------------------------

      data first /.true./
      data error /.false./

c  save header info
      save imbc,jmbc,npxbc,npybc,lat0,lon0,ndatebc,undef,nymdbc,nhmsbc
      save nymdbc1, nymdbc2, nhmsbc1, nhmsbc2
      save first
      save sstbc1, sstbc2

c  this only works for between 1950-2050
      if (nymd .lt. 500101) then
        nymdmod = 20000000 + nymd
      else if (nymd .le. 991231) then
        nymdmod = 19000000 + nymd
      else
        nymdmod = nymd
      endif

      sstdata='sst19232.weekly.clim'

c  initialize so that first time through they have values for the check
c  these vaules make the iyear .ne. iyearbc true anyways for
c  for the first time so first isnt checked below.
      if (first) then
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

       close(iunit)
       open (iunit,file=sstdata,form='unformatted',access='direct',
     .                                        recl=im2*jm2*nPgx*nPgy*4)
       nrec = 1
       call bcheader (iunit, ndmax, nrec,
     .          cname, cdscrip, imbc, jmbc, npxbc, npybc, lat0, lon0, 
     .          ndatebc, nymdbc, nhmsbc, undef, error)

C--------- Check data for Compatibility

C Check for correct data in boundary condition file
       if (.not.error .and. cname.ne.'SST') then
        write(6,*)'Wrong data in SST boundary condition file => ',cname
        error = .true.
       endif

C Check Horizontal Resolution
       if(.not.error.and.imbc*jmbc*npxbc*npybc.ne.im2*jm2*npgx*npgy)then
        write(6,*) ' B.C. Resolution DOES NOT match Model Resolution!'
        write(6,*) ' B.C. Resolution:  ',imbc*jmbc*npxbc*npybc
        write(6,*) 'Model Resolution:  ',im2*jm2*npgx*npgy
        error = .true.
       endif

C Check Year 
       iyearbc = nymdbc(2)/10000
       if (.not.error .and. iyear.ne.iyearbc .and. iyearbc.ne.0) then
        write(6,*)'     B.C. Year DOES NOT match REQUESTED Year!'
        write(6,*)'     B.C. Year:  ', iyearbc
        write(6,*)'Requested Year:  ', iyear
        error = .true.
       endif

       if (.not.error)   then
C if climatology, fill dates for data with current model year
        if (iyearbc.eq.0) then		
         write(6,*)
         write(6,*)'Climatological Dataset is being used.'  
         write(6,*)'Current model year is used to fill Header Dates'
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
        write(6,*) ' Resolution: x= ',imbc,' y= ',jmbc,
     .                                       ' Undefined value = ',undef
        write(6,*) ' Starting latitude = ',lat0,' longitude =',lon0
        write(6,*) ' Data valid at these times: '
        ndby3 = ndatebc/3
        do n = 1, ndby3*3,3
         write(6,1000) (nn,nymdbc(nn),nhmsbc(nn),nn=n,n+2)
 1000    format(3(2x,i3,':',i8,2x,i8))
        enddo
        write(6,1000) (nn,nymdbc(nn),nhmsbc(nn),nn=ndby3*3+1,ndatebc)
       endif

       if( error ) call my_exit (101)

      endif

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
         call bcdata (iunit,imbc,jmbc,nPgx,nPgy,nd,nd+1,sstbc1,sstbc2)
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
       print *, 'STOP: Could not find SST dates for model time.'
       call my_finalize
       call my_exit (101)
      endif

C---------- Interpolate SST data ------------------------------------

      call interp_time(nymdmod,nhms,nymdbc1,nhmsbc1,nymdbc2,nhmsbc2,
     .                                                        fac1,fac2)

      do j = jm1,jm2
      do i = im1,im2
       sst(i,j,bi,bj) = sstbc1(i,j,biglobal,bjglobal)*fac1 
     .                + sstbc2(i,j,biglobal,bjglobal)*fac2 
      enddo
      enddo
 
      return
      end

      subroutine bcdata (iunit,im,jm,nPx,nPy,nrec1,nrec2,field1,field2)
C************************************************************************
C
C!ROUTINE:      BCDATA
C!DESCRIPTION:  BCDATA reads the data from the file assigned to the
C!              passed unit number and returns data from the two times
C!              surrounding the current model time.  The two record 
C!              postitions are not assumed to be next to each other.
C
C!INPUT PARAMETERS:
C!      im      number of x points
C!      im      number of x points
C!      nPx     number of faces in x-direction
C!      nPy     number of faces in y-direction
C!      nrec1   record number of the time before the model time
C!      nrec2   record number of the time after the model time
C
C!OUTPUT PARAMETERS:
C!      field1(im,jm,nPx,nPy)  data field before the model time
C!      field2(im,jm,nPx,nPy)  data field after the model time
C
C--------------------------------------------------------------------------
      implicit none

      integer iunit,im,jm,nPx,nPy,nrec1,nrec2

      _RL  field1(im,jm,nPx,nPy)
      _RL  field2(im,jm,nPx,nPy)

      integer i,j,n1,n2
      real*4 f1(im,jm,nPx,nPy), f2(im,jm,nPx,nPy)

C--------- Read file -----------------------------------------------
      read(iunit,rec=nrec1) f1
      read(iunit,rec=nrec2) f2

      do n2=1,nPy
      do n1=1,nPx
#ifdef _BYTESWAPIO
      call MDS_BYTESWAPR4( im*jm, f1(1,1,n1,n2))
      call MDS_BYTESWAPR4( im*jm, f2(1,1,n1,n2))
#endif
      do j=1,jm
      do i=1,im
       field1(i,j,n1,n2) = f1(i,j,n1,n2)
       field2(i,j,n1,n2) = f2(i,j,n1,n2)
      enddo
      enddo
      enddo
      enddo

      return
      end
      subroutine bcheader (iunit, ndmax, nrec,
     .           cname, cdscrip, im, jm, npx, npy, lat0, lon0, ndatebc, 
     .           nymdbc, nhmsbc, undef, error)
C************************************************************************
C
C!ROUTINE:     BCHEADER
C!DESCRIPTION: BCHEADER reads the header from a file and returns the info.  
C
C!INPUT PARAMETERS:
C!      iunit    unit number assigned to the data file
C!      ndmax    maximum number of date/times of the data
C!      nrec     record number of the header info (or assume 1??)
C
C!OUTPUT PARAMETERS:
C!      cname         name of the data in the file header
C!      cdscrip	      description of the data in the file header
C!      im            number of x points
C!      jm            number of y points
C!      npx           number of faces (processors) in x-direction
C!      npy           number of faces (processors) in x-direction
C!      lat0          starting latitude for the data grid
C!      lon0          starting longitude for the data grid
C!      ndatebc	      number of date/times of the data in the file
C!      nymdbc(ndmax) array of dates for the data including century
C!      nhmsbc(ndmax) array of times for the data
C!      undef         value for undefined values in the data
C!      error         logical TRUE if dataset problem
C
C--------------------------------------------------------------------------
      implicit none

      integer iunit, ndmax, nrec

      character*8  cname
      character*80 cdscrip
      integer im,jm,npx,npy,ndatebc,nymdbc(ndmax),nhmsbc(ndmax)
      _RL lat0,lon0,undef
      logical error

      integer i
      integer*4 im_32,jm_32,npx_32,npy_32
      integer*4 ndatebc_32,nhmsbc_32(ndmax),nymdbc_32(ndmax)
      real*4 lat0_32,lon0_32,undef_32

C--------- Read file -----------------------------------------------

      read(iunit,rec=nrec,err=500) cname, cdscrip, 
     .     im_32, jm_32, npx_32, npy_32, lat0_32, lon0_32, 
     .     ndatebc_32, undef_32,
     .     (nymdbc_32(i), nhmsbc_32(i), i=1,ndatebc_32)
#ifdef _BYTESWAPIO
      call MDS_BYTESWAPR4( 1, lat0_32)
      call MDS_BYTESWAPR4( 1, lon0_32)
      call MDS_BYTESWAPR4( 1, undef_32)
#endif

      im = im_32
      jm = jm_32
      npx = npx_32
      npy = npy_32
      lat0 = lat0_32
      lon0 = lon0_32
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
