C $Header: /u/gcmpack/MITgcm/pkg/fizhi/update_ocean_exports.F,v 1.33 2010/09/22 22:21:41 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
       subroutine update_ocean_exports (myTime, myIter, myThid)
c----------------------------------------------------------------------
c  Subroutine update_ocean_exports - 'Wrapper' routine to update
c        the fields related to the ocean surface that are needed
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
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */

       integer myIter, myThid
       _RL myTime

       INTEGER xySize
#if defined(ALLOW_EXCH2)
       PARAMETER ( xySize = W2_ioBufferSize )
#else
       PARAMETER ( xySize = Nx*Ny )
#endif
       integer i, j, bi, bj, bislot, bjslot
       integer im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2
       integer xsize, ysize
       _RL        sstmin
       parameter ( sstmin = 273.16 )

       _RL sst1 (xySize), sst2 (xySize)
       _RL sice1(xySize), sice2(xySize)
c      _RL sst1(xsize,ysize),sst2(xsize,ysize)
c      _RL sice1(xsize,ysize),sice2(xsize,ysize)
       integer nymd1sst(nSx,nSy),nymd2sst(nSx,nSy)
       integer nymd1sice(nSx,nSy),nymd2sice(nSx,nSy)
       integer nhms1sst(nSx,nSy),nhms2sst(nSx,nSy)
       integer nhms1sice(nSx,nSy),nhms2sice(nSx,nSy)
       integer sstdates(370,nSx,nSy),sicedates(370,nSx,nSy)
       integer ssttimes(370,nSx,nSy),sicetimes(370,nSx,nSy)
       logical first(nSx,nSy)
       integer nSxnSy
       parameter(nSxnSy = nSx*nSy)
       data first/nSxnSy*.true./

       save nymd1sst,nymd2sst,nymd1sice,nymd2sice
       save nhms1sst,nhms2sst,nhms1sice,nhms2sice
       save sst1, sst2, sice1, sice2
       save sstdates, sicedates
       save ssttimes, sicetimes

#if defined(ALLOW_EXCH2)
       xsize = exch2_global_Nx
       ysize = exch2_global_Ny
#else
       xsize = Nx
       ysize = Ny
#endif
       idim1 = 1-OLx
       idim2 = sNx+OLx
       jdim1 = 1-OLy
       jdim2 = sNy+OLy
       im1 = 1
       im2 = sNx
       jm1 = 1
       jm2 = sNy

C***********************************************************************

       DO BJ = myByLo(myThid),myByHi(myThid)
       DO BI = myBxLo(myThid),myBxHi(myThid)
#if defined(ALLOW_EXCH2)
       bislot = exch2_txglobalo(W2_myTileList(bi,bj))-1
       bjslot = exch2_tyglobalo(W2_myTileList(bi,bj))-1
#else
       bislot = myXGlobalLo-1+(bi-1)*sNx
       bjslot = myYGlobalLo-1+(bj-1)*sNy
#endif

       call getsst(ksst,sstclim,idim1,idim2,jdim1,jdim2,im1,im2,
     .  jm1,jm2,nSx,nSy,xsize,ysize,bi,bj,bislot,bjslot,nymd,nhms,
     .  sst1,sst2,first(bi,bj),nymd1sst(bi,bj),nymd2sst(bi,bj),
     .  nhms1sst(bi,bj),nhms2sst(bi,bj),sstdates(1,bi,bj),
     .  ssttimes(1,bi,bj),sst,myThid)
       call getsice(kice,siceclim,idim1,idim2,jdim1,jdim2,im1,im2,
     .  jm1,jm2,nSx,nSy,xsize,ysize,bi,bj,bislot,bjslot,nymd,nhms,
     .  sice1,sice2,first(bi,bj),nymd1sice(bi,bj),nymd2sice(bi,bj),
     .  nhms1sice(bi,bj),nhms2sice(bi,bj),sicedates(1,bi,bj),
     .  sicetimes(1,bi,bj),sice,myThid)

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
       _EXCH_XY_RL(sst,myThid)
       _EXCH_XY_RL(sice,myThid)

       return
       end

       subroutine getsice(iunit,clim,idim1,idim2,jdim1,jdim2,im1,im2,
     .   jm1,jm2,nSumx,nSumy,xsize,ysize,bi,bj,bislot,bjslot,nymd,nhms,
     .   sicebc1,sicebc2,first,nymdbc1,nymdbc2,nhmsbc1,nhmsbc2,
     .   nymdbc,nhmsbc,sice,mythid)
C***********************************************************************
C
C!ROUTINE: GETSICE
C!DESCRIPTION:  GETSICE returns the sea ice depth.
C!              This routine is adaptable for any frequency
C!              data upto a daily frequency.
C!              note: for diurnal data ndmax should be increased.
C
C!INPUT PARAMETERS:
C!      iunit     Unit number assigned to the sice data file
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
C!      xsize      Number of processors in x-direction (global)
C!      ysize      Number of processors in y-direction (global)
C!      bi        Processor number in x-direction (local to processor)
C!      bj        Processor number in y-direction (local to processor)
C!      bislot  Processor number in x-direction (global)
C!      bjslot  Processor number in y-direction (global)
C!      nymd    YYMMDD of the current model timestep
C!      nhms    HHMMSS of the model time
C
C!OUTPUT PARAMETERS:
C!      sice(idim1:idim2,jdim1:jdim2,nSumx,nSumy) Sea ice depth in meters
C
C!ROUTINES CALLED:
C
C!      bcdata       Reads the data for a given unit number
C!      bcheader     Reads the header info for a given unit number
C!      interp_time  Returns weights for linear interpolation
C
C--------------------------------------------------------------------------

      implicit none
#include "SIZE.h"

      integer iunit,idim1,idim2,jdim1,jdim2,im1,im2,jm1,jm2,nSumx,nSumy
      integer xsize,ysize,bi,bj,bislot,bjslot,nymd,nhms,mythid
      logical clim

      _RL sicebc1(xsize,ysize)
      _RL sicebc2(xsize,ysize)
      _RL sice(idim1:idim2,jdim1:jdim2,nSumx,nSumy)
      integer nhmsbc1,nhmsbc2,nymdbc1,nymdbc2
      logical first

C Maximum number of dates in one year for the data
      integer ndmax
      parameter (ndmax = 370)
      integer nymdbc(ndmax),nhmsbc(ndmax)

      character*8  cname
      character*80 cdscrip
      character*22 sicedata
      _RL fac1, fac2, lat0, lon0, timebc1, timebc2, timemod, undef
      logical found, error
      integer i,j,n,nn,iyear,iyearbc,nd,ndby3,imbc,jmbc
      integer ndatebc,nrec
      integer nymdmod


C--------- Variable Initialization ---------------------------------

      data error /.false./

c  save header info
      save imbc,jmbc,lat0,lon0,ndatebc,undef

c  this only works for between 1950-2050
      if (nymd .lt. 500101) then
        nymdmod = 20000000 + nymd
      else if (nymd .le. 991231) then
        nymdmod = 19000000 + nymd
      else
        nymdmod = nymd
      endif

      iyear   = nymdmod/10000
      if(clim) then
       if(xsize.eq.192)sicedata='sice19232.weekly.clim'
       if(xsize.eq.612)sicedata='sice612102.weekly.clim'
      else
       if(xsize.eq.192)
     .           WRITE(sicedata,'(A,I4)')'sice19232.weekly.y',iyear
       if(xsize.eq.612)
     .           WRITE(sicedata,'(A,I4)')'sice612102.weekly.y',iyear
      endif

c  initialize so that first time through they have values for the check
c  these values make the iyear .ne. iyearbc true anyways for
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

      iyearbc = nymdbc(2)/10000

      if( iyear.ne.iyearbc ) then

       close(iunit)
       open (iunit,file=sicedata,form='unformatted',access='direct',
     .                                         recl=xsize*ysize*4)
       nrec = 1
       call bcheader (iunit, ndmax, nrec,
     .          cname, cdscrip, imbc, jmbc, lat0, lon0,
     .          ndatebc, nymdbc, nhmsbc, undef, error)

C--------- Check data for Compatibility ------------------------------

C Check for correct data in boundary condition file
       if (.not.error .and. cname.ne.'SICE') then
        write(6,*)'Wrong data in SICE boundary condition file => ',cname
        error = .true.
       endif

C Check Horizontal Resolution
       if(.not.error.and.imbc*jmbc.ne.xsize*ysize)then
        write(6,*) ' B.C. Resolution DOES NOT match Model Resolution!'
        write(6,*) ' B.C. Resolution:  ',imbc*jmbc
        write(6,*) 'Model Resolution:  ',xsize*ysize
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
        _BEGIN_MASTER( myThid )
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
        _END_MASTER( myThid )
       endif

      endif

C---------- Read sice data if necessary -------------------------------

      found = .false.
      nd = 2

c  If model time is not within the times of saved sice data
c  from previous call to getsice then read new data

      timemod = dfloat(nymdmod) + dfloat(nhms)   /1000000
      timebc1 = dfloat(nymdbc1) + dfloat(nhmsbc1)/1000000
      timebc2 = dfloat(nymdbc2) + dfloat(nhmsbc2)/1000000

      if (timemod .lt. timebc1 .or. timemod .ge. timebc2) then

       do while (.not.found .and. nd .le. ndatebc)
        timebc2 = dfloat(nymdbc(nd)) + dfloat(nhmsbc(nd))/1000000
        if (timebc2 .gt. timemod) then
         nymdbc1 = nymdbc(nd-1)
         nymdbc2 = nymdbc(nd)
         nhmsbc1 = nhmsbc(nd-1)
         nhmsbc2 = nhmsbc(nd)
         call bcdata (iunit,imbc,jmbc,nd,nd+1,sicebc1,sicebc2)
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
       sice(i,j,bi,bj) = sicebc1(i+bislot,j+bjslot)*fac1
     .                 + sicebc2(i+bislot,j+bjslot)*fac2
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
      subroutine getsst(iunit,clim,idim1,idim2,jdim1,jdim2,im1,im2,
     .   jm1,jm2,nSumx,nSumy,xsize,ysize,bi,bj,bislot,bjslot,nymd,nhms,
     .   sstbc1,sstbc2,first,nymdbc1,nymdbc2,nhmsbc1,nhmsbc2,
     .   nymdbc,nhmsbc,sst,mythid)
C***********************************************************************
C
C!ROUTINE: GETSST
C!DESCRIPTION:  GETSST gets the SST data.
C!              This routine is adaptable for any frequency
C!              data upto a daily frequency.
C!              note: for diurnal data ndmax should be increased.
C
C!INPUT PARAMETERS:
C!      iunit     Unit number assigned to the sice data file
C!      idim1     Start dimension in x-direction
C!      idim2     End dimension in x-direction
C!      jdim1     Start dimension in y-direction
C!      jdim2     End dimension in y-direction
C!      im1       Begin of x-direction span for filling sst
C!      im2       End of x-direction span for filling sst
C!      jm1       Begin of y-direction span for filling sst
C!      jm2       End of y-direction span for filling sst
C!      nSumx     Number of processors in x-direction (local processor)
C!      nSumy     Number of processors in y-direction (local processor)
C!      xsize     x-dimension of global array
C!      ysize     y-dimension of global array
C!      bi        Processor number in x-direction (local to processor)
C!      bj        Processor number in y-direction (local to processor)
C!      bislot    Slot number into global array in x-direction (global)
C!      bjslot    Slot number into global array in y-direction (global)
C!      nymd      YYMMDD of the current model timestep
C!      nhms      HHMMSS of the model time
C
C!OUTPUT PARAMETERS:
C!     sst(idim1:idim2,jdim1:jdim2,nSumx,nSumy) Sea surface temperature (K)
C
C!ROUTINES CALLED:
C
C!     bcdata     Reads the data for a given unit number
C!     bcheader   Reads the header info for a given unit number
C!     interp_time  Returns weights for linear interpolation
C
C--------------------------------------------------------------------------

      implicit none
#include "SIZE.h"

      integer iunit,idim1,idim2,jdim1,jdim2,im1,im2,jm1,jm2,nSumx,nSumy
      integer xsize,ysize,bi,bj,bislot,bjslot,nymd,nhms,mythid
      logical clim

      _RL sstbc1(xsize,ysize)
      _RL sstbc2(xsize,ysize)
      _RL sst(idim1:idim2,jdim1:jdim2,nSumx,nSumy)
      integer nhmsbc1,nhmsbc2,nymdbc1,nymdbc2
      logical first

C Maximum number of dates in one year for the data
      integer ndmax
      parameter (ndmax = 370)
      integer nymdbc(ndmax),nhmsbc(ndmax)

      character*8  cname
      character*80 cdscrip
      character*21 sstdata
      _RL fac1, fac2, lat0, lon0, timebc1, timebc2, timemod, undef
      logical found, error
      integer i,j,n,nn,iyear,iyearbc,nd,ndby3,imbc,jmbc
      integer ndatebc,nrec
      integer nymdmod


C--------- Variable Initialization ---------------------------------

      data error /.false./

c  save header info
      save imbc,jmbc,lat0,lon0,ndatebc,undef

c  this only works for between 1950-2050
      if (nymd .lt. 500101) then
        nymdmod = 20000000 + nymd
      else if (nymd .le. 991231) then
        nymdmod = 19000000 + nymd
      else
        nymdmod = nymd
      endif

      iyear   = nymdmod/10000
      if(clim) then
       if(xsize.eq.192)sstdata='sst19232.weekly.clim'
       if(xsize.eq.612)sstdata='sst612102.weekly.clim'
      else
       if(xsize.eq.192)
     .           WRITE(sstdata,'(A,I4)')'sst19232.weekly.y',iyear
       if(xsize.eq.612)
     .           WRITE(sstdata,'(A,I4)')'sst612102.weekly.y',iyear
      endif

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

      iyearbc = nymdbc(2)/10000

      if( iyear.ne.iyearbc ) then

       close(iunit)
       open (iunit,file=sstdata,form='unformatted',access='direct',
     .                                        recl=xsize*ysize*4)
       nrec = 1
       call bcheader (iunit, ndmax, nrec,
     .          cname, cdscrip, imbc, jmbc, lat0, lon0,
     .          ndatebc, nymdbc, nhmsbc, undef, error)

C--------- Check data for Compatibility

C Check for correct data in boundary condition file
       if (.not.error .and. cname.ne.'SST') then
        write(6,*)'Wrong data in SST boundary condition file => ',cname
        error = .true.
       endif

C Check Horizontal Resolution
       if(.not.error.and.imbc*jmbc.ne.xsize*ysize)then
        write(6,*) ' B.C. Resolution DOES NOT match Model Resolution!'
        write(6,*) ' B.C. Resolution:  ',imbc*jmbc
        write(6,*) 'Model Resolution:  ',xsize*ysize
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
        _BEGIN_MASTER( myThid )
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
        _END_MASTER( myThid )
       endif

       if( error ) call my_exit (101)

      endif

C---------- Read SST data if necessary -------------------------------

      found = .false.
      nd = 2

c  If model time is not within the times of saved sst data
c  from previous call to getsst then read new data

      timemod = dfloat(nymdmod) + dfloat(nhms)   /1000000
      timebc1 = dfloat(nymdbc1) + dfloat(nhmsbc1)/1000000
      timebc2 = dfloat(nymdbc2) + dfloat(nhmsbc2)/1000000

      if (timemod .lt. timebc1 .or. timemod .ge. timebc2) then

       do while (.not.found .and. nd .le. ndatebc)
        timebc2 = dfloat(nymdbc(nd)) + dfloat(nhmsbc(nd))/1000000
        if (timebc2 .gt. timemod) then
         nymdbc1 = nymdbc(nd-1)
         nymdbc2 = nymdbc(nd)
         nhmsbc1 = nhmsbc(nd-1)
         nhmsbc2 = nhmsbc(nd)
         call bcdata (iunit,imbc,jmbc,nd,nd+1,sstbc1,sstbc2)
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
       sst(i,j,bi,bj) = sstbc1(i+bislot,j+bjslot)*fac1
     .                + sstbc2(i+bislot,j+bjslot)*fac2
      enddo
      enddo


      return
      end

      subroutine bcdata (iunit,im,jm,nrec1,nrec2,field1,field2)
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
C!      nrec1   record number of the time before the model time
C!      nrec2   record number of the time after the model time
C
C!OUTPUT PARAMETERS:
C!      field1(im,jm)  data field before the model time
C!      field2(im,jm)  data field after the model time
C
C--------------------------------------------------------------------------
      implicit none

      integer iunit,im,jm,nrec1,nrec2

      _RL  field1(im,jm)
      _RL  field2(im,jm)

      integer i,j
      real*4 f1(im,jm), f2(im,jm)

C--------- Read file -----------------------------------------------
      read(iunit,rec=nrec1) f1
      read(iunit,rec=nrec2) f2

#ifdef _BYTESWAPIO
      call MDS_BYTESWAPR4( im*jm, f1)
      call MDS_BYTESWAPR4( im*jm, f2)
#endif
      do j=1,jm
      do i=1,im
       field1(i,j) = f1(i,j)
       field2(i,j) = f2(i,j)
      enddo
      enddo

      return
      end
      subroutine bcheader (iunit, ndmax, nrec,
     .           cname, cdscrip, im, jm, lat0, lon0, ndatebc,
     .           nymdbc, nhmsbc, undef, error)
C************************************************************************
C
C!ROUTINE:     BCHEADER
C!DESCRIPTION: BCHEADER reads the header from a file and returns the info.
C
C!INPUT PARAMETERS:
C!      iunit    unit number assigned to the data file
C!      ndmax    maximum number of date/times of the data
C!      nrec     record number of the header info (or assume 1 ?)
C
C!OUTPUT PARAMETERS:
C!      cname         name of the data in the file header
C!      cdscrip       description of the data in the file header
C!      im            number of x points
C!      jm            number of y points
C!      lat0          starting latitude for the data grid
C!      lon0          starting longitude for the data grid
C!      ndatebc       number of date/times of the data in the file
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
      character*112 dummy112
      integer im,jm,ndatebc,nymdbc(ndmax),nhmsbc(ndmax)
      _RL lat0,lon0,undef
      logical error

      integer i
      integer*4 im_32,jm_32
      integer*4 ndatebc_32,nhmsbc_32(ndmax),nymdbc_32(ndmax)
      real*4 lat0_32,lon0_32,undef_32

C--------- Read file -----------------------------------------------

      read(iunit,rec=nrec,err=500) cname, cdscrip,
     .     im_32, jm_32, lat0_32, lon0_32,
     .     ndatebc_32, undef_32

#ifdef _BYTESWAPIO
      call MDS_BYTESWAPI4( 1, im_32)
      call MDS_BYTESWAPI4( 1, jm_32)
      call MDS_BYTESWAPR4( 1, lat0_32)
      call MDS_BYTESWAPR4( 1, lon0_32)
      call MDS_BYTESWAPI4( 1, ndatebc_32)
      call MDS_BYTESWAPR4( 1, undef_32)
#endif

      read(iunit,rec=nrec,err=500) dummy112,
     .     (nymdbc_32(i), nhmsbc_32(i), i=1,ndatebc_32)

      im = im_32
      jm = jm_32
      lat0 = lat0_32
      lon0 = lon0_32
      undef = undef_32

      ndatebc = ndatebc_32
      do i=1,ndatebc
#ifdef _BYTESWAPIO
      call MDS_BYTESWAPI4( 1, nymdbc_32(i))
      call MDS_BYTESWAPI4( 1, nhmsbc_32(i))
#endif
      nymdbc(i) = nymdbc_32(i)
      nhmsbc(i) = nhmsbc_32(i)
      enddo

      return
  500 continue
      print *, 'Error reading boundary condition from unit ',iunit
      error = .true.
      return
      end
