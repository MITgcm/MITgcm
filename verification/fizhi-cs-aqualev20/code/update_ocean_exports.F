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
C!DESCRIPTION: GETSICE returns the sea ice depth.
C!             This routine is adaptable for any frequency
C!             data upto a daily frequency.
C!             note: for diurnal data ndmax should be increased.
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
C!      nymd   YYMMDD of the current model timestep
C!      nhms   HHMMSS of the model time
C
C!OUTPUT PARAMETERS:
C!      sice(idim1:idim2,jdim1:jdim2,nSumx,nSumy) Sea ice depth in meters
C
C!ROUTINES CALLED:
C
C!      bcdata      Reads the data for a given unit number
C!      bcheader     Reads the header info for a given unit number
C!      interp_time  Returns weights for linear interpolation
C
C--------------------------------------------------------------------------

      implicit none
#include "SIZE.h"
#include "GRID.h"

      integer iunit,idim1,idim2,jdim1,jdim2,im1,im2,jm1,jm2,nSumx,nSumy
      integer xsize,ysize,bi,bj,bislot,bjslot,nymd,nhms,mythid

      _RL sicebc1(xsize,ysize)
      _RL sicebc2(xsize,ysize)
      _RL sice(idim1:idim2,jdim1:jdim2,nSumx,nSumy)
      integer nhmsbc1,nhmsbc2,nymdbc1,nymdbc2
      logical first
      logical clim

C Maximum number of dates in one year for the data
      integer   ndmax
      parameter (ndmax = 370)
      integer nymdbc(ndmax),nhmsbc(ndmax)

      integer i,j

      do j = jm1,jm2
      do i = im1,im2
       sice(i,j,bi,bj) = 0.
      enddo
      enddo

      return
      end
      subroutine getsst(iunit,clim,idim1,idim2,jdim1,jdim2,im1,im2,
     .   jm1,jm2,nSumx,nSumy,xsize,ysize,bi,bj,bislot,bjslot,nymd,nhms,
     .   sstbc1,sstbc2,first,nymdbc1,nymdbc2,nhmsbc1,nhmsbc2,
     .   nymdbc,nhmsbc,sst,mythid)
C***********************************************************************
C
C!ROUTINE: GETSST
C!DESCRIPTION: GETSST gets the SST data.
C!             This routine is adaptable for any frequency
C!             data upto a daily frequency.
C!             note: for diurnal data ndmax should be increased.
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
C!     bcdata      Reads the data for a given unit number
C!     bcheader     Reads the header info for a given unit number
C!     interp_time      Returns weights for linear interpolation
C
C--------------------------------------------------------------------------

      implicit none
#include "SIZE.h"
#include "GRID.h"

      integer iunit,idim1,idim2,jdim1,jdim2,im1,im2,jm1,jm2,nSumx,nSumy
      integer xsize,ysize,bi,bj,bislot,bjslot,nymd,nhms,mythid

      _RL sstbc1(xsize,ysize)
      _RL sstbc2(xsize,ysize)
      _RL sst(idim1:idim2,jdim1:jdim2,nSumx,nSumy)
      integer nhmsbc1,nhmsbc2,nymdbc1,nymdbc2
      logical first
      logical clim

C Maximum number of dates in one year for the data
      integer   ndmax
      parameter (ndmax = 370)
      integer nymdbc(ndmax),nhmsbc(ndmax)

      _RL getcon
      _RL pi,pio2,pio3,mpio3,pio36,deg2rad,sinarg 
c     _RL factor,cosarg1,cosarg2

      integer i,j

      deg2rad = getcon('DEG2RAD')
      pi = getcon('PI')
      pio2 = pi / 2. _d 0
      pio3 = pi / 3. _d 0
      pio36 = pi / 36. _d 0
      mpio3 = -1. _d 0 * pi / 3. _d 0

      do j = jm1,jm2
      do i = im1,im2
C Control - max sst on equator, zonally symmetric
       if( abs(yc(i,j,bi,bj)*deg2rad)  .lt. pio3 ) then
        sinarg = 3.*yc(i,j,bi,bj)*deg2rad/2.
        sst(i,j,bi,bj) = 273.16 + 27.*(1.-(sin(sinarg)*sin(sinarg)))
       else
        sst(i,j,bi,bj) = 273.16
       endif
C Experiment 2 - Peaked
C      if( abs(yc(i,j,bi,bj)*deg2rad)  .lt. pio3 ) then
C       factor = 3.*abs(yc(i,j,bi,bj))*deg2rad/pi
C       sst(i,j,bi,bj) = 273.16 + 27.*(1.-factor)
C      else
C       sst(i,j,bi,bj) = 273.16
C      endif
C Experiment 3 - Flat
C      if( abs(yc(i,j,bi,bj)*deg2rad)  .lt. pio3 ) then
C       sinarg = 3.*yc(i,j,bi,bj)*deg2rad/2.
C       sst(i,j,bi,bj) = 273.16 +
C    .        27.*(1.-(sin(sinarg)*sin(sinarg)*sin(sinarg)*sin(sinarg)))
C      else
C       sst(i,j,bi,bj) = 273.16
C      endif
C Experiment 4 - Qobs - average of control and exp 3
C      if( abs(yc(i,j,bi,bj)*deg2rad)  .lt. pio3 ) then
C       sinarg = 3.*yc(i,j,bi,bj)*deg2rad/2.
C       sst(i,j,bi,bj) = 273.16 + 0.5*27*
C    .   (2.- (sin(sinarg)*sin(sinarg)) -
C    .        (sin(sinarg)*sin(sinarg)*sin(sinarg)*sin(sinarg)))
C      else
C       sst(i,j,bi,bj) = 273.16
C      endif
C Experiment 5 - max sst at 5N, zonally symmetric
C      if( (yc(i,j,bi,bj)*deg2rad  .lt. pio3 ) .and.
C    .     (yc(i,j,bi,bj)*deg2rad  .gt. pio36 ) ) then
C       sinarg = (90./55.)*(yc(i,j,bi,bj)*deg2rad-pio36)
C       sst(i,j,bi,bj) = 273.16 + 27.*(1.-(sin(sinarg)*sin(sinarg)))
C      elseif ( (yc(i,j,bi,bj)*deg2rad  .le. pio36 ) .and.
C    .     (yc(i,j,bi,bj)*deg2rad  .gt. mpio3 ) ) then
C       sinarg = (90./65.)*(yc(i,j,bi,bj)*deg2rad-pio36)
C       sst(i,j,bi,bj) = 273.16 + 27.*(1.-(sin(sinarg)*sin(sinarg)))
C      else
C       sst(i,j,bi,bj) = 273.16
C      endif
C Experiment 6 - 1KEQ max sst at equator, + anomaly centered at greenwich
C   first set the control sst profile
C      if( abs(yc(i,j,bi,bj)*deg2rad)  .lt. pio3 ) then
C       sinarg = 3.*yc(i,j,bi,bj)*deg2rad/2.
C       sst(i,j,bi,bj) = 273.16 + 27.*(1.-(sin(sinarg)*sin(sinarg)))
C      else
C       sst(i,j,bi,bj) = 273.16
C      endif
C   and now add the anomaly
C      if( (abs(yc(i,j,bi,bj)) .lt. 15. _d 0) .and.
C    .     (abs(xc(i,j,bi,bj)) .lt. 30. _d 0) ) then
C       cosarg1 = pio2*(xc(i,j,bi,bj)/30. _d 0)
C       cosarg2 = pio2*(yc(i,j,bi,bj)/15. _d 0)
C       sst(i,j,bi,bj) = sst(i,j,bi,bj) +
C    .     cos(cosarg1)*cos(cosarg1)*cos(cosarg2)*cos(cosarg2)
C      endif
C Experiment 7 - 3KEQ max sst at equator, + anomaly centered at greenwich
C   first set the control sst profile
C      if( abs(yc(i,j,bi,bj)*deg2rad)  .lt. pio3 ) then
C       sinarg = 3.*yc(i,j,bi,bj)*deg2rad/2.
C       sst(i,j,bi,bj) = 273.16 + 27.*(1.-(sin(sinarg)*sin(sinarg)))
C      else
C       sst(i,j,bi,bj) = 273.16
C      endif
C   and now add the anomaly
C      if( (abs(yc(i,j,bi,bj)) .lt. 15. _d 0) .and.
C    .     (abs(xc(i,j,bi,bj)) .lt. 30. _d 0) ) then
C       cosarg1 = pio2*(xc(i,j,bi,bj)/30. _d 0)
C       cosarg2 = pio2*(yc(i,j,bi,bj)/15. _d 0)
C       sst(i,j,bi,bj) = sst(i,j,bi,bj) +
C    .     3.*cos(cosarg1)*cos(cosarg1)*cos(cosarg2)*cos(cosarg2)
C      endif
C Experiment 8 - 3KW1 max sst at equator, +/- anomaly centered at greenwich
C   first set the control sst profile
C      if( abs(yc(i,j,bi,bj)*deg2rad)  .lt. pio3 ) then
C       sinarg = 3.*yc(i,j,bi,bj)*deg2rad/2.
C       sst(i,j,bi,bj) = 273.16 + 27.*(1.-(sin(sinarg)*sin(sinarg)))
C      else
C       sst(i,j,bi,bj) = 273.16
C      endif
C   and now add the anomaly
C      if( abs(yc(i,j,bi,bj))  .lt. 30.0 _d 0 ) then
C       cosarg1 = (xc(i,j,bi,bj))*deg2rad
C       cosarg2 = pio2*(yc(i,j,bi,bj)/30.0 _d 0)
C       sst(i,j,bi,bj) = sst(i,j,bi,bj) +
C    .             3.*cos(cosarg1)*cos(cosarg2)*cos(cosarg2)
C      endif
      enddo
      enddo

      return
      end
