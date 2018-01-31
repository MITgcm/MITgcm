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

       integer myIter, myThid
       _RL myTime

       integer i, j, bi, bj, biglobal, bjglobal
       integer im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2
       integer nSxglobal, nSyglobal
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

      integer i,j

      do j = jm1,jm2
      do i = im1,im2
       sice(i,j,bi,bj) = 0.
      enddo
      enddo

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

      integer i,j

      do j = jm1,jm2
      do i = im1,im2
       sst(i,j,bi,bj) = 280.
      enddo
      enddo
 
      return
      end
