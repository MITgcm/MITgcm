C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_init_veg.F,v 1.2 2004/06/07 16:04:17 molod Exp $
C $Name:  $

      subroutine fizhi_init_veg ( mythid, vegdata,im,jm,bi,bj,maxtyp,
     . surftype,tilefrac,igrd,ityp,chfr )
C***********************************************************************
C Subroutine fizhi_init_veg - routine to read in the land surface types,
C      interpolate to the models grid, and set up tile space for use by 
C      the land surface model, the albedo calculation and the surface 
C      roughness calculation.
C
C INPUT:
C 
C mythid   - thread number (processor number)
C vegdata  - Character*40 Vegetation Dataset name
C im       - model grid longitude dimension
C jm       - model grid latitude dimension (number of lat. points)
C bi       - Number of processors in x-direction
C bj       - Number of processors in y-direction
C maxtyp   - maximum allowable number of land surface types per grid box
C nchpmax  - integer maximum per-processor number of tiles in tile space
C
C OUTPUT:
C
C surftype - integer array of land surface types [im,jm,maxtyp,bi,bj]
C tilefrac - real array of corresponding land surface type fractions 
C            [im,jm,maxtyp,bi,bj]
C igrd     - integer array in tile space of grid point number for each 
C            tile [nchp,bi,bj]
C ityp     - integer array in tile space of land surface type for each 
C            tile [nchp,bi,bj]
C chfr     - real array in tile space of land surface type fraction for 
C            each tile [nchp,bi,bj]
C
C NOTES:
C       Vegetation type as follows:
C                  1:  BROADLEAF EVERGREEN TREES
C                  2:  BROADLEAF DECIDUOUS TREES
C                  3:  NEEDLELEAF TREES
C                  4:  GROUND COVER
C                  5:  BROADLEAF SHRUBS
C                  6:  DWARF TREES (TUNDRA)
C                  7:  BARE SOIL
C                  8:  DESERT    
C                  9:  GLACIER
C                 10:  DARK DESERT
C                100:  OCEAN
C***********************************************************************
      implicit none
#include "EEPARAMS.h"

      integer mythid,im,jm,maxtyp,nchpmax
      integer surftype(im,jm,maxtyp,bi,bj)
      integer igrd(nchpmax,bi,bj),ityp(nchpmax,bi,bj)
      real tilefrac(im,jm,maxtyp,bi,bj)
      real chfr(nchpmax,bi,bj)
      character*40 vegdata
      integer imdata,jmdata,bidata,bjdata
      integer nchp,nchpland

      integer*4 im_32, jm_32, bi_32, bj_32
      integer*4 iveg_32(im,jm,maxtyp,bi,bj)
         real*4  veg_32(im,jm,maxtyp,bi,bj)

      integer i,j,k,bilocal,bjlocal,ierr1,kveg

      IF (myThid.eq.1) THEN
      call mdsfindunit( kveg, myThid )
      close(kveg)
      open(kveg,file=vegdata,form='unformatted',access='sequential', 
     .                      iostat=ierr1)
      if( ierr1.eq.0 ) then
          read(kveg)im_32,jm_32,bi_32,bj_32,IVEG_32,VEG_32
      else
       print *
       print *, 'Veg Dataset: ',vegdata,' not found!'
       print *
       call exit(101)
      endif
      close(kveg)
      imdata = im_32
      jmdata = jm_32
      bidata = bi_32
      bjdata = bj_32
      if( (imdata.ne.im) .or. (jmdata.ne.jm) .or. 
     .                        (bi.ne.bidata) .or. (bjdata.ne.bj) ) then
       print *
       print *, 'Veg Data Resolution is Incorrect! '
       print *,' Model Res: ',im,'x',jm,' Data Res: ',imdata,'x',jmdata
       print *,' Model Bij: ',bi,'x',bj,' Data Bij: ',bidata,'x',bjdata
       print *
       call exit(102)
      ENDIF
                                                                                 
      imdata = im_32
      jmdata = jm_32
      bidata = bi_32
      bjdata = bj_32

      DO BJLOCAL = myByLo(myThid), myByHi(myThid)
      DO BILOCAL = myBxLo(myThid), myBxHi(myThid)
                                                                                 
      do k = 1,maxtyp
      do j = 1,jm_32
      do i = 1,im_32
       surftype(i,j,k,bilocal,bjlocal) = iveg_32(i,j,k,bilocal,bjlocal) 
       tilefrac(i,j,k,bilocal,bjlocal) = veg_32(i,j,k,bilocal,bjlocal)
      enddo
      enddo
      enddo

c   create chip arrays for :
c      igrd :  grid index
c      ityp :  veg. type
c      chfr :  vegetation fraction

c  nchplnd<=nchp is the actual number of land chips

c land points
c -----------
      nchplnd = 0
      do k=1,maxtyp
      do j=1,jm
      do i=1,im
      if(surftype(i,j,k,bi,bj).lt.100.and.
     .                                  tilefrac(i,j,k,bi,bj).gt.0.)then
             nchplnd  = nchplnd + 1
       igrd (nchplnd,bi,bj) = i + (j-1)*im
       ityp (nchplnd,bi,bj) = surftype(i,j,k,bi,bj)
       chfr (nchplnd,bi,bj) = tilefrac(i,j,k,bi,bj)
      endif
      enddo
      enddo
      enddo

c ocean points
c ------------
      nchp = nchplnd

      do k=1,maxtyp
      do j=1,jm
      do i=1,im
      if(surftype(i,j,k,bi,bj).ge.100 .and. 
     .                                  tilefrac(i,j,k,bi,bj).gt.0.)then
             nchp  = nchp + 1
       igrd (nchp,bi,bj) = i + (j-1)*im
       ityp (nchp,bi,bj) = surftype(i,j,k,bi,bj)
       chfr (nchp,bi,bj) = tilefrac(i,j,k,bi,bj)
      endif
      enddo
      enddo
      enddo

      print *, 'bi ',bilocal,' bj ',bjlocal
      print *, 'Number of Total Tiles: ',nchp
      print *, 'Number of Land  Tiles: ',nchplnd
      print *

      ENDDO
      ENDDO

      RETURN
      END
