C $Header: /u/gcmpack/MITgcm/verification/fizhi-cs-aqualev20/code/fizhi_init_veg.F,v 1.1 2006/04/03 20:55:14 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"

      subroutine fizhi_init_veg(mythid,vegdata,im,jm,Nsx,Nsy,Nxg,Nyg,
     . maxtyp,nchp,nchptot,nchpland,lons,lats,surftype,tilefrac,igrd,
     . ityp,chfr,chlt,chlon)
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
C im       - longitude dimension
C jm       - latitude dimension (number of lat. points)
C Nsx      - Number of processors in x-direction
C Nsy      - Number of processors in y-direction
C maxtyp   - maximum allowable number of land surface types per grid box
C nchp     - integer per-processor number of tiles in tile space
C lons     - longitude in degrees [im,jm,nSx,nSy]
C lats     - latitude in degrees [im,jm,nSx,nSy]
C
C OUTPUT:
C
C surftype - integer array of land surface types [im,jm,maxtyp,Nsx,Nsy]
C tilefrac - real array of corresponding land surface type fractions 
C            [im,jm,maxtyp,Nsx,Nsy]
C igrd     - integer array in tile space of grid point number for each 
C            tile [nchp,Nsx,Nsy]
C ityp     - integer array in tile space of land surface type for each 
C            tile [nchp,Nsx,Nsy]
C chfr     - real array in tile space of land surface type fraction for 
C            each tile [nchp,Nsx,Nsy]
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

      integer mythid,im,jm,maxtyp,nchp,Nsx,Nsy,Nxg,Nyg
      integer nchptot(Nsx,Nsy),nchpland(Nsx,Nsy)
      integer surftype(im,jm,maxtyp,Nsx,Nsy)
      integer igrd(nchp,Nsx,Nsy),ityp(nchp,Nsx,Nsy)
      _RL tilefrac(im,jm,maxtyp,Nsx,Nsy)
      _RL lats(im,jm,nSx,nSy), lons(im,jm,nSx,nSy)
      _RL chfr(nchp,Nsx,Nsy),chlt(nchp,Nsx,Nsy),chlon(nchp,Nsx,Nsy)
      character*40 vegdata

      integer i,j,k,bi,bj

      DO BJ = myByLo(myThid), myByHi(myThid)
      DO BI = myBxLo(myThid), myBxHi(myThid)

      do j = 1,jm
      do i = 1,im
       surftype(i,j,1,bi,bj) = 100
       tilefrac(i,j,1,bi,bj) = 0.99
       surftype(i,j,2,bi,bj) = 100
       tilefrac(i,j,2,bi,bj) = 0.01
      enddo
      enddo
      do k = 3,maxtyp
      do j = 1,jm
      do i = 1,im
       surftype(i,j,k,bi,bj) = 0
       tilefrac(i,j,k,bi,bj) = 0.
      enddo
      enddo
      enddo

      ENDDO
      ENDDO

c     create chip arrays for :
c      igrd :  grid index
c      ityp :  veg. type
c      chfr :  vegetation fraction
c      chlon:  chip longitude
c      chlt :  chip latitude

c     nchpland<=nchptot is the actual number of land chips

      DO BJ = myByLo(myThid), myByHi(myThid)
        DO BI = myBxLo(myThid), myBxHi(myThid)

c         land points
c         -----------
          nchpland(bi,bj) = 0
          
c         ocean points
c         ------------
          nchptot(bi,bj) = nchpland(bi,bj)
          
          do k=1,maxtyp
            do j=1,jm
              do i=1,im
                if(surftype(i,j,k,bi,bj).ge.100 .and. 
     .               tilefrac(i,j,k,bi,bj).gt.0.) then
                  nchptot(bi,bj)  = nchptot(bi,bj) + 1
                  igrd (nchptot(bi,bj),bi,bj) = i + (j-1)*im
                  ityp (nchptot(bi,bj),bi,bj) = surftype(i,j,k,bi,bj)
                  chfr (nchptot(bi,bj),bi,bj) = tilefrac(i,j,k,bi,bj)
                  chlon(nchptot(bi,bj),bi,bj) = lons(i,j,bi,bj)
                  chlt (nchptot(bi,bj),bi,bj) = lats(i,j,bi,bj)
                endif
              enddo
            enddo
          enddo
          
         print *,'Number of Total Tiles for bi=',bi,': ',nchptot(bi,bj)
         print *,'Number of Land  Tiles for bi=',bi,': ',nchpland(bi,bj)

        ENDDO
      ENDDO

      RETURN
      END
