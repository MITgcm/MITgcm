C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_init_veg.F,v 1.1 2004/06/04 20:19:26 molod Exp $
C $Name:  $

      subroutine fizhi_init_veg ( mythid, im,jm,vegdata,
     .                    maxtyp,nchp,outlons,outlats,
     .                    flwi,nchplnd,surftype,tilefrac,
     .                    igrd,ityp,chfr )
C***********************************************************************
C Subroutine mkwrld - routine to read in the land surface types,
C                     interpolate to the models grid,
C                     and set up tile space for use by the
C                     land surface model and the albedo calculation
C                     and the surface roughness calculation.
C
C INPUT:
C 
C mythid   - thread number (processor number)
C im       - model grid longitude dimension
C jm       - model grid latitude dimension (number of lat. points)
C vegdata  - Character*40 Vegetation Dataset name
C maxtyp   - maximum allowable number of land surface types per grid box
C nchp     - integer actual number of tiles in tile space
C outlons  - longitude value in degrees at model grid points
C outlats  - latitude value in degrees at model grid points
C flwi     - land-water mask from the old boundary conditions dataset
C            (for the temporary!! compatibility check)
C
C OUTPUT:
C
C surftype - integer array of land surface types [im,jm,maxtyp]
C tilefrac - real array of corresponding land surface type fractions 
C            [im,jm,maxtyp]
C igrd     - integer array in tile space of grid point number for each 
C            tile [nchp]
C ityp     - integer array in tile space of land surface type for each 
C            tile [nchp]
C chfr     - real array in tile space of land surface type fraction for 
C            each tile [nchp]
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

      integer exist
      real zero,one
      parameter (zero = 0.)
      parameter (one = 1.)
      parameter (exist=11)

      integer mythid,im,imdata,jm,jmdata,maxtyp,nchp,nchplnd
      real chfr(1)
      integer surftype(im,jm,maxtyp),igrd(1),ityp(1)
      real tilefrac(im,jm,maxtyp)
      real outlons(im,jm),outlats(im,jm),flwi(im,jm)
      character*40 vegdata
      character*40 veg1x1
      data veg1x1 /'veg360181.data'/

      integer*4 im_32, jm_32
      integer*4 iveg_32(360*181*10)
         real*4  veg_32(360*181*10)
         real    vegf  (360*181*10)
      integer   ivegt  (360*181*10)

      real locfracin(360,181,exist),locfracout(im,jm,exist)
      real inlons(360*181),inlats(360*181)
      integer i,ii,iii,iindex,j,jindex,k,kk,kkk,kbig,numtypes
      real fract(maxtyp),undef,found,biggest
      integer stype(maxtyp), ierr1, ierr2
      logical okay,ordered
      integer whichtypes(exist)
      integer kveg

      real dummy(im,jm)

      real getcon
      undef = getcon('UNDEF')

      IF (myThid.eq.1) THEN
      call mdsfindunit( kveg, myThid )
      close (kveg)
      open  (kveg, file=vegdata, form='unformatted', access='sequential', iostat=ierr1)
      if( ierr1.eq.0 ) then
          read  (kveg) im_32,jm_32, (IVEG_32(i),i=1,im_32*jm_32*maxtyp),
     .                               (VEG_32(i),i=1,im_32*jm_32*maxtyp)
      else
       print *
       print *, 'Veg Dataset: ',vegdata,' not found!'
       print *
       call exit (101)
      endif
      close (kveg)
      ENDIF

      imdata = im_32
      jmdata = jm_32
      do i = 1,im_32*jm_32*maxtyp
      ivegt(i) = iveg_32(i)
       vegf(i) =  veg_32(i)
      enddo

      do iii = 1,im*jm*maxtyp
       tilefrac(iii,1,1) = vegf(iii)
       surftype(iii,1,1) = ivegt(iii)
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
      if(surftype(i,j,k).lt.100 .and. tilefrac(i,j,k).gt.0.) then
             nchplnd  = nchplnd + 1
       igrd (nchplnd) = i + (j-1)*im
       ityp (nchplnd) = surftype(i,j,k)
       chfr (nchplnd) = tilefrac(i,j,k)
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
      if(surftype(i,j,k).ge.100 .and. tilefrac(i,j,k).gt.0. ) then
             nchp  = nchp + 1
       igrd (nchp) = i + (j-1)*im
       ityp (nchp) = surftype(i,j,k)
       chfr (nchp) = tilefrac(i,j,k)
      endif
      enddo
      enddo
      enddo

      print *, 'Number of Total Tiles: ',nchp
      print *, 'Number of Land  Tiles: ',nchplnd
      print *

      RETURN
      END
