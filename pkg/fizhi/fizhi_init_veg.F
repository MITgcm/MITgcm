C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_init_veg.F,v 1.19 2005/02/25 18:35:30 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"

      subroutine fizhi_init_veg(mythid,vegdata,im,jm,Nsx,Nsy,Nxg,Nyg,
     . maxtyp,nchp,nchptot,nchpland,lons,lats,surftype,tilefrac,
     . igrd,ityp,chfr,chlt,chlon)
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
      integer nchptot(Nsx,Nsy), nchpland(Nsx,Nsy)
      integer surftype(im,jm,maxtyp,Nsx,Nsy)
      integer igrd(nchp,Nsx,Nsy),ityp(nchp,Nsx,Nsy)
      _RL tilefrac(im,jm,maxtyp,Nsx,Nsy)
      _RL lats(im,jm,nSx,nSy), lons(im,jm,nSx,nSy)
      _RL chfr(nchp,Nsx,Nsy),chlt(nchp,Nsx,Nsy),chlon(nchp,Nsx,Nsy)
      character*40 vegdata
      integer imdata,jmdata,Nxgdata,Nygdata
      integer biglobal,bjglobal

      integer*4 im_32, jm_32, Nxg_32, Nyg_32
      integer*4 iveg_32(im,jm,maxtyp,Nxg,Nyg)
         real*4  veg_32(im,jm,maxtyp,Nxg,Nyg)

      integer i,j,k,bi,bj,ierr1,kveg

#ifdef ALLOW_MNC
      character*(MAX_LEN_FNAM) fizhi_veg_bn
#endif

C     Allow for MDSIO format if someday needed
#ifdef ALLOW_MDSIO
      IF ( .FALSE. ) THEN

      call mdsfindunit( kveg, myThid )
      close(kveg)
      open(kveg,file=vegdata,form='unformatted',access='sequential', 
     .                      iostat=ierr1)
      if( ierr1.eq.0 ) then
          read(kveg)im_32,jm_32,Nxg_32,Nyg_32,IVEG_32,VEG_32
      else
       print *
       print *, 'Veg Dataset: ',vegdata,' not found!'
       print *
       call exit(101)
      endif
      close(kveg)

      IF (myThid.eq.1) THEN
      imdata = im_32
      jmdata = jm_32
      Nxgdata = Nxg_32
      Nygdata = Nyg_32
      if( (imdata.ne.im) .or. (jmdata.ne.jm) .or. 
     .                     (Nxgdata.ne.Nxg) .or. (Nygdata.ne.Nyg) ) then
       print *
       print *, 'Veg Data Resolution is Incorrect! '
       print *,' Model Res: ',im,'x',jm,' Data Res: ',imdata,'x',jmdata
       print *,' Model Nxg Nyg: ',Nxg,' ',Nyg,' Data Nxg Nyg: ',Nxgdata,
     .                    ' ',Nygdata
       print *
       call exit(102)
      ENDIF
      ENDIF

      DO BJ = myByLo(myThid), myByHi(myThid)
      DO BI = myBxLo(myThid), myBxHi(myThid)

      biglobal=bi+(myXGlobalLo-1)/im
      bjglobal=bj+(myYGlobalLo-1)/jm
#if defined( _BYTESWAPIO ) && defined( ALLOW_MDSIO )
      call MDS_BYTESWAPR4(im*jm*maxtyp,veg_32(1,1,1,biglobal,bjglobal))
#endif
      do k = 1,maxtyp
      do j = 1,jm
      do i = 1,im
       surftype(i,j,k,bi,bj) = iveg_32(i,j,k,biglobal,bjglobal) 
       tilefrac(i,j,k,bi,bj) = veg_32(i,j,k,biglobal,bjglobal)
      enddo
      enddo
      enddo

      ENDDO
      ENDDO

      ENDIF
#endif

#ifdef ALLOW_MNC
      IF (useMNC) THEN
        _BEGIN_MASTER( myThid )
      
        do i = 1,MAX_LEN_FNAM
          fizhi_veg_bn(i:i) = ' '
        enddo
      
C       The following base name should be handled by some sort of input
C       name parameter in FIZHI_READPARMS() plus a possible size.
      
C       Set the base name     1234567890
        fizhi_veg_bn(1:10) = 'fizhi_veg '
      
        CALL MNC_CW_I_R('I', fizhi_veg_bn, 0,0, 
     &       'surftype',surftype,myThid)
        CALL MNC_CW_RL_R('R', fizhi_veg_bn, 0,0,
     &       'tilefrac', tilefrac, myThid)
      
        _END_MASTER( myThid )
      ENDIF
#endif

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
          do k=1,maxtyp
            do j=1,jm
              do i=1,im
                if(surftype(i,j,k,bi,bj).lt.100 .and.
     .               tilefrac(i,j,k,bi,bj).gt.0.) then
                  nchpland(bi,bj)  = nchpland(bi,bj) + 1
                  igrd (nchpland(bi,bj),bi,bj) = i + (j-1)*im
                  ityp (nchpland(bi,bj),bi,bj) = surftype(i,j,k,bi,bj)
                  chfr (nchpland(bi,bj),bi,bj) = tilefrac(i,j,k,bi,bj)
                  chlon(nchpland(bi,bj),bi,bj) = lons(i,j,bi,bj)
                  chlt (nchpland(bi,bj),bi,bj) = lats(i,j,bi,bj)
                endif
              enddo
            enddo
          enddo
          
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
