C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_init_veg.F,v 1.25 2012/03/23 20:23:16 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"

      SUBROUTINE FIZHI_INIT_VEG(myThid,vegdata,im,jm,nSx,nSy,Nxg,Nyg,
     & maxtyp,nchp,nchptot,nchpland,lons,lats,surftype,tilefrac,
     & igrd,ityp,chfr,chlt,chlon)
C***********************************************************************
C Subroutine fizhi_init_veg - routine to read in the land surface types,
C      interpolate to the models grid, and set up tile space for use by
C      the land surface model, the albedo calculation and the surface
C      roughness calculation.
C
C INPUT:
C
C myThid   - thread number (processor number)
C vegdata  - Character*40 Vegetation Dataset name
C im       - longitude dimension
C jm       - latitude dimension (number of lat. points)
C nSx      - Number of processors in x-direction
C nSy      - Number of processors in y-direction
C maxtyp   - maximum allowable number of land surface types per grid box
C nchp     - integer per-processor number of tiles in tile space
C lons     - longitude in degrees [im,jm,nSx,nSy]
C lats     - latitude in degrees [im,jm,nSx,nSy]
C
C OUTPUT:
C
C surftype - integer array of land surface types [im,jm,maxtyp,nSx,nSy]
C tilefrac - real array of corresponding land surface type fractions
C            [im,jm,maxtyp,nSx,nSy]
C igrd     - integer array in tile space of grid point number for each
C            tile [nchp,nSx,nSy]
C ityp     - integer array in tile space of land surface type for each
C            tile [nchp,nSx,nSy]
C chfr     - real array in tile space of land surface type fraction for
C            each tile [nchp,nSx,nSy]
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
      IMPLICIT NONE
#include "EEPARAMS.h"

      INTEGER myThid,im,jm,maxtyp,nchp,nSx,nSy,Nxg,Nyg
      INTEGER nchptot(nSx,nSy), nchpland(nSx,nSy)
      INTEGER surftype(im,jm,maxtyp,nSx,nSy)
      INTEGER igrd(nchp,nSx,nSy),ityp(nchp,nSx,nSy)
      _RL tilefrac(im,jm,maxtyp,nSx,nSy)
      _RL lats(im,jm,nSx,nSy), lons(im,jm,nSx,nSy)
      _RL chfr(nchp,nSx,nSy),chlt(nchp,nSx,nSy),chlon(nchp,nSx,nSy)

C-    local variables:
      CHARACTER*40 vegdata
      INTEGER i,j,k,bi,bj

      INTEGER imdata,jmdata,Nxgdata,Nygdata
      INTEGER biglobal,bjglobal
      INTEGER ierr1,kveg
      INTEGER*4 im_32, jm_32, Nxg_32, Nyg_32
      INTEGER*4 iveg_32(im,jm,maxtyp,Nxg,Nyg)
      REAL*4    veg_32(im,jm,maxtyp,Nxg,Nyg)

      WRITE(standardMessageUnit,'(2A)') ' FIZHI_INIT_VEG: ',
     &   'defining surface type and fraction: ----------------------'

#ifdef _BYTESWAPIO
C     using MDS_BYTESSWAP for sequential acces file does not work
      STOP 'ABNORMAL END: S/R FIZHI_INIT_VEG'
#endif
      CALL MDSFINDUNIT( kveg, myThid )
      close(kveg)
      open(kveg,file=vegdata,form='unformatted',access='sequential',
     &                      iostat=ierr1)
      if( ierr1.eq.0 ) then
          rewind(kveg)
          read(kveg)im_32,jm_32,Nxg_32,Nyg_32,iveg_32,veg_32
      else
       print *
       print *, 'Veg Dataset: ',vegdata,' not found!'
       print *
       call exit(101)
      endif
      close(kveg)
#if defined( _BYTESWAPIO ) && defined( ALLOW_MDSIO )
      CALL MDS_BYTESWAPI4(1,im_32)
      CALL MDS_BYTESWAPI4(1,jm_32)
      CALL MDS_BYTESWAPI4(1,nxg_32)
      CALL MDS_BYTESWAPI4(1,nyg_32)
#endif

      IF (myThid.EQ.1) THEN
       imdata = im_32
       jmdata = jm_32
       Nxgdata = Nxg_32
       Nygdata = Nyg_32
       if( (imdata.ne.im) .or. (jmdata.ne.jm) .or.
     &     (Nxgdata.ne.Nxg) .or. (Nygdata.ne.Nyg) ) then
        print *
        print *, 'Veg Data Resolution is Incorrect! '
        print *,' Model Res: ',im,'x',jm,' Data Res: ',imdata,'x',jmdata
        print *,' Model Nxg Nyg: ',Nxg,' ',Nyg,
     &           ' Data Nxg Nyg: ',Nxgdata,' ',Nygdata
        print *
        call exit(102)
       endif
      ENDIF

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        biglobal=bi+(myXGlobalLo-1)/im
        bjglobal=bj+(myYGlobalLo-1)/jm
#if defined( _BYTESWAPIO ) && defined( ALLOW_MDSIO )
        CALL MDS_BYTESWAPR4( im*jm*maxtyp,
     &                       veg_32(1,1,1,biglobal,bjglobal) )
        CALL MDS_BYTESWAPI4( im*jm*maxtyp,
     &                       iveg_32(1,1,1,biglobal,bjglobal) )
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

C     create chip arrays for :
C      igrd :  grid index
C      ityp :  veg. type
C      chfr :  vegetation fraction
C      chlon:  chip longitude
C      chlt :  chip latitude

C     nchpland<=nchptot is the actual number of land chips
      WRITE(standardMessageUnit,'(2A)') ' FIZHI_INIT_VEG: ',
     &   'setting surface Tiles:'

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-       initialise grid index array:
         do i=1,nchp
           igrd(i,bi,bj) = 1
         enddo

C-       land points:
         nchpland(bi,bj) = 0
         do k=1,maxtyp
          do j=1,jm
           do i=1,im
             if(surftype(i,j,k,bi,bj).lt.100 .and.
     &            tilefrac(i,j,k,bi,bj).gt.0.) then
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

C-       ocean points:
         nchptot(bi,bj) = nchpland(bi,bj)
         do k=1,maxtyp
          do j=1,jm
           do i=1,im
             if(surftype(i,j,k,bi,bj).ge.100 .and.
     &            tilefrac(i,j,k,bi,bj).gt.0.) then
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

         WRITE(standardMessageUnit,'(2(A,I4),2(A,I10))') '  bi=', bi,
     &    ', bj=', bj, ', # of Land Tiles=', nchpland(bi,bj),
     &                 ', Total # of Tiles=', nchptot(bi,bj)

       ENDDO
      ENDDO

      WRITE(standardMessageUnit,'(2A)') ' FIZHI_INIT_VEG: done'

      RETURN
      END
