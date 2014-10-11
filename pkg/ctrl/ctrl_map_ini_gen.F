C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_map_ini_gen.F,v 1.23 2014/10/11 19:04:20 gforget Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

      subroutine ctrl_map_ini_gen3D(xxFileCur, wFileCur, xxDummyCur,
     & boundsVec, paramFld3d, maskFld3d, paramSmooth, mythid )

c     ==================================================================
c     SUBROUTINE ctrl_map_ini_gen3D
c     ==================================================================
c
c     started: Gael Forget gforget@mit.edu 8-Feb-2008
c
c              - Generetic routine for an individual 3D control term
c                (to be called from ctrl_map_ini in a loop e.g.)
c
c     ==================================================================
c     SUBROUTINE ctrl_map_ini_gen3D
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "CTRL_GENARR.h"
# include "ctrl_dummy.h"
# include "optim.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "tamc.h"
# include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF */

c     == routine arguments ==

      integer mythid
      character*(*) wFileCur,xxFileCur
      _RL boundsVec(5),tmpMax,xxDummyCur

      _RL wFld3d(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL xxFld3d(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL paramFld3d(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RS maskFld3d(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      integer paramSmooth

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer il

      logical doglobalread
      logical ladinit

      character*( 80) fnamegeneric

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

#ifdef ALLOW_AUTODIFF_TAMC
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = 0
          ikey = (act3 + 1) + act4*max3
#endif /* ALLOW_AUTODIFF_TAMC */

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
c--   only do interior, and exchange outside
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      doglobalread = .false.
      ladinit      = .false.

      call mdsreadfield(wFileCur,ctrlprec,'RL',nR,wFld3d,1,mythid)
      _EXCH_XYZ_RL( wFld3d, mythid )

      il=ilnblnk( xxFileCur )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xxFileCur(1:il),'.',optimcycle
      call active_read_xyz( fnamegeneric, xxFld3d, 1,
     & doglobalread, ladinit, optimcycle, mythid, xxDummyCur )

      IF ( .NOT.ctrlSmoothCorrel3D ) THEN

c avoid xx larger than boundsVec(5) X uncertainty
      if ( boundsVec(5).GT.0. ) then
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
      if ( (maskFld3d(i,j,k,bi,bj).NE.0.).AND.
     & (wFld3d(i,j,k,bi,bj).GT.0.) ) then
       tmpMax=boundsVec(5)/sqrt(wFld3d(i,j,k,bi,bj))
      if ( abs(xxFld3d(i,j,k,bi,bj)).GT.tmpMax ) then
       xxFld3d(i,j,k,bi,bj)=sign(tmpMax,xxFld3d(i,j,k,bi,bj))
      else
       xxFld3d(i,j,k,bi,bj)=xxFld3d(i,j,k,bi,bj)
      endif
      endif
              enddo
            enddo
          enddo
       enddo
      enddo
      endif

#ifdef ALLOW_SMOOTH
# ifdef ALLOW_SMOOTH_CTRL3D
      if (useSMOOTH) call smooth3D(xxFld3d,paramSmooth,mythid)
      write(fnamegeneric(1:80),'(2a,i10.10)')
     & xxFileCur(1:il),'.smooth.',optimcycle
      call mdswritefield(fnamegeneric,ctrlprec,.FALSE.,'RL',
     & nr, xxFld3d, 1, optimcycle, mythid)
# endif
#endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
      paramFld3d(i,j,k,bi,bj) = paramFld3d(i,j,k,bi,bj)
     & + xxFld3d(i,j,k,bi,bj)
              enddo
            enddo
          enddo
       enddo
      enddo

      ELSE !IF ( .NOT.ctrlSmoothCorrel3D ) THEN

#ifdef ALLOW_SMOOTH
c apply Weaver And Courtier correlation operator
      if ( paramSmooth.NE.0 .AND. useSMOOTH ) then
       call smooth_correl3D(xxFld3d,paramSmooth,mythid)
      endif
#endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
c scale param adjustment
      if ( (maskFld3d(i,j,k,bi,bj).NE.0.)
     & .AND. (wFld3d(i,j,k,bi,bj).GT.0.) ) then
      xxFld3d(i,j,k,bi,bj)=xxFld3d(i,j,k,bi,bj)
     & /sqrt( wFld3d(i,j,k,bi,bj) )
      else
      xxFld3d(i,j,k,bi,bj)=0.
      endif
      paramFld3d(i,j,k,bi,bj) = paramFld3d(i,j,k,bi,bj)
     & + xxFld3d(i,j,k,bi,bj)
              enddo
            enddo
          enddo
       enddo
      enddo

      ENDIF !IF ( .NOT.ctrlSmoothCorrel3D ) THEN

c avoid param out of [boundsVec(1) boundsVec(4)]
      CALL CTRL_BOUND_3D(paramFld3d,maskFld3d,boundsVec,myThid)

      IF ( ctrlSmoothCorrel3D ) THEN
        write(fnamegeneric(1:80),'(2a,i10.10)')
     &    xxFileCur(1:il),'.effective.',optimcycle
        call mdswritefield(fnamegeneric,ctrlprec,.FALSE.,'RL',
     &    nr, paramFld3d, 1, optimcycle, mythid)
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      subroutine ctrl_map_ini_gen2D(xxFileCur, wFileCur, xxDummyCur,
     & boundsVec, paramFld2d, maskFld3d, paramSmooth, mythid )

c     ==================================================================
c     SUBROUTINE ctrl_map_ini_gen2D
c     ==================================================================
c
c     started: Gael Forget gforget@mit.edu 8-Feb-2008
c
c              - Generetic routine for an individual 2D control term
c                (to be called from ctrl_map_ini in a loop e.g.)
c
c     ==================================================================
c     SUBROUTINE ctrl_map_ini_gen3D
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "optim.h"
#endif
#ifdef ALLOW_AUTODIFF
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF */

c     == routine arguments ==

      integer mythid
      character*(*) wFileCur,xxFileCur
      _RL boundsVec(5),tmpMax,xxDummyCur

      _RL wFld2d(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL xxFld2d(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL paramFld2d(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RS maskFld3d(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      integer paramSmooth

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer il

      logical doglobalread
      logical ladinit

      character*( 80) fnamegeneric

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

#ifdef ALLOW_AUTODIFF_TAMC
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = 0
          ikey = (act3 + 1) + act4*max3
#endif /* ALLOW_AUTODIFF_TAMC */

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
c--   only do interior, and exchange outside
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      doglobalread = .false.
      ladinit      = .false.

      call mdsreadfield(wFileCur,ctrlprec,'RL',1,wFld2d,1,mythid)
      _EXCH_XY_RL( wFld2d, mythid )

      il=ilnblnk( xxFileCur )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xxFileCur(1:il),'.',optimcycle
      call active_read_xy( fnamegeneric, xxFld2d, 1,
     & doglobalread, ladinit, optimcycle, mythid, xxDummyCur )

      IF ( .NOT.ctrlSmoothCorrel2D ) THEN

c avoid xx larger than boundsVec(5) X uncertainty
      if ( boundsVec(5).GT.0.) then
      do bj = jtlo,jthi
        do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
      if ( (maskFld3d(i,j,1,bi,bj).NE.0.).AND.
     & (wFld2d(i,j,bi,bj).GT.0.) ) then
       tmpMax=boundsVec(5)/sqrt(wFld2d(i,j,bi,bj))
      if ( abs(xxFld2d(i,j,bi,bj)).GT.tmpMax ) then
       xxFld2d(i,j,bi,bj)=sign(tmpMax,xxFld2d(i,j,bi,bj))
      else
       xxFld2d(i,j,bi,bj)=xxFld2d(i,j,bi,bj)
      endif
      endif
              enddo
            enddo
       enddo
      enddo
      endif

#ifdef ALLOW_SMOOTH
# ifdef ALLOW_SMOOTH_CTRL2D
      if (useSMOOTH) call smooth2d(xxFld2d,maskFld3d,paramSmooth,mythid)
      write(fnamegeneric(1:80),'(2a,i10.10)')
     & xxFileCur(1:il),'.smooth.',optimcycle
      call mdswritefield(fnamegeneric,ctrlprec,.FALSE.,'RL',
     & 1, xxFld2d, 1, optimcycle, mythid)
# endif
#endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
      paramFld2d(i,j,bi,bj) = paramFld2d(i,j,bi,bj)
     & + xxFld2d(i,j,bi,bj)
              enddo
            enddo
       enddo
      enddo

      ELSE !IF ( .NOT.ctrlSmoothCorrel2D ) THEN

#ifdef ALLOW_SMOOTH
c apply Weaver And Courtier correlation operator
      if ( paramSmooth.NE.0 .AND. useSMOOTH ) then
       call smooth_correl2d(xxFld2d,maskFld3d,paramSmooth,mythid)
      endif
#endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
c scale param adjustment
      if ( (maskFld3d(i,j,1,bi,bj).NE.0.)
     & .AND. (wFld2d(i,j,bi,bj).GT.0.) ) then
      xxFld2d(i,j,bi,bj)=xxFld2d(i,j,bi,bj)
     & /sqrt( wFld2d(i,j,bi,bj) )
      else
      xxFld2d(i,j,bi,bj)=0.
      endif
      paramFld2d(i,j,bi,bj) = paramFld2d(i,j,bi,bj)
     & + xxFld2d(i,j,bi,bj)
              enddo
            enddo
       enddo
      enddo

      ENDIF !IF ( .NOT.ctrlSmoothCorrel2D ) THEN

      CALL CTRL_BOUND_2D(paramFld2d,maskFld3d,boundsVec,myThid)

      IF ( ctrlSmoothCorrel2D ) THEN
        write(fnamegeneric(1:80),'(2a,i10.10)')
     &   xxFileCur(1:il),'.effective.',optimcycle
        call mdswritefield(fnamegeneric,ctrlprec,.FALSE.,'RL',
     &   1, paramFld2d, 1, optimcycle, mythid)
      endif

      RETURN
      END
