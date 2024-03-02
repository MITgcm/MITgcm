#include "CTRL_OPTIONS.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

      subroutine ctrl_pack( first, mythid )

c     ==================================================================
c     SUBROUTINE ctrl_pack
c     ==================================================================
c
c     o Compress the control vector such that only ocean points are
c       written to file.
c
c     started: Christian Eckert eckert@mit.edu  10-Mar=2000
c
c     changed: Patrick Heimbach heimbach@mit.edu 06-Jun-2000
c              - Transferred some filename declarations
c                from here to namelist in ctrl_init_fixed
c
c              Patrick Heimbach heimbach@mit.edu 16-Jun-2000
c              - single file name convention with or without
c                ALLOW_ECCO_OPTIMIZATION
c
c              G. Gebbie, added open boundary control packing,
c                  gebbie@mit.edu  18 -Mar- 2003
c
c              heimbach@mit.edu totally restructured 28-Oct-2003
c
c     ==================================================================
c     SUBROUTINE ctrl_pack
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "CTRL.h"
# include "CTRL_GENARR.h"
# include "OPTIMCYCLE.h"
# include "CTRL_OBCS.h"
#endif
#ifdef ALLOW_COST
# include "cost.h"
#endif

c     == routine arguments ==
      logical first
      integer mythid

#ifndef EXCLUDE_CTRL_PACK
c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == local variables ==

      logical doglobalread
      logical ladinit
      logical lxxadxx

      integer k, ivar, iarr
      integer cunit
      integer ictrlgrad
      _RL    fcloc

      character*(128) cfile
      integer il, ilDir

      character*(MAX_LEN_FNAM) fname_local(3)

#if ( defined ALLOW_GENARR2D_CONTROL || defined ALLOW_GENTIM2D_CONTROL )
C 9 character limit set by set_(un)pack
      character*(9) mskNameForSetPack
#endif

#ifdef ALLOW_OBCS
      integer iobcs
#endif

c     == end of interface ==

c--   Tiled files are used.
      doglobalread = .false.

c--   Initialise adjoint variables on active files.
      ladinit = .false.

c--   Initialise global buffer index
      nbuffglobal = 0

c--   Initialise local cost function value
      fcloc     = zeroRL

c--   Find ctrlDir (w/o trailing blanks) length
      ilDir = ilnblnk(ctrlDir)

c--   Only the master thread will do I/O.
      _BEGIN_MASTER( mythid )

      if ( first ) then
c     >>> Initialise control vector for optimcycle=0 <<<
          lxxadxx   = .TRUE.
          ictrlgrad = 1
          fcloc     = zeroRL
          write(cfile(1:128),'(4a,i4.4)')
     &         ctrlname(1:9),'_',yctrlid(1:10),
     &         yctrlpospack, optimcycle
          write(standardMessageUnit,*) 'ph-pack: packing ',ctrlname(1:9)
      else
c     >>> Write gradient vector <<<
          lxxadxx   = .FALSE.
          ictrlgrad = 2
#ifdef ALLOW_COST
# ifdef ALLOW_OPENAD
          fcloc     = fc%v
# else
          fcloc     = fc
# endif
#endif
          write(cfile(1:128),'(4a,i4.4)')
     &         costname(1:9),'_',yctrlid(1:10),
     &         yctrlpospack, optimcycle
          write(standardMessageUnit,*) 'ph-pack: packing ',costname(1:9)
       endif

c--   Only Proc 0 will do I/O.
      IF ( myProcId .eq. 0 ) THEN

       call mdsfindunit( cunit, mythid )
       open( cunit, file   = cfile,
     &      status = 'unknown',
     &      form   = 'unformatted',
     &      access  = 'sequential'   )

c--       Header information.
          write(cunit) nvartype
          write(cunit) nvarlength
          write(cunit) yctrlid
          write(cunit) optimCycle
          write(cunit) fcloc
C     place holder of obsolete variable iG
          write(cunit) 1
C     place holder of obsolete variable jG
          write(cunit) 1
          write(cunit) nSx
          write(cunit) nSy
          write(cunit) (nWetcGlobal(k), k=1,Nr)
          write(cunit) (nWetsGlobal(k), k=1,Nr)
          write(cunit) (nWetwGlobal(k), k=1,Nr)
c#ifdef ALLOW_CTRL_WETV
c         write(cunit) (nWetvGlobal(k), k=1,Nr)
c#endif
#ifdef ALLOW_SHELFICE
          write(cunit) (nWetiGlobal(k), k=1,Nr)
c          write(cunit) nWetiGlobal(1)
#endif

#ifdef ALLOW_OBCS
# ifdef ALLOW_OBCSN_CONTROL
          write(cunit) ((nWetobcsnGlo(k,iobcs), k=1,Nr),iobcs= 1,nobcs)
# endif
# ifdef ALLOW_OBCSS_CONTROL
          write(cunit) ((nWetobcssGlo(k,iobcs), k=1,Nr),iobcs= 1,nobcs)
# endif
# ifdef ALLOW_OBCSW_CONTROL
          write(cunit) ((nWetobcswGlo(k,iobcs), k=1,Nr),iobcs= 1,nobcs)
# endif
# ifdef ALLOW_OBCSE_CONTROL
          write(cunit) ((nWetobcseGlo(k,iobcs), k=1,Nr),iobcs= 1,nobcs)
# endif
#endif

          write(cunit) (ncvarindex(ivar), ivar=1,nvartype)
          write(cunit) (ncvarrecs(ivar),  ivar=1,nvartype)
          write(cunit) (ncvarxmax(ivar),  ivar=1,nvartype)
          write(cunit) (ncvarymax(ivar),  ivar=1,nvartype)
          write(cunit) (ncvarnrmax(ivar), ivar=1,nvartype)
          write(cunit) (ncvargrd(ivar),   ivar=1,nvartype)
          write(cunit) (ncvartype(ivar),  ivar=1,nvartype)

#ifdef ALLOW_PACKUNPACK_METHOD2
      ENDIF
      _END_MASTER( mythid )
      _BARRIER
#endif

c     do ivar = 1, maxcvars
      do ivar = 1, nvartype
       if ( ncvarindex(ivar) .GE. 0 ) THEN
        il = ilnblnk(ncvarfname(ivar))
        iarr = ncvarindex(ivar)
        call ctrl_set_fname(ctrlDir(1:ilDir)//ncvarfname(ivar),
     &                      fname_local, mythid )
        if ( ncvargrd(ivar).EQ.'m' ) then
#ifdef ALLOW_OBCS
         if ( iarr.EQ.1 ) then
# ifdef ALLOW_OBCSN_CONTROL
          call ctrl_set_pack_xz( cunit, ivar, fname_local(ictrlgrad),
     &               'maskobcsn', 'wobcsn', wobcsn, lxxadxx, mythid )
# endif
         elseif ( iarr.EQ.2 ) then
# ifdef ALLOW_OBCSS_CONTROL
          call ctrl_set_pack_xz( cunit, ivar, fname_local(ictrlgrad),
     &               'maskobcss', 'wobcss', wobcss, lxxadxx, mythid )
# endif
         elseif ( iarr.EQ.3 ) then
# ifdef ALLOW_OBCSE_CONTROL
          call ctrl_set_pack_yz( cunit, ivar, fname_local(ictrlgrad),
     &               'maskobcse', 'wobcse', wobcse, lxxadxx, mythid )
# endif
         elseif ( iarr.EQ.4 ) then
# ifdef ALLOW_OBCSW_CONTROL
          call ctrl_set_pack_yz( cunit, ivar, fname_local(ictrlgrad),
     &               'maskobcsw', 'wobcsw', wobcsw, lxxadxx, mythid )
# endif
         endif
#endif /* ALLOW_OBCS */

#ifdef ALLOW_GENARR2D_CONTROL
        elseif ( ncvartype(ivar).EQ.'Arr2D' ) then
         if ( xx_genarr2d_weight(iarr).NE.' ') then
          mskNameForSetPack='maskCtrlC'
# ifdef ALLOW_SHELFICE
          if ( ncvargrd(ivar).EQ.'i' ) mskNameForSetPack='maskCtrlI'
# endif
          call ctrl_set_pack_xy(
     &         cunit, ivar, genarr2dPrecond(iarr),
     &         fname_local(ictrlgrad), mskNameForSetPack,
     &         xx_genarr2d_weight(iarr),
     &         lxxadxx, mythid )
         endif
#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
        elseif ( ncvartype(ivar).EQ.'Arr3D' ) then
         if ( xx_genarr3d_weight(iarr).NE.' ') then
          call ctrl_set_pack_xyz(
     &         cunit, ivar, fname_local(ictrlgrad), 'maskCtrlC',
     &         xx_genarr3d_weight(iarr),
     &         wunit, lxxadxx, mythid )
         endif
#endif /* ALLOW_GENARR3D_CONTROL */

#ifdef ALLOW_GENTIM2D_CONTROL
        elseif ( ncvartype(ivar).EQ.'Tim2D' ) then
         if ( xx_gentim2d_weight(iarr).NE.' ') then
          mskNameForSetPack='maskCtrlC'
# ifdef ALLOW_SHELFICE
          if ( ncvargrd(ivar).EQ.'i' ) mskNameForSetPack='maskCtrlI'
# endif
          call ctrl_set_pack_xy(
     &         cunit, ivar, gentim2dPrecond(iarr),
     &         fname_local(ictrlgrad), mskNameForSetPack,
     &         xx_gentim2d_weight(iarr),
     &         lxxadxx, mythid )
         endif
#endif /* ALLOW_GENTIM2D_CONTROL */
        endif
C     if ( ncvarindex(ivar) .ge. 0 ) then
       endif
C     do ivar = 1, maxcvars
      enddo

#ifdef ALLOW_PACKUNPACK_METHOD2
      _BEGIN_MASTER( mythid )
      IF ( myProcId .eq. 0 ) THEN
#endif

       close ( cunit )
       ENDIF !IF ( myProcId .eq. 0 )
       _END_MASTER( mythid )
      _BARRIER
#endif /* EXCLUDE_CTRL_PACK */

      return
      end
