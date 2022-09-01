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
c                from here to namelist in ctrl_init
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

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#ifdef ALLOW_CTRL
# include "ctrl.h"
# include "CTRL_SIZE.h"
# include "CTRL_GENARR.h"
# include "optim.h"
# include "CTRL_OBCS.h"
#endif
#ifdef ALLOW_COST
# include "cost.h"
#endif

c     == routine arguments ==
      logical first
      integer mythid

#ifndef EXCLUDE_CTRL_PACK
c     == local variables ==

      logical doglobalread
      logical ladinit
      logical lxxadxx

      integer i, k
c     integer ig,jg
      integer ivartype
      integer cunit
      integer ictrlgrad
      _RL    fcloc

      character*(128) cfile

#ifdef ALLOW_OBCS
      character*( 80) weighttype
      integer iobcs
      character*(80) fname_obcsn(3)
      character*(80) fname_obcss(3)
      character*(80) fname_obcsw(3)
      character*(80) fname_obcse(3)
#endif
#if (defined ALLOW_GENARR2D_CONTROL) \
      || (defined ALLOW_GENARR3D_CONTROL) \
      || (defined ALLOW_GENTIM2D_CONTROL)
C-    Provided we set the file name just before calling ctrl_set_pack,
C     the same local file name variable can be used for different variables.
C     This is how GENARR2/3D_CONTROL is implemented (+ provides an example)
      integer iarr
      character*(80) fname_local(3)
#endif
#if ( defined ALLOW_GENARR2D_CONTROL || defined ALLOW_GENTIM2D_CONTROL )
C 9 character limit set by set_(un)pack
      character*( 9) mskNameForSetPack
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

c--   Assign file names.

#ifdef ALLOW_OBCS
      call ctrl_set_fname(xx_obcsn_file, fname_obcsn, mythid)
      call ctrl_set_fname(xx_obcss_file, fname_obcss, mythid)
      call ctrl_set_fname(xx_obcsw_file, fname_obcsw, mythid)
      call ctrl_set_fname(xx_obcse_file, fname_obcse, mythid)
#endif

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
          write(cunit) nsx
          write(cunit) nsy
          write(cunit) (nWetcGlobal(k), k=1,nr)
          write(cunit) (nWetsGlobal(k), k=1,nr)
          write(cunit) (nWetwGlobal(k), k=1,nr)
#ifdef ALLOW_CTRL_WETV
          write(cunit) (nWetvGlobal(k), k=1,nr)
#endif
#ifdef ALLOW_SHELFICE
          write(cunit) (nWetiGlobal(k), k=1,nr)
c          write(cunit) nWetiGlobal(1)
#endif

#ifdef ALLOW_OBCS
#ifdef ALLOW_OBCSN_CONTROL
          write(cunit) ((nWetobcsnGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
          write(cunit) ((nWetobcssGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
          write(cunit) ((nWetobcswGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
          write(cunit) ((nWetobcseGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
#endif

          write(cunit) (ncvarindex(i), i=1,maxcvars)
          write(cunit) (ncvarrecs(i),  i=1,maxcvars)
          write(cunit) (ncvarxmax(i),  i=1,maxcvars)
          write(cunit) (ncvarymax(i),  i=1,maxcvars)
          write(cunit) (ncvarnrmax(i), i=1,maxcvars)
          write(cunit) (ncvargrd(i),   i=1,maxcvars)
          write(cunit)

#ifdef ALLOW_PACKUNPACK_METHOD2
      ENDIF
      _END_MASTER( mythid )
      _BARRIER
#endif

#ifdef ALLOW_OBCS
#ifdef ALLOW_OBCSN_CONTROL
          ivartype = 11
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcsn"
          call ctrl_set_pack_xz(
     &         cunit, ivartype, fname_obcsn(ictrlgrad), "maskobcsn",
     &         weighttype, wobcsn, lxxadxx, mythid)
#endif
#ifdef ALLOW_OBCSS_CONTROL
          ivartype = 12
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcss"
          call ctrl_set_pack_xz(
     &         cunit, ivartype, fname_obcss(ictrlgrad), "maskobcss",
     &         weighttype, wobcss, lxxadxx, mythid)
#endif
#ifdef ALLOW_OBCSW_CONTROL
          ivartype = 13
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcsw"
          call ctrl_set_pack_yz(
     &         cunit, ivartype, fname_obcsw(ictrlgrad), "maskobcsw",
     &         weighttype, wobcsw, lxxadxx, mythid)
#endif
#ifdef ALLOW_OBCSE_CONTROL
          ivartype = 14
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcse"
          call ctrl_set_pack_yz(
     &         cunit, ivartype, fname_obcse(ictrlgrad), "maskobcse",
     &         weighttype, wobcse, lxxadxx, mythid)
#endif
#endif /* ALLOW_OBCS */

#ifdef ALLOW_GENARR2D_CONTROL
       do iarr = 1, maxCtrlArr2D
        if (xx_genarr2d_weight(iarr).NE.' ') then
         mskNameForSetPack='maskCtrlC'
# ifdef ALLOW_SHELFICE
         if ((xx_genarr2d_file(iarr)(1:11).eq.'xx_shicoeff').or.
     &       (xx_genarr2d_file(iarr)(1:11).eq.'xx_shicdrag')) then
          mskNameForSetPack='maskCtrlI'
         endif
# endif
          call ctrl_set_fname( xx_genarr2d_file(iarr),
     O                         fname_local, mythid )
          ivartype    = 100+iarr
cc          write(weighttype(1:80),'(80a)') ' '
cc          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, genarr2dPrecond(iarr),
     &         fname_local(ictrlgrad), mskNameForSetPack,
     &         xx_genarr2d_weight(iarr),
     &         lxxadxx, mythid)
        endif
       enddo
#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
       do iarr = 1, maxCtrlArr3D
        if (xx_genarr3d_weight(iarr).NE.' ') then
          call ctrl_set_fname( xx_genarr3d_file(iarr),
     O                         fname_local, mythid )
          ivartype    = 200+iarr
cc          write(weighttype(1:80),'(80a)') ' '
cc          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_local(ictrlgrad), "maskCtrlC",
     &         xx_genarr3d_weight(iarr),
     &         wunit, lxxadxx, mythid)
        endif
       enddo
#endif /* ALLOW_GENARR3D_CONTROL */

#ifdef ALLOW_GENTIM2D_CONTROL
       do iarr = 1, maxCtrlTim2D
        if (xx_gentim2d_weight(iarr).NE.' ') then
         mskNameForSetPack='maskCtrlC'
# ifdef ALLOW_SHELFICE
         if (xx_gentim2d_file(iarr)(1:11).eq.'xx_shifwflx')
     &          mskNameForSetPack='maskCtrlI'
# endif
          call ctrl_set_fname( xx_gentim2d_file(iarr),
     O                         fname_local, mythid )
          ivartype    = 300+iarr
cc          write(weighttype(1:80),'(80a)') ' '
cc          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, gentim2dPrecond(iarr),
     &         fname_local(ictrlgrad), mskNameForSetPack,
     &         xx_gentim2d_weight(iarr),
     &         lxxadxx, mythid)
        endif
       enddo
#endif /* ALLOW_GENTIM2D_CONTROL */

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
