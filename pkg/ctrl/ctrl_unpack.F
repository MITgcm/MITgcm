#include "CTRL_OPTIONS.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

      subroutine ctrl_unpack( first, mythid )

c     ==================================================================
c     SUBROUTINE ctrl_unpack
c     ==================================================================
c
c     o Unpack the control vector such that the land points are filled
c       in.
c
c     started: Christian Eckert eckert@mit.edu  10-Mar-2000
c
c     changed: Patrick Heimbach heimbach@mit.edu 06-Jun-2000
c              - Transferred some filename declarations
c                from here to namelist in ctrl_init
c
c              Patrick Heimbach heimbach@mit.edu 16-Jun-2000
c              - single file name convention with or without
c                ALLOW_ECCO_OPTIMIZATION
C
c              Armin Koehl akoehl@ucsd.edu 05-Dec-2000
c              - single processor reads global parameter file
c               and writes multiple xx* and adxx* files
c
c              G Gebbie gebbie@mit.edu 18-Mar-2003
c              - open boundary packing
c
c              heimbach@mit.edu totally restructured 28-Oct-2003
c
c     ==================================================================
c     SUBROUTINE ctrl_unpack
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#ifdef ALLOW_CTRL
# include "CTRL.h"
# include "CTRL_SIZE.h"
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

      integer i, k
      integer ivartype
      integer ictrlgrad
      integer cunit
      logical lxxadxx
      character*(128) cfile
      integer ilDir

#if (defined ALLOW_GENARR2D_CONTROL) \
      || (defined ALLOW_GENARR3D_CONTROL) \
      || (defined ALLOW_GENTIM2D_CONTROL)
C-    Provided we set the file name just before calling ctrl_set_unpack,
C     the same local file name variable can be used for different variables.
C     This is how GENARR2/3D_CONTROL is implemented (+ provides an example)
      integer iarr
      character*(MAX_LEN_FNAM) fname_local(3)
#endif
#if ( defined ALLOW_GENARR2D_CONTROL || defined ALLOW_GENTIM2D_CONTROL )
      integer nwettmp(Nr)
      character*( 9) mskNameForSetUnpack
#endif

#ifdef ALLOW_OBCS
      character*(MAX_LEN_FNAM) weighttype
      character*(MAX_LEN_FNAM) fname_obcsn(3)
      character*(MAX_LEN_FNAM) fname_obcss(3)
      character*(MAX_LEN_FNAM) fname_obcsw(3)
      character*(MAX_LEN_FNAM) fname_obcse(3)
cgg(  Add OBCS mask names.
#ifdef ALLOW_OBCSN_CONTROL
      integer        filenWetobcsnGlo(Nr,nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
      integer        filenWetobcssGlo(Nr,nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
      integer        filenWetobcswGlo(Nr,nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
      integer        filenWetobcseGlo(Nr,nobcs)
#endif
      integer iobcs
cgg)
#endif /* ALLOW_OBCS */

c     == end of interface ==

c--   Initialise
      nbuffGlobal = 0

c--   Find ctrlDir (w/o trailing blanks) length
      ilDir = ilnblnk(ctrlDir)

c--   Assign file names.

#ifdef ALLOW_OBCS
      call ctrl_set_fname(ctrlDir(1:ilDir)//xx_obcsn_file,
     & fname_obcsn, mythid)
      call ctrl_set_fname(ctrlDir(1:ilDir)//xx_obcss_file,
     & fname_obcss, mythid)
      call ctrl_set_fname(ctrlDir(1:ilDir)//xx_obcsw_file,
     & fname_obcsw, mythid)
      call ctrl_set_fname(ctrlDir(1:ilDir)//xx_obcse_file,
     & fname_obcse, mythid)
#endif

c--     Only the master thread will do I/O.
        _BEGIN_MASTER( mythid )

c *********************************************************************

      if ( first ) then
c     >>> Initialise control vector for optimcycle=0 <<<
          lxxadxx   = .TRUE.
          ictrlgrad = 1
          write(cfile(1:128),'(4a,i4.4)')
     &         ctrlname(1:9),'_',yctrlid(1:10),
     &         yctrlposunpack(1:4), optimcycle
          write(standardMessageUnit,*) 'ph-pack: unpacking ',
     &         ctrlname(1:9)
      else
c     >>> Write gradient vector <<<
          lxxadxx   = .FALSE.
          ictrlgrad = 2
          write(cfile(1:128),'(4a,i4.4)')
     &         costname(1:9),'_',yctrlid(1:10),
     &         yctrlposunpack(1:4), optimcycle
          write(standardMessageUnit,*) 'ph-pack: unpacking ',
     &         costname(1:9)
       endif

c--   Only Proc 0 will do I/O.
      IF ( myProcId .eq. 0 ) THEN

          call mdsfindunit( cunit, mythid )

#ifndef ALLOW_ADMTLM

          open( cunit, file   = cfile,
     &         status = 'old',
     &         form   = 'unformatted',
     &         access  = 'sequential'   )

c--       Header information.
          read(cunit) filenvartype
          read(cunit) filenvarlength
          read(cunit) fileYctrlid
          read(cunit) fileOptimCycle
          read(cunit) filefc
          read(cunit) fileIg
          read(cunit) fileJg
          read(cunit) filensx
          read(cunit) filensy
          read(cunit) (filenWetcGlobal(k),   k=1,Nr)
          read(cunit) (filenWetsGlobal(k),   k=1,Nr)
          read(cunit) (filenWetwGlobal(k),   k=1,Nr)
#ifdef ALLOW_CTRL_WETV
          read(cunit) (filenWetvGlobal(k),   k=1,Nr)
#endif
#ifdef ALLOW_SHELFICE
          read(cunit) (filenWetiGlobal(k),   k=1,Nr)
c         read(cunit) filenWetiGlobal(1)
#endif

#ifdef ALLOW_OBCS
cgg(     Add OBCS mask information to the header.
#ifdef ALLOW_OBCSN_CONTROL
          read(cunit) ((filenWetobcsnGlo(k,iobcs),
     &         k=1,Nr), iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
          read(cunit) ((filenWetobcssGlo(k,iobcs),
     &         k=1,Nr), iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
          read(cunit) ((filenWetobcswGlo(k,iobcs),
     &         k=1,Nr), iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
          read(cunit) ((filenWetobcseGlo(k,iobcs),
     &         k=1,Nr), iobcs= 1,nobcs)
#endif
cgg)
#endif /* ALLOW_OBCS */

          read(cunit) (filencvarindex(i), i=1,maxcvars)
          read(cunit) (filencvarrecs(i),  i=1,maxcvars)
          read(cunit) (filencvarxmax(i),  i=1,maxcvars)
          read(cunit) (filencvarymax(i),  i=1,maxcvars)
          read(cunit) (filencvarnrmax(i), i=1,maxcvars)
          read(cunit) (filencvargrd(i),   i=1,maxcvars)
          read(cunit)

c         Check file header info.
c
          if ( filenvarlength .NE. nvarlength ) then
             print *, 'WARNING: wrong nvarlength ',
     &            filenvarlength, nvarlength
             STOP 'in S/R ctrl_unpack'
          else if ( filensx .NE. nSx .OR. filensy .NE. nSy ) then
             print *, 'WARNING: wrong nSx or nSy ',
     &            filensx, nSx, filensy, nSy
             STOP 'in S/R ctrl_unpack'
          endif
          do k = 1, Nr
             if ( filenWetcGlobal(k) .NE. nWetcGlobal(k) .OR.
     &            filenWetsGlobal(k) .NE. nWetsGlobal(k) .OR.
     &            filenWetwGlobal(k) .NE. nWetwGlobal(k) .OR.
     &            filenWetvGlobal(k) .NE. nWetvGlobal(k)  ) then
                print *, 'WARNING: wrong nWet?Global for k = ', k
                STOP
             endif
          end do
#ifdef ALLOW_SHELFICE
          do k=1,1
           if ( filenWetiGlobal(k) .NE. nWetiGlobal(k) ) then
            print *, 'WARNING: wrong nWetiGlobal for k = ', k
            STOP
           endif
          enddo
#endif /* ALLOW_SHELFICE */

#ifdef ALLOW_OBCS
cgg(   Lets also check the OBCS mask info in the header.
#ifdef ALLOW_OBCSN_CONTROL
       do iobcs = 1, nobcs
         do k = 1, Nr
           if (filenWetobcsnGlo(k,iobcs) .NE.
     &           nWetobcsnGlo(k,iobcs)) then
             print *, 'WARNING: OBCSN wrong nWet?Global for k = ', k
             STOP
           endif
         end do
       end do
#endif
#ifdef ALLOW_OBCSS_CONTROL
       do iobcs = 1, nobcs
         do k = 1, Nr
           if (filenWetobcssGlo(k,iobcs) .NE.
     &           nWetobcssGlo(k,iobcs)) then
             print *, 'WARNING: OBCSS wrong nWet?Global for k = ', k
             STOP
           endif
         end do
       end do
#endif
#ifdef ALLOW_OBCSW_CONTROL
       do iobcs = 1, nobcs
         do k = 1, Nr
           if (filenWetobcswGlo(k,iobcs) .NE.
     &           nWetobcswGlo(k,iobcs)) then
             print *, 'WARNING: OBCSW wrong nWet?Global for k = ', k
             STOP
           endif
         end do
       end do
#endif
#ifdef ALLOW_OBCSE_CONTROL
       do iobcs = 1, nobcs
         do k = 1, Nr
           if (filenWetobcseGlo(k,iobcs) .NE.
     &           nWetobcseGlo(k,iobcs)) then
             print *, 'WARNING: OBCSE wrong nWet?Global for k = ', k
             STOP
           endif
         end do
       end do
#endif
cgg)  End OBCS mask check.
#endif /* ALLOW_OBCS */

#endif /* ndef ALLOW_ADMTLM */

#ifdef ALLOW_PACKUNPACK_METHOD2
      ENDIF
      _END_MASTER( mythid )
      _BARRIER
#endif

c----------------------------------------------------------------------

#ifdef ALLOW_OBCS
#ifdef ALLOW_OBCSN_CONTROL
          ivartype    = 11
          write(weighttype,'(a)') 'wobcsn'
          call ctrl_set_unpack_xz(
     &         cunit, ivartype, fname_obcsn(ictrlgrad), 'maskobcsn',
     &         weighttype, wobcsn, nWetobcsnGlo, mythid)
#endif
#ifdef ALLOW_OBCSS_CONTROL
          ivartype    = 12
          write(weighttype,'(a)') 'wobcss'
          call ctrl_set_unpack_xz(
     &         cunit, ivartype, fname_obcss(ictrlgrad), 'maskobcss',
     &         weighttype, wobcss, nWetobcssGlo, mythid)
#endif
#ifdef ALLOW_OBCSW_CONTROL
          ivartype    = 13
          write(weighttype,'(a)') 'wobcsw'
          call ctrl_set_unpack_yz(
     &         cunit, ivartype, fname_obcsw(ictrlgrad), 'maskobcsw',
     &         weighttype, wobcsw, nWetobcswGlo, mythid)
#endif
#ifdef ALLOW_OBCSE_CONTROL
          ivartype    = 14
          write(weighttype,'(a)') 'wobcse'
          call ctrl_set_unpack_yz(
     &         cunit, ivartype, fname_obcse(ictrlgrad), 'maskobcse',
     &         weighttype, wobcse, nWetobcseGlo, mythid)
#endif
#endif /* ALLOW_OBCS */

#ifdef ALLOW_GENARR2D_CONTROL
       do iarr = 1, maxCtrlArr2D
        if (xx_genarr2d_weight(iarr).NE.' ') then
         mskNameForSetUnpack='maskCtrlC'
         DO k=1,Nr
           nwettmp(k) = nwetcglobal(k)
         ENDDO
# ifdef ALLOW_SHELFICE
         if ((xx_genarr2d_file(iarr)(1:11).eq.'xx_shicoeff').or.
     &       (xx_genarr2d_file(iarr)(1:11).eq.'xx_shicdrag')) then
          mskNameForSetUnpack='maskCtrlI'
          DO k=1,Nr
           nwettmp(k) = nwetiglobal(k)
          ENDDO
         endif
# endif
          call ctrl_set_fname( ctrlDir(1:ilDir)//xx_genarr2d_file(iarr),
     O                         fname_local, mythid )
          ivartype    = 100+iarr
cc          write(weighttype,'(a)') ' '
cc          write(weighttype,'(a)') 'wunit'
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, genarr2dPrecond(iarr),
     &         fname_local(ictrlgrad), mskNameForSetUnpack,
     &         xx_genarr2d_weight(iarr),
     &         nwettmp, mythid)
        endif
       enddo
#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
       do iarr = 1, maxCtrlArr3D
        if (xx_genarr3d_weight(iarr).NE.' ') then
          call ctrl_set_fname( ctrlDir(1:ilDir)//xx_genarr3d_file(iarr),
     O                         fname_local, mythid )
          ivartype    = 200+iarr
c          write(weighttype,'(a)') ' '
c          write(weighttype,'(a)') 'wunit'
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_local(ictrlgrad), 'maskCtrlC',
     &         xx_genarr3d_weight(iarr),
     &         wunit, nwetcglobal, mythid)
        endif
       enddo
#endif /* ALLOW_GENARR3D_CONTROL */

#ifdef ALLOW_GENTIM2D_CONTROL
       do iarr = 1, maxCtrlTim2D
        if (xx_gentim2d_weight(iarr).NE.' ') then
         mskNameForSetUnpack='maskCtrlC'
         DO k=1,Nr
           nwettmp(k) = nwetcglobal(k)
         ENDDO
# ifdef ALLOW_SHELFICE
         if (xx_gentim2d_file(iarr)(1:11).eq.'xx_shifwflx') then
           mskNameForSetUnpack='maskCtrlI'
           DO k=1,Nr
             nwettmp(k) = nwetiglobal(k)
           ENDDO
         endif
# endif
          call ctrl_set_fname( ctrlDir(1:ilDir)//xx_gentim2d_file(iarr),
     O                         fname_local, mythid )
          ivartype    = 300+iarr
c          write(weighttype,'(a)') ' '
c          write(weighttype,'(a)') 'wunit'
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, gentim2dPrecond(iarr),
     &         fname_local(ictrlgrad), mskNameForSetUnpack,
     &         xx_gentim2d_weight(iarr),
     &         nwettmp, mythid)
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
