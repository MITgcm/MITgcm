#include "CTRL_OPTIONS.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

      subroutine ctrl_unpack( first, myThid )

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
c                from here to namelist in ctrl_init_fixed
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
      integer myThid

#ifndef EXCLUDE_CTRL_PACK
c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == local variables ==

      integer k, ivar, iarr
      integer ictrlgrad
      integer cunit
      logical lxxadxx
      CHARACTER*(128) cfile
      integer il, ilDir

C     These are variables needed to read the header, but subsequently
C     they are not used.
      INTEGER maxLocal, numLoc
#ifdef READ_OLD_CTRL_PACK_FILE
      PARAMETER( maxLocal = old_maxcvars )
#else
      PARAMETER( maxLocal = maxcvars )
#endif
      INTEGER file_nvarType
      INTEGER file_nvarLength
      CHARACTER*(10) file_YctrlId
      INTEGER file_OptimCycle
      _RL     file_fc
      INTEGER file_Ig
      INTEGER file_Jg
      INTEGER file_nSx
      INTEGER file_nSy
      INTEGER file_nWetcGlobal(Nr)
      INTEGER file_nWetsGlobal(Nr)
      INTEGER file_nWetwGlobal(Nr)
c     INTEGER file_nWetvGlobal(Nr)
#ifdef ALLOW_SHELFICE
      INTEGER file_nWetiGlobal(Nr)
#endif
      INTEGER file_varIndex(maxLocal)
      INTEGER file_varRecs(maxLocal)
      INTEGER file_varNxMax(maxLocal)
      INTEGER file_varNyMax(maxLocal)
      INTEGER file_varNrMax(maxLocal)
      CHARACTER*(1) file_varGrid(maxLocal)
      CHARACTER*(5) file_varType(maxLocal)

      CHARACTER*(MAX_LEN_FNAM) fname_local(3)
#if ( defined ALLOW_GENARR2D_CONTROL || defined ALLOW_GENTIM2D_CONTROL )
      integer nwettmp(Nr)
      CHARACTER*(9) mskNameForSetUnpack
#endif

#ifdef ALLOW_OBCS
      INTEGER iobcs
#ifdef ALLOW_OBCSN_CONTROL
      INTEGER file_nWetobcsnGlo(Nr,nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
      INTEGER file_nWetobcssGlo(Nr,nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
      INTEGER file_nWetobcswGlo(Nr,nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
      INTEGER file_nWetobcseGlo(Nr,nobcs)
#endif
#endif /* ALLOW_OBCS */

c     == end of interface ==

c--   Initialise
      nbuffGlobal = 0
      DO k = 1, Nr
       file_nWetcGlobal(k) = 0
       file_nWetsGlobal(k) = 0
       file_nWetwGlobal(k) = 0
c      file_nWetvGlobal(k) = 0
      ENDDO
      DO ivar = 1, maxLocal
       file_varIndex(ivar) = 0
       file_varRecs(ivar)  = 0
       file_varNxMax(ivar) = 0
       file_varNyMax(ivar) = 0
       file_varNrMax(ivar) = 0
       file_varGrid(ivar)  = ' '
       file_varType(ivar)  = '     '
      ENDDO

c--   Find ctrlDir (w/o trailing blanks) length
      ilDir = ilnblnk(ctrlDir)

c--   Only the master thread will do I/O.
      _BEGIN_MASTER( myThid )

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

          call mdsfindunit( cunit, myThid )

#ifndef ALLOW_ADMTLM

          open( cunit, file = cfile,
     &          status = 'old',
     &          form   = 'unformatted',
     &          access = 'sequential' )

c--       Header information.
          read(cunit)  file_nvarType
          read(cunit)  file_nvarLength
          read(cunit)  file_YctrlId
          read(cunit)  file_OptimCycle
          read(cunit)  file_fc
          read(cunit)  file_Ig
          read(cunit)  file_Jg
          read(cunit)  file_nSx
          read(cunit)  file_nSy
          read(cunit) (file_nWetcGlobal(k), k=1,Nr)
          read(cunit) (file_nWetsGlobal(k), k=1,Nr)
          read(cunit) (file_nWetwGlobal(k), k=1,Nr)
c#ifdef ALLOW_CTRL_WETV
c         read(cunit) (file_nWetvGlobal(k), k=1,Nr)
c#endif
#ifdef ALLOW_SHELFICE
          read(cunit) (file_nWetiGlobal(k), k=1,Nr)
c         read(cunit)  file_nWetiGlobal(1)
#endif

#ifdef ALLOW_OBCS
cgg(     Add OBCS mask information to the header.
#ifdef ALLOW_OBCSN_CONTROL
          read(cunit) ((file_nWetobcsnGlo(k,iobcs),
     &         k=1,Nr), iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
          read(cunit) ((file_nWetobcssGlo(k,iobcs),
     &         k=1,Nr), iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
          read(cunit) ((file_nWetobcswGlo(k,iobcs),
     &         k=1,Nr), iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
          read(cunit) ((file_nWetobcseGlo(k,iobcs),
     &         k=1,Nr), iobcs= 1,nobcs)
#endif
cgg)
#endif /* ALLOW_OBCS */

C--       Check file header info.

          if ( file_nvarType .NE. nvartype ) then
             print *, 'ERROR: wrong nvartype ',
     &            file_nvarType, nvartype
             STOP 'in S/R ctrl_unpack'
          endif
          if ( file_nvarLength .NE. nvarlength ) then
             print *, 'WARNING: wrong nvarlength ',
     &            file_nvarLength, nvarlength
             STOP 'in S/R ctrl_unpack'
          elseif ( file_nSx .NE. nSx .OR. file_nSy .NE. nSy ) then
             print *, 'WARNING: wrong nSx or nSy ',
     &            file_nSx, nSx, file_nSy, nSy
             STOP 'in S/R ctrl_unpack'
          endif
          do k = 1, Nr
             if ( file_nWetcGlobal(k) .NE. nWetcGlobal(k) .OR.
c    &            file_nWetvGlobal(k) .NE. nWetvGlobal(k) .OR.
     &            file_nWetsGlobal(k) .NE. nWetsGlobal(k) .OR.
     &            file_nWetwGlobal(k) .NE. nWetwGlobal(k)  ) then
                print *, 'WARNING: wrong nWet?Global for k = ', k
                print *, 'c', file_nWetcGlobal(k), nWetcGlobal(k)
                print *, 's', file_nWetsGlobal(k), nWetsGlobal(k)
                print *, 'w', file_nWetwGlobal(k), nWetwGlobal(k)
c               print *, 'v', file_nWetvGlobal(k), nWetvGlobal(k)
                STOP
             endif
          end do
#ifdef ALLOW_SHELFICE
          do k=1,1
           if ( file_nWetiGlobal(k) .NE. nWetiGlobal(k) ) then
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
           if (file_nWetobcsnGlo(k,iobcs) .NE.
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
           if (file_nWetobcssGlo(k,iobcs) .NE.
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
           if (file_nWetobcswGlo(k,iobcs) .NE.
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
           if (file_nWetobcseGlo(k,iobcs) .NE.
     &           nWetobcseGlo(k,iobcs)) then
             print *, 'WARNING: OBCSE wrong nWet?Global for k = ', k
             STOP
           endif
         end do
       end do
#endif
cgg)  End OBCS mask check.
#endif /* ALLOW_OBCS */

C--   Finish to read header information.
          numLoc = nvartype
#ifdef READ_OLD_CTRL_PACK_FILE
          numLoc = maxLocal
#endif
          read(cunit) ( file_varIndex(ivar), ivar=1,numLoc )
          read(cunit) ( file_varRecs(ivar),  ivar=1,numLoc )
          read(cunit) ( file_varNxMax(ivar), ivar=1,numLoc )
          read(cunit) ( file_varNyMax(ivar), ivar=1,numLoc )
          read(cunit) ( file_varNrMax(ivar), ivar=1,numLoc )
          read(cunit) ( file_varGrid(ivar),  ivar=1,numLoc )
#ifdef READ_OLD_CTRL_PACK_FILE
          read(cunit)
          CALL CTRL_CONVERT_HEADER(
     I              maxLocal, file_nvarType, errorMessageUnit,
     U              file_varIndex, file_varRecs,
     U              file_varNxMax, file_varNyMax, file_varNrMax,
     U              file_varGrid, file_varType,
     I              myThid )
#else
          read(cunit) ( file_varType(ivar),  ivar=1,numLoc )
#endif

#endif /* ndef ALLOW_ADMTLM */

#ifdef ALLOW_PACKUNPACK_METHOD2
      ENDIF
      _END_MASTER( myThid )
      _BARRIER
#endif

c----------------------------------------------------------------------

c     do ivar = 1, maxcvars
      do ivar = 1, nvartype
       if ( ncvarindex(ivar) .GE. 0 ) THEN
        il = ilnblnk(ncvarfname(ivar))
        iarr = ncvarindex(ivar)
        call ctrl_set_fname(ctrlDir(1:ilDir)//ncvarfname(ivar),
     &                      fname_local, myThid )
        if ( ncvargrd(ivar).EQ.'m' ) then
#ifdef ALLOW_OBCS
         if ( iarr.EQ.1 ) then
# ifdef ALLOW_OBCSN_CONTROL
          call ctrl_set_unpack_xz( cunit, ivar, fname_local(ictrlgrad),
     &            'maskobcsn', 'wobcsn', wobcsn, nWetobcsnGlo, myThid )
# endif
         elseif ( iarr.EQ.2 ) then
# ifdef ALLOW_OBCSS_CONTROL
          call ctrl_set_unpack_xz( cunit, ivar, fname_local(ictrlgrad),
     &            'maskobcss', 'wobcss', wobcss, nWetobcssGlo, myThid )
# endif
         elseif ( iarr.EQ.3 ) then
# ifdef ALLOW_OBCSE_CONTROL
          call ctrl_set_unpack_yz( cunit, ivar, fname_local(ictrlgrad),
     &            'maskobcse', 'wobcse', wobcse, nWetobcseGlo, myThid )
# endif
         elseif ( iarr.EQ.4 ) then
# ifdef ALLOW_OBCSW_CONTROL
          call ctrl_set_unpack_yz( cunit, ivar, fname_local(ictrlgrad),
     &            'maskobcsw', 'wobcsw', wobcsw, nWetobcswGlo, myThid )
# endif
         endif
#endif /* ALLOW_OBCS */

#ifdef ALLOW_GENARR2D_CONTROL
        elseif ( ncvartype(ivar).EQ.'Arr2D' ) then
         if ( xx_genarr2d_weight(iarr).NE.' ') then
          mskNameForSetUnpack='maskCtrlC'
          DO k=1,Nr
           nwettmp(k) = nwetcglobal(k)
          ENDDO
# ifdef ALLOW_SHELFICE
          if ( ncvargrd(ivar).EQ.'i' ) then
           mskNameForSetUnpack='maskCtrlI'
           DO k=1,Nr
            nwettmp(k) = nwetiglobal(k)
           ENDDO
          endif
# endif
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivar, genarr2dPrecond(iarr),
     &         fname_local(ictrlgrad), mskNameForSetUnpack,
     &         xx_genarr2d_weight(iarr),
     &         nwettmp, myThid )
         endif
#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
        elseif ( ncvartype(ivar).EQ.'Arr3D' ) then
         if ( xx_genarr3d_weight(iarr).NE.' ') then
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivar,
     &         fname_local(ictrlgrad), 'maskCtrlC',
     &         xx_genarr3d_weight(iarr),
     &         wunit, nwetcglobal, myThid )
         endif
#endif /* ALLOW_GENARR3D_CONTROL */

#ifdef ALLOW_GENTIM2D_CONTROL
        elseif ( ncvartype(ivar).EQ.'Tim2D' ) then
         if ( xx_gentim2d_weight(iarr).NE.' ') then
          mskNameForSetUnpack='maskCtrlC'
          DO k=1,Nr
           nwettmp(k) = nwetcglobal(k)
          ENDDO
# ifdef ALLOW_SHELFICE
          if ( ncvargrd(ivar).EQ.'i' ) then
           mskNameForSetUnpack='maskCtrlI'
           DO k=1,Nr
            nwettmp(k) = nwetiglobal(k)
           ENDDO
          endif
# endif
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivar, gentim2dPrecond(iarr),
     &         fname_local(ictrlgrad), mskNameForSetUnpack,
     &         xx_gentim2d_weight(iarr),
     &         nwettmp, myThid )
         endif
#endif /* ALLOW_GENTIM2D_CONTROL */
        endif
C     if ( ncvarindex(ivar) .ge. 0 ) then
       endif
C     do ivar = 1, maxcvars
      enddo

#ifdef ALLOW_PACKUNPACK_METHOD2
      _BEGIN_MASTER( myThid )
      IF ( myProcId .eq. 0 ) THEN
#endif

      close ( cunit )
      ENDIF !IF ( myProcId .eq. 0 )
      _END_MASTER( myThid )
      _BARRIER
#endif /* EXCLUDE_CTRL_PACK */

      RETURN
      END
