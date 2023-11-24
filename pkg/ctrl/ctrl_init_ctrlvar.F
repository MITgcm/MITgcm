#include "CTRL_OPTIONS.h"
#include "AD_CONFIG.h"

      SUBROUTINE CTRL_INIT_CTRLVAR(
     &       xx_fname,
     &       ivarindex,
     &       ncvarindex_loc,
     &       ncvarrecs_loc,
     &       ncvarrecstart_loc,
     &       ncvarrecsend_loc,
     &       ncvarxmax_loc,
     &       ncvarymax_loc,
     &       ncvarnrmax_loc,
     &       ncvargrd_loc,
     &       whichxyz,
     &       myThid )

c     ==================================================================
c     SUBROUTINE ctrl_init_ctrlvar
c     ==================================================================
c
c     started: heimbach@mit.edu 28-Oct-2003
c
c     ==================================================================
c     SUBROUTINE ctrl_init_ctrlvar
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CTRL.h"
#include "OPTIMCYCLE.h"

c     == routine arguments ==
      character*(MAX_LEN_FNAM) xx_fname
      integer ivarindex
      integer ncvarindex_loc
      integer ncvarrecs_loc
      integer ncvarrecstart_loc
      integer ncvarrecsend_loc
      integer ncvarxmax_loc
      integer ncvarymax_loc
      integer ncvarnrmax_loc
      character*(1) ncvargrd_loc
      character*(2) whichxyz
      integer myThid

C     == external ==
      integer ILNBLNK
      external ILNBLNK

c     == local variables ==
      integer il,ilDir
      character*(MAX_LEN_FNAM) fname(3), gfname
      character*(MAX_LEN_MBUF) msgBuf
      logical  exst, g_exst
C     == end of interface ==

c     _BEGIN_MASTER( myThid )
      ncvarindex(ivarindex)    = ncvarindex_loc
      ncvarrecs (ivarindex)    = ncvarrecs_loc
      ncvarrecstart(ivarindex) = ncvarrecstart_loc
      ncvarrecsend(ivarindex)  = ncvarrecsend_loc
      ncvarxmax (ivarindex)    = ncvarxmax_loc
      ncvarymax (ivarindex)    = ncvarymax_loc
      ncvarnrmax(ivarindex)    = ncvarnrmax_loc
      ncvargrd  (ivarindex)    = ncvargrd_loc
c     _END_MASTER( myThid )

cph add following flag to make pack/unpack only less error-prone
#ifndef CTRL_DO_PACK_UNPACK_ONLY

      ilDir=ilnblnk( ctrlDir )
      call ctrl_set_fname( ctrlDir(1:ilDir)//xx_fname, fname, myThid )

c     _BEGIN_MASTER( myThid )
      inquire( file='costfinal', exist=exst )
c     _END_MASTER( myThid )

C     In an adjoint run, adxx_ files should always be initialized with zeros
C     For TLM, all zeros means zero sensitivity, so check
C     for existing perturbation file g_xx_, otherwise
C     initialize to zero and warn user
      if (yadprefix.eq.'g_') then
        il = ILNBLNK(fname(2))
        write(gfname(1:il+5),'(2a)') fname(2)(1:il),'.data'
        inquire(file=gfname,exist=g_exst)
        if (.not. g_exst) then
            write(msgBuf,'(3A)')'** WARNING ** CTRL_INIT_CTRLVAR: ',
     &          'could not find ', gfname(1:il+5)
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            write(msgBuf,'(3A)')'** WARNING ** ',
     &          'for TLM perturbation ',
     &          'so will initialize this file with all zeros'
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
        endif
      else
        g_exst = .FALSE.
      endif

      IF ( .NOT. exst) THEN

         if ( whichxyz .EQ. '3d') then
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
            if ( yadprefix.EQ.'ad' .OR. .NOT.(g_exst .OR. doAdmTlm) )
     &           call ctrl_set_globfld_xyz( fname(2), ivarindex, myThid)
#endif
            if ( ( doInitXX .AND. optimcycle.eq.0 ) .OR. doAdmTlm )
     &           call ctrl_set_globfld_xyz( fname(1), ivarindex, myThid)
         else if ( whichxyz .EQ. 'xy') then
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
            if ( yadprefix.EQ.'ad' .OR. .NOT.(g_exst .OR. doAdmTlm) )
     &           call ctrl_set_globfld_xy( fname(2), ivarindex, myThid)
#endif
            if ( ( doInitXX .AND. optimcycle.eq.0 ) .OR. doAdmTlm )
     &           call ctrl_set_globfld_xy( fname(1), ivarindex, myThid )
         else if ( whichxyz .EQ. 'xz') then
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
            if ( yadprefix.EQ.'ad' .OR. .NOT.(g_exst .OR. doAdmTlm) )
     &           call ctrl_set_globfld_xz( fname(2), ivarindex, myThid)
#endif
            if ( ( doInitXX .AND. optimcycle.eq.0 ) .OR. doAdmTlm )
     &           call ctrl_set_globfld_xz( fname(1), ivarindex, myThid )
         else if ( whichxyz .EQ. 'yz') then
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
            if ( yadprefix.EQ.'ad' .OR. .NOT.(g_exst .OR. doAdmTlm) )
     &           call ctrl_set_globfld_yz( fname(2), ivarindex, myThid)
#endif
            if ( ( doInitXX .AND. optimcycle.eq.0 ) .OR. doAdmTlm )
     &           call ctrl_set_globfld_yz( fname(1), ivarindex, myThid )
         else
            STOP 'whichxyz option not implemented'
         end if

      ELSE

c-- for DIVA, avoid forward-related output in adjoint part
         CALL TURNOFF_MODEL_IO( 1, myThid )

      ENDIF

#endif /* CTRL_DO_PACK_UNPACK_ONLY */

      RETURN
      END
