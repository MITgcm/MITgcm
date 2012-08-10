C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_init_ctrlvar.F,v 1.13 2012/08/10 19:38:57 jmc Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"
#include "AD_CONFIG.h"

      subroutine ctrl_init_ctrlvar(
     &       xx_fname
     &     , ivarindex
     &     , ncvarindex_loc
     &     , ncvarrecs_loc
     &     , ncvarrecstart_loc
     &     , ncvarrecsend_loc
     &     , ncvarxmax_loc
     &     , ncvarymax_loc
     &     , ncvarnrmax_loc
     &     , ncvargrd_loc
     &     , whichxyz
     &     , mythid
     &     )

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
#include "ctrl.h"
#include "optim.h"

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
      character ncvargrd_loc*1
      character whichxyz*2
      integer mythid

c     == local variables ==
      character*( 80) fname(3)
      logical  exst

c     _BEGIN_MASTER( mythid )
      ncvarindex(ivarindex)    = ncvarindex_loc
      ncvarrecs (ivarindex)    = ncvarrecs_loc
      ncvarrecstart(ivarindex) = ncvarrecstart_loc
      ncvarrecsend(ivarindex)  = ncvarrecsend_loc
      ncvarxmax (ivarindex)    = ncvarxmax_loc
      ncvarymax (ivarindex)    = ncvarymax_loc
      ncvarnrmax(ivarindex)    = ncvarnrmax_loc
      ncvargrd  (ivarindex)    = ncvargrd_loc
c     _END_MASTER( mythid )

cph add following flag to make pack/unpack only less error-prone
#ifndef CTRL_DO_PACK_UNPACK_ONLY

      call ctrl_set_fname( xx_fname, fname, mythid )

c     _BEGIN_MASTER( mythid )
      inquire( file='costfinal', exist=exst )
c     _END_MASTER( mythid )

      IF ( .NOT. exst) THEN

         if ( whichxyz .EQ. '3d') then
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
            if ( .NOT.doAdmTlm .OR. (doAdmTlm.AND.yadprefix.EQ.'ad') )
     &           call ctrl_set_globfld_xyz( fname(2), ivarindex, mythid)
#endif
            if ( ( doInitXX .AND. optimcycle.eq.0 ) .OR. doAdmTlm )
     &           call ctrl_set_globfld_xyz( fname(1), ivarindex, mythid)
         else if ( whichxyz .EQ. 'xy') then
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
            if ( .NOT.doAdmTlm .OR. (doAdmTlm.AND.yadprefix.EQ.'ad') )
     &           call ctrl_set_globfld_xy( fname(2), ivarindex, mythid)
#endif
            if ( ( doInitXX .AND. optimcycle.eq.0 ) .OR. doAdmTlm )
     &           call ctrl_set_globfld_xy( fname(1), ivarindex, mythid )
         else if ( whichxyz .EQ. 'xz') then
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
            if ( .NOT.doAdmTlm .OR. (doAdmTlm.AND.yadprefix.EQ.'ad') )
     &           call ctrl_set_globfld_xz( fname(2), ivarindex, mythid)
#endif
            if ( ( doInitXX .AND. optimcycle.eq.0 ) .OR. doAdmTlm )
     &           call ctrl_set_globfld_xz( fname(1), ivarindex, mythid )
         else if ( whichxyz .EQ. 'yz') then
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
            if ( .NOT.doAdmTlm .OR. (doAdmTlm.AND.yadprefix.EQ.'ad') )
     &           call ctrl_set_globfld_yz( fname(2), ivarindex, mythid)
#endif
            if ( ( doInitXX .AND. optimcycle.eq.0 ) .OR. doAdmTlm )
     &           call ctrl_set_globfld_yz( fname(1), ivarindex, mythid )
         else
            STOP 'whichxyz option not implemented'
         end if

      ELSE

c-- for DIVA, avoid forward-related output in adjoint part
         CALL TURNOFF_MODEL_IO( 1, myThid )

      ENDIF

#endif /* CTRL_DO_PACK_UNPACK_ONLY */

      return
      end
