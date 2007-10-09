C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcplr_match_comp.F,v 1.2 2007/10/08 23:58:21 jmc Exp $
C $Name:  $

!=======================================================================
      integer function mitcplr_match_comp( carg )
      implicit none
! Predefined constants/arrays
#include "CPLR_SIG.h"
! Arguments
      character*(*) carg
! Local
      integer j,jmatch
!     ------------------------------------------------------------------

      if ( carg .EQ. coupler_Name ) then
       mitcplr_match_comp=-1
       stop 'mitcplr_match_comp: I should not reach this point'
      else

! Search through list of components
      jmatch=0
      do j=1,num_components
       if ( carg .EQ. component_List(j) ) then
        if ( jmatch .NE. 0 ) then
         if (DEBUG) write(0,*) 'mitcplr_match_comp: String arg = ',carg
         stop 'mitcplr_match_comp: More than one match to string'
        else
         jmatch=j
        endif
       endif
      enddo

! If no matches, create entry in list
      if ( jmatch .EQ. 0) then
       if ( num_components .EQ. MAX_COMPONENTS ) then
        stop 'mitcplr_match_comp: Too many components types entered'
       endif
       num_components=num_components+1
       jmatch=num_components
       call mitcplr_initcomp( jmatch, carg )
      endif

! Return index within list
      mitcplr_match_comp=jmatch

      endif

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
