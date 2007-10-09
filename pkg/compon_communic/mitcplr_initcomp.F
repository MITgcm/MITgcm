C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcplr_initcomp.F,v 1.2 2007/10/08 23:58:21 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine mitcplr_initcomp( compind, carg )
      implicit none
! Predefined constants/arrays
#include "CPLR_SIG.h"
! Arguments
      integer compind
      character*(MAXLEN_COMP_NAME) carg
! Local
!     ------------------------------------------------------------------

      component_List(compind)=carg

      num_component_procs( compind ) = 0
      num_compcplr_procs( compind ) = 0
      component_Name( compind ) = carg

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
