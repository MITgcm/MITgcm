C $Header: /u/gcmpack/MITgcm/pkg/autodiff/Attic/autodiff_cost_summary.F,v 1.1 2009/04/08 02:39:07 heimbach Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

      subroutine autodiff_cost_summary
     &     ( fname_gen, costname_gen, f_gen, no_gen, mythid )

c     ==================================================================
c     SUBROUTINE autodiff_cost_summary
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "EESUPPORT.h"
#ifdef ALLOW_COST
#include "cost.h"
#include "ctrl.h"
#include "optim.h"
#endif

c     == routine arguments ==

      integer mythid
      _RL f_gen
      _RL no_gen
      character*(MAX_LEN_MBUF) fname_gen
      character*(MAX_LEN_MBUF) costname_gen

#ifdef ALLOW_COST

c     == local variables ==

      integer il
      integer ifc

      character*(MAX_LEN_MBUF) cfname
      character*(MAX_LEN_MBUF) msgbuf

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      ifc = 30

c--   Only one proc writes cost summary file
      _BEGIN_MASTER( mythid )
#ifdef ALLOW_USE_MPI
       IF( mpiMyId .EQ. 0 ) THEN
#endif

        il = ilnblnk(fname_gen)
        write(cfname,'(A,i4.4)') fname_gen(1:il), optimcycle
        open(unit=ifc,file=cfname)

        il = ilnblnk(costname_gen)
        write(ifc,*) costname_gen(1:il), ' = ', f_gen, no_gen

        close(ifc)

#ifdef ALLOW_USE_MPI
       END IF
#endif
      _END_MASTER( mythid )

#endif

      end
