C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_final.F,v 1.1 2005/08/25 16:10:42 heimbach Exp $

#include "SEAICE_OPTIONS.h"


      subroutine seaice_cost_final( mythid )

c     ==================================================================
c     SUBROUTINE seaice_cost_final
c     ==================================================================
c
c     ==================================================================
c     SUBROUTINE seaice_cost_final
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "SEAICE_COST.h"

#include "cost.h"
#include "ctrl.h"
#include "optim.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer ifc
      integer totnum

      _RL f_ice

      _RL no_ice

      character*20 cfname
#ifdef ECCO_VERBOSE
      character*(MAX_LEN_MBUF) msgbuf
#endif

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      ifc = 30
      
      f_ice     = 0. _d 0
      no_ice    = 0. _d 0

c--   Sum up all contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi
 
          fc = fc
     &            + mult_ice     * objf_ice(bi,bj)

          f_ice = f_ice + objf_ice(bi,bj)

          no_ice = no_ice + num_ice(bi,bj)

        enddo
      enddo
cph(
      print *, 'ph-ice ', fc, mult_ice, objf_ice(1,1)
cph)


c--   Do global summation.
      _GLOBAL_SUM_R8( fc , myThid )

c--   Do global summation for each part of the cost function
            
      _GLOBAL_SUM_R8( f_ice , myThid )

      _GLOBAL_SUM_R8( no_ice , myThid )

      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_ice     =',f_ice

c--   Each process has calculated the global part for itself.
      _BEGIN_MASTER( mythid )
    
        write(cfname,'(A,i4.4)') 'costfunction_seaice',optimcycle
        open(unit=ifc,file=cfname)
      
        write(ifc,*) 'fc =', fc
        write(ifc,*) 'f_ice   =', f_ice, no_ice

        close(ifc)
        
      _END_MASTER( mythid )

      taveFreq   = 0.
      dumpFreq   = 0.
      pChkptFreq = 0.
      monitorFreq = 0.
      useDiagnostics = .FALSE.

      end
