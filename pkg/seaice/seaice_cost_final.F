C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_final.F,v 1.4 2005/09/01 05:34:31 heimbach Exp $

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
#ifdef ALLOW_COST
#include "SEAICE_COST.h"
#include "cost.h"
#include "ctrl.h"
#include "optim.h"
#endif

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_COST

c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer ifc
      integer totnum

      _RL f_ice
      _RL f_smrarea

      _RL no_ice
      _RL no_smrarea

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
      
      f_ice      = 0. _d 0
      f_smrarea  = 0. _d 0
c
      no_ice     = 0. _d 0
      no_smrarea = 0. _d 0

c--   Sum up all contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi
 
          fc = fc
     &            + mult_ice     * objf_ice(bi,bj)
     &            + mult_smrarea * objf_smrarea(bi,bj)

          f_ice = f_ice + objf_ice(bi,bj)
          f_smrarea = f_smrarea + objf_smrarea(bi,bj)

          no_ice = no_ice + num_ice(bi,bj)
          no_smrarea = no_smrarea + num_smrarea(bi,bj)

        enddo
      enddo

c--   Do global summation.
      _GLOBAL_SUM_R8( fc , myThid )

c--   Do global summation for each part of the cost function
            
      _GLOBAL_SUM_R8( f_ice , myThid )
      _GLOBAL_SUM_R8( f_smrarea , myThid )

      _GLOBAL_SUM_R8( no_ice , myThid )
      _GLOBAL_SUM_R8( no_smrarea , myThid )

      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_ice     =',f_ice
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_smrarea =',f_smrarea

c--   Each process has calculated the global part for itself.
      _BEGIN_MASTER( mythid )
    
        write(cfname,'(A,i4.4)') 'costfunction_seaice',optimcycle
        open(unit=ifc,file=cfname)
      
        write(ifc,*) 'fc =', fc
        write(ifc,*) 'f_ice   =', f_ice, no_ice
        write(ifc,*) 'f_smrarea   =', f_smrarea, no_smrarea

        close(ifc)
        
      _END_MASTER( mythid )

      taveFreq   = 0.
      dumpFreq   = 0.
      pChkptFreq = 0.
      monitorFreq = 0.
      useDiagnostics = .FALSE.

#endif

      end
