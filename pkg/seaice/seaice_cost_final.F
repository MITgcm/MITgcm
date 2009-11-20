C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_final.F,v 1.14 2009/11/20 22:33:21 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"


      subroutine seaice_cost_final( mythid )

c     ==================================================================
c     SUBROUTINE seaice_cost_final
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "SEAICE_PARAMS.h"
#ifdef ALLOW_COST
#include "SEAICE_COST.h"
#include "cost.h"
#include "ctrl.h"
#include "optim.h"
#endif

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_COST

C     === Functions ====
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD

c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer ifc
      integer totnum

      _RL f_ice
      _RL f_smrarea
      _RL f_smrsst
      _RL f_smrsss

      _RL no_ice
      _RL no_smrarea
      _RL no_smrsst
      _RL no_smrsss

      character*23 cfname
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
      f_smrsst   = 0. _d 0
      f_smrsss   = 0. _d 0
c
      no_ice     = 0. _d 0
      no_smrarea = 0. _d 0
      no_smrsst  = 0. _d 0
      no_smrsss  = 0. _d 0

#ifdef ALLOW_SEAICE_COST_EXPORT
      call seaice_cost_export( myThid )
#endif

c--   Sum up all contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi

          fc = fc
     &          + mult_ice_export * objf_ice_export(bi,bj)
     &          + mult_ice        * objf_ice(bi,bj)
     &          + mult_smrarea    * objf_smrarea(bi,bj)
     &          + mult_smrsst     * objf_smrsst(bi,bj)
     &          + mult_smrsss     * objf_smrsss(bi,bj)

          f_ice = f_ice + objf_ice(bi,bj)
          f_smrarea = f_smrarea + objf_smrarea(bi,bj)
          f_smrsst = f_smrsst + objf_smrsst(bi,bj)
          f_smrsss = f_smrsss + objf_smrsss(bi,bj)

          no_ice = no_ice + num_ice(bi,bj)
          no_smrarea = no_smrarea + num_smrarea(bi,bj)
          no_smrsst = no_smrsst + num_smrsst(bi,bj)
          no_smrsss = no_smrsss + num_smrsss(bi,bj)

        enddo
      enddo

c--   Do global summation.
cph this is done only in ecco_cost_final!
cph      _GLOBAL_SUM_RL( fc , myThid )

c--   Do global summation for each part of the cost function

      _GLOBAL_SUM_RL( f_ice , myThid )
      _GLOBAL_SUM_RL( f_smrarea , myThid )
      _GLOBAL_SUM_RL( f_smrsst , myThid )
      _GLOBAL_SUM_RL( f_smrsss , myThid )

      _GLOBAL_SUM_RL( no_ice , myThid )
      _GLOBAL_SUM_RL( no_smrarea , myThid )
      _GLOBAL_SUM_RL( no_smrsst , myThid )
      _GLOBAL_SUM_RL( no_smrsss , myThid )

      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_ice     =',f_ice
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_smrarea =',f_smrarea
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_smrarea =',f_smrsst
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_smrarea =',f_smrsss

c--   Each process has calculated the global part for itself.
      IF ( MASTER_CPU_THREAD(myThid) ) THEN

        write(cfname,'(A,i4.4)') 'costfunction_seaice',optimcycle
        open(unit=ifc,file=cfname)

        write(ifc,*) 'fc =', fc
        write(ifc,*) 'f_ice   =', f_ice, no_ice
        write(ifc,*) 'f_smrarea   =', f_smrarea, no_smrarea
        write(ifc,*) 'f_smrsst    =', f_smrsst, no_smrsst
        write(ifc,*) 'f_smrsss    =', f_smrsss, no_smrsss

        close(ifc)

      ENDIF

      SEAICE_dumpFreq = 0.
      SEAICE_taveFreq = 0.

#endif /* ALLOW_COST */

      end
