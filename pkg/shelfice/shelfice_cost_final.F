C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_cost_final.F,v 1.5 2012/03/13 16:17:33 mlosch Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"

      subroutine shelfice_cost_final( myThid )
C     *==========================================================*
C     | subroutine shelfice_cost_final                           |
C     | o final cost evalualtion                                 |
C     *==========================================================*
C     |                                                          |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SHELFICE.h"
#include "SHELFICE_COST.h"

#ifdef ALLOW_COST
# include "cost.h"
# include "optim.h"
#endif
#if  (defined ALLOW_SHIFWFLX_CONTROL && \
      defined ALLOW_SHIFWFLX_COST_CONTRIBUTION)
# include "ctrl.h"
#endif

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_COST
C     === Functions ====
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD

C     == Local variables
      integer bi, bj
      integer itlo,ithi
      integer jtlo,jthi
      integer ifc
      character*(25) cfname

      _RL f_shelfice
      _RL f_shifwflx

CML      _RL no_shelfice
      _RL no_shifwflx

#ifndef ALLOW_ECCO
      integer i, j
#endif

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      f_shelfice = 0. _d 0
      f_shifwflx = 0. _d 0
CML      no_shelfice= 0. _d 0
      no_shifwflx= 0. _d 0

#ifdef ALLOW_ECCO
#if  (defined ALLOW_SHIFWFLX_CONTROL && \
      defined ALLOW_SHIFWFLX_COST_CONTRIBUTION)
      CALL SHELFICE_COST_SHIFWFLX( mythid )
#endif /* ALLOW_SHIFWFLX_CONTROL and ALLOW_SHIFWFLX_COST_CONTRIBUTION */
#else
C--   Calculate cost function on tile of this instance
      do bj = jtlo,jthi
       do bi = itlo,ithi
ccc        do klev = 1, Nr
         do j=1,sNy
          do i=1,sNx
c--
             objf_shelfice(bi,bj) = objf_shelfice(bi,bj)
     &            + cMeanSHIforS(i,j,bi,bj)/lastinterval/rhoConstFresh
     &            *_rA(i,j,bi,bj)
c--
              end do
            end do
ccc          end do
        end do
      end do
#endif /* ALLOW_ECCO */

c--   Sum up all contributions.
      do bj = jtlo,jthi
       do bi = itlo,ithi
        write(standardmessageunit,'(A,D22.15)')
     &       ' --> objf_shelfice(bi,bj) = ', objf_shelfice(bi,bj)
        write(standardmessageunit,'(A,D22.15)')
     &       ' --> objf_shifwflx(bi,bj) = ', objf_shifwflx(bi,bj)
        fc = fc
     &       + mult_shelfice * objf_shelfice(bi,bj)
     &       + mult_shifwflx * objf_shifwflx(bi,bj)

        f_shelfice = f_shelfice + objf_shelfice(bi,bj)
        f_shifwflx = f_shifwflx + objf_shifwflx(bi,bj)
CML        no_shelfice = no_shelfice + num_shelfice(bi,bj)
        no_shifwflx = no_shifwflx + num_shifwflx(bi,bj)

       enddo
      enddo

      _GLOBAL_SUM_RL( f_shelfice , myThid )
      _GLOBAL_SUM_RL( f_shifwflx , myThid )
CML      _GLOBAL_SUM_RL( no_shelfice , myThid )
      _GLOBAL_SUM_RL( no_shifwflx , myThid )

c--   Each process has calculated the global part for itself.
      IF ( MASTER_CPU_THREAD(myThid) ) THEN

       ifc = 30
       WRITE(cfname,'(A,i4.4)') 'costfunction_shelfice',optimcycle
       OPEN(unit=ifc,file=cfname)

       WRITE(ifc,*) 'fc =', fc
CML       WRITE(ifc,*) 'f_shelfice =', f_shelfice, no_shelfice
       WRITE(ifc,*) 'f_shelfice =', f_shelfice
       WRITE(ifc,*) 'f_shifwflx =', f_shifwflx, no_shifwflx

       CLOSE(ifc)

      ENDIF
#endif /* ALLOW_COST */

      END
