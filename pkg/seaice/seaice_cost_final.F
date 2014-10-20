C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_final.F,v 1.20 2014/10/20 03:20:57 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_COST
#include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
#include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_COST_FINAL
C     !INTERFACE:
      SUBROUTINE SEAICE_COST_FINAL( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE SEAICE_COST_FINAL
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_COST.h"
#ifdef ALLOW_COST
# include "cost.h"
#endif
#ifdef ALLOW_CTRL
# include "ctrl.h"
# include "optim.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid

#ifdef ALLOW_COST
#if (defined(ALLOW_SEAICE_COST_SMR_AREA) || defined(ALLOW_COST_ICE))
C     ! FUNCTIONS:
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD

C     !LOCAL VARIABLES:
      INTEGER bi, bj
      INTEGER ifc
      _RL f_ice
      _RL f_smrarea
      _RL f_smrsst
      _RL f_smrsss
      _RL no_ice
      _RL no_smrarea
      _RL no_smrsst
      _RL no_smrsss
      CHARACTER*23 cfname
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      ifc = 30

      f_ice      = 0. _d 0
      f_smrarea  = 0. _d 0
      f_smrsst   = 0. _d 0
      f_smrsss   = 0. _d 0

      no_ice     = 0. _d 0
      no_smrarea = 0. _d 0
      no_smrsst  = 0. _d 0
      no_smrsss  = 0. _d 0

#ifdef ALLOW_SEAICE_COST_EXPORT
      CALL SEAICE_COST_EXPORT( myThid )
#endif

C--   Sum up all contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

          tile_fc(bi,bj) = tile_fc(bi,bj)
     &          + mult_ice_export * objf_ice_export(bi,bj)
     &          + mult_ice        * objf_ice(bi,bj)
     &          + mult_smrarea    * objf_smrarea(bi,bj)
     &          + mult_smrsst     * objf_smrsst(bi,bj)
     &          + mult_smrsss     * objf_smrsss(bi,bj)

       ENDDO
      ENDDO

C--   Note: global summation (tile_fc --> fc) is done only in cost_final

C--   Do global summation for each part of the cost function

      CALL GLOBAL_SUM_TILE_RL( objf_ice,     f_ice,     myThid )
      CALL GLOBAL_SUM_TILE_RL( objf_smrarea, f_smrarea, myThid )
      CALL GLOBAL_SUM_TILE_RL( objf_smrsst,  f_smrsst,  myThid )
      CALL GLOBAL_SUM_TILE_RL( objf_smrsss,  f_smrsss,  myThid )

      CALL GLOBAL_SUM_TILE_RL( num_ice,     no_ice,     myThid )
      CALL GLOBAL_SUM_TILE_RL( num_smrarea, no_smrarea, myThid )
      CALL GLOBAL_SUM_TILE_RL( num_smrsst,  no_smrsst,  myThid )
      CALL GLOBAL_SUM_TILE_RL( num_smrsss,  no_smrsss,  myThid )

      WRITE(standardMessageUnit,'(A,D22.15)')
     &     ' --> f_ice     =',f_ice
      WRITE(standardMessageUnit,'(A,D22.15)')
     &     ' --> f_smrarea =',f_smrarea
      WRITE(standardMessageUnit,'(A,D22.15)')
     &     ' --> f_smrarea =',f_smrsst
      WRITE(standardMessageUnit,'(A,D22.15)')
     &     ' --> f_smrarea =',f_smrsss

C--   Each process has calculated the global part for itself.
      IF ( MASTER_CPU_THREAD(myThid) ) THEN

        WRITE(cfname,'(A,i4.4)') 'costfunction_seaice',optimcycle
        OPEN(unit=ifc,file=cfname)

        WRITE(ifc,*) 'fc =', fc
        WRITE(ifc,*) 'f_ice   =', f_ice, no_ice
        WRITE(ifc,*) 'f_smrarea   =', f_smrarea, no_smrarea
        WRITE(ifc,*) 'f_smrsst    =', f_smrsst, no_smrsst
        WRITE(ifc,*) 'f_smrsss    =', f_smrsss, no_smrsss

        CLOSE(ifc)

      ENDIF

#endif /* ALLOW_SEAICE_COST_SMR_AREA */
#endif /* ALLOW_COST */

      RETURN
      END
