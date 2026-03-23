#include "SHELFICE_OPTIONS.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SHELFICE_COST_FINAL
C     !INTERFACE:
      SUBROUTINE SHELFICE_COST_FINAL( ifc, optimcycle, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | subroutine shelfice_cost_final                           |
C     | o final cost evalualtion                                 |
C     *==========================================================*

C     !USES:
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
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     ifc        :: file unit for costfunction_shelfice.XXXX
C     optimcycle :: cycle number of the off-line optimization.
C     myThid     :: Thread number for this instance of the routine.
      INTEGER ifc
      INTEGER optimcycle
      INTEGER myThid

#ifdef ALLOW_COST
C     !LOCAL VARIABLES:
      INTEGER bi, bj
      CHARACTER*15  tile15c
#ifndef ALLOW_OPENAD
      CHARACTER*(26) cfname
#endif

      _RL f_shelfice
      _RL f_shifwflx
cML      _RL no_shelfice
      _RL no_shifwflx

      INTEGER i, j
CEOP

      f_shelfice = 0. _d 0
      f_shifwflx = 0. _d 0
cML      no_shelfice= 0. _d 0
      no_shifwflx= 0. _d 0

      IF ( .NOT. useECCO ) THEN
C--   Calculate cost function on tile of this instance
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
ccc        do klev = 1, Nr
          DO j=1,sNy
           DO i=1,sNx

             objf_shelfice(bi,bj) = objf_shelfice(bi,bj)
     &            + cMeanSHIforS(i,j,bi,bj)/lastinterval/rhoConstFresh
     &            *_rA(i,j,bi,bj)

           ENDDO
          ENDDO
ccc        end do
        ENDDO
       ENDDO
      ENDIF

C--   Sum up all contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        WRITE(tile15c,'(2(A,I3),A)') '(bi,bj=', bi, ',', bj, ')'
        WRITE(standardMessageUnit,'(3A,1PE22.14,1X,1PE9.2)')
     &       ' --> objf_shelfice ', tile15c, ' =',
     &       objf_shelfice(bi,bj), mult_shelfice
        tile_fc(bi,bj) = tile_fc(bi,bj)
     &       + mult_shelfice * objf_shelfice(bi,bj)

       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( objf_shelfice, f_shelfice, myThid )
cML      CALL GLOBAL_SUM_TILE_RL( num_shelfice, no_shelfice, myThid )

# ifndef ALLOW_OPENAD
C--   Each process has calculated the global part for itself.
      IF ( ifc .NE. -1 ) THEN

       WRITE(cfname,'(A,i4.4)') 'costfunction_shelfice.',optimcycle
       OPEN(unit=ifc,file=cfname)

C     makes little sense to print this now, because it is zero at this point
c       WRITE(ifc,*) 'fc =', fc
cML       WRITE(ifc,*) 'f_shelfice =', f_shelfice, no_shelfice
       WRITE(ifc,'(A,1PE22.14,1X,1PE9.2)')
     &      'f_shelfice =', f_shelfice, mult_shelfice

       CLOSE(ifc)

      ENDIF
# endif /* ALLOW_OPENAD */

#endif /* ALLOW_COST */

      RETURN
      END
