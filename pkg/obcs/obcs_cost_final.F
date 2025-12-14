#include "OBCS_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
c#ifdef ALLOW_COST
c# include "COST_OPTIONS.h"
c#endif

CBOP
C !ROUTINE: OBCS_COST_DRIVER

C !INTERFACE: ==========================================================
      SUBROUTINE OBCS_COST_FINAL( myThid )

C !DESCRIPTION:
C     Compute cost function contribution (Tikhonov regularisation)
C     of pkg/obcs and print to STDOUT and costfunction_obcs-file.

C !USES: ===============================================================
      IMPLICIT NONE
C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_COST
# include "cost.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OBCS.h"
# include "OPTIMCYCLE.h"
#endif

C !INPUT PARAMETERS: ===================================================
      INTEGER myThid

#ifdef ALLOW_COST
C    defined ALLOW_OBCS_CONTROL
C    defined ALLOW_OBCS_COST_CONTRIBUTION

C !FUNCTIONS: ==========================================================
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD

C !LOCAL VARIABLES: ====================================================
      INTEGER bi, bj
      _RL f_obcsn, f_obcss, f_obcsw, f_obcse, f_ageos
      _RL no_obcsn, no_obcss, no_obcsw, no_obcse, no_ageos

      INTEGER ifc
      CHARACTER*22 cfname
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      ifc = 30

      f_obcsn  = 0. _d 0
      f_obcss  = 0. _d 0
      f_obcsw  = 0. _d 0
      f_obcse  = 0. _d 0
      f_ageos  = 0. _d 0
      no_obcsn = 0. _d 0
      no_obcss = 0. _d 0
      no_obcsw = 0. _d 0
      no_obcse = 0. _d 0
      no_ageos = 0. _d 0

c--   Sum up all contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        tile_fc(bi,bj) = tile_fc(bi,bj)
     &         + mult_obcsn   * objf_obcsn(bi,bj)
     &         + mult_obcss   * objf_obcss(bi,bj)
     &         + mult_obcsw   * objf_obcsw(bi,bj)
     &         + mult_obcse   * objf_obcse(bi,bj)
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
     &         + mult_ageos   * objf_ageos(bi,bj)
# endif
        f_obcsn  = f_obcsn + objf_obcsn(bi,bj)
        f_obcss  = f_obcss + objf_obcss(bi,bj)
        f_obcsw  = f_obcsw + objf_obcsw(bi,bj)
        f_obcse  = f_obcse + objf_obcse(bi,bj)
        no_obcsn = no_obcsn + num_obcsn(bi,bj)
        no_obcss = no_obcss + num_obcss(bi,bj)
        no_obcse = no_obcse + num_obcse(bi,bj)
        no_obcsw = no_obcsw + num_obcsw(bi,bj)
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
        f_ageos  = f_ageos + objf_ageos(bi,bj)
        no_ageos = no_ageos + num_ageos(bi,bj)
# endif
       ENDDO
      ENDDO

      _GLOBAL_SUM_RL( f_obcsn , myThid )
      _GLOBAL_SUM_RL( f_obcss , myThid )
      _GLOBAL_SUM_RL( f_obcsw , myThid )
      _GLOBAL_SUM_RL( f_obcse , myThid )
      _GLOBAL_SUM_RL( no_obcsn, myThid )
      _GLOBAL_SUM_RL( no_obcss, myThid )
      _GLOBAL_SUM_RL( no_obcsw, myThid )
      _GLOBAL_SUM_RL( no_obcse, myThid )
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
      _GLOBAL_SUM_RL( f_ageos , myThid )
      _GLOBAL_SUM_RL( no_ageos, myThid )
# endif

C     start printing to STDOUT
      WRITE(msgBuf,'(A,D22.15)') ' --> f_obcsn    =',f_obcsn
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A,D22.15)') ' --> f_obcss    =',f_obcss
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A,D22.15)') ' --> f_obcsw    =',f_obcsw
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A,D22.15)') ' --> f_obcse    =',f_obcse
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
      WRITE(msgBuf,'(A,D22.15)') ' --> f_ageos    =',f_ageos
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
# endif

C     write contributions to costfunction file

C--   Each process has calculated the global part for itself.
      IF ( MASTER_CPU_THREAD(myThid) .AND. obcsWriteCostFunction ) THEN
       WRITE(cfname,'(A,I4.4)') 'costfunction_obcs',optimcycle
       OPEN(unit=ifc,file=cfname)
       WRITE(msgBuf,'(A,A)')
     &      'Writing obcs ctrl cost function info to ', cfname
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

       WRITE(ifc,'(A,2D22.15)') 'f_obcsn =', f_obcsn, no_obcsn
       WRITE(ifc,'(A,2D22.15)') 'f_obcss =', f_obcss, no_obcss
       WRITE(ifc,'(A,2D22.15)') 'f_obcsw =', f_obcsw, no_obcsw
       WRITE(ifc,'(A,2D22.15)') 'f_obcse =', f_obcse, no_obcse
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
       WRITE(ifc,'(A,2D22.15)') 'f_ageos =', f_ageos, no_ageos
# endif
       CLOSE(ifc)
C     Do it only once per run, otherwise grdchk will overwrite or append
C     files in an uncontrolled way.
       obcsWriteCostFunction = .FALSE.
      ENDIF

#endif /* ALLOW_COST etc */

      RETURN
      END
