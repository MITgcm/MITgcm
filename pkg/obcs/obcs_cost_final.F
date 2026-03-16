#include "OBCS_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
c#ifdef ALLOW_COST
c# include "COST_OPTIONS.h"
c#endif

CBOP
C !ROUTINE: OBCS_COST_FINAL

C !INTERFACE: ==========================================================
      SUBROUTINE OBCS_COST_FINAL( ifc, optimcycle, myThid )

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
#endif

C !INPUT PARAMETERS: ===================================================
C     ifc        :: file unit for costfunction_obcs.XXXX
C     optimcycle :: cycle number of the off-line optimization.
C     myThid     :: my Thread Id number
      INTEGER ifc
      INTEGER optimcycle
      INTEGER myThid

#if defined ALLOW_COST && defined ALLOW_CTRL
C    defined ALLOW_OBCS_CONTROL
C    defined ALLOW_OBCS_COST_CONTRIBUTION

C !LOCAL VARIABLES: ====================================================
      INTEGER bi, bj
      _RL f_obcsN, f_obcsS, f_obcsE, f_obcsW, f_ageos
      _RL no_obcsN, no_obcsS, no_obcsE, no_obcsW, no_ageos

      CHARACTER*23 cfname
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      f_obcsN  = 0. _d 0
      f_obcsS  = 0. _d 0
      f_obcsE  = 0. _d 0
      f_obcsW  = 0. _d 0
      f_ageos  = 0. _d 0
      no_obcsN = 0. _d 0
      no_obcsS = 0. _d 0
      no_obcsE = 0. _d 0
      no_obcsW = 0. _d 0
      no_ageos = 0. _d 0

c--   Sum up all contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        tile_fc(bi,bj) = tile_fc(bi,bj)
     &         + mult_obcsN   * objf_obcsN(bi,bj)
     &         + mult_obcsS   * objf_obcsS(bi,bj)
     &         + mult_obcsE   * objf_obcsE(bi,bj)
     &         + mult_obcsW   * objf_obcsW(bi,bj)
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
     &         + mult_ageos   * objf_ageos(bi,bj)
# endif
        f_obcsN  = f_obcsN + objf_obcsN(bi,bj)
        f_obcsS  = f_obcsS + objf_obcsS(bi,bj)
        f_obcsE  = f_obcsE + objf_obcsE(bi,bj)
        f_obcsW  = f_obcsW + objf_obcsW(bi,bj)
        no_obcsN = no_obcsN + num_obcsN(bi,bj)
        no_obcsS = no_obcsS + num_obcsS(bi,bj)
        no_obcsE = no_obcsE + num_obcsE(bi,bj)
        no_obcsW = no_obcsW + num_obcsW(bi,bj)
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
        f_ageos  = f_ageos + objf_ageos(bi,bj)
        no_ageos = no_ageos + num_ageos(bi,bj)
# endif
       ENDDO
      ENDDO

      _GLOBAL_SUM_RL( f_obcsN , myThid )
      _GLOBAL_SUM_RL( f_obcsS , myThid )
      _GLOBAL_SUM_RL( f_obcsE , myThid )
      _GLOBAL_SUM_RL( f_obcsW , myThid )
      _GLOBAL_SUM_RL( no_obcsN, myThid )
      _GLOBAL_SUM_RL( no_obcsS, myThid )
      _GLOBAL_SUM_RL( no_obcsE, myThid )
      _GLOBAL_SUM_RL( no_obcsW, myThid )
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
      _GLOBAL_SUM_RL( f_ageos , myThid )
      _GLOBAL_SUM_RL( no_ageos, myThid )
# endif

C     start printing to STDOUT
      WRITE(msgBuf,'(A,1PE22.14,1X,1PE9.2)')
     &     ' --> f_obcsN    =', f_obcsN, mult_obcsN
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A,1PE22.14,1X,1PE9.2)')
     &     ' --> f_obcsS    =', f_obcsS, mult_obcsS
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A,1PE22.14,1X,1PE9.2)')
     &     ' --> f_obcsE    =', f_obcsE, mult_obcsE
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A,1PE22.14,1X,1PE9.2)')
     &     ' --> f_obcsW    =', f_obcsW, mult_obcsW
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
      WRITE(msgBuf,'(A,1PE22.14,1X,1PE9.2)')
     &     ' --> f_ageos    =', f_ageos, mult_ageos
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
# endif

C     write contributions to costfunction file

C--   Each process has calculated the global part for itself.
      IF ( ifc .NE. -1 ) THEN
       WRITE(cfname,'(A,I4.4)') 'costfunction_obcs.',optimcycle
       OPEN(unit=ifc,file=cfname)
       WRITE(msgBuf,'(A,A)')
     &      'Writing obcs ctrl cost function info to ', cfname
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

       WRITE(ifc,'(A,1PE22.14,1PE22.14,1X,1PE9.2)')
     &      'f_obcsN =', f_obcsN, no_obcsN, mult_obcsN
       WRITE(ifc,'(A,1PE22.14,1PE22.14,1X,1PE9.2)')
     &      'f_obcsS =', f_obcsS, no_obcsS, mult_obcsS
       WRITE(ifc,'(A,1PE22.14,1PE22.14,1X,1PE9.2)')
     &      'f_obcsE =', f_obcsE, no_obcsE, mult_obcsE
       WRITE(ifc,'(A,1PE22.14,1PE22.14,1X,1PE9.2)')
     &      'f_obcsW =', f_obcsW, no_obcsW, mult_obcsW
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
       WRITE(ifc,'(A,1PE22.14,1PE22.14,1X,1PE9.2)')
     &      'f_ageos =', f_ageos, no_ageos, mult_ageos
# endif
       CLOSE(ifc)
      ENDIF

#endif /* ALLOW_COST etc */

      RETURN
      END
