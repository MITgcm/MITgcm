C $Header: /u/gcmpack/MITgcm/pkg/gmredi/Attic/gmredi_init.F,v 1.5 2001/03/06 18:01:07 jmc Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

      SUBROUTINE GMREDI_INIT( myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_INIT                                   |
C     | o Routine to initialize GM/Redi variables and constants. |
C     |==========================================================|
C     | Initialize GM/Redi parameters, read in data.gmredi       |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GMREDI.h"
#include "GMREDI_DIAGS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of KPP_INIT
      INTEGER myThid

#ifdef ALLOW_GMREDI

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER k,bi,bj

#ifdef ALLOW_TIMEAVE
C     Initialize averages to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL TIMEAVE_RESET(GM_Kwx_T,Nr, bi,bj,myThid)
        CALL TIMEAVE_RESET(GM_Kwy_T,Nr, bi,bj,myThid)
        CALL TIMEAVE_RESET(GM_Kwz_T,Nr, bi,bj,myThid)
        DO k=1,Nr
         GM_TimeAve(k,bi,bj)=0.
        ENDDO
#ifdef GM_VISBECK_VARIABLE_K
        CALL TIMEAVE_RESET(Visbeck_K_T, 1, bi,bj,myThid)
#endif
       ENDDO
      ENDDO
#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_GMREDI */

      return
      end
