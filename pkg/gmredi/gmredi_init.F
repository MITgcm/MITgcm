C $Header: /u/gcmpack/MITgcm/pkg/gmredi/Attic/gmredi_init.F,v 1.2 2000/11/13 16:35:28 heimbach Exp $

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

#ifdef INCLUDE_DIAGNOSTICS_INTERFACE_CODE
C     Initialize averages to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         CALL TIMEAVER_INI_XYZ(GM_Kwx_T,bi,bj,K,myThid)
         CALL TIMEAVER_INI_XYZ(GM_Kwy_T,bi,bj,K,myThid)
         CALL TIMEAVER_INI_XYZ(GM_Kwz_T,bi,bj,K,myThid)
         GM_TimeAve(k,bi,bj)=0.
        ENDDO
#ifdef GM_VISBECK_VARIABLE_K
        CALL TIMEAVER_INI_XY(Visbeck_K_T, bi,bj,myThid)
#endif
       ENDDO
      ENDDO
#endif /* INCLUDE_DIAGNOSTICS_INTERFACE_CODE */

#endif /* ALLOW_GMREDI */

      return
      end
