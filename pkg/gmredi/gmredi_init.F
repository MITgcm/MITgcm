C $Header: /u/gcmpack/MITgcm/pkg/gmredi/Attic/gmredi_init.F,v 1.6 2001/12/16 18:54:49 jmc Exp $
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
      INTEGER i,j,k,bi,bj

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C     Initialize arrays in common blocks :
        DO k=1,Nr
         DO j=1-Oly,sNy+OLy
          DO i=1-Olx,sNx+Olx
           Kwx(i,j,k,bi,bj) = 0.
           Kwy(i,j,k,bi,bj) = 0.
           Kwz(i,j,k,bi,bj) = 0.
#ifdef GM_EXTRA_DIAGONAL
           Kuz(i,j,k,bi,bj) = 0.
           Kvz(i,j,k,bi,bj) = 0.
#endif
#ifdef GM_NON_UNITY_DIAGONAL
           Kux(i,j,k,bi,bj) = 0.
           Kvy(i,j,k,bi,bj) = 0.
#endif
#ifdef GM_BOLUS_ADVEC
           GM_PsiX(i,j,k,bi,bj) = 0.
           GM_PsiY(i,j,k,bi,bj) = 0.
#endif
#ifdef GM_VISBECK_VARIABLE_K
          VisbeckK(i,j,bi,bj) = 0.
#endif
          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_TIMEAVE
C     Initialize averages to zero
        CALL TIMEAVE_RESET(GM_Kwx_T,Nr, bi,bj,myThid)
        CALL TIMEAVE_RESET(GM_Kwy_T,Nr, bi,bj,myThid)
        CALL TIMEAVE_RESET(GM_Kwz_T,Nr, bi,bj,myThid)
        DO k=1,Nr
         GM_TimeAve(k,bi,bj)=0.
        ENDDO
#ifdef GM_VISBECK_VARIABLE_K
        CALL TIMEAVE_RESET(Visbeck_K_T, 1, bi,bj,myThid)
#endif
#ifdef GM_BOLUS_ADVEC
        CALL TIMEAVE_RESET(GM_PsiXtave,Nr, bi,bj,myThid)
        CALL TIMEAVE_RESET(GM_PsiYtave,Nr, bi,bj,myThid)
#endif
#endif /* ALLOW_TIMEAVE */

C- end bi,bj loops
       ENDDO
      ENDDO
#endif /* ALLOW_GMREDI */

      return
      end
