C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_init_varia.F,v 1.4 2013/07/11 14:33:23 m_bates Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: GMREDI_INIT_VARIA
C     !INTERFACE:
      SUBROUTINE GMREDI_INIT_VARIA( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_INIT_VARIA
C     | o Routine to initialize GM/Redi variables
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GMREDI.h"
#include "GMREDI_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_GMREDI

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j,k,bi,bj

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C     Initialize arrays in common blocks :
        DO k=1,Nr
         DO j=1-Oly,sNy+OLy
          DO i=1-Olx,sNx+Olx
           Kwx(i,j,k,bi,bj) = 0. _d 0
           Kwy(i,j,k,bi,bj) = 0. _d 0
           Kwz(i,j,k,bi,bj) = 0. _d 0
#ifdef GM_EXTRA_DIAGONAL
           Kuz(i,j,k,bi,bj) = 0. _d 0
           Kvz(i,j,k,bi,bj) = 0. _d 0
#endif
#ifdef GM_NON_UNITY_DIAGONAL
           Kux(i,j,k,bi,bj) = 0. _d 0
           Kvy(i,j,k,bi,bj) = 0. _d 0
#endif
#ifdef GM_BOLUS_ADVEC
           GM_PsiX(i,j,k,bi,bj) = 0. _d 0
           GM_PsiY(i,j,k,bi,bj) = 0. _d 0
#endif
#ifdef GM_VISBECK_VARIABLE_K
          VisbeckK(i,j,bi,bj) = 0. _d 0
#endif
#ifdef GM_K3D
          K3D(i,j,k,bi,bj) = 0. _d 0
#endif
          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_TIMEAVE
C     Initialize averages to zero
        CALL TIMEAVE_RESET(GM_Kwx_T,Nr, bi,bj,myThid)
        CALL TIMEAVE_RESET(GM_Kwy_T,Nr, bi,bj,myThid)
        CALL TIMEAVE_RESET(GM_Kwz_T,Nr, bi,bj,myThid)
        GM_timeAve(bi,bj) = 0. _d 0
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

C--   write GM scaling factors to file:
      IF ( GM_iso1dFile .NE. ' ' ) THEN
        CALL WRITE_GLVEC_RS( 'GM_isoFac1d', ' ', GM_isoFac1d,
     I                        Nr, -1, myThid )
      ENDIF
      IF ( GM_bol1dFile .NE. ' ' ) THEN
        CALL WRITE_GLVEC_RS( 'GM_bolFac1d', ' ', GM_bolFac1d,
     I                        Nr, -1, myThid )
      ENDIF
      IF ( GM_iso2dFile .NE. ' ' ) THEN
        CALL WRITE_FLD_XY_RS( 'GM_isoFac2d',' ',GM_isoFac2d,-1,myThid )
      ENDIF
      IF ( GM_bol2dFile .NE. ' ' ) THEN
        CALL WRITE_FLD_XY_RS( 'GM_bolFac2d',' ',GM_bolFac2d,-1,myThid )
      ENDIF
#endif /* ALLOW_GMREDI */


#ifdef GM_K3D
      IF (.NOT.( startTime.EQ.baseTime .AND. nIter0.EQ.0
     &     .AND. pickupSuff.EQ.' ' )) THEN
        IF (GM_useK3D) CALL GMREDI_READ_PICKUP( niter0, myThid )
      ENDIF
#endif
      RETURN
      END
