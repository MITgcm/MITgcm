C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_eta.F,v 1.3 2005/12/07 23:55:16 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_APPLY_ETA( bi, bj,
     U                          etaFld,
     I                          myThid )
C     /==========================================================\
C     | S/R OBCS_APPLY_ETA                                       |
C     \==========================================================/
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"

C     == Routine Arguments ==
      INTEGER bi,bj
      _RL etaFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_OBCS
C     == Local variables ==
      INTEGER i,j,i_obc,j_obc

C     Set model variable to zero at the Open Boundaries
      DO i=1-Olx,sNx+Olx
C Northern boundary
        j_obc = OB_Jn(i,bi,bj)
        IF (j_obc.NE.0) etaFld(i,j_obc,bi,bj) = 0. _d 0
C Southern boundary
        j_obc = OB_Js(i,bi,bj)
        IF (j_obc.NE.0) etaFld(i,j_obc,bi,bj) = 0. _d 0
      ENDDO
      DO j=1-Oly,sNy+Oly
C Eastern boundary
        i_obc = OB_Ie(j,bi,bj)
        IF (i_obc.NE.0) etaFld(i_obc,j,bi,bj) = 0. _d 0
C Western boundary
        i_obc = OB_Iw(j,bi,bj)
        IF (i_obc.NE.0) etaFld(i_obc,j,bi,bj) = 0. _d 0
      ENDDO

#ifdef NONLIN_FRSURF
      IF ( nonlinFreeSurf.GT.0 ) THEN

C     Set model variable to OB values on North/South Boundaries
        DO i=1-Olx,sNx+Olx
C Northern boundary
          j_obc = OB_Jn(i,bi,bj)
          IF (j_obc.NE.0) THEN
            IF ( ksurfS(i,j_obc,bi,bj).LE.Nr )
     &        etaFld(i,j_obc,bi,bj) = OBNeta(i,bi,bj)
          ENDIF
C Southern boundary
          j_obc = OB_Js(i,bi,bj)
          IF (j_obc.NE.0) THEN
            IF ( ksurfS(i,j_obc+1,bi,bj).LE.Nr )
     &        etaFld(i,j_obc,bi,bj) = OBSeta(i,bi,bj)
          ENDIF
        ENDDO

C     Set model variables to OB values on East/West Boundaries
        DO j=1-Oly,sNy+Oly
C Eastern boundary
          i_obc = OB_Ie(j,bi,bj)
          IF (i_obc.NE.0) THEN
            IF ( ksurfW(i_obc,j,bi,bj).LE.Nr )
     &        etaFld(i_obc,j,bi,bj) = OBEeta(j,bi,bj)
          ENDIF
C Western boundary
          i_obc=OB_Iw(j,bi,bj)
          IF (i_obc.NE.0) THEN
            IF ( ksurfW(i_obc+1,j,bi,bj).LE.Nr )
     &        etaFld(i_obc,j,bi,bj) = OBEeta(j,bi,bj)
          ENDIF
        ENDDO

      ENDIF
#endif /* NONLIN_FRSURF */

#endif /* ALLOW_OBCS */
      RETURN
      END
