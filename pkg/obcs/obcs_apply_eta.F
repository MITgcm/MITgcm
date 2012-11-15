C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_eta.F,v 1.9 2012/11/15 15:55:42 dimitri Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_APPLY_ETA
C     !INTERFACE:
      SUBROUTINE OBCS_APPLY_ETA( bi, bj,
     U                           etaFld,
     I                           myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R OBCS_APPLY_ETA
C     |   Apply surface position anomaly (Eta) OB values
C     |   to corresponding field array
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C    bi, bj   :: indices of current tile
C    etaFld   :: surface r-position anomaly field
C    myThid   :: my Thread Id number
      INTEGER bi, bj
      _RL etaFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid
CEOP

#ifdef NONLIN_FRSURF
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i, j
      INTEGER Iobc, Jobc

c     IF ( nonlinFreeSurf.GT.0 ) THEN

C     Set model variable to OB values on North/South Boundaries
        IF ( tileHasOBN(bi,bj) ) THEN
C Northern boundary
         DO i=1-OLx,sNx+OLx
          Jobc = OB_Jn(i,bi,bj)
          IF ( Jobc.NE.OB_indexNone ) THEN
            IF ( kSurfS(i,Jobc,bi,bj).LE.Nr )
     &        etaFld(i,Jobc,bi,bj) = OBNeta(i,bi,bj)
          ENDIF
         ENDDO
        ENDIF
        IF ( tileHasOBS(bi,bj) ) THEN
C Southern boundary
         DO i=1-OLx,sNx+OLx
          Jobc = OB_Js(i,bi,bj)
          IF ( Jobc.NE.OB_indexNone ) THEN
            IF ( kSurfS(i,Jobc+1,bi,bj).LE.Nr )
     &        etaFld(i,Jobc,bi,bj) = OBSeta(i,bi,bj)
          ENDIF
         ENDDO
        ENDIF

C     Set model variables to OB values on East/West Boundaries
        IF ( tileHasOBE(bi,bj) ) THEN
C Eastern boundary
         DO j=1-OLy,sNy+OLy
          Iobc = OB_Ie(j,bi,bj)
          IF ( Iobc.NE.OB_indexNone ) THEN
            IF ( kSurfW(Iobc,j,bi,bj).LE.Nr )
     &        etaFld(Iobc,j,bi,bj) = OBEeta(j,bi,bj)
          ENDIF
         ENDDO
        ENDIF
        IF ( tileHasOBW(bi,bj) ) THEN
C Western boundary
         DO j=1-OLy,sNy+OLy
          Iobc = OB_Iw(j,bi,bj)
          IF ( Iobc.NE.OB_indexNone ) THEN
            IF ( kSurfW(Iobc+1,j,bi,bj).LE.Nr )
     &        etaFld(Iobc,j,bi,bj) = OBWeta(j,bi,bj)
          ENDIF
         ENDDO
        ENDIF

c     ENDIF
#endif /* NONLIN_FRSURF */

      RETURN
      END
