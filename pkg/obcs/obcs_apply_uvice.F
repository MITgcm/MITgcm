C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_uvice.F,v 1.1 2007/10/26 02:00:47 dimitri Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_APPLY_UVICE( bi, bj,
     O                          uFld, vFld,
     I                          myThid )
C     /==========================================================\
C     | S/R OBCS_APPLY_UVICE                                     |
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
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      INTEGER myThid

#if (defined (ALLOW_OBCS) && defined (ALLOW_SEAICE))

C     == Local variables ==
      INTEGER I,J,K

      K = 1

C     Set model variables to OB values on North/South Boundaries
#ifdef ALLOW_OBCS_NORTH
      DO I=1-Olx,sNx+Olx
C Northern boundary
       IF (OB_Jn(I,bi,bj).NE.0) THEN
        vFld(I,OB_Jn(I,bi,bj),1,bi,bj)=OBNvice(I,bi,bj)
     &                              *_maskS(I,OB_Jn(I,bi,bj),K,bi,bj)
        uFld(I,OB_Jn(I,bi,bj),1,bi,bj)=OBNuice(I,bi,bj)
     &                              *_maskW(I,OB_Jn(I,bi,bj),K,bi,bj)
       ENDIF
      ENDDO
#endif
#ifdef ALLOW_OBCS_SOUTH
      DO I=1-Olx,sNx+Olx
C Southern boundary
       IF (OB_Js(I,bi,bj).NE.0) THEN
        vFld(I,OB_Js(I,bi,bj)+1,1,bi,bj)=OBSvice(I,bi,bj)
     &                              *_maskS(I,OB_Js(I,bi,bj)+1,K,bi,bj)
        uFld(I,OB_Js(I,bi,bj),1,bi,bj)=OBSuice(I,bi,bj)
     &                              *_maskW(I,OB_Js(I,bi,bj),K,bi,bj)
       ENDIF
      ENDDO
#endif

C     Set model variables to OB values on East/West Boundaries
#ifdef ALLOW_OBCS_EAST
      DO J=1-Oly,sNy+Oly
C Eastern boundary
       IF (OB_Ie(J,bi,bj).NE.0) THEN
        uFld(OB_Ie(J,bi,bj),J,1,bi,bj)=OBEuice(J,bi,bj)
     &                              *_maskW(OB_Ie(J,bi,bj),J,K,bi,bj)
        vFld(OB_Ie(J,bi,bj),J,1,bi,bj)=OBEvice(J,bi,bj)
     &                              *_maskS(OB_Ie(J,bi,bj),J,K,bi,bj)
       ENDIF
      ENDDO
#endif
#ifdef ALLOW_OBCS_WEST
      DO J=1-Oly,sNy+Oly
C Western boundary
       IF (OB_Iw(J,bi,bj).NE.0) THEN
        uFld(OB_Iw(J,bi,bj)+1,1,J,bi,bj)=OBWuice(J,bi,bj)
     &                              *_maskW(OB_Iw(J,bi,bj)+1,J,K,bi,bj)
        vFld(OB_Iw(J,bi,bj),J,1,bi,bj)=OBWvice(J,bi,bj)
     &                              *_maskS(OB_Iw(J,bi,bj),J,K,bi,bj)
       ENDIF
      ENDDO
#endif

#endif /* defined (ALLOW_OBCS) && defined (ALLOW_SEAICE) */

      RETURN
      END
