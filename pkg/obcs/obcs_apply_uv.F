C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_uv.F,v 1.2 2001/02/02 21:36:29 adcroft Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_APPLY_UV( bi, bj, K,
     U                          uFld, vFld,
     I                          myThid )
C     /==========================================================\
C     | S/R OBCS_APPLY_UV                                        |
C     \==========================================================/
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"

C     == Routine Arguments ==
      INTEGER bi,bj,K
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER i,j

C     Set model variables to OB values on North/South Boundaries
      DO I=1-Olx,sNx+Olx
C Northern boundary
       IF (OB_Jn(I,bi,bj).NE.0) THEN
        vFld(I,OB_Jn(I,bi,bj),K,bi,bj)=OBNv(I,K,bi,bj)
     &                              *_maskS(I,OB_Jn(I,bi,bj),K,bi,bj)
        uFld(I,OB_Jn(I,bi,bj),K,bi,bj)=OBNu(I,K,bi,bj)
     &                              *_maskW(I,OB_Jn(I,bi,bj),K,bi,bj)
        vFld(I,OB_Jn(I,bi,bj)+1,K,bi,bj)=OBNv(I,K,bi,bj)
     &                              *_maskS(I,OB_Jn(I,bi,bj),K,bi,bj)
       ENDIF
C Southern boundary
       IF (OB_Js(I,bi,bj).NE.0) THEN
        vFld(I,OB_Js(I,bi,bj)+1,K,bi,bj)=OBSv(I,K,bi,bj)
     &                              *_maskS(I,OB_Js(I,bi,bj)+1,K,bi,bj)
        uFld(I,OB_Js(I,bi,bj),K,bi,bj)=OBSu(I,K,bi,bj)
     &                              *_maskW(I,OB_Js(I,bi,bj),K,bi,bj)
        vFld(I,OB_Js(I,bi,bj),K,bi,bj)=OBSv(I,K,bi,bj)
     &                              *_maskS(I,OB_Js(I,bi,bj)+1,K,bi,bj)
       ENDIF
      ENDDO



C     Set model variables to OB values on East/West Boundaries
      DO J=1-Oly,sNy+Oly
C Eastern boundary
       IF (OB_Ie(J,bi,bj).NE.0) THEN
        uFld(OB_Ie(J,bi,bj),J,K,bi,bj)=OBEu(J,K,bi,bj)
     &                              *_maskW(OB_Ie(J,bi,bj),J,K,bi,bj)
        vFld(OB_Ie(J,bi,bj),J,K,bi,bj)=OBEv(J,K,bi,bj)
     &                              *_maskS(OB_Ie(J,bi,bj),J,K,bi,bj)
        uFld(OB_Ie(J,bi,bj)+1,J,K,bi,bj)=OBEu(J,K,bi,bj)
     &                              *_maskW(OB_Ie(J,bi,bj),J,K,bi,bj)
       ENDIF
C Western boundary
       IF (OB_Iw(J,bi,bj).NE.0) THEN
        uFld(OB_Iw(J,bi,bj)+1,J,K,bi,bj)=OBWu(J,K,bi,bj)
     &                              *_maskW(OB_Iw(J,bi,bj)+1,J,K,bi,bj)
        vFld(OB_Iw(J,bi,bj),J,K,bi,bj)=OBWv(J,K,bi,bj)
     &                              *_maskS(OB_Iw(J,bi,bj),J,K,bi,bj)
        uFld(OB_Iw(J,bi,bj),J,K,bi,bj)=OBWu(J,K,bi,bj)
     &                              *_maskW(OB_Iw(J,bi,bj)+1,J,K,bi,bj)
       ENDIF
      ENDDO

#endif
      RETURN
      END
