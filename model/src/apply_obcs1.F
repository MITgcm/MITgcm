C $Header: /u/gcmpack/MITgcm/model/src/Attic/apply_obcs1.F,v 1.1 1998/12/08 19:44:28 adcroft Exp $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R APPLY_OBCS                                           |
C     | o Calculate the gradient of the surface pressure         |
C     \==========================================================/
      SUBROUTINE APPLY_OBCS1( bi, bj, K,
     I                        myThid )
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"
#include "DYNVARS.h"

C     == Routine Arguments ==
      INTEGER bi,bj,K
      INTEGER myThid
C     == Local variables ==
      INTEGER i,j

C     Set model variables to OB values on North/South Boundaries
      DO I=1-Olx,sNx+Olx
C Northern boundary
       IF (OB_Jn(I,bi,bj).NE.0) THEN
        uVel(I,OB_Jn(I,bi,bj),K,bi,bj)=OBNu(I,K,bi,bj)
     &                              *_maskW(I,OB_Jn(I,bi,bj),K,bi,bj)
        vVel(I,OB_Jn(I,bi,bj),K,bi,bj)=OBNv(I,K,bi,bj)
     &                              *_maskS(I,OB_Jn(I,bi,bj),K,bi,bj)
        theta(I,OB_Jn(I,bi,bj),K,bi,bj)=OBNt(I,K,bi,bj)
     &                              *_maskS(I,OB_Jn(I,bi,bj),K,bi,bj)
c       vVel(I,OB_Jn(I,bi,bj)+1,K,bi,bj)=OBNv(I,K,bi,bj)
c    &                              *_maskS(I,OB_Jn(I,bi,bj)+1,K,bi,bj)
       ENDIF
C Southern boundary
       IF (OB_Js(I,bi,bj).NE.0) THEN
        uVel(I,OB_Js(I,bi,bj),K,bi,bj)=OBSu(I,K,bi,bj)
     &                              *_maskW(I,OB_Js(I,bi,bj),K,bi,bj)
        vVel(I,OB_Js(I,bi,bj)+1,K,bi,bj)=OBSv(I,K,bi,bj)
     &                              *_maskS(I,OB_Js(I,bi,bj)+1,K,bi,bj)
        theta(I,OB_Js(I,bi,bj),K,bi,bj)=OBSt(I,K,bi,bj)
     &                              *_maskS(I,OB_Js(I,bi,bj)+1,K,bi,bj)
c       vVel(I,OB_Js(I,bi,bj),K,bi,bj)=OBSv(I,K,bi,bj)
c    &                              *_maskS(I,OB_Js(I,bi,bj),K,bi,bj)
       ENDIF
      ENDDO

C     Set model variables to OB values on East/West Boundaries
      DO J=1-Oly,sNy+Oly
C Eastern boundary
       IF (OB_Ie(J,bi,bj).NE.0) THEN
        uVel(OB_Ie(J,bi,bj),J,K,bi,bj)=OBEu(J,K,bi,bj)
     &                              *_maskW(OB_Ie(J,bi,bj),J,K,bi,bj)
        vVel(OB_Ie(J,bi,bj),J,K,bi,bj)=OBEv(J,K,bi,bj)
     &                              *_maskS(OB_Ie(J,bi,bj),J,K,bi,bj)
        theta(OB_Ie(J,bi,bj),J,K,bi,bj)=OBEt(J,K,bi,bj)
     &                              *_maskW(OB_Ie(J,bi,bj),J,K,bi,bj)
c       uVel(OB_Ie(J,bi,bj)+1,J,K,bi,bj)=OBEu(J,K,bi,bj)
c    &                              *_maskW(OB_Ie(J,bi,bj)+1,J,K,bi,bj)
       ENDIF
C Western boundary
       IF (OB_Iw(J,bi,bj).NE.0) THEN
        uVel(OB_Iw(J,bi,bj)+1,J,K,bi,bj)=OBWu(J,K,bi,bj)
     &                              *_maskW(OB_Iw(J,bi,bj)+1,J,K,bi,bj)
        vVel(OB_Iw(J,bi,bj),J,K,bi,bj)=OBWv(J,K,bi,bj)
     &                              *_maskS(OB_Iw(J,bi,bj),J,K,bi,bj)
        theta(OB_Iw(J,bi,bj),J,K,bi,bj)=OBWt(J,K,bi,bj)
     &                              *_maskW(OB_Iw(J,bi,bj)+1,J,K,bi,bj)
c       uVel(OB_Iw(J,bi,bj),J,K,bi,bj)=OBWu(J,K,bi,bj)
c    &                              *_maskW(OB_Iw(J,bi,bj),J,K,bi,bj)
       ENDIF
      ENDDO

      RETURN
      END

