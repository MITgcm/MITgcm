C $Header: /u/gcmpack/MITgcm/model/src/Attic/apply_obcs2.F,v 1.5 1999/08/30 18:29:25 adcroft Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE APPLY_OBCS2( bi, bj, K,
     I                       myThid )
C     /==========================================================\
C     | S/R APPLY_OBCS2                                          |
C     | o                                                        |
C     \==========================================================/
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

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER i,j

C     Set model variables to OB values on North/South Boundaries
      DO I=1-Olx,sNx+Olx
C Northern boundary
       IF (OB_Jn(I,bi,bj).NE.0) THEN
        gVnm1(I,OB_Jn(I,bi,bj),K,bi,bj)=OBNv(I,K,bi,bj)
     &                              *_maskS(I,OB_Jn(I,bi,bj),K,bi,bj)
        gUnm1(I,OB_Jn(I,bi,bj),K,bi,bj)=OBNu(I,K,bi,bj)
     &                              *_maskW(I,OB_Jn(I,bi,bj),K,bi,bj)
        gVnm1(I,OB_Jn(I,bi,bj)+1,K,bi,bj)=OBNv(I,K,bi,bj)
     &                              *_maskS(I,OB_Jn(I,bi,bj),K,bi,bj)
       ENDIF
C Southern boundary
       IF (OB_Js(I,bi,bj).NE.0) THEN
        gVnm1(I,OB_Js(I,bi,bj)+1,K,bi,bj)=OBSv(I,K,bi,bj)
     &                              *_maskS(I,OB_Js(I,bi,bj)+1,K,bi,bj)
        gUnm1(I,OB_Js(I,bi,bj),K,bi,bj)=OBSu(I,K,bi,bj)
     &                              *_maskW(I,OB_Js(I,bi,bj),K,bi,bj)
        gVnm1(I,OB_Js(I,bi,bj),K,bi,bj)=OBSv(I,K,bi,bj)
     &                              *_maskS(I,OB_Js(I,bi,bj)+1,K,bi,bj)
       ENDIF
      ENDDO



C     Set model variables to OB values on East/West Boundaries
      DO J=1-Oly,sNy+Oly
C Eastern boundary
       IF (OB_Ie(J,bi,bj).NE.0) THEN
        gUnm1(OB_Ie(J,bi,bj),J,K,bi,bj)=OBEu(J,K,bi,bj)
     &                              *_maskW(OB_Ie(J,bi,bj),J,K,bi,bj)
        gVnm1(OB_Ie(J,bi,bj),J,K,bi,bj)=OBEv(J,K,bi,bj)
     &                              *_maskS(OB_Ie(J,bi,bj),J,K,bi,bj)
        gUnm1(OB_Ie(J,bi,bj)+1,J,K,bi,bj)=OBEu(J,K,bi,bj)
     &                              *_maskW(OB_Ie(J,bi,bj),J,K,bi,bj)
       ENDIF
C Western boundary
       IF (OB_Iw(J,bi,bj).NE.0) THEN
        gUnm1(OB_Iw(J,bi,bj)+1,J,K,bi,bj)=OBWu(J,K,bi,bj)
     &                              *_maskW(OB_Iw(J,bi,bj)+1,J,K,bi,bj)
        gVnm1(OB_Iw(J,bi,bj),J,K,bi,bj)=OBWv(J,K,bi,bj)
     &                              *_maskS(OB_Iw(J,bi,bj),J,K,bi,bj)
        gUnm1(OB_Iw(J,bi,bj),J,K,bi,bj)=OBWu(J,K,bi,bj)
     &                              *_maskW(OB_Iw(J,bi,bj)+1,J,K,bi,bj)
       ENDIF
      ENDDO

#endif
      RETURN
      END
