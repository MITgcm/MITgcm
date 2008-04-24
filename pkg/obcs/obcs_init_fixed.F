C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_init_fixed.F,v 1.7 2008/04/24 07:57:20 mlosch Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_INIT_FIXED( myThid )
C     /==========================================================\
C     | SUBROUTINE OBCS_INIT_FIXED                               |
C     | o Initialise OBCs fixed arrays                           |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"
#ifdef NONLIN_FRSURF
#include "GRID.h"
#include "SURFACE.h"
#endif

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_DEPTHS
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER bi, bj
      INTEGER I, J
#ifdef NONLIN_FRSURF
      INTEGER k
#endif

#ifdef NONLIN_FRSURF

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C- save the initial hFacS at the N & S boundaries :
        DO i=1-Olx,sNx+Olx
          OBNhFac0(i,bi,bj)=0.
          OBShFac0(i,bi,bj)=0.
C  Northern boundary
          IF ( OB_Jn(i,bi,bj).NE.0 ) THEN
            j = OB_Jn(i,bi,bj)
            k = ksurfS(i,j,bi,bj) 
            IF (k.LE.Nr) OBNhFac0(i,bi,bj)=hFacS(i,j,k,bi,bj)
          ENDIF
C  Southern boundary
          IF ( OB_Js(i,bi,bj).NE.0 ) THEN
            j = OB_Js(i,bi,bj)+1
            k = ksurfS(i,j,bi,bj) 
            IF (k.LE.Nr) OBShFac0(i,bi,bj)=hFacS(i,j,k,bi,bj)
          ENDIF
        ENDDO     

C- save the initial hFacW at the E & W boundaries :
        DO j=1-Oly,sNy+Oly
          OBEhFac0(j,bi,bj)=0.
          OBWhFac0(j,bi,bj)=0.
C  Eastern boundary
          IF ( OB_Ie(j,bi,bj).NE.0 ) THEN
            i = OB_Ie(j,bi,bj)
            k = ksurfW(i,j,bi,bj) 
            IF (k.LE.Nr) OBEhFac0(j,bi,bj)=hFacW(i,j,k,bi,bj)
          ENDIF
C  Western boundary
          IF ( OB_Iw(j,bi,bj).NE.0 ) THEN
            i = OB_Iw(j,bi,bj)+1
            k = ksurfW(i,j,bi,bj) 
            IF (k.LE.Nr) OBWhFac0(j,bi,bj)=hFacW(i,j,k,bi,bj)
          ENDIF
        ENDDO     

       ENDDO
      ENDDO

#endif /* NONLIN_FRSURF */    

#endif /* ALLOW_OBCS */
      RETURN
      END
