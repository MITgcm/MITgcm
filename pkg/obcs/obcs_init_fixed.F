C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_init_fixed.F,v 1.4 2002/01/30 04:22:31 jmc Exp $
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
      INTEGER iG, jG, iGm, jGm
      INTEGER bi, bj
      INTEGER I, J, K

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        DO I=1-Olx,sNx+Olx
         OB_Jn(I,bi,bj)=0
         OB_Js(I,bi,bj)=0
        ENDDO

        DO J=1-Oly,sNy+Oly
         OB_Ie(J,bi,bj)=0
         OB_Iw(J,bi,bj)=0
        ENDDO

        DO J=1-Oly,sNy+Oly
C convert from local y index J to global y index jG
         jG = myYGlobalLo-1+(bj-1)*sNy+J
C use periodicity to deal with out of range points caused by the overlaps.
C they will be excluded by the mask in any case, but this saves array
C out-of-bounds errors here.
         jGm = 1+mod( jG-1+Ny , Ny )
C loop over local x index I
         DO I=1,sNx
          iG = myXGlobalLo-1+(bi-1)*sNx+I
          iGm = 1+mod( iG-1+Nx , Nx )
C OB_Ieast(jGm) allows for the eastern boundary to be at variable x locations
          IF (iG.EQ.OB_Ieast(jGm))  OB_Ie(J,bi,bj)=I
          IF (iG.EQ.OB_Iwest(jGm))  OB_Iw(J,bi,bj)=I
         ENDDO
        ENDDO
        DO J=1,sNy
         jG = myYGlobalLo-1+(bj-1)*sNy+J
         jGm = 1+mod( jG-1+Ny , Ny )
         DO I=1-Olx,sNx+Olx
          iG = myXGlobalLo-1+(bi-1)*sNx+I
          iGm = 1+mod( iG-1+Nx , Nx )
C OB_Jnorth(iGm) allows for the northern boundary to be at variable y locations
          IF (jG.EQ.OB_Jnorth(iGm)) OB_Jn(I,bi,bj)=J
          IF (jG.EQ.OB_Jsouth(iGm)) OB_Js(I,bi,bj)=J
         ENDDO
        ENDDO

#ifdef NONLIN_FRSURF

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

#endif /* NONLIN_FRSURF */    

       ENDDO
      ENDDO

#endif /* ALLOW_OBCS */
      RETURN
      END
