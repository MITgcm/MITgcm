C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_obcs.F,v 1.5 1999/08/30 18:29:26 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_OBCS( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_OBCS                                      |
C     | o Initialise OB arrays                                   |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_DEPTHS
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_OBCS

C     == Local variables ==
C     xG, yG - Global coordinate location.
C     zG
C     zUpper - Work arrays for upper and lower
C     zLower   cell-face heights.
C     phi    - Temporary scalar
C     iG, jG - Global coordinate index
C     bi,bj  - Loop counters
C     zUpper - Temporary arrays holding z coordinates of
C     zLower   upper and lower faces.
C     I,i,K
      INTEGER iG, jG, iGm, jGm
      INTEGER bi, bj
      INTEGER  I,  J, K
c

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
         jG = myYGlobalLo-1+(bj-1)*sNy+J
         jGm = 1+mod( jG-1+Ny , Ny )
         DO I=1,sNx
          iG = myXGlobalLo-1+(bi-1)*sNx+I
          iGm = 1+mod( iG-1+Nx , Nx )
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
          IF (jG.EQ.OB_Jnorth(iGm)) OB_Jn(I,bi,bj)=J
          IF (jG.EQ.OB_Jsouth(iGm)) OB_Js(I,bi,bj)=J
         ENDDO
        ENDDO

        DO K=1,Nr
         DO I=1-Olx,sNx+Olx
          OBNu(I,K,bi,bj)=0.
          OBNv(I,K,bi,bj)=0.
          OBNt(I,K,bi,bj)=0.
          OBSu(I,K,bi,bj)=0.
          OBSv(I,K,bi,bj)=0.
          OBSt(I,K,bi,bj)=0.
#ifdef ALLOW_NONHYDROSTATIC
          OBNw(I,K,bi,bj)=0.
          OBSw(I,K,bi,bj)=0.
#endif
         ENDDO
         DO J=1-Oly,sNy+Oly
          OBEu(J,K,bi,bj)=0.
          OBEv(J,K,bi,bj)=0.
          OBEt(J,K,bi,bj)=0.
          OBWu(J,K,bi,bj)=0.
          OBWv(J,K,bi,bj)=0.
          OBWt(J,K,bi,bj)=0.
#ifdef ALLOW_NONHYDROSTATIC
          OBEw(J,K,bi,bj)=0.
          OBWw(J,K,bi,bj)=0.
#endif
         ENDDO
        ENDDO

       ENDDO
      ENDDO

#endif
      RETURN
      END
