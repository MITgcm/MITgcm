C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_init_fixed.F,v 1.2 2001/02/02 21:36:30 adcroft Exp $
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

       ENDDO
      ENDDO

#endif /* ALLOW_OBCS */
      RETURN
      END
