C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_predictor.F,v 1.3 1998/08/22 17:51:08 cnh Exp $

#include "CPP_EEOPTIONS.h"

C     /==========================================================\
C     | S/R INI_PREDICTOR                                        |
C     | o Copies Gu,Gv,Gt and Gs in U,V,theta and salt           |
C     |   This is to initialise the predictor-corrector time-    |
C     |   stepping scheme                                        |
C     \==========================================================/
      SUBROUTINE INI_PREDICTOR( 
     I                       myThid )
      implicit none
! Common
#include "SIZE.h"
#include "DYNVARS.h"
C     == Routine Arguments ==
      INTEGER myThid
C     == Local variables ==
      INTEGER i,j,k,bi,bj

      DO bj=1,nSy
       DO bi=1,nSx
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           gUNm1(i,j,k,bi,bj)=uVel(i,j,k,bi,bj)
           gVNm1(i,j,k,bi,bj)=vVel(i,j,k,bi,bj)
           gTNm1(i,j,k,bi,bj)=theta(i,j,k,bi,bj)
           gSNm1(i,j,k,bi,bj)=salt(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
