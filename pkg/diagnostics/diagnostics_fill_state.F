C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_fill_state.F,v 1.13 2004/12/13 21:55:48 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

       SUBROUTINE DIAGNOSTICS_FILL_STATE( myThid )

       IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "SURFACE.h"

      INTEGER myThid 

#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
      _RL tmpMk(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tmp1k(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER i,j,K,bi,bj
      INTEGER km1
      
      CALL DIAGNOSTICS_FILL(etaN,'ETAN    ',0,1,0,1,1,myThid)
      
      IF ( DIAGNOSTICS_IS_ON('ETANSQ  ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1,sNy
            DO i = 1,sNx
              tmp1k(i,j,bi,bj) = etaN(i,j,bi,bj)*etaN(i,j,bi,bj)
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmp1k,'ETANSQ  ',0,1,0,1,1,myThid)
      ENDIF
      
#ifdef EXACT_CONSERV
      IF ( DIAGNOSTICS_IS_ON('DETADT2 ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1,sNy
            DO i = 1,sNx
              tmp1k(i,j,bi,bj) = dEtaHdt(i,j,bi,bj)*dEtaHdt(i,j,bi,bj)
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmp1k,'DETADT2 ',0,1,0,1,1,myThid)
      ENDIF
#endif
      
      CALL DIAGNOSTICS_FILL(uVel, 'UVEL    ',0,Nr,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(vVel, 'VVEL    ',0,Nr,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(wVel, 'WVEL    ',0,Nr,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(theta,'THETA   ',0,Nr,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(salt, 'SALT    ',0,Nr,0,1,1,myThid)
      
      IF ( DIAGNOSTICS_IS_ON('UVELSQ  ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*uVel(i,j,K,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'UVELSQ  ',0,Nr,0,1,1,myThid)
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('VVELSQ  ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = vVel(i,j,K,bi,bj)*vVel(i,j,K,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'VVELSQ  ',0,Nr,0,1,1,myThid)
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('WVELSQ  ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = wVel(i,j,K,bi,bj)*wVel(i,j,K,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'WVELSQ  ',0,Nr,0,1,1,myThid)
      ENDIF
      
      IF ( DIAGNOSTICS_IS_ON('THETASQ ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) 
     &               = theta(i,j,K,bi,bj)*theta(i,j,K,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'THETASQ ',0,Nr,0,1,1,myThid)
      ENDIF
      
      IF ( DIAGNOSTICS_IS_ON('SALTSQ  ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = salt(i,j,K,bi,bj)*salt(i,j,K,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'SALTSQ  ',0,Nr,0,1,1,myThid)
      ENDIF
      
      IF ( DIAGNOSTICS_IS_ON('UVELVVEL',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*vVel(i,j,K,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'UVELVVEL',0,Nr,0,1,1,myThid)
      ENDIF
      
      IF ( DIAGNOSTICS_IS_ON('UVELTH  ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*0.5 _d 0
     &                  *(theta(i,j,K,bi,bj)+theta(i-1,j,K,bi,bj))
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'UVELTH  ',0,Nr,0,1,1,myThid)
      ENDIF
      
      IF ( DIAGNOSTICS_IS_ON('VVELTH  ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = vVel(i,j,K,bi,bj)*0.5 _d 0
     &                  *(theta(i,j,K,bi,bj)+theta(i,j-1,K,bi,bj))
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'VVELTH  ',0,Nr,0,1,1,myThid)
      ENDIF
      
      IF ( DIAGNOSTICS_IS_ON('WVELTH  ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            km1 = MAX(k-1,1)
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = wVel(i,j,K,bi,bj)*0.5 _d 0
     &                  *(theta(i,j,K,bi,bj)+theta(i,j,km1,bi,bj))
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'WVELTH  ',0,Nr,0,1,1,myThid)
      ENDIF
      
      IF ( DIAGNOSTICS_IS_ON('UVELSLT ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*0.5 _d 0
     &                  *(salt(i,j,K,bi,bj)+salt(i-1,j,K,bi,bj))
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'UVELSLT ',0,Nr,0,1,1,myThid)
      ENDIF
      
      IF ( DIAGNOSTICS_IS_ON('VVELSLT ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = vVel(i,j,K,bi,bj)*0.5 _d 0
     &                  *(salt(i,j,K,bi,bj)+salt(i,j-1,K,bi,bj))
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'VVELSLT ',0,Nr,0,1,1,myThid)
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('WVELSLT ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            km1 = MAX(k-1,1)
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = wVel(i,j,K,bi,bj)*0.5 _d 0
     &                  *(salt(i,j,K,bi,bj)+salt(i,j,km1,bi,bj))
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'WVELSLT ',0,Nr,0,1,1,myThid)
      ENDIF
      
      IF ( DIAGNOSTICS_IS_ON('UVELMASS',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) 
     &               = uVel(i,j,K,bi,bj)*hFacW(i,j,K,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'UVELMASS',0,Nr,0,1,1,myThid)
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('VVELMASS',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) 
     &               = vVel(i,j,K,bi,bj)*hFacS(i,j,K,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'VVELMASS',0,Nr,0,1,1,myThid)
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('UTHMASS ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*0.5 _d 0
     &                  *(theta(i,j,K,bi,bj)+theta(i-1,j,K,bi,bj))
     &                  * hFacW(i,j,K,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'UTHMASS ',0,Nr,0,1,1,myThid)
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('VTHMASS ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = vVel(i,j,K,bi,bj)*0.5 _d 0
     &                  *(theta(i,j,K,bi,bj)+theta(i,j-1,K,bi,bj))
     &                  * hFacS(i,j,K,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'VTHMASS ',0,Nr,0,1,1,myThid)
      ENDIF
      
      IF ( DIAGNOSTICS_IS_ON('USLTMASS',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = uVel(i,j,K,bi,bj)*0.5 _d 0
     &                  *(salt(i,j,K,bi,bj)+salt(i-1,j,K,bi,bj))
     &                  * hFacW(i,j,K,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'USLTMASS',0,Nr,0,1,1,myThid)
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('VSLTMASS',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO K=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx
                tmpMk(i,j,K,bi,bj) = vVel(i,j,K,bi,bj)*0.5 _d 0
     &                  *(salt(i,j,K,bi,bj)+salt(i,j-1,K,bi,bj))
     &                  * hFacS(i,j,K,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk,'VSLTMASS',0,Nr,0,1,1,myThid)
      ENDIF
      
      
#endif /* ALLOW_DIAGNOSTICS */
      
      RETURN 
      END
