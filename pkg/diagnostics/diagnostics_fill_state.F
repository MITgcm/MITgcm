C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_fill_state.F,v 1.23 2005/07/13 00:46:07 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGNOSTICS_FILL_STATE
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_FILL_STATE( selectVars, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DIAGNOSTICS_FILL_STATE
C     | o Fill-in main code, state-variables diagnostics
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     selectVars :: select which group of dianostics variables to fill-in
C            = 1 :: fill-in diagnostics for tracer   variables only
C            = 2 :: fill-in diagnostics for momentum variables only
C            = 3 :: fill-in diagnostics for momentum & tracer variables
C     myThid     :: my Thread Id number
      INTEGER selectVars 
      INTEGER myThid 

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:
C     == Local variables ==
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
      _RL tmpMk(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tmpMk1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1,nSx,nSy)
      _RL tmp1k(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tmpFac, uBarC, vBarC
      INTEGER i,j,K,bi,bj
      INTEGER km1
      
      tmpFac = 1. _d 0

      IF ( selectVars .GE. 2 ) THEN
C--   fill momentum state-var diagnostics:

        CALL DIAGNOSTICS_FILL(etaN, 'ETAN    ',0, 1,0,1,1,myThid)

        IF ( DIAGNOSTICS_IS_ON('RSURF   ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
              tmp1k(i,j,bi,bj) = Ro_surf(i,j,bi,bj) + etaH(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmp1k,'RSURF   ',0,1,0,1,1,myThid)
        ENDIF

        CALL DIAGNOSTICS_SCALE_FILL(etaN,tmpFac,2,
     &                              'ETANSQ  ',0, 1,0,1,1,myThid)
      
#ifdef EXACT_CONSERV
        CALL DIAGNOSTICS_SCALE_FILL(dEtaHdt,tmpFac,2,
     &                              'DETADT2 ',0, 1,0,1,1,myThid)
#endif
      
        CALL DIAGNOSTICS_FILL(uVel, 'UVEL    ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(vVel, 'VVEL    ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(wVel, 'WVEL    ',0,Nr,0,1,1,myThid)
      
        CALL DIAGNOSTICS_SCALE_FILL(uVel,tmpFac,2,
     &                              'UVELSQ  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_SCALE_FILL(vVel,tmpFac,2,
     &                              'VVELSQ  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_SCALE_FILL(wVel,tmpFac,2,
     &                              'WVELSQ  ',0,Nr,0,1,1,myThid)

        IF ( DIAGNOSTICS_IS_ON('UVEL_k2 ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
              tmp1k(i,j,bi,bj) = UVEL(i,j,2,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmp1k,'UVEL_k2 ',0,1,0,1,1,myThid)
        ENDIF
      
        IF ( DIAGNOSTICS_IS_ON('VVEL_k2 ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
              tmp1k(i,j,bi,bj) = VVEL(i,j,2,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmp1k,'VVEL_k2 ',0,1,0,1,1,myThid)
        ENDIF
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        IF ( DIAGNOSTICS_IS_ON('UV_VEL_C',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO K=1,Nr
            DO j = 1,sNy
             DO i = 1,sNx
              uBarC = 0.5 _d 0
     &           *(uVel(i,j,K,bi,bj)+uVel(i+1,j,K,bi,bj))
              vBarC = 0.5 _d 0
     &           *(vVel(i,j,K,bi,bj)+vVel(i,j+1,K,bi,bj))
              tmpMk(i,j,K,bi,bj) =
     &            ( angleCosC(i,j,bi,bj)*uBarC
     &             -angleSinC(i,j,bi,bj)*vBarC )
     &           *( angleSinC(i,j,bi,bj)*uBarC
     &             +angleCosC(i,j,bi,bj)*vBarC )
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'UV_VEL_C',0,Nr,0,1,1,myThid)
        ENDIF
      
        IF ( DIAGNOSTICS_IS_ON('UV_VEL_Z',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO K=1,Nr
            DO j = 1,sNy+1
             DO i = 1,sNx+1
              tmpMk(i,j,K,bi,bj) = 0.25 _d 0
     &           *(uVel(i,j-1,K,bi,bj)+uVel(i,j,K,bi,bj))
     &           *(vVel(i-1,j,K,bi,bj)+vVel(i,j,K,bi,bj))
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'UV_VEL_Z',0,Nr,0,1,1,myThid)
        ENDIF
      
        IF ( DIAGNOSTICS_IS_ON('WU_VEL  ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO K=1,Nr
            km1 = MAX(k-1,1)
            DO j = 1,sNy
             DO i = 1,sNx+1
              tmpMk(i,j,K,bi,bj) = 0.25 _d 0
     &           *(uVel(i,j,km1,bi,bj)+uVel(i,j,K,bi,bj))
     &           *(wVel(i-1,j,K,bi,bj)*rA(i-1,j,bi,bj)
     &            +wVel( i ,j,K,bi,bj)*rA( i ,j,bi,bj)
     &            )*recip_rAw(i,j,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'WU_VEL  ',0,Nr,0,1,1,myThid)
        ENDIF

        IF ( DIAGNOSTICS_IS_ON('WV_VEL  ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO K=1,Nr
            km1 = MAX(k-1,1)
            DO j = 1,sNy+1
             DO i = 1,sNx
              tmpMk(i,j,K,bi,bj) = 0.25 _d 0
     &           *(vVel(i,j,km1,bi,bj)+vVel(i,j,K,bi,bj))
     &           *(wVel(i,j-1,K,bi,bj)*rA(i,j-1,bi,bj)
     &            +wVel(i, j ,K,bi,bj)*rA(i, j ,bi,bj)
     &            )*recip_rAs(i,j,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'WV_VEL  ',0,Nr,0,1,1,myThid)
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        IF ( DIAGNOSTICS_IS_ON('UVELTH  ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO K=1,Nr
            DO j = 1,sNy
             DO i = 1,sNx+1
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
            DO j = 1,sNy+1
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
             DO i = 1,sNx+1
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
            DO j = 1,sNy+1
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
      
        IF ( DIAGNOSTICS_IS_ON('PRESSURE',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           do j = 1,sNy
           do i = 1,sNx
            do K = 1,Nr
             tmpMk1(i,j,K,bi,bj) = 0.
            enddo
           enddo
           enddo
           do j = 1,sNy
           do i = 1,sNx
            if(ksurfC(i,j,bi,bj).ne.0.) 
     .          tmpMk1(i,j,ksurfC(i,j,bi,bj),bi,bj) = 
     .                            (Ro_surf(i,j,bi,bj) + etaH(i,j,bi,bj))
           enddo
           enddo
           do j = 1,sNy
           do i = 1,sNx
            do K = ksurfC(i,j,bi,bj)+1,Nr+1
             tmpMk1(i,j,K,bi,bj) = tmpMk1(i,j,K-1,bi,bj) - 
     .                        drF(K-1)*hfacC(i,j,K-1,bi,bj)
            enddo
            do K = 1,Nr
             tmpMk(i,j,K,bi,bj) = 
     .           (tmpMk1(i,j,K,bi,bj) + tmpMk1(i,j,K+1,bi,bj)) /2.
            enddo
           enddo
           enddo
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'PRESSURE',0,Nr,0,1,1,myThid)
        ENDIF

C--   fill momentum state-var diagnostics: end
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( selectVars.EQ.1 .OR. selectVars.EQ.3 ) THEN
C--   fill tracer state-var diagnostics:

        CALL DIAGNOSTICS_FILL(theta,'THETA   ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(salt, 'SALT    ',0,Nr,0,1,1,myThid)

        CALL DIAGNOSTICS_SCALE_FILL(theta,tmpFac,2,
     &                              'THETASQ ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_SCALE_FILL(salt,tmpFac,2,
     &                              'SALTSQ  ',0,Nr,0,1,1,myThid)
      
        IF ( DIAGNOSTICS_IS_ON('SST     ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
              tmp1k(i,j,bi,bj) = THETA(i,j,1,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmp1k,'SST     ',0,1,0,1,1,myThid)
        ENDIF
      
        IF ( DIAGNOSTICS_IS_ON('SSS     ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
              tmp1k(i,j,bi,bj) = SALT(i,j,1,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmp1k,'SSS     ',0,1,0,1,1,myThid)
        ENDIF

        IF ( DIAGNOSTICS_IS_ON('SALTanom',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO K=1,Nr
            DO j = 1,sNy
             DO i = 1,sNx
               tmpMk(i,j,K,bi,bj) = salt(i,j,K,bi,bj)-35
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'SALTanom',0,Nr,0,1,1,myThid)
        ENDIF
      
        IF ( DIAGNOSTICS_IS_ON('SALTSQan',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO K=1,Nr
            DO j = 1,sNy
             DO i = 1,sNx
               tmpMk(i,j,K,bi,bj) =
     &               (salt(i,j,K,bi,bj)-35)*(salt(i,j,K,bi,bj)-35)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'SALTSQan',0,Nr,0,1,1,myThid)
        ENDIF
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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

        CALL DIAGNOSTICS_FILL(wVel, 'WVELMASS',0,Nr,0,1,1,myThid)

        IF ( DIAGNOSTICS_IS_ON('UTHMASS ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO K=1,Nr
            DO j = 1,sNy
             DO i = 1,sNx+1
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
            DO j = 1,sNy+1
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
      
        IF ( DIAGNOSTICS_IS_ON('WTHMASS ',myThid) ) THEN
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
         CALL DIAGNOSTICS_FILL(tmpMk,'WTHMASS ',0,Nr,0,1,1,myThid)
        ENDIF

        IF ( DIAGNOSTICS_IS_ON('USLTMASS',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO K=1,Nr
            DO j = 1,sNy
             DO i = 1,sNx+1
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
            DO j = 1,sNy+1
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
      
        IF ( DIAGNOSTICS_IS_ON('WSLTMASS',myThid) ) THEN
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
         CALL DIAGNOSTICS_FILL(tmpMk,'WSLTMASS',0,Nr,0,1,1,myThid)
        ENDIF
      
C--   fill tracer state-var diagnostics: end
      ENDIF

#endif /* ALLOW_DIAGNOSTICS */
      
      RETURN 
      END
