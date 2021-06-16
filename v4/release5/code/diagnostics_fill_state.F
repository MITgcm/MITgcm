C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_fill_state.F,v 1.43 2015/06/09 16:31:45 rpa Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: DIAGNOSTICS_FILL_STATE
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_FILL_STATE( selectVars, myIter, myThid )

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
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "NH_VARS.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif


C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     selectVars :: select which group of dianostics variables to fill-in
C            = 1 :: fill-in diagnostics for tracer   variables only
C            = 2 :: fill-in diagnostics for momentum variables only
C            = 3 :: fill-in diagnostics for momentum & tracer variables
C            = 4 :: fill-in state variable tendency diagnostics the second time
C     myIter     :: current Iteration number
C     myThid     :: my Thread Id number
      INTEGER selectVars
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:
C     == Local variables ==
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
      _RL tmpMk(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tmp1k(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tmpU (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpV (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpFac, uBarC, vBarC
#ifdef ALLOW_FIZHI
      _RL dummy1, dummy2, dummy3, dummy4, kappa, getcon
#endif
#ifdef ALLOW_ADAMSBASHFORTH_3
      INTEGER m1
#endif
      INTEGER i,j,k,bi,bj
      INTEGER km1

      IF ( selectVars.EQ.2 .OR. selectVars.EQ.3 ) THEN
C--   fill momentum state-var diagnostics:

        CALL DIAGNOSTICS_FILL(etaN, 'ETAN    ',0, 1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(m_eta,'SSHNOIBC',0, 1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(m_eta_ib,'SSHIBC  ',
     &                        0, 1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(m_eta_dyn,'SSH     ',
     &                        0, 1,0,1,1,myThid)

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

        CALL DIAGNOSTICS_SCALE_FILL( etaN, oneRL, 2,
     &                              'ETANSQ  ',0, 1,0,1,1,myThid)

#ifdef EXACT_CONSERV
        CALL DIAGNOSTICS_SCALE_FILL( dEtaHdt, oneRL, 2,
     &                              'DETADT2 ',0, 1,0,1,1,myThid)
#endif
#ifdef ALLOW_NONHYDROSTATIC
        IF ( use3Dsolver ) THEN
          CALL DIAGNOSTICS_FILL( phi_nh,'PHI_NH  ',0,Nr,0,1,1,myThid )
        ENDIF
#endif

        CALL DIAGNOSTICS_FILL(uVel, 'UVEL    ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(vVel, 'VVEL    ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(wVel, 'WVEL    ',0,Nr,0,1,1,myThid)

        CALL DIAGNOSTICS_SCALE_FILL( uVel, oneRL, 2,
     &                              'UVELSQ  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_SCALE_FILL( vVel, oneRL, 2,
     &                              'VVELSQ  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_SCALE_FILL( wVel, oneRL, 2,
     &                              'WVELSQ  ',0,Nr,0,1,1,myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        IF ( DIAGNOSTICS_IS_ON('UE_VEL_C',myThid) .OR.
     &       DIAGNOSTICS_IS_ON('VN_VEL_C',myThid) .OR.
     &       DIAGNOSTICS_IS_ON('UV_VEL_C',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            DO j = 1,sNy
             DO i = 1,sNx
              uBarC = 0.5 _d 0
     &           *(uVel(i,j,k,bi,bj)+uVel(i+1,j,k,bi,bj))
              vBarC = 0.5 _d 0
     &           *(vVel(i,j,k,bi,bj)+vVel(i,j+1,k,bi,bj))
              tmpU(i,j) = angleCosC(i,j,bi,bj)*uBarC
     &                   -angleSinC(i,j,bi,bj)*vBarC
              tmpV(i,j) = angleSinC(i,j,bi,bj)*uBarC
     &                   +angleCosC(i,j,bi,bj)*vBarC
              tmpMk(i,j,k,bi,bj) = tmpU(i,j)*tmpV(i,j)
             ENDDO
            ENDDO
            CALL DIAGNOSTICS_FILL(tmpU,'UE_VEL_C',k,1,2,bi,bj,myThid)
            CALL DIAGNOSTICS_FILL(tmpV,'VN_VEL_C',k,1,2,bi,bj,myThid)
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'UV_VEL_C',0,Nr,0,1,1,myThid)
        ENDIF

        IF ( DIAGNOSTICS_IS_ON('UV_VEL_Z',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            DO j = 1,sNy+1
             DO i = 1,sNx+1
              tmpMk(i,j,k,bi,bj) = 0.25 _d 0
     &           *(uVel(i,j-1,k,bi,bj)+uVel(i,j,k,bi,bj))
     &           *(vVel(i-1,j,k,bi,bj)+vVel(i,j,k,bi,bj))
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
           DO k=1,Nr
            km1 = MAX(k-1,1)
            DO j = 1,sNy
             DO i = 1,sNx+1
              tmpMk(i,j,k,bi,bj) = 0.25 _d 0
     &           *(uVel(i,j,km1,bi,bj)+uVel(i,j,k,bi,bj))
     &           *(wVel(i-1,j,k,bi,bj)*rA(i-1,j,bi,bj)
     &            +wVel( i ,j,k,bi,bj)*rA( i ,j,bi,bj)
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
           DO k=1,Nr
            km1 = MAX(k-1,1)
            DO j = 1,sNy+1
             DO i = 1,sNx
              tmpMk(i,j,k,bi,bj) = 0.25 _d 0
     &           *(vVel(i,j,km1,bi,bj)+vVel(i,j,k,bi,bj))
     &           *(wVel(i,j-1,k,bi,bj)*rA(i,j-1,bi,bj)
     &            +wVel(i, j ,k,bi,bj)*rA(i, j ,bi,bj)
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
           DO k=1,Nr
            DO j = 1,sNy
             DO i = 1,sNx+1
               tmpMk(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)*0.5 _d 0
     &                  *(theta(i,j,k,bi,bj)+theta(i-1,j,k,bi,bj))
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
           DO k=1,Nr
            DO j = 1,sNy+1
             DO i = 1,sNx
               tmpMk(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)*0.5 _d 0
     &                  *(theta(i,j,k,bi,bj)+theta(i,j-1,k,bi,bj))
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
           DO k=1,Nr
            km1 = MAX(k-1,1)
            DO j = 1,sNy
             DO i = 1,sNx
               tmpMk(i,j,k,bi,bj) = wVel(i,j,k,bi,bj)*0.5 _d 0
     &                  *(theta(i,j,k,bi,bj)+theta(i,j,km1,bi,bj))
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
           DO k=1,Nr
            DO j = 1,sNy
             DO i = 1,sNx+1
               tmpMk(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)*0.5 _d 0
     &                  *(salt(i,j,k,bi,bj)+salt(i-1,j,k,bi,bj))
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
           DO k=1,Nr
            DO j = 1,sNy+1
             DO i = 1,sNx
               tmpMk(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)*0.5 _d 0
     &                  *(salt(i,j,k,bi,bj)+salt(i,j-1,k,bi,bj))
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
           DO k=1,Nr
            km1 = MAX(k-1,1)
            DO j = 1,sNy
             DO i = 1,sNx
               tmpMk(i,j,k,bi,bj) = wVel(i,j,k,bi,bj)*0.5 _d 0
     &                  *(salt(i,j,k,bi,bj)+salt(i,j,km1,bi,bj))
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'WVELSLT ',0,Nr,0,1,1,myThid)
        ENDIF

        IF ( DIAGNOSTICS_IS_ON('UVELPHI ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            DO j = 1,sNy
             DO i = 1,sNx+1
               tmpMk(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)*hFacW(i,j,k,bi,bj)
     &       *0.5 _d 0*(totPhiHyd(i,j,k,bi,bj)+totPhiHyd(i-1,j,k,bi,bj))
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'UVELPHI ',0,Nr,0,1,1,myThid)
        ENDIF

        IF ( DIAGNOSTICS_IS_ON('VVELPHI ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            DO j = 1,sNy+1
             DO i = 1,sNx
               tmpMk(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)*hFacS(i,j,k,bi,bj)
     &       *0.5 _d 0*(totPhiHyd(i,j,k,bi,bj)+totPhiHyd(i,j-1,k,bi,bj))
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'VVELPHI ',0,Nr,0,1,1,myThid)
        ENDIF

        IF ( DIAGNOSTICS_IS_ON('RCENTER ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
              tmp1k(i,j,bi,bj) = R_low(i,j,bi,bj)
            ENDDO
           ENDDO
           DO k = Nr,1,-1
            DO j = 1,sNy
             DO i = 1,sNx
              tmpMk(i,j,k,bi,bj) = tmp1k(i,j,bi,bj)
     &             + (rF(k+1)-rC(k))*hFacC(i,j,k,bi,bj)*rkSign
C         above: more general (setInterFDr/setCenterDr) than line below
c    &                      + drF(k)*hFacC(i,j,k,bi,bj)*0.5 _d 0
              tmp1k(i,j,bi,bj) =   tmp1k(i,j,bi,bj)
     &                      + drF(k)*hFacC(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'RCENTER ',0,Nr,0,1,1,myThid)
        ENDIF

C First fill sequence for state variable tendency diagnostics: subtract state variable
C NOTE: send a '0' for the bibjflag and allow counter to be incremented
C     (next fill for these diagnostics will NOT allow counter to be incremented)

        tmpFac = -86400. _d 0/deltaTMom
        CALL DIAGNOSTICS_SCALE_FILL( uVel, tmpFac, 1,
     &                              'TOTUTEND',0,Nr,0,1,1,myThid )
        CALL DIAGNOSTICS_SCALE_FILL( vVel, tmpFac, 1,
     &                              'TOTVTEND',0,Nr,0,1,1,myThid )

        IF ( DIAGNOSTICS_IS_ON('TOTTTEND',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            tmpFac = -86400. _d 0/dTtracerLev(k)
            DO j = 1,sNy
             DO i = 1,sNx
               tmpMk(i,j,k,bi,bj) = tmpFac*theta(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'TOTTTEND',0,Nr,0,1,1,myThid)
#ifdef ALLOW_LAYERS
         IF ( useLayers ) THEN
           CALL LAYERS_FILL(tmpMk,1,'TOT',0,Nr,0,1,1,myThid)
         ENDIF
#endif /* ALLOW_LAYERS */         
        ENDIF

        IF ( DIAGNOSTICS_IS_ON('TOTSTEND',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            tmpFac = -86400. _d 0/dTtracerLev(k)
            DO j = 1,sNy
             DO i = 1,sNx
               tmpMk(i,j,k,bi,bj) = tmpFac*salt(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'TOTSTEND',0,Nr,0,1,1,myThid)
#ifdef ALLOW_LAYERS
         IF ( useLayers ) THEN
           CALL LAYERS_FILL(tmpMk,2,'TOT',0,Nr,0,1,1,myThid)
         ENDIF
#endif /* ALLOW_LAYERS */
        ENDIF

C--   fill momentum state-var diagnostics: end
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( selectVars.EQ.1 .OR. selectVars.EQ.3 ) THEN
C--   fill tracer state-var diagnostics:

        CALL DIAGNOSTICS_FILL(theta,'THETA   ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(salt, 'SALT    ',0,Nr,0,1,1,myThid)

#ifdef ALLOW_FIZHI
      IF ( useFIZHI .AND. DIAGNOSTICS_IS_ON('RELHUM  ',myThid) ) THEN
       kappa = getcon('KAPPA')
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           DO k = 1,Nr
            dummy1 = theta(i,j,k,bi,bj) * ((rC(k)/100.)/1000.)**kappa
            dummy2 = rC(k) / 100.
            CALL QSAT(dummy1,dummy2,dummy3,dummy4,.false.)
            tmpMk(i,j,k,bi,bj) = hFacC(i,j,k,bi,bj)
     &                          *salt(i,j,k,bi,bj)*100. / dummy3
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpMk, 'RELHUM  ',0,Nr,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_FIZHI */

        CALL DIAGNOSTICS_SCALE_FILL( theta, oneRL, 2,
     &                              'THETASQ ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_SCALE_FILL( salt, oneRL, 2,
     &                              'SALTSQ  ',0,Nr,0,1,1,myThid)

#ifdef ALLOW_GENERIC_ADVDIFF
# ifdef ALLOW_ADAMSBASHFORTH_3
      IF ( selectVars.EQ.1 ) THEN
C-    stagger time-step: fill diags after updating myIter
        m1 = 1 + MOD(myIter,2)
      ELSE
C-    synchronous time-step: fill diags before updating myIter
        m1 = 1 + MOD(myIter+1,2)
      ENDIF
      IF ( AdamsBashforthGt )
     & CALL DIAGNOSTICS_FILL( gtNm(1-OLx,1-OLy,1,1,1,m1),
     &                        'gTinAB  ',0,Nr,0,1,1,myThid )
      IF ( AdamsBashforthGs )
     & CALL DIAGNOSTICS_FILL( gsNm(1-OLx,1-OLy,1,1,1,m1),
     &                        'gSinAB  ',0,Nr,0,1,1,myThid )
# else /* ALLOW_ADAMSBASHFORTH_3 */
      IF ( AdamsBashforthGt )
     & CALL DIAGNOSTICS_FILL( gtNm1,'gTinAB  ',0,Nr,0,1,1,myThid )
      IF ( AdamsBashforthGs )
     & CALL DIAGNOSTICS_FILL( gsNm1,'gSinAB  ',0,Nr,0,1,1,myThid )
# endif /* ALLOW_ADAMSBASHFORTH_3 */
#endif /* ALLOW_GENERIC_ADVDIFF */

c       IF ( DIAGNOSTICS_IS_ON('SST     ',myThid) ) THEN
c        DO bj = myByLo(myThid), myByHi(myThid)
c         DO bi = myBxLo(myThid), myBxHi(myThid)
c          DO j = 1,sNy
c           DO i = 1,sNx
c             tmp1k(i,j,bi,bj) = THETA(i,j,1,bi,bj)
c           ENDDO
c          ENDDO
c         ENDDO
c        ENDDO
c        CALL DIAGNOSTICS_FILL(tmp1k,'SST     ',0,1,0,1,1,myThid)
c       ENDIF

c       IF ( DIAGNOSTICS_IS_ON('SSS     ',myThid) ) THEN
c        DO bj = myByLo(myThid), myByHi(myThid)
c         DO bi = myBxLo(myThid), myBxHi(myThid)
c          DO j = 1,sNy
c           DO i = 1,sNx
c             tmp1k(i,j,bi,bj) = SALT(i,j,1,bi,bj)
c           ENDDO
c          ENDDO
c         ENDDO
c        ENDDO
c        CALL DIAGNOSTICS_FILL(tmp1k,'SSS     ',0,1,0,1,1,myThid)
c       ENDIF

        IF ( fluidIsWater .AND.
     &       ( DIAGNOSTICS_IS_ON('SALTanom',myThid)
     &     .OR.DIAGNOSTICS_IS_ON('SALTSQan',myThid) ) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            DO j = 1,sNy
             DO i = 1,sNx
               tmpMk(i,j,k,bi,bj) = salt(i,j,k,bi,bj)-35. _d 0
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL( tmpMk,'SALTanom',0,Nr,0,1,1,myThid)
         CALL DIAGNOSTICS_SCALE_FILL( tmpMk, oneRL, 2,
     &                                'SALTSQan',0,Nr,0,1,1,myThid)
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        IF ( DIAGNOSTICS_IS_ON('UVELMASS',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            DO j = 1,sNy
              DO i = 1,sNx+1
                tmpMk(i,j,k,bi,bj)
     &               = uVel(i,j,k,bi,bj)*hFacW(i,j,k,bi,bj)
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
           DO k=1,Nr
            DO j = 1,sNy+1
              DO i = 1,sNx
                tmpMk(i,j,k,bi,bj)
     &               = vVel(i,j,k,bi,bj)*hFacS(i,j,k,bi,bj)
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
           DO k=1,Nr
            DO j = 1,sNy
             DO i = 1,sNx+1
               tmpMk(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)*0.5 _d 0
     &                  *(theta(i,j,k,bi,bj)+theta(i-1,j,k,bi,bj))
     &                  * hFacW(i,j,k,bi,bj)
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
           DO k=1,Nr
            DO j = 1,sNy+1
             DO i = 1,sNx
               tmpMk(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)*0.5 _d 0
     &                  *(theta(i,j,k,bi,bj)+theta(i,j-1,k,bi,bj))
     &                  * hFacS(i,j,k,bi,bj)
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
           DO k=1,Nr
            km1 = MAX(k-1,1)
            DO j = 1,sNy
             DO i = 1,sNx
               tmpMk(i,j,k,bi,bj) = wVel(i,j,k,bi,bj)*0.5 _d 0
     &                  *(theta(i,j,k,bi,bj)+theta(i,j,km1,bi,bj))
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
           DO k=1,Nr
            DO j = 1,sNy
             DO i = 1,sNx+1
               tmpMk(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)*0.5 _d 0
     &                  *(salt(i,j,k,bi,bj)+salt(i-1,j,k,bi,bj))
     &                  * hFacW(i,j,k,bi,bj)
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
           DO k=1,Nr
            DO j = 1,sNy+1
             DO i = 1,sNx
               tmpMk(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)*0.5 _d 0
     &                  *(salt(i,j,k,bi,bj)+salt(i,j-1,k,bi,bj))
     &                  * hFacS(i,j,k,bi,bj)
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
           DO k=1,Nr
            km1 = MAX(k-1,1)
            DO j = 1,sNy
             DO i = 1,sNx
               tmpMk(i,j,k,bi,bj) = wVel(i,j,k,bi,bj)*0.5 _d 0
     &                  *(salt(i,j,k,bi,bj)+salt(i,j,km1,bi,bj))
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(tmpMk,'WSLTMASS',0,Nr,0,1,1,myThid)
        ENDIF

C--   fill tracer state-var diagnostics: end
      ENDIF

      IF ( selectVars.EQ.4 ) THEN
C Second fill sequence for state variable tendency diagnostics: add state variable
C NOTE: send a '-1' for the bibjflag and do not increment counter
C     (previous fill for these diagnostics DID allow counter to be incremented)

        tmpFac = 86400. _d 0/deltaTMom
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL DIAGNOSTICS_SCALE_FILL( uVel, tmpFac, 1,
     &                           'TOTUTEND',0,Nr,-1,bi,bj,myThid )
          CALL DIAGNOSTICS_SCALE_FILL( vVel, tmpFac, 1,
     &                           'TOTVTEND',0,Nr,-1,bi,bj,myThid )
         ENDDO
        ENDDO

        IF ( DIAGNOSTICS_IS_ON('TOTTTEND',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            tmpFac = 86400. _d 0/dTtracerLev(k)
            DO j = 1,sNy
             DO i = 1,sNx
               tmpMk(i,j,k,bi,bj) = tmpFac*theta(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL(tmpMk,'TOTTTEND',0,Nr,-1,bi,bj,myThid)
#ifdef ALLOW_LAYERS
           IF ( useLayers ) THEN
             CALL LAYERS_FILL(tmpMk,1,'TOT',0,Nr,-1,bi,bj,myThid)
           ENDIF
#endif /* ALLOW_LAYERS */    
          ENDDO
         ENDDO
        ENDIF

        IF ( DIAGNOSTICS_IS_ON('TOTSTEND',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            tmpFac = 86400. _d 0/dTtracerLev(k)
            DO j = 1,sNy
             DO i = 1,sNx
               tmpMk(i,j,k,bi,bj) = tmpFac*salt(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL(tmpMk,'TOTSTEND',0,Nr,-1,bi,bj,myThid)
#ifdef ALLOW_LAYERS
           IF ( useLayers ) THEN
            CALL LAYERS_FILL(tmpMk,2,'TOT',0,Nr,-1,bi,bj,myThid)
           ENDIF
#endif /* ALLOW_LAYERS */    
          ENDDO
         ENDDO
        ENDIF

C--   fill state tendency diagnostics the second time: end
      ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
