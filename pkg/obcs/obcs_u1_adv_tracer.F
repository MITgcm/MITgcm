C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_u1_adv_tracer.F,v 1.1 2012/03/09 20:13:03 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CBOP
C !ROUTINE: OBCS_U1_ADV_TRACER

C !INTERFACE: ==========================================================
      SUBROUTINE OBCS_U1_ADV_TRACER(
     I           doAdvXdir,
     I           trIdentity, bi, bj, k,
     I           maskLoc, vTrans, tracer,
     U           vT,
     I           myThid )

C !DESCRIPTION:
C  Update advective flux by replacing values at Open-Boundaries
C  with simply 1rst Order upwind advection scheme calculation.
C  Provide the option to do the replacement only in case of outflow
C  or indpendently of the sign of the flow.

C !USES: ===============================================================
      IMPLICIT NONE
C == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif

C !INPUT/OUTPUT PARAMETERS: ============================================
C  doAdvXdir    :: =T: advection in X-direction ; =F: in Y-direction
C  trIdentity   :: tracer identifier
C  bi,bj        :: tile indices
C  k            :: vertical level
C  maskLoc      :: local mask at velocity location
C  vTrans       :: volume transport
C  tracer       :: tracer field
C  vT           :: advective flux
C  myThid       :: thread number
      LOGICAL doAdvXdir
      INTEGER trIdentity
      INTEGER bi, bj, k
      _RS maskLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vT     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

#ifdef ALLOW_OBCS
#ifdef ALLOW_GENERIC_ADVDIFF
C !LOCAL VARIABLES: ====================================================
C  i,j          :: loop indices
C  msgBuf       :: message buffer
      INTEGER i,j
      INTEGER updateAdvFlx
      _RL vAbs, tmpVar
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_PTRACERS
      INTEGER iTr
#endif /* ALLOW_PTRACERS */
CEOP

      updateAdvFlx = 0
      IF ( trIdentity.EQ.GAD_TEMPERATURE ) THEN
        updateAdvFlx = OBCS_u1_adv_T
      ELSEIF ( trIdentity.EQ.GAD_SALINITY) THEN
        updateAdvFlx = OBCS_u1_adv_S
#ifdef ALLOW_PTRACERS
      ELSEIF ( trIdentity.GE.GAD_TR1) THEN
        iTr = trIdentity - GAD_TR1 + 1
        updateAdvFlx = OBCS_u1_adv_Tr(iTr)
#endif /* ALLOW_PTRACERS */
      ELSE
        WRITE(msgBuf,'(A,I4)')
     &       ' OBCS_U1_ADV_TRACER: Invalid tracer Id: ',trIdentity
        CALL PRINT_ERROR(msgBuf, myThid)
        STOP 'ABNORMAL END: S/R OBCS_U1_ADV_TRACER'
      ENDIF

      IF ( updateAdvFlx .GT. 0 ) THEN

#ifdef ALLOW_AUTODIFF_TAMC
         STOP 'ABNORMAL END: S/R OBCS_U1_ADV_TRACER'
#else /* ALLOW_AUTODIFF_TAMC */

        IF ( doAdvXdir ) THEN
C--   Advective flux in X-direction

         IF ( updateAdvFlx .EQ. 1 ) THEN
C-    only if outflow
          DO j=1-OLy,sNy+OLy
           DO i=2-OLx,sNx+OLx
            tmpVar = vTrans(i,j)*maskLoc(i,j)
     &             *( maskInC(i-1,j,bi,bj) - maskInC(i,j,bi,bj) )
            IF ( tmpVar.GT. 0. _d 0 ) THEN
              vAbs = ABS(vTrans(i,j))
              vT(i,j) = ( vTrans(i,j)+vAbs )* 0.5 _d 0 * tracer(i-1,j)
     &                + ( vTrans(i,j)-vAbs )* 0.5 _d 0 * tracer(i,j)
            ENDIF
           ENDDO
          ENDDO
         ELSE
C-    no condition (inflow & outflow)
          DO j=1-OLy,sNy+OLy
           DO i=2-OLx,sNx+OLx
            IF ( maskLoc(i,j).EQ.1. .AND.
     &           maskInC(i-1,j,bi,bj).NE.maskInC(i,j,bi,bj) ) THEN
              vAbs = ABS(vTrans(i,j))
              vT(i,j) = ( vTrans(i,j)+vAbs )* 0.5 _d 0 * tracer(i-1,j)
     &                + ( vTrans(i,j)-vAbs )* 0.5 _d 0 * tracer(i,j)
            ENDIF
           ENDDO
          ENDDO
         ENDIF

        ELSE
C--   Advective flux in Y-direction

         IF ( updateAdvFlx .EQ. 1 ) THEN
C-    only if outflow
          DO j=2-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            tmpVar = vTrans(i,j)*maskLoc(i,j)
     &             *( maskInC(i,j-1,bi,bj) - maskInC(i,j,bi,bj) )
            IF ( tmpVar.GT. 0. _d 0 ) THEN
              vAbs = ABS(vTrans(i,j))
              vT(i,j) = ( vTrans(i,j)+vAbs )* 0.5 _d 0 * tracer(i,j-1)
     &                + ( vTrans(i,j)-vAbs )* 0.5 _d 0 * tracer(i,j)
            ENDIF
           ENDDO
          ENDDO
         ELSE
C-    no condition (inflow & outflow)
          DO j=2-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            IF ( maskLoc(i,j).EQ.1. .AND.
     &           maskInC(i,j-1,bi,bj).NE.maskInC(i,j,bi,bj) ) THEN
              vAbs = ABS(vTrans(i,j))
              vT(i,j) = ( vTrans(i,j)+vAbs )* 0.5 _d 0 * tracer(i,j-1)
     &                + ( vTrans(i,j)-vAbs )* 0.5 _d 0 * tracer(i,j)
            ENDIF
           ENDDO
          ENDDO
         ENDIF

C--   end if X-direction / Y-direction
        ENDIF

#endif /* ALLOW_AUTODIFF_TAMC */

C--   end if updateAdvFlx > 0
      ENDIF

#endif /* ALLOW_GENERIC_ADVDIFF */
#endif /* ALLOW_OBCS */

      RETURN
      END
