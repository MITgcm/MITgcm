C $Header: /u/gcmpack/MITgcm/pkg/down_slope/dwnslp_calc_flow.F,v 1.4 2011/06/07 21:05:13 jmc Exp $
C $Name:  $

#include "DWNSLP_OPTIONS.h"

CBOP
C     !ROUTINE: DWNSLP_CALC_FLOW
C     !INTERFACE:
      SUBROUTINE DWNSLP_CALC_FLOW(
     I                             bi, bj, kBottom, rho3d,
     I                             myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DWNSLP_CALC_FLOW
C     | o Detect active site of Down-Sloping flow and compute
C     |   the corresponding volume transport
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DWNSLP_SIZE.h"
#include "DWNSLP_PARAMS.h"
#include "DWNSLP_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     bi,bj     :: Tile indices
C     kBottom   :: Vertical index of bottom grid cell.
C     rho3d     :: In-situ density [kg/m3] computed at z=rC ;
C     myTime    :: Current time in simulation
C     myIter    :: Current time-step number
C     myThid    :: my Thread Id number
      INTEGER bi, bj
      INTEGER kBottom( xySize, nSx,nSy )
      _RL     rho3d  ( xySize, Nr,nSx,nSy )
      _RL     myTime
      INTEGER myIter, myThid

#ifdef ALLOW_DOWN_SLOPE

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf    :: Informational/error message buffer
C     ijd     :: horiz. index of deep water column receiving dense water flow
C     ijs     :: horiz. index of shallow water column (e.g. shelf)
C                from which dense water flow originates
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER k
      INTEGER n, ijd, ijr, ijs
      INTEGER kdeep, ishelf, jshelf, kshelf
      _RL     dRhoH
      INTEGER downward
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL doDiagDwnSlpFlow
      INTEGER ij
      _RL     sgnFac
      _RL     uFlow( xySize )
      _RL     vFlow( xySize )
C-    Functions:
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */

CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     downward = rkSign*NINT(gravitySign)
      downward = 1
      IF ( usingPCoords ) downward = -1

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        doDiagDwnSlpFlow = DIAGNOSTICS_IS_ON( 'DSLPuFlw', myThid )
     &                .OR. DIAGNOSTICS_IS_ON( 'DSLPvFlw', myThid )
        IF ( doDiagDwnSlpFlow ) THEN
          DO ij=1,xySize
           uFlow(ij) = 0. _d 0
           vFlow(ij) = 0. _d 0
          ENDDO
        ENDIF
      ELSE
        doDiagDwnSlpFlow = .FALSE.
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      DO n=1,DWNSLP_NbSite(bi,bj)
        DWNSLP_deepK(n,bi,bj) = 0

C- detect density gradient along the slope => Downsloping flow

        ijd = DWNSLP_ijDeep(n,bi,bj)
        ijr = DWNSLP_shVsD(n,bi,bj)
        ijs = ijd + ijr
        kshelf = kBottom(ijs,bi,bj)

        dRhoH = rho3d(ijs,kshelf,bi,bj)
     &         -rho3d(ijd,kshelf,bi,bj)
c       IF ( dRhoH.GT.0. _d 0 ) THEN
        IF ( rho3d(ijs,kshelf+1,bi,bj).GT.rho3d(ijd,kshelf+1,bi,bj)
     &    .AND. dRhoH.GT.0. _d 0 ) THEN

C- search for deepest level where Rho_shelf > Rho_deep
          kdeep = kshelf
          DO k=kshelf+1,kBottom(ijd,bi,bj),downward
           IF ( rho3d(ijs,k,bi,bj).GT.rho3d(ijd,k,bi,bj) ) kdeep = k
          ENDDO
          DWNSLP_deepK(n,bi,bj) = kdeep

C- Compute the Volume Transport :
C- same formulation as described in the paper:
c         downslpFlow  = DWNSLP_gamma/mu *gravity*dRhoH*recip_rhoConst
C    with DWNSLP_Gamma = slope * effective cross-section area
          DWNSLP_Transp(n,bi,bj) = DWNSLP_Gamma(n,bi,bj)
     &             *DWNSLP_rec_mu*gravity*dRhoH*recip_rhoConst

#ifdef ALLOW_DIAGNOSTICS
          IF ( doDiagDwnSlpFlow ) THEN
            ij = MAX( ijd, ijs )
            sgnFac = SIGN(1,-ijr)
            IF ( ABS(ijr).EQ.1 ) THEN
             uFlow(ij) = sgnFac*DWNSLP_Transp(n,bi,bj)
            ELSE
             vFlow(ij) = sgnFac*DWNSLP_Transp(n,bi,bj)
            ENDIF
          ENDIF
#endif /* ALLOW_DIAGNOSTICS */

        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ENDDO

      IF ( DWNSLP_ioUnit.GT.0 ) THEN
       _BEGIN_MASTER(myThid)
       WRITE(DWNSLP_ioUnit,'(A,I8,2I4)')
     &      ' DWNSLP_CALC_FLOW: iter,bi,bj=',myIter,bi,bj
       WRITE(DWNSLP_ioUnit,'(A)')
     &   '  bi  bj     n :     ijd   ijr  is  js ;  ks kd-s Transp :'
       DO n=1,DWNSLP_NbSite(bi,bj)
        IF (DWNSLP_deepK(n,bi,bj).NE.0) THEN
         ijs = DWNSLP_ijDeep(n,bi,bj) + DWNSLP_shVsD(n,bi,bj)
         ishelf = 1-OLx + mod(ijs-1,xSize)
         jshelf = 1-OLy + (ijs-1)/xSize
         kshelf = kBottom(ijs,bi,bj)
         WRITE(DWNSLP_ioUnit,'(2I4,I6,A,I8,I6,2I4,A,2I4,1PE14.6)')
     &     bi,bj,n,' :', DWNSLP_ijDeep(n,bi,bj),
     &                   DWNSLP_shVsD(n,bi,bj), ishelf,jshelf,
     &             ' ;', kshelf, DWNSLP_deepK(n,bi,bj)-kshelf,
     &                   DWNSLP_Transp(n,bi,bj)
        ENDIF
       ENDDO
       WRITE(DWNSLP_ioUnit,*)
       _END_MASTER(myThid)
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( doDiagDwnSlpFlow ) THEN
        CALL DIAGNOSTICS_FILL( uFlow, 'DSLPuFlw', 0,1,2,bi,bj,myThid )
        CALL DIAGNOSTICS_FILL( vFlow, 'DSLPvFlw', 0,1,2,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_DOWN_SLOPE */

      RETURN
      END
