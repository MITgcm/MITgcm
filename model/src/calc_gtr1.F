C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_gtr1.F,v 1.12 2004/06/26 02:38:08 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_GTR1
C     !INTERFACE:
      SUBROUTINE CALC_GTR1( 
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,rTransKp1,maskUp,
     I           KappaRT,
     U           fVerT,
     I           myTime,myIter,myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_GTR1                                      
C     | o Calculate the passive tracer tendency terms.            
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"
#ifdef ALLOW_PASSIVE_TRACER
#include "TR1.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fVerT   :: Flux of temperature (T) in the vertical 
C               direction at the upper(U) and lower(D) faces of a cell.
C     maskUp  :: Land mask used to denote base of the domain.
C     xA      :: Tracer cell face area normal to X
C     yA      :: Tracer cell face area normal to X
C     uTrans  :: Zonal volume transport through cell face
C     vTrans  :: Meridional volume transport through cell face
C     rTrans  ::   Vertical volume transport at interface k
C     rTransKp1 :: Vertical volume transport at inteface k+1
C     bi, bj, iMin, iMax, jMin, jMax :: Range of points for which calculation
C                                      results will be set.
C     myThid - Instance number for this innvocation of CALC_GT
      _RL fVerT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTransKp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER k,kUp,kDown,kM1
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

CEOP

ceh3 needs an IF ( usePASSIVE_TRACER ) THEN

#ifdef ALLOW_PASSIVE_TRACER
C     === Local variables ===
      LOGICAL calcAdvection

#ifdef ALLOW_AUTODIFF_TAMC
C--   only the kUp part of fverT is set in this subroutine
C--   the kDown is still required
      fVerT(1,1,kDown) = fVerT(1,1,kDown)
#endif

      calcAdvection = .NOT.tr1_MultiDimAdvec
      CALL GAD_CALC_RHS(
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,rTransKp1,maskUp,
     I           uVel, vVel, wVel,
     I           diffKhT, diffK4T, KappaRT, tr1,
     I           GAD_TR1, tracerAdvScheme, tracerAdvScheme,
     I           calcAdvection, .FALSE.,
     U           fVerT, gTr1,
     I           myThid )

#ifdef INCLUDE_TR_FORCING_CODE
C--   External forcing term(s) inside Adams-Bashforth:
      IF ( forcing_In_AB )
     & CALL EXTERNAL_FORCING_TR(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)
#endif /*  INCLUDE_TR_FORCING_CODE */

      IF ( tr1_AdamsBashforth ) THEN
        CALL ADAMS_BASHFORTH2(
     I                        bi, bj, K,
     U                        gTr1, gTr1nm1,
     I                        myIter, myThid )
      ENDIF

#ifdef INCLUDE_TR_FORCING_CODE
C--   External forcing term(s) outside Adams-Bashforth:
      IF ( .NOT.forcing_In_AB )
     & CALL EXTERNAL_FORCING_TR(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)
#endif /*  INCLUDE_TR_FORCING_CODE */

#ifdef NONLIN_FRSURF
      IF (nonlinFreeSurf.GT.0) THEN
        CALL FREESURF_RESCALE_G(
     I                          bi, bj, K,
     U                          gTr1,
     I                          myThid )
        IF ( tr1_AdamsBashforth )
     &  CALL FREESURF_RESCALE_G(
     I                          bi, bj, K,
     U                          gTr1nm1,
     I                          myThid )
      ENDIF
#endif /* NONLIN_FRSURF */

#endif /* ALLOW_PASSIVE_TRACER */

      RETURN
      END
