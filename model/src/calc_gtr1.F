C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_gtr1.F,v 1.6 2001/09/26 18:09:14 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_GTR1
C     !INTERFACE:
      SUBROUTINE CALC_GTR1( 
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,maskUp,
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
#include "TR1.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fVerT   :: Flux of temperature (T) in the vertical 
C               direction at the upper(U) and lower(D) faces of a cell.
C     maskUp  :: Land mask used to denote base of the domain.
C     xA      :: Tracer cell face area normal to X
C     yA      :: Tracer cell face area normal to X
C     uTrans  :: Zonal volume transport through cell face
C     vTrans  :: Meridional volume transport through cell face
C     rTrans  :: Vertical volume transport through cell face
C     bi, bj, iMin, iMax, jMin, jMax :: Range of points for which calculation
C                                      results will be set.
C     myThid - Instance number for this innvocation of CALC_GT
      _RL fVerT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER k,kUp,kDown,kM1
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

CEOP

#ifdef ALLOW_AUTODIFF_TAMC
C--   only the kUp part of fverT is set in this subroutine
C--   the kDown is still required
      fVerT(1,1,kDown) = fVerT(1,1,kDown)
#endif

      CALL GAD_CALC_RHS(
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,maskUp,
     I           diffKhT, diffK4T, KappaRT, tr1,
     I           GAD_TR1, tracerAdvScheme,
     U           fVerT, gTr1,
     I           myThid )

#ifdef INCLUDE_TR_FORCING_CODE
C--   External thermal forcing term(s)
      CALL EXTERNAL_FORCING_TR(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)
#endif /*  INCLUDE_TR_FORCING_CODE */

      IF ( tracerAdvScheme.EQ.ENUM_CENTERED_2ND
     & .OR.tracerAdvScheme.EQ.ENUM_UPWIND_3RD
     & .OR.tracerAdvScheme.EQ.ENUM_CENTERED_4TH ) THEN
        CALL ADAMS_BASHFORTH2(
     I                        bi, bj, K,
     U                        gTr1, gTr1nm1,
     I                        myIter, myThid )
      ENDIF

#ifdef NONLIN_FRSURF
      IF (nonlinFreeSurf.GT.0) THEN
        CALL FREESURF_RESCALE_G(
     I                          bi, bj, K,
     U                          gTr1,
     I                          myThid )
      ENDIF
#endif /* NONLIN_FRSURF */

      RETURN
      END
