C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_calc.F,v 1.15 2012/10/17 18:49:15 rpa Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"
#ifdef ALLOW_GMREDI
#include "GMREDI_OPTIONS.h"
#endif

CBOP 0
C !ROUTINE: LAYERS_CALC

C !INTERFACE:
      SUBROUTINE LAYERS_CALC(
     I                  myTime, myIter, myThid )

C !DESCRIPTION:
C ===================================================================
C     Calculate the transport in isopycnal layers.
C     This was the meat of the LAYERS package, which
C     has been moved to S/R LAYERS_FLUXCALC.F
C ===================================================================

C !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif

C !INPUT PARAMETERS:
C     myTime :: Current time in simulation
C     myIter :: Current iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_LAYERS

C !LOCAL VARIABLES:
C     bi, bj   :: tile indices
C     i,j      :: horizontal indices
C     iLa      :: layer coordinate index
C     k        :: vertical index for model grid

      INTEGER bi, bj, iLa
#ifdef LAYERS_PRHO_REF
      INTEGER i, j, k
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO iLa=1,layers_maxNum
      
      IF (layers_num(iLa) .EQ. 1) THEN
       CALL LAYERS_FLUXCALC( uVel,vVel,theta,iLa,
     &  layers_UH(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_VH(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_Hw(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_Hs(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_PIw(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_PIs(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_U(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_V(1-OLx,1-OLy,1,1,1,iLa),
     &  myThid)
      ELSEIF (layers_num(iLa) .EQ. 2) THEN
       CALL LAYERS_FLUXCALC( uVel,vVel,salt,iLa,
     &  layers_UH(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_VH(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_Hw(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_Hs(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_PIw(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_PIs(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_U(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_V(1-OLx,1-OLy,1,1,1,iLa),
     &  myThid)
      ELSEIF (layers_num(iLa) .EQ. 3) THEN
#ifdef LAYERS_PRHO_REF
C     For layers_num(iLa) = 3, calculate the potential density referenced to
C     the model level given by layers_krho.
       DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
       DO k = 1,Nr
        CALL FIND_RHO_2D( 1-OLx, sNx+OLx, 1-OLy, sNy+OLy,
     &     layers_krho(iLa),
     &     theta(1-OLx,1-OLy,k,bi,bj),
     &     salt(1-OLx,1-OLy,k,bi,bj),
     &     prho(1-OLx,1-OLy,k,bi,bj,iLa),
     &     k, bi, bj, myThid )
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
           prho(i,j,k,bi,bj,iLa) = rhoConst + prho(i,j,k,bi,bj,iLa)
         ENDDO
        ENDDO
       ENDDO
       ENDDO
       ENDDO
       CALL LAYERS_FLUXCALC( uVel,vVel,
     &  prho(1-OLx,1-OLy,1,1,1,iLa),iLa,
     &  layers_UH(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_VH(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_Hw(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_Hs(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_PIw(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_PIs(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_U(1-OLx,1-OLy,1,1,1,iLa),
     &  layers_V(1-OLx,1-OLy,1,1,1,iLa),     
     &  myThid)
#endif
      ENDIF

#ifdef ALLOW_LAYERS_OUTPUT

#ifdef ALLOW_TIMEAVE
C--   Time-average
cgf layers_maxNum loop and dimension would be needed for
cgf the following and tave output to work beyond iLa.EQ.1
      IF ( iLa.EQ.1 ) THEN
      IF ( layers_taveFreq.GT.0. ) THEN
C --- The tile loops
      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef LAYERS_UFLUX
         CALL TIMEAVE_CUMULATE( layers_UH_T, layers_UFlux, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
#ifdef LAYERS_THICKNESS
         CALL TIMEAVE_CUMULATE( layers_Hw_T, layers_HU, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
         CALL TIMEAVE_CUMULATE( layers_PIw_T, layers_PIw, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
         CALL TIMEAVE_CUMULATE( layers_U_T, layers_U, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */
#ifdef LAYERS_VFLUX
         CALL TIMEAVE_CUMULATE( layers_VH_T, layers_VFlux, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
#ifdef LAYERS_THICKNESS
         CALL TIMEAVE_CUMULATE( layers_Hs_T, layers_HV, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
         CALL TIMEAVE_CUMULATE( layers_PIs_T, layers_PIs, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
         CALL TIMEAVE_CUMULATE( layers_V_T, layers_V, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

#ifdef LAYERS_PRHO_REF
      IF (layers_num(iLa) .EQ. 3) THEN
         CALL TIMEAVE_CUMULATE( prho_tave, prho, Nr,
     &                          deltaTclock, bi, bj, myThid )
      ENDIF
#endif /* LAYERS_PRHO_REF */

         layers_TimeAve(bi,bj)=layers_TimeAve(bi,bj)+deltaTclock

C --- End bi,bj loop
      ENDDO
      ENDDO
      ENDIF !IF ( layers_taveFreq.GT.0. ) THEN
      ENDIF !IF ( iLa.EQ.1 ) THEN
#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_LAYERS_OUTPUT */

      ENDDO !DO iLa=1,layers_maxNum
      
#endif /* ALLOW_LAYERS */

      RETURN
      END
