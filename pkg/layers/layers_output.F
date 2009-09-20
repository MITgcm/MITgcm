C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_output.F,v 1.2 2009/09/20 20:54:48 rpa Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"

      SUBROUTINE LAYERS_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LAYERS_OUTPUT
C     | o general routine for LAYERS output
C     *==========================================================*
C     |   write time-average & snap-shot output
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

C     !INPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time of simulation ( s )
C     myIter :: Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_LAYERS

C     !LOCAL VARIABLES:
C     == Local variables ==
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER bi, bj, K
      CHARACTER*(MAX_LEN_MBUF) suff
      CHARACTER*(1) pf

      IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
        pf(1:1) = 'D'
      ELSE
        pf(1:1) = 'R'
      ENDIF

      IF ( DIFFERENT_MULTIPLE(dumpFreq,myTime,deltaTClock)
     &  .AND. myIter .GT. 0 ) THEN

        IF ( layers_MDSIO ) THEN
          WRITE(suff,'(I10.10)') myIter
#ifdef LAYERS_UFLUX
            CALL WRITE_FLD_XYG_RL(
     &       'layers_UFlux.',suff,layers_UFlux,myIter,myThid)
#ifdef LAYERS_THICKNESS
            CALL WRITE_FLD_XYG_RL(
     &       'layers_HU.',suff,layers_HU,myIter,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYESR_UFLUX */
#ifdef LAYERS_VFLUX
            CALL WRITE_FLD_XYG_RL(
     &       'layers_VFlux.',suff,layers_VFlux,myIter,myThid)
#ifdef LAYERS_THICKNESS
            CALL WRITE_FLD_XYG_RL(
     &       'layers_HV.',suff,layers_HV,myIter,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYESR_VFLUX */
          ENDIF
        ENDIF

#ifdef ALLOW_MNC
#ifdef LAYERS_MNC
        IF ( layers_MNC) THEN
C           Do MNC output...
C           But how?
        ENDIF
#endif /* LAYERS_MNC */
#endif /* ALLOW_MNC */

#ifdef ALLOW_TIMEAVE

C     Dump files and restart average computation if needed
      IF ( DIFFERENT_MULTIPLE(taveFreq,myTime,deltaTClock)
     & .AND. myIter .GT. 0   ) THEN

C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

#ifdef LAYERS_UFLUX
         CALL TIMEAVE_NORMALIZ(layers_UFlux_T,layers_timeave,Nlayers,
     &                 bi,bj,myThid)
#ifdef LAYERS_THICKNESS
         CALL TIMEAVE_NORMALIZ(layers_HU_T,layers_timeave,Nlayers,
     &                 bi,bj,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
         CALL TIMEAVE_NORMALIZ(layers_VFlux_T,layers_timeave,Nlayers,
     &                 bi,bj,myThid)
#ifdef LAYERS_THICKNESS
         CALL TIMEAVE_NORMALIZ(layers_HV_T,layers_timeave,Nlayers,
     &                 bi,bj,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

        ENDDO
       ENDDO

       IF ( layers_MDSIO ) THEN
         WRITE(suff,'(I10.10)') myIter
#ifdef LAYERS_UFLUX
         CALL WRITE_FLD_XYG_RL(
     &    'layers_UFlux-tave.',suff,layers_UFlux_T,myIter,myThid)
#ifdef LAYERS_THICKNESS
         CALL WRITE_FLD_XYG_RL(
     &    'layers_HU-tave.',suff,layers_HU_T,myIter,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */
#ifdef LAYERS_VFLUX
         CALL WRITE_FLD_XYG_RL(
     &    'layers_VFlux-tave.',suff,layers_VFlux_T,myIter,myThid)
#ifdef LAYERS_THICKNESS
         CALL WRITE_FLD_XYG_RL(
     &    'layers_HV-tave.',suff,layers_HV_T,myIter,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */
       ENDIF

#ifdef ALLOW_MNC
C     Do MNC output.
#endif

C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef LAYERS_UFLUX
          CALL TIMEAVE_RESET(layers_UFlux_T,Nlayers,bi,bj,myThid)
#ifdef LAYERS_THICKNESS
          CALL TIMEAVE_RESET(layers_HU_T,Nlayers,bi,bj,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
          CALL TIMEAVE_RESET(layers_VFlux_T,Nlayers,bi,bj,myThid)
#ifdef LAYERS_THICKNESS
          CALL TIMEAVE_RESET(layers_HV_T,Nlayers,bi,bj,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */
         DO k=1,Nlayers
          layers_TimeAve(k,bi,bj)=0.
         ENDDO
        ENDDO
       ENDDO

      ENDIF


#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_LAYERS */

      RETURN
      END
