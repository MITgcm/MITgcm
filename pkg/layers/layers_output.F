C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_output.F,v 1.8 2012/09/19 18:48:18 gforget Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"

CBOP 0
C     !ROUTINE: LAYERS_OUTPUT

C     !INTERFACE:
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
      CHARACTER*(MAX_LEN_MBUF) suff
      INTEGER iLa
#ifdef ALLOW_TIMEAVE
      INTEGER bi, bj
#endif
#ifdef ALLOW_MNC
      CHARACTER*(1) pf
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics )
     &  CALL LAYERS_DIAGNOSTICS_FILL( myThid )
#endif

#ifdef ALLOW_LAYERS_OUTPUT

cgf layers_maxNum loop and dimension would be needed for
cgf the following and tave output to work beyond iLa.EQ.1
      DO iLa=1,layers_maxNum
      IF ( layers_num(iLa).EQ.1 ) THEN      

#ifdef ALLOW_MNC
      IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
        pf(1:1) = 'D'
      ELSE
        pf(1:1) = 'R'
      ENDIF
#endif /* ALLOW_MNC */

      IF ( DIFFERENT_MULTIPLE(layers_diagFreq,myTime,deltaTClock)
     &  .AND. myIter .GT. 0 ) THEN

cgf this needs layers_maxNum loop
        IF ( layers_MDSIO ) THEN
          WRITE(suff,'(I10.10)') myIter
#ifdef LAYERS_UFLUX
            CALL WRITE_FLD_3D_RL( 'layers_UFlux.', suff, Nlayers,
     &                             layers_UFlux, myIter, myThid )
#ifdef LAYERS_THICKNESS
            CALL WRITE_FLD_3D_RL( 'layers_HU.', suff, Nlayers,
     &                             layers_HU, myIter, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */
#ifdef LAYERS_VFLUX
            CALL WRITE_FLD_3D_RL( 'layers_VFlux.', suff, Nlayers,
     &                             layers_VFlux, myIter, myThid )
#ifdef LAYERS_THICKNESS
            CALL WRITE_FLD_3D_RL( 'layers_HV.', suff, Nlayers,
     &                             layers_HV, myIter, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */
#ifdef LAYERS_PRHO_REF
       IF (layers_num(1) .EQ. 3) THEN
            CALL WRITE_FLD_3D_RL( 'layers_prho.', suff, Nr,
     &                             prho, myIter, myThid )
       ENDIF
#endif /* LAYERS_PRHO_REF */
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
      IF ( layers_taveFreq.GT.0. ) THEN

c set arrays to zero if first timestep
       IF ( myIter.EQ.nIter0 ) THEN
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
          layers_TimeAve(bi,bj) = 0.
         ENDDO
        ENDDO

C     Dump files and restart average computation if needed
       ELSEIF (
     &  DIFFERENT_MULTIPLE(layers_taveFreq,myTime,deltaTClock)
     &        ) THEN

C      Normalize by integrated time
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)

#ifdef LAYERS_UFLUX
          CALL TIMEAVE_NORMALIZE( layers_UFlux_T, layers_timeave,
     &                            Nlayers, bi, bj, myThid )
#ifdef LAYERS_THICKNESS
          CALL TIMEAVE_NORMALIZE( layers_HU_T, layers_timeave,
     &                            Nlayers, bi, bj, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
          CALL TIMEAVE_NORMALIZE( layers_VFlux_T, layers_timeave,
     &                            Nlayers, bi, bj, myThid )
#ifdef LAYERS_THICKNESS
          CALL TIMEAVE_NORMALIZE( layers_HV_T, layers_timeave,
     &                            Nlayers, bi, bj, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

#ifdef LAYERS_PRHO_REF
       IF (layers_num(1) .EQ. 3) THEN
          CALL TIMEAVE_NORMALIZE( prho_tave, layers_timeave,
     &                            Nr, bi, bj, myThid )
       ENDIF
#endif /* LAYERS_PRHO_REF */

         ENDDO
        ENDDO

        IF ( layers_MDSIO ) THEN
         WRITE(suff,'(I10.10)') myIter
#ifdef LAYERS_UFLUX
         CALL WRITE_FLD_3D_RL( 'layers_UFlux-tave.', suff, Nlayers,
     &                          layers_UFlux_T, myIter, myThid )
#ifdef LAYERS_THICKNESS
         CALL WRITE_FLD_3D_RL( 'layers_HU-tave.', suff, Nlayers,
     &                          layers_HU_T, myIter, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */
#ifdef LAYERS_VFLUX
         CALL WRITE_FLD_3D_RL( 'layers_VFlux-tave.', suff, Nlayers,
     &                          layers_VFlux_T, myIter, myThid )
#ifdef LAYERS_THICKNESS
         CALL WRITE_FLD_3D_RL( 'layers_HV-tave.', suff, Nlayers,
     &                          layers_HV_T, myIter, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

#ifdef LAYERS_PRHO_REF
      IF (layers_num(1) .EQ. 3) THEN
         CALL WRITE_FLD_3D_RL( 'layers_prho-tave.', suff, Nr,
     &                          prho_tave, myIter, myThid )
      ENDIF
#endif /* LAYERS_PRHO_REF */

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

#ifdef LAYERS_PRHO_REF
          IF (layers_num(1) .EQ. 3) THEN
          CALL TIMEAVE_RESET(prho_tave,Nr,bi,bj,myThid)
          ENDIF
#endif /* LAYERS_PRHO_REF */

          layers_TimeAve(bi,bj) = 0.
         ENDDO
        ENDDO

C--   end of bloc: if time is a multiple of layers_taveFreq
       ENDIF

      ENDIF
#endif /* ALLOW_TIMEAVE */

      ENDIF !IF ( layers_num(iLa).EQ.1 ) THEN      
      ENDDO !DO iLa=1,layers_maxNum

#endif /* ALLOW_LAYERS_OUTPUT */

#endif /* ALLOW_LAYERS */

      RETURN
      END
