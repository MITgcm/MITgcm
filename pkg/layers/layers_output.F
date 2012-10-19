C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_output.F,v 1.12 2012/10/19 18:01:45 rpa Exp $
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
#ifdef ALLOW_TIMEAVE
C     !LOCAL VARIABLES:
C     == Local variables ==
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      CHARACTER*(MAX_LEN_MBUF) suff
      INTEGER iLa
      INTEGER bi, bj

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( layers_taveFreq.GT.0. ) THEN
cgf layers_maxNum loop and dimension would be needed for
cgf the following and tave output to work beyond iLa=1
c      DO iLa=1,layers_maxNum
       iLa=1

c set arrays to zero if first timestep
       IF ( myIter.EQ.nIter0 ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          layers_TimeAve(bi,bj) = 0.
#ifdef LAYERS_UFLUX
          CALL TIMEAVE_RESET(layers_UH_T,  Nlayers,bi,bj,myThid)
#ifdef LAYERS_THICKNESS
          CALL TIMEAVE_RESET(layers_Hw_T, Nlayers,bi,bj,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
          CALL TIMEAVE_RESET(layers_VH_T, Nlayers,bi,bj,myThid)
#ifdef LAYERS_THICKNESS
          CALL TIMEAVE_RESET(layers_Hs_T, Nlayers,bi,bj,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

#ifdef LAYERS_PRHO_REF
          CALL TIMEAVE_RESET(prho_tave,Nr,bi,bj,myThid)
#endif /* LAYERS_PRHO_REF */
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
          CALL TIMEAVE_NORMALIZE( layers_UH_T, layers_TimeAve,
     &                            Nlayers, bi, bj, myThid )
#ifdef LAYERS_THICKNESS
          CALL TIMEAVE_NORMALIZE( layers_Hw_T, layers_TimeAve,
     &                            Nlayers, bi, bj, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
          CALL TIMEAVE_NORMALIZE( layers_VH_T, layers_TimeAve,
     &                            Nlayers, bi, bj, myThid )
#ifdef LAYERS_THICKNESS
          CALL TIMEAVE_NORMALIZE( layers_Hs_T, layers_TimeAve,
     &                            Nlayers, bi, bj, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

#ifdef LAYERS_PRHO_REF
          IF ( layers_num(1).EQ.3 )
     &    CALL TIMEAVE_NORMALIZE( prho_tave, layers_TimeAve,
     &                            Nr, bi, bj, myThid )
#endif /* LAYERS_PRHO_REF */

         ENDDO
        ENDDO

        IF ( layers_MDSIO ) THEN
         WRITE(suff,'(I10.10)') myIter
#ifdef LAYERS_UFLUX
         CALL WRITE_FLD_3D_RL( 'layers_UH-tave.', suff, Nlayers,
     &                          layers_UH_T, myIter, myThid )
#ifdef LAYERS_THICKNESS
         CALL WRITE_FLD_3D_RL( 'layers_Hw-tave.', suff, Nlayers,
     &                          layers_Hw_T, myIter, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */
#ifdef LAYERS_VFLUX
         CALL WRITE_FLD_3D_RL( 'layers_VH-tave.', suff, Nlayers,
     &                          layers_VH_T, myIter, myThid )
#ifdef LAYERS_THICKNESS
         CALL WRITE_FLD_3D_RL( 'layers_Hs-tave.', suff, Nlayers,
     &                          layers_Hs_T, myIter, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

#ifdef LAYERS_PRHO_REF
         IF ( layers_num(1).EQ.3 )
     &   CALL WRITE_FLD_3D_RL( 'layers_prho-tave.', suff, Nr,
     &                          prho_tave, myIter, myThid )
#endif /* LAYERS_PRHO_REF */

        ENDIF

c#ifdef ALLOW_MNC
C     Do MNC output.
c#endif

C      Reset averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          layers_TimeAve(bi,bj) = 0.
#ifdef LAYERS_UFLUX
          CALL TIMEAVE_RESET(layers_UH_T,  Nlayers,bi,bj,myThid)
#ifdef LAYERS_THICKNESS
          CALL TIMEAVE_RESET(layers_Hw_T, Nlayers,bi,bj,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
          CALL TIMEAVE_RESET(layers_VH_T, Nlayers,bi,bj,myThid)
#ifdef LAYERS_THICKNESS
          CALL TIMEAVE_RESET(layers_Hs_T, Nlayers,bi,bj,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

#ifdef LAYERS_PRHO_REF
          IF ( layers_num(1).EQ.3 )
     &    CALL TIMEAVE_RESET(prho_tave,Nr,bi,bj,myThid)
#endif /* LAYERS_PRHO_REF */
         ENDDO
        ENDDO

C--   end of bloc: if time is a multiple of layers_taveFreq
       ENDIF

      ENDIF
#endif /* ALLOW_TIMEAVE */
#endif /* ALLOW_LAYERS */

      RETURN
      END
