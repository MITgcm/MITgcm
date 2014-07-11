C $Header: /u/gcmpack/MITgcm/pkg/land/land_output.F,v 1.6 2014/07/11 23:24:58 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_OUTPUT
C     !INTERFACE:
      SUBROUTINE LAND_OUTPUT( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_OUTPUT
C     | o general routine for Land output
C     *==========================================================*
C     | - write snap-shot & time-average output
C     | - call monitor to write global quantities
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "LAND_SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAND_PARAMS.h"
#include "LAND_VARS.h"
#include "LAND_TAVE.h"
#ifdef ALLOW_AIM
#include "AIM_FFIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid - Number of this instance of the routine
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_LAND

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi, bj, k
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(10) suff
#ifdef ALLOW_MNC
      CHARACTER*(1) pf
#endif

#ifdef ALLOW_AIM
      IF ( land_monFreq.NE.0. ) THEN
        CALL LAND_MONITOR( aim_landFr, myTime, myIter, myThid )
      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (
     &     DIFFERENT_MULTIPLE( land_diagFreq, myTime, land_deltaT )
     &  .OR. dumpInitAndLast.AND.( myTime.EQ.endTime .OR.
     &                             myTime.EQ.startTime  )
     &   ) THEN

C--   Write snap-shot
C jmc: previously done from LAND_DO_DIAGS, but much better here.

        IF ( land_snapshot_mdsio ) THEN

          WRITE(suff,'(I10.10)') myIter

C--   Write ground Temp and soil moisture :
          CALL WRITE_FLD_3D_RL( 'land_groundT.', suff, land_nLev,
     &                           land_groundT, myIter, myThid )
          CALL WRITE_FLD_3D_RL( 'land_enthalp.', suff, land_nLev,
     &                           land_enthalp, myIter, myThid )
          CALL WRITE_FLD_3D_RL( 'land_groundW.', suff, land_nLev,
     &                           land_groundW, myIter, myThid )
C--   other (2-D) state variables:
          CALL WRITE_FLD_XY_RL(
     &         'land_skinT.', suff, land_skinT, myIter, myThid )
          CALL WRITE_FLD_XY_RL(
     &         'land_hSnow.', suff, land_hSnow, myIter, myThid )
          CALL WRITE_FLD_XY_RL(
     &         'land_snAge.', suff, land_snowAge, myIter, myThid )

          IF ( myIter.NE.nIter0 ) THEN
C--   fluxes (2-D map):
          CALL WRITE_FLD_XY_RL(
     &         'land_RunOff.', suff, land_runOff, myIter, myThid )
          CALL WRITE_FLD_XY_RL(
     &         'land_enRnOf.', suff, land_enRnOf, myIter, myThid )
          CALL WRITE_FLD_XY_RL(
     &         'land_HeatFx.', suff, land_HeatFlx, myIter, myThid )
          CALL WRITE_FLD_XY_RL(
     &         'land_frWaFx.', suff, land_Pr_m_Ev, myIter, myThid )
          CALL WRITE_FLD_XY_RL(
     &         'land_EnWaFx.', suff, land_EnWFlux, myIter, myThid )
          ENDIF

        ENDIF

#ifdef ALLOW_MNC
        IF ( land_snapshot_mnc ) THEN
          _BARRIER

          IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
            pf(1:1) = 'D'
          ELSE
            pf(1:1) = 'R'
          ENDIF
          WRITE(fn,'(A)') 'land_snapshot'
          CALL MNC_CW_SET_UDIM(fn, -1, myThid)
          CALL MNC_CW_RL_W_S('D',fn,0,0,'T', myTime, myThid)
          CALL MNC_CW_SET_UDIM(fn, 0, myThid)
          CALL MNC_CW_I_W_S('I',fn,0,0,'iter', myIter, myThid)

          CALL MNC_CW_RL_W(pf,fn,0,0,
     &         'land_groundT', land_groundT, myThid)
          CALL MNC_CW_RL_W(pf,fn,0,0,
     &         'land_enthalp', land_enthalp, myThid)
          CALL MNC_CW_RL_W(pf,fn,0,0,
     &         'land_groundW', land_groundW, myThid)

          CALL MNC_CW_RL_W(pf,fn,0,0,
     &         'land_skinT', land_skinT, myThid)
          CALL MNC_CW_RL_W(pf,fn,0,0,
     &         'land_hSnow', land_hSnow, myThid)
          CALL MNC_CW_RL_W(pf,fn,0,0,
     &         'land_snAge', land_snowAge, myThid)
          CALL MNC_CW_RL_W(pf,fn,0,0,
     &         'land_RunOff', land_runOff, myThid)
          CALL MNC_CW_RL_W(pf,fn,0,0,
     &         'land_enRnOf', land_enRnOf, myThid)

          CALL MNC_CW_RL_W(pf,fn,0,0,
     &         'land_HeatFx', land_HeatFlx, myThid)
          CALL MNC_CW_RL_W(pf,fn,0,0,
     &         'land_frWaFx', land_Pr_m_Ev, myThid)
          CALL MNC_CW_RL_W(pf,fn,0,0,
     &         'land_EnWaFx', land_EnWFlux, myThid)

          _BARRIER
        ENDIF
#endif

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_LAND_TAVE

      IF (land_taveFreq.LE.0.) RETURN

      IF ( myIter.EQ.nIter0 ) THEN
C      Initialize time-average arrays to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(land_grTtave,land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_entave, land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_grWtave,land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_sTtave,         1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_hStave,         1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_sAtave,         1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_ROftave,        1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_eROtave,        1, bi, bj, myThid)
         land_timeAve(bi,bj) = 0.
        ENDDO
       ENDDO

C     Dump files and restart average computation if needed
      ELSEIF (
     &     DIFFERENT_MULTIPLE( land_taveFreq, myTime, land_deltaT )
     &       ) THEN

C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZE( land_grTtave, land_timeAve,
     &                           land_nLev, bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( land_entave,  land_timeAve,
     &                           land_nLev, bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( land_grWtave, land_timeAve,
     &                           land_nLev, bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( land_sTtave,  land_timeAve,
     &                                   1, bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( land_hStave,  land_timeAve,
     &                                   1, bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( land_sAtave,  land_timeAve,
     &                                   1, bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( land_ROftave, land_timeAve,
     &                                   1, bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( land_eROtave, land_timeAve,
     &                                   1, bi, bj, myThid )
        ENDDO
       ENDDO

       IF ( land_timeave_mdsio ) THEN

        WRITE(fn,'(A,I10.10)') 'land_tave.', myIter
        CALL WRITE_REC_3D_RL( fn, writeBinaryPrec, land_nLev,
     &                        land_grTtave, 1, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, writeBinaryPrec, land_nLev,
     &                        land_entave,  2, myIter, myThid )
        CALL WRITE_REC_3D_RL( fn, writeBinaryPrec, land_nLev,
     &                        land_grWtave, 3, myIter, myThid )
        k = 3*land_nLev
        CALL WRITE_REC_XY_RL( fn, land_sTtave,  k+1, myIter, myThid )
        CALL WRITE_REC_XY_RL( fn, land_hStave,  k+2, myIter, myThid )
        CALL WRITE_REC_XY_RL( fn, land_sAtave,  k+3, myIter, myThid )
        CALL WRITE_REC_XY_RL( fn, land_ROftave, k+4, myIter, myThid )
        CALL WRITE_REC_XY_RL( fn, land_eROtave, k+5, myIter, myThid )

       ENDIF

#ifdef ALLOW_MNC
       IF ( land_timeave_mnc ) THEN
         _BARRIER

         IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
           pf(1:1) = 'D'
         ELSE
           pf(1:1) = 'R'
         ENDIF
         WRITE(fn,'(A)') 'land_tave'
         CALL MNC_CW_SET_UDIM(fn, -1, myThid)
         CALL MNC_CW_I_W_S('I',fn,0,0,'iter', myIter, myThid)
         CALL MNC_CW_SET_UDIM(fn, 0, myThid)
         CALL MNC_CW_RL_W_S('D',fn,0,0,'T', myTime, myThid)
         CALL MNC_CW_RL_W(pf,fn,0,0,
     &        'land_groundT', land_grTtave, myThid)
         CALL MNC_CW_RL_W(pf,fn,0,0,
     &        'land_enthalp', land_entave, myThid)
         CALL MNC_CW_RL_W(pf,fn,0,0,
     &        'land_groundW', land_grWtave, myThid)

         CALL MNC_CW_RL_W(pf,fn,0,0,
     &        'land_skinT', land_sTtave, myThid)
         CALL MNC_CW_RL_W(pf,fn,0,0,
     &        'land_hSnow', land_hStave, myThid)
         CALL MNC_CW_RL_W(pf,fn,0,0,
     &        'land_snAge', land_sAtave, myThid)
         CALL MNC_CW_RL_W(pf,fn,0,0,
     &        'land_RunOff', land_ROftave, myThid)
         CALL MNC_CW_RL_W(pf,fn,0,0,
     &        'land_enRnOf', land_eROtave, myThid)

         _BARRIER
       ENDIF
#endif

       WRITE(msgBuf,'(A,I10)')
     &  '// Land Time-average  written, t-step', myIter
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)') ' '
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(land_grTtave,land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_entave, land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_grWtave,land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_sTtave,         1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_hStave,         1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_sAtave,         1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_ROftave,        1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_eROtave,        1, bi, bj, myThid)
         land_timeAve(bi,bj) = 0.
        ENDDO
       ENDDO

      ENDIF

#endif /* ALLOW_LAND_TAVE */

#endif /* ALLOW_LAND */

      RETURN
      END
