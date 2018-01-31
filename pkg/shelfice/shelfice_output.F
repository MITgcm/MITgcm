#include "SHELFICE_OPTIONS.h"

CBOP
C     !ROUTINE: SHELFICE_OUTPUT
C     !INTERFACE:
      SUBROUTINE SHELFICE_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_OUTPUT
C     | o Do SHELFICE snapshot output
C     |   and compute & write time-average output.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SHELFICE.h"
#include "SHELFICE_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: my time in simulation ( s )
C     myIter :: my Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(10) suff
#ifdef ALLOW_TIMEAVE
      INTEGER i, j, bi, bj
      LOGICAL dumpFiles
#endif
#ifdef ALLOW_MNC
      CHARACTER*(1) pf
#endif
CEOP

      IF (SHELFICEwriteState) THEN

C- note: surf.Fluxes have not yet been computed when called @ nIter0
       IF ( myIter .NE. nIter0 .AND.
     &      DIFFERENT_MULTIPLE(SHELFICE_dumpFreq,myTime,deltaTClock)
     &    ) THEN

#ifdef ALLOW_MNC
        IF ( useMNC .AND. SHELFICE_dump_mnc ) THEN
         IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
           pf(1:1) = 'D'
         ELSE
           pf(1:1) = 'R'
         ENDIF
         CALL MNC_CW_SET_UDIM('shelfice', -1, myThid)
         CALL MNC_CW_RL_W_S('D','shelfice',0,0,'T', myTime, myThid)
         CALL MNC_CW_SET_UDIM('shelfice', 0, myThid)
         CALL MNC_CW_I_W_S('I','shelfice',0,0,'iter', myIter, myThid)
         CALL MNC_CW_RL_W_S('D','shelfice',0,0,'model_time',
     &        myTime,myThid)
         CALL MNC_CW_RS_W(pf,'shelfice',0,0,'fwFlux',
     &        shelfIceFreshWaterFlux,myThid)
         CALL MNC_CW_RS_W(pf,'shelfice',0,0,'heatFlux',
     &        shelfIceHeatFlux,myThid)
        ENDIF
#endif
        IF (SHELFICE_dump_mdsio) THEN
         IF ( rwSuffixType.EQ.0 ) THEN
           WRITE(suff,'(I10.10)') myIter
         ELSE
           CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
         ENDIF
         CALL WRITE_FLD_XY_RS( 'SHICE_fwFlux.',suff,
     &        shelficeFreshWaterFlux,myIter,myThid)
         CALL WRITE_FLD_XY_RS( 'SHICE_heatFlux.',suff,
     &        shelficeHeatFlux,myIter,myThid)
        ENDIF

       ENDIF
      ENDIF

C----------------------------------------------------------------
C     Do SHELFICE time averaging.
C----------------------------------------------------------------

#ifdef ALLOW_TIMEAVE
      IF ( SHELFICE_taveFreq .GT. 0 ) THEN

       IF ( myIter.EQ.nIter0 ) THEN
C--    Initialise averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET( shelfice_frshFluxtave, 1, bi,bj, myThid )
          CALL TIMEAVE_RESET( shelfice_heatFluxtave, 1, bi,bj, myThid )
          SHELFICE_timeAve(bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ELSE
C--   Time-cumulations
C- note: surf.Fluxes have not yet been computed when called @ nIter0
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            shelfice_FrshFluxtave(i,j,1,bi,bj)=
     &         shelfice_frshFluxtave(i,j,1,bi,bj)
     &         + shelficeFreshWaterFlux(i,j,bi,bj) *deltaTClock
            shelfice_heatFluxtave(i,j,1,bi,bj) =
     &         shelfice_heatFluxtave(i,j,1,bi,bj)
     &         + shelficeHeatFlux(i,j,bi,bj)  *deltaTClock
           ENDDO
          ENDDO
          SHELFICE_timeAve(bi,bj) = SHELFICE_timeAve(bi,bj)+deltaTClock
         ENDDO
        ENDDO
       ENDIF

C     Dump files and restart average computation if needed
       dumpFiles = .FALSE.
       IF ( myIter .NE. nIter0 ) THEN
        dumpFiles =
     &     DIFFERENT_MULTIPLE(SHELFICE_taveFreq,myTime,deltaTClock)
#ifdef ALLOW_CAL
        IF ( useCAL ) THEN
          CALL CAL_TIME2DUMP( zeroRL, SHELFICE_taveFreq, deltaTClock,
     U                        dumpFiles,
     I                        myTime, myIter, myThid )
        ENDIF
#endif
       ENDIF

       IF (dumpFiles) THEN
C-     Normalize by integrated time
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_NORMALIZE( shelfice_frshFluxtave,
     &                            SHELFICE_timeAve, 1, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( shelfice_heatFluxtave,
     &                            SHELFICE_timeAve, 1, bi, bj, myThid )
         ENDDO
        ENDDO

#ifdef ALLOW_MNC
        IF (useMNC .AND. SHELFICE_tave_mnc) THEN
         IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
           pf(1:1) = 'D'
         ELSE
           pf(1:1) = 'R'
         ENDIF
         CALL MNC_CW_SET_UDIM('shelfice_tave', -1, myThid)
         CALL MNC_CW_RL_W_S( 'D','shelfice_tave', 0, 0,
     &                       'T', myTime, myThid )
         CALL MNC_CW_SET_UDIM('shelfice_tave', 0, myThid)
         CALL MNC_CW_I_W_S( 'I', 'shelfice_tave', 0, 0,
     &                       'iter', myIter, myThid )
C        CALL MNC_CW_RL_W_S( 'D','shelfice_tave', 0, 0,
C     &                      'model_time', myTime, myThid )
         CALL MNC_CW_RL_W( pf, 'shelfice_tave', 0, 0,
     &             'fwFluxtave', Shelfice_FrshFluxtave, myThid )
         CALL MNC_CW_RL_W( pf, 'shelfice_tave', 0, 0,
     &           'heatFluxtave', Shelfice_HeatFluxtave, myThid )
        ENDIF
#endif
        IF (SHELFICE_tave_mdsio) THEN
         IF ( rwSuffixType.EQ.0 ) THEN
           WRITE(suff,'(I10.10)') myIter
         ELSE
           CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
         ENDIF
         CALL WRITE_FLD_XY_RL( 'SHICE_fwFluxtave.', suff,
     &                         shelfice_frshFluxtave, myIter, myThid )
         CALL WRITE_FLD_XY_RL( 'SHICE_heatFluxtave.', suff,
     &                         shelfice_heatFluxtave, myIter, myThid )
        ENDIF

C-     Reset averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET( shelfice_frshFluxtave, 1, bi,bj, myThid )
          CALL TIMEAVE_RESET( shelfice_heatFluxtave, 1, bi,bj, myThid )
          SHELFICE_timeAve(bi,bj) = 0. _d 0
         ENDDO
        ENDDO

C--   end if dumpFiles
       ENDIF

C--   end if SHELFICE_taveFreq > 0
      ENDIF
#endif /* ALLOW_TIMEAVE */

      RETURN
      END
