#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

CBOP 0
C !ROUTINE: SEAICE_OUTPUT

C !INTERFACE:
      SUBROUTINE SEAICE_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE SEAICE_OUTPUT
C     | o Do SEAICE output and TimeAve averaging and output.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"
#ifdef ALLOW_EXF
# include "EXF_FIELDS.h"
#endif

C     !INPUT PARAMETERS:
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
C     == Local variables in common block :

C     == Local variables ==
      CHARACTER*(10) suff
#ifdef ALLOW_SITRACER
      CHARACTER*(13) pref
      INTEGER iTracer
#endif
#ifdef ALLOW_MNC
      CHARACTER*(1) pf
#endif /* ALLOW_MNC */
CEOP

      IF (SEAICEwriteState) THEN

        IF ( DIFFERENT_MULTIPLE(SEAICE_dumpFreq,myTime,deltaTClock)
     &    .OR. dumpInitAndLast.AND.( myTime.EQ.endTime .OR.
     &                               myTime.EQ.startTime  )
     &       ) THEN

#ifdef ALLOW_MNC
          IF ( useMNC .AND. SEAICE_dump_mnc ) THEN
            IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
              pf(1:1) = 'D'
            ELSE
              pf(1:1) = 'R'
            ENDIF
            CALL MNC_CW_SET_UDIM('sice', -1, myThid)
            CALL MNC_CW_RL_W_S('D','sice',0,0,'T', myTime, myThid)
            CALL MNC_CW_SET_UDIM('sice', 0, myThid)
            CALL MNC_CW_I_W_S('I','sice',0,0,'iter', myIter, myThid)
            CALL MNC_CW_RL_W_S('D','sice',0,0,'model_time',
     &           myTime,myThid)
#if ( defined(SEAICE_CGRID) && defined(SEAICE_ALLOW_EVP) )
            IF ( SEAICEuseEVP ) THEN
             CALL MNC_CW_RL_W(pf,'sice',0,0,'si_sigma1',
     &            seaice_sigma1,myThid)
             CALL MNC_CW_RL_W(pf,'sice',0,0,'si_sigma2',
     &            seaice_sigma2,myThid)
             CALL MNC_CW_RL_W(pf,'sice',0,0,'si_sigma12',
     &            seaice_sigma12,myThid)
            ENDIF
#endif /* SEAICE_CGRID and SEAICE_ALLOW_EVP */
            CALL MNC_CW_RL_W(pf,'sice',0,0,'si_UICE',uIce,myThid)
            CALL MNC_CW_RL_W(pf,'sice',0,0,'si_VICE',vIce,myThid)
            IF ( .NOT.useThSIce ) THEN
             CALL MNC_CW_RL_W(pf,'sice',0,0,'si_HEFF',hEff,myThid)
             CALL MNC_CW_RL_W(pf,'sice',0,0,'si_AREA',area,myThid)
#ifdef SEAICE_ITD
c           CALL MNC_CW_RL_W(pf,'sice',0,0,'si_HEFFITD',HEFFITD,myThid)
c           CALL MNC_CW_RL_W(pf,'sice',0,0,'si_AREAITD',AREAITD,myThid)
#endif
            ENDIF
#ifdef ALLOW_EXF
            CALL MNC_CW_RL_W(pf,'sice',0,0,'si_UWIND',uwind,myThid)
            CALL MNC_CW_RL_W(pf,'sice',0,0,'si_VWIND',vwind,myThid)
#endif
            CALL MNC_CW_RS_W(pf,'sice',0,0,'fu',fu,myThid)
            CALL MNC_CW_RS_W(pf,'sice',0,0,'fv',fv,myThid)
            CALL MNC_CW_RS_W(pf,'sice',0,0,'EmPmR',EmPmR,myThid)
            CALL MNC_CW_RS_W(pf,'sice',0,0,'Qnet',Qnet,myThid)
            CALL MNC_CW_RS_W(pf,'sice',0,0,'Qsw',Qsw,myThid)
          ENDIF
#endif /* ALLOW_MNC */
          IF (SEAICE_dump_mdsio) THEN
            IF ( rwSuffixType.EQ.0 ) THEN
              WRITE(suff,'(I10.10)') myIter
            ELSE
              CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
            ENDIF
            IF ( myIter.NE.nIter0 ) THEN
#ifdef ALLOW_EXF
             CALL WRITE_FLD_XY_RL('UWIND.',suff,uwind,myIter,myThid)
             CALL WRITE_FLD_XY_RL('VWIND.',suff,vwind,myIter,myThid)
#endif
             CALL WRITE_FLD_XY_RS('FU.',suff,fu,myIter,myThid)
             CALL WRITE_FLD_XY_RS('FV.',suff,fv,myIter,myThid)
             CALL WRITE_FLD_XY_RS('EmPmR.',suff,EmPmR,myIter,myThid)
             CALL WRITE_FLD_XY_RS('Qnet.',suff,Qnet,myIter,myThid)
             CALL WRITE_FLD_XY_RS('Qsw.',suff,Qsw,myIter,myThid)
            ENDIF
            CALL WRITE_FLD_XY_RL( 'UICE.',suff,uIce,myIter,myThid)
            CALL WRITE_FLD_XY_RL( 'VICE.',suff,vIce,myIter,myThid)
            IF ( .NOT.useThSIce ) THEN
             CALL WRITE_FLD_XY_RL( 'HEFF.',suff,hEff,myIter,myThid)
             CALL WRITE_FLD_XY_RL( 'AREA.',suff,area,myIter,myThid)
             CALL WRITE_FLD_XY_RL( 'HSNOW.',suff,HSNOW,myIter,myThid)
#ifdef SEAICE_ITD
             CALL WRITE_FLD_3D_RL( 'HEFFITD.', suff, nITD, HEFFITD,
     &                             myIter, myThid )
             CALL WRITE_FLD_3D_RL( 'AREAITD.', suff, nITD, AREAITD,
     &                             myIter, myThid )
             CALL WRITE_FLD_3D_RL('HSNOWITD.', suff, nITD, HSNOWITD,
     &                             myIter, myThid )
#endif
#ifdef SEAICE_VARIABLE_SALINITY
             CALL WRITE_FLD_XY_RL( 'HSALT.',suff,HSALT,myIter,myThid)
#endif
#ifdef ALLOW_SITRACER
             DO iTracer = 1, SItrNumInUse
              WRITE(pref,'(A8,I2.2,A1)') 'SITRACER', iTracer, '.'
              CALL WRITE_FLD_XY_RL(
     &             pref,suff,
     &             SItracer(1-OLx,1-OLy,1,1,iTracer),
     &             myIter, myThid )
             ENDDO
#endif
            ENDIF
#if ( defined(SEAICE_CGRID) && defined(SEAICE_ALLOW_EVP) )
            IF ( SEAICEuseEVP ) THEN
             CALL WRITE_FLD_XY_RL('SIGMA1.',suff,seaice_sigma1,
     &            myIter,myThid)
             CALL WRITE_FLD_XY_RL('SIGMA2.',suff,seaice_sigma2,
     &            myIter,myThid)
             CALL WRITE_FLD_XY_RL('SIGMA12.',suff,seaice_sigma12,
     &            myIter,myThid)
            ENDIF
#endif /* SEAICE_CGRID and SEAICE_ALLOW_EVP */
C--   end SEAICE_dump_mdsio block
          ENDIF

        ENDIF
      ENDIF

C----------------------------------------------------------------

C--   do SEAICE monitor output : print some statistics about seaice fields
      CALL SEAICE_MONITOR( myTime, myIter, myThid )

C--   do SEAICE Open-Boundary output
      IF ( useOBCS ) CALL SEAICE_OBCS_OUTPUT( myTime, myIter, myThid )

      RETURN
      END
