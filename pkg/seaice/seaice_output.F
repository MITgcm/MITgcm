C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_output.F,v 1.16 2013/05/25 16:19:56 jmc Exp $
C $Name:  $

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
#include "SEAICE_TAVE.h"
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
#ifdef ALLOW_TIMEAVE
      INTEGER i, j, bi, bj
#ifdef SEAICE_ITD
      INTEGER k
#endif
      LOGICAL dumpFiles
#endif /* ALLOW_TIMEAVE */
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
            CALL MNC_CW_RL_W(pf,'sice',0,0,'si_HEFF',hEff,myThid)
            CALL MNC_CW_RL_W(pf,'sice',0,0,'si_AREA',area,myThid)
#ifdef SEAICE_ITD
c           CALL MNC_CW_RL_W(pf,'sice',0,0,'si_HEFFITD',HEFFITD,myThid)
c           CALL MNC_CW_RL_W(pf,'sice',0,0,'si_AREAITD',AREAITD,myThid)
#endif
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
            WRITE(suff,'(I10.10)') myIter
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
            CALL WRITE_FLD_XY_RL( 'HEFF.',suff,hEff,myIter,myThid)
            CALL WRITE_FLD_XY_RL( 'AREA.',suff,area,myIter,myThid)
            CALL WRITE_FLD_XY_RL( 'HSNOW.',suff,HSNOW,myIter,myThid)
#ifdef SEAICE_ITD
            CALL WRITE_FLD_3D_RL( 'HEFFITD.', suff, nITD, HEFFITD,
     &                            myIter, myThid )
            CALL WRITE_FLD_3D_RL( 'AREAITD.', suff, nITD, AREAITD,
     &                            myIter, myThid )
            CALL WRITE_FLD_3D_RL('HSNOWITD.', suff, nITD, HSNOWITD,
     &                            myIter, myThid )
#endif
#ifdef SEAICE_VARIABLE_SALINITY
            CALL WRITE_FLD_XY_RL( 'HSALT.',suff,HSALT,myIter,myThid)
#endif
#ifdef ALLOW_SITRACER
            DO iTracer = 1, SItrNumInUse
             WRITE(pref,'(A8,I2.2,A1)') 'SITRACER', iTracer, '.'
             CALL WRITE_FLD_XY_RL(
     &            pref,suff,
     &            SItracer(1-OLx,1-OLy,1,1,iTracer),
     &            myIter,myThid)
            ENDDO
#endif
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
C     Do SEAICE time averaging.
C----------------------------------------------------------------

#ifdef ALLOW_TIMEAVE
      IF ( SEAICE_taveFreq.GT.0. _d 0 ) THEN

C--   Time-cumulations
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
C- note(jmc): surf.Fluxes have not yet been computed when called @ nIter0
           FUtave(i,j,bi,bj)   =
     &         FUtave(i,j,bi,bj)   +FU(i,j,bi,bj)    *deltaTclock
           FVtave(i,j,bi,bj)   =
     &         FVtave(i,j,bi,bj)   +FV(i,j,bi,bj)    *deltaTclock
           EmPmRtave(i,j,bi,bj)=
     &         EmPmRtave(i,j,bi,bj)+EmPmR(i,j,bi,bj) *deltaTclock
           QNETtave(i,j,bi,bj) =
     &         QNETtave(i,j,bi,bj) +QNET(i,j,bi,bj)  *deltaTclock
           QSWtave(i,j,bi,bj)  =
     &         QSWtave(i,j,bi,bj)  +QSW(i,j,bi,bj)   *deltaTclock
           UICEtave(i,j,bi,bj) =
     &         UICEtave(i,j,bi,bj) +UICE(i,j,bi,bj)*deltaTclock
           VICEtave(i,j,bi,bj) =
     &         VICEtave(i,j,bi,bj) +VICE(i,j,bi,bj)*deltaTclock
           HEFFtave(i,j,bi,bj) =
     &         HEFFtave(i,j,bi,bj) +HEFF(i,j,bi,bj)*deltaTclock
           AREAtave(i,j,bi,bj) =
     &         AREAtave(i,j,bi,bj) +AREA(i,j,bi,bj)*deltaTclock
          ENDDO
         ENDDO
#ifdef SEAICE_ITD
         DO k=1,nITD
          DO j=1,sNy
           DO i=1,sNx
            HEFFITDtave(i,j,k,bi,bj) =
     &        HEFFITDtave(i,j,k,bi,bj) +HEFFITD(i,j,k,bi,bj)*deltaTclock
            AREAITDtave(i,j,k,bi,bj) =
     &        AREAITDtave(i,j,k,bi,bj) +AREAITD(i,j,k,bi,bj)*deltaTclock
           ENDDO
          ENDDO
         ENDDO
#endif
         SEAICE_timeAve(bi,bj) = SEAICE_timeAve(bi,bj)+deltaTclock
        ENDDO
       ENDDO

C     Dump files and restart average computation if needed
       dumpFiles = .FALSE.
       IF ( myIter .NE. nIter0 ) THEN
        dumpFiles =
     &     DIFFERENT_MULTIPLE(SEAICE_taveFreq,myTime,deltaTClock)
#ifdef ALLOW_CAL
        IF ( useCAL ) THEN
          CALL CAL_TIME2DUMP( ZERO, SEAICE_taveFreq, deltaTClock,
     U                        dumpFiles,
     I                        myTime, myIter, myThid )
        ENDIF
#endif
       ENDIF

       IF (dumpFiles) THEN
C      Normalize by integrated time
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_NORMALIZE( FUtave,
     &                            SEAICE_timeAve, 1, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( FVtave,
     &                            SEAICE_timeAve, 1, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( EmPmRtave,
     &                            SEAICE_timeAve, 1, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( QNETtave,
     &                            SEAICE_timeAve, 1, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( QSWtave,
     &                            SEAICE_timeAve, 1, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( UICEtave,
     &                            SEAICE_timeAve, 1, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( VICEtave,
     &                            SEAICE_timeAve, 1, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( HEFFtave,
     &                            SEAICE_timeAve, 1, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( AREAtave,
     &                            SEAICE_timeAve, 1, bi, bj, myThid )
#ifdef SEAICE_ITD
          CALL TIMEAVE_NORMALIZE( HEFFITDtave,
     &                            SEAICE_timeAve, nITD, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( AREAITDtave,
     &                            SEAICE_timeAve, nITD, bi, bj, myThid )
#endif
         ENDDO
        ENDDO
c       IF (myIter.EQ.10) WRITE(0,*) myThid, dumpFiles

#ifdef ALLOW_MNC
        IF (useMNC .AND. SEAICE_tave_mnc) THEN
         IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
           pf(1:1) = 'D'
         ELSE
           pf(1:1) = 'R'
         ENDIF
         CALL MNC_CW_SET_UDIM('sice_tave', -1, myThid)
         CALL MNC_CW_RL_W_S('D','sice_tave',0,0,'T', myTime, myThid)
         CALL MNC_CW_SET_UDIM('sice_tave', 0, myThid)
         CALL MNC_CW_I_W_S('I','sice_tave',0,0,'iter', myIter, myThid)
C        CALL MNC_CW_RL_W_S('D','sice_tave',0,0,'model_time',
C    &        myTime,myThid)
         CALL MNC_CW_RL_W(pf,'sice_tave',0,0,
     &        'si_UICEtave',UICEtave,myThid)
         CALL MNC_CW_RL_W(pf,'sice_tave',0,0,
     &        'si_VICEtave',VICEtave,myThid)
         CALL MNC_CW_RL_W(pf,'sice_tave',0,0,
     &        'si_FUtave',FUtave,myThid)
         CALL MNC_CW_RL_W(pf,'sice_tave',0,0,
     &        'si_FVtave',FVtave,myThid)
         CALL MNC_CW_RL_W(pf,'sice_tave',0,0,
     &        'si_EmPmRtave',EmPmRtave,myThid)
         CALL MNC_CW_RL_W(pf,'sice_tave',0,0,
     &        'si_QNETtave',QNETtave,myThid)
         CALL MNC_CW_RL_W(pf,'sice_tave',0,0,
     &        'si_QSWtave',QSWtave,myThid)
         CALL MNC_CW_RL_W(pf,'sice_tave',0,0,
     &        'si_HEFFtave',HEFFtave,myThid)
         CALL MNC_CW_RL_W(pf,'sice_tave',0,0,
     &        'si_AREAtave',AREAtave,myThid)
        ENDIF
#endif
        IF (SEAICE_tave_mdsio) THEN
         WRITE(suff,'(I10.10)') myIter
         CALL WRITE_FLD_XY_RL('FUtave.'   ,suff,FUtave   ,myIter,myThid)
         CALL WRITE_FLD_XY_RL('FVtave.'   ,suff,FVtave   ,myIter,myThid)
         CALL WRITE_FLD_XY_RL('EmPmRtave.',suff,EmPmRtave,myIter,myThid)
         CALL WRITE_FLD_XY_RL('QNETtave.' ,suff,QNETtave ,myIter,myThid)
         CALL WRITE_FLD_XY_RL('QSWtave.'  ,suff,QSWtave  ,myIter,myThid)
         CALL WRITE_FLD_XY_RL('UICEtave.' ,suff,UICEtave ,myIter,myThid)
         CALL WRITE_FLD_XY_RL('VICEtave.' ,suff,VICEtave ,myIter,myThid)
         CALL WRITE_FLD_XY_RL('HEFFtave.' ,suff,HEFFtave ,myIter,myThid)
         CALL WRITE_FLD_XY_RL('AREAtave.' ,suff,AREAtave ,myIter,myThid)
        ENDIF

C      Reset averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET( FUtave   , 1, bi, bj, myThid )
          CALL TIMEAVE_RESET( FVtave   , 1, bi, bj, myThid )
          CALL TIMEAVE_RESET( EmPmRtave, 1, bi, bj, myThid )
          CALL TIMEAVE_RESET( QNETtave , 1, bi, bj, myThid )
          CALL TIMEAVE_RESET( QSWtave  , 1, bi, bj, myThid )
          CALL TIMEAVE_RESET( UICEtave , 1, bi, bj, myThid )
          CALL TIMEAVE_RESET( VICEtave , 1, bi, bj, myThid )
          CALL TIMEAVE_RESET( HEFFtave , 1, bi, bj, myThid )
          CALL TIMEAVE_RESET( AREAtave , 1, bi, bj, myThid )
          SEAICE_timeAve(bi,bj) = ZERO
         ENDDO
        ENDDO

C--   end dumpFiles block
       ENDIF

C--   end if SEAICE_taveFreq > 0
      ENDIF
#endif /* ALLOW_TIMEAVE */

C--   do SEAICE monitor output : print some statistics about seaice fields
      CALL SEAICE_MONITOR( myTime, myIter, myThid )

C--   do SEAICE Open-Boundary output
      IF ( useOBCS ) CALL SEAICE_OBCS_OUTPUT( myTime, myIter, myThid )

      RETURN
      END
