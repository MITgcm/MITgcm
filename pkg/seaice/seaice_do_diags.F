C $Header: /u/gcmpack/MITgcm/pkg/seaice/Attic/seaice_do_diags.F,v 1.30 2007/05/18 02:46:42 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

      SUBROUTINE SEAICE_DO_DIAGS( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_DO_DIAGS                               |
C     | o Do SEAICE diagnostic output.                           |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "SEAICE_DIAGS.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_FFIELDS.h"
#include "SEAICE.h"

C     == Routine arguments ==
C     myTime        - Current time of simulation ( s )
C     myIter        - Iteration number
C     myThid        - Number of this instance of SEAICE_DO_DIAGS
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) suff
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
      INTEGER i, j, k, bi, bj
      _RL arr  (1-oLx:sNx+oLx,1-oLy:sNy+oLy,nSx,nSy)
#ifdef ALLOW_DIAGNOSTICS
      _RL brr  (1-oLx:sNx+oLx,1-oLy:sNy+oLy,nSx,nSy)
      _RL e11  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e22  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e12  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL PRESS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sig1, sig2, sig12, sigTmp, recip_prs
#endif /* ALLOW_DIAGNOSTICS */
      LOGICAL dumpFiles
      CHARACTER*(1) pf

      IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
        pf(1:1) = 'D'
      ELSE
        pf(1:1) = 'R'
      ENDIF

      IF (SEAICEwriteState) THEN

        IF ( DIFFERENT_MULTIPLE(SEAICE_dumpFreq,myTime,deltaTClock)
     &    .OR. dumpInitAndLast.AND.( myTime.EQ.endTime .OR.
     &                               myTime.EQ.startTime  )
     &       ) THEN

          _BARRIER
#ifdef ALLOW_MNC
          IF ( useMNC .AND. SEAICE_dump_mnc ) THEN
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
            CALL MNC_CW_RS_W(pf,'sice',0,0,'si_UWIND',uwind,myThid)
            CALL MNC_CW_RS_W(pf,'sice',0,0,'si_VWIND',vwind,myThid)
            CALL MNC_CW_RS_W(pf,'sice',0,0,'fu',fu,myThid)
            CALL MNC_CW_RS_W(pf,'sice',0,0,'fv',fv,myThid)
            CALL MNC_CW_RS_W(pf,'sice',0,0,'EmPmR',EmPmR,myThid)
            CALL MNC_CW_RS_W(pf,'sice',0,0,'Qnet',Qnet,myThid)
            CALL MNC_CW_RS_W(pf,'sice',0,0,'Qsw',Qsw,myThid)
          ENDIF
#endif
          IF (SEAICE_dump_mdsio) THEN
            WRITE(suff,'(I10.10)') myIter
            IF ( myIter.NE.nIter0 ) THEN
            CALL WRITE_FLD_XY_RS( 'UWIND.',suff,uwind,myIter,myThid)
            CALL WRITE_FLD_XY_RS( 'VWIND.',suff,vwind,myIter,myThid)
            CALL WRITE_FLD_XY_RS( 'FU.',suff,fu,myIter,myThid)
            CALL WRITE_FLD_XY_RS( 'FV.',suff,fv,myIter,myThid)
            CALL WRITE_FLD_XY_RS( 'EmPmR.',suff,EmPmR,myIter,myThid)
            CALL WRITE_FLD_XY_RS( 'Qnet.',suff,Qnet,myIter,myThid)
            CALL WRITE_FLD_XY_RS( 'Qsw.',suff,Qsw,myIter,myThid)
            ENDIF
            CALL WRITE_FLD_XY_RL( 'HSNOW.',suff,HSNOW,myIter,myThid)
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
          ENDIF

#ifdef SEAICE_DEBUG
#ifndef SEAICE_EXTERNAL_FORCING
       CALL PLOT_FIELD_XYRS( uwind , 'Current uwind ', myIter, myThid )
       CALL PLOT_FIELD_XYRS( vwind , 'Current vwind ', myIter, myThid )
       CALL PLOT_FIELD_XYRS( atemp , 'Current atemp ', myIter, myThid )
       CALL PLOT_FIELD_XYRS( aqh   , 'Current aqh   ', myIter, myThid )
       CALL PLOT_FIELD_XYRS( lwdown, 'Current lwdown', myIter, myThid )
       CALL PLOT_FIELD_XYRS( swdown, 'Current swdown', myIter, myThid )
       CALL PLOT_FIELD_XYRS( precip, 'Current precip', myIter, myThid )
       CALL PLOT_FIELD_XYRL( evap  , 'Current evap  ', myIter, myThid )
       CALL PLOT_FIELD_XYRS( runoff, 'Current runoff', myIter, myThid )
       CALL PLOT_FIELD_XYRS( SSS   , 'Current SSS   ', myIter, myThid )
       CALL PLOT_FIELD_XYRS( SST   , 'Current SST   ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( fu    , 'Current fu    ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( fv    , 'Current fv    ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( EmPmR , 'Current EmPmR ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( Qnet  , 'Current Qnet  ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( Qsw   , 'Current Qsw   ', myIter, myThid )
#endif
#endif /* SEAICE_DEBUG */

          DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
              DO j=1-Oly,sNy+Oly
                DO i=1-Olx,sNx+Olx
                  arr(i,j,bi,bj)=UICE(i,j,1,bi,bj)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
          _BARRIER
          IF (SEAICE_dump_mdsio) THEN
            CALL WRITE_FLD_XY_RL( 'UICE.',suff,arr,myIter,myThid)
          ENDIF
#ifdef ALLOW_MNC
          IF ( useMNC .AND. SEAICE_dump_mnc ) THEN
            CALL MNC_CW_RL_W(pf,'sice',0,0,'si_UICE',arr,myThid)
          ENDIF
#endif
#ifdef SEAICE_DEBUG
          _EXCH_XY_R8( arr, myThid )
          CALL PLOT_FIELD_XYRL( arr   , 'Current uice  ',
     &         myIter, myThid )
#endif
          _BARRIER

          DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
              DO j=1-Oly,sNy+Oly
                DO i=1-Olx,sNx+Olx
                  arr(i,j,bi,bj)=VICE(i,j,1,bi,bj)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
          _BARRIER
          IF (SEAICE_dump_mdsio) THEN
            CALL WRITE_FLD_XY_RL( 'VICE.',suff,arr,myIter,myThid)
          ENDIF
#ifdef ALLOW_MNC
          IF ( useMNC .AND. SEAICE_dump_mnc ) THEN
            CALL MNC_CW_RL_W(pf,'sice',0,0,'si_VICE',arr,myThid)
          ENDIF
#endif
#ifdef SEAICE_DEBUG
          _EXCH_XY_R4( arr, myThid )
          CALL PLOT_FIELD_XYRL( arr   , 'Current vice  ',
     &         myIter, myThid )
#endif
          _BARRIER

          DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
              DO j=1-Oly,sNy+Oly
                DO i=1-Olx,sNx+Olx
                  arr(i,j,bi,bj)=HEFF(i,j,1,bi,bj)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
          _BARRIER
          IF (SEAICE_dump_mdsio) THEN
            CALL WRITE_FLD_XY_RL( 'HEFF.',suff,arr,myIter,myThid)
          ENDIF
#ifdef ALLOW_MNC
          IF ( useMNC .AND. SEAICE_dump_mnc ) THEN
            CALL MNC_CW_RL_W(pf,'sice',0,0,'si_HEFF',arr,myThid)
          ENDIF
#endif
#ifdef SEAICE_DEBUG
          _EXCH_XY_R4( arr, myThid )
          CALL PLOT_FIELD_XYRL( arr   , 'Current heff  ',
     &         myIter, myThid )
#endif
          _BARRIER

          DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
              DO j=1-Oly,sNy+Oly
                DO i=1-Olx,sNx+Olx
                  arr(i,j,bi,bj)=AREA(i,j,1,bi,bj)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
          _BARRIER
          IF (SEAICE_dump_mdsio) THEN
            CALL WRITE_FLD_XY_RL( 'AREA.',suff,arr,myIter,myThid)
          ENDIF
#ifdef ALLOW_MNC
          IF ( useMNC .AND. SEAICE_dump_mnc ) THEN
            CALL MNC_CW_RL_W(pf,'sice',0,0,'si_AREA',arr,myThid)
          ENDIF
#endif
#ifdef SEAICE_DEBUG
          _EXCH_XY_R4( arr, myThid )
          CALL PLOT_FIELD_XYRL( arr   , 'Current area  ',
     &         myIter, myThid )
#endif
          _BARRIER

         ENDIF
      ENDIF

C----------------------------------------------------------------
C     Do SEAICE time averaging.
C----------------------------------------------------------------

#ifdef ALLOW_TIMEAVE

C--   Time-cumulations
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          FUtave(i,j,1,bi,bj)   =
     &         FUtave(i,j,1,bi,bj)   +FU(i,j,bi,bj)    *deltaTclock
          FVtave(i,j,1,bi,bj)   =
     &         FVtave(i,j,1,bi,bj)   +FV(i,j,bi,bj)    *deltaTclock
          EmPmRtave(i,j,1,bi,bj)=
     &         EmPmRtave(i,j,1,bi,bj)+EmPmR(i,j,bi,bj) *deltaTclock
          QNETtave(i,j,1,bi,bj) =
     &         QNETtave(i,j,1,bi,bj) +QNET(i,j,bi,bj)  *deltaTclock
          QSWtave(i,j,1,bi,bj)  =
     &         QSWtave(i,j,1,bi,bj)  +QSW(i,j,bi,bj)   *deltaTclock
          UICEtave(i,j,1,bi,bj) =
     &         UICEtave(i,j,1,bi,bj) +UICE(i,j,1,bi,bj)*deltaTclock
          VICEtave(i,j,1,bi,bj) =
     &         VICEtave(i,j,1,bi,bj) +VICE(i,j,1,bi,bj)*deltaTclock
          HEFFtave(i,j,1,bi,bj) =
     &         HEFFtave(i,j,1,bi,bj) +HEFF(i,j,1,bi,bj)*deltaTclock
          AREAtave(i,j,1,bi,bj) =
     &         AREAtave(i,j,1,bi,bj) +AREA(i,j,1,bi,bj)*deltaTclock
         ENDDO
        ENDDO
        DO k=1,Nr
         SEAICE_TimeAve(k,bi,bj)=SEAICE_TimeAve(k,bi,bj)+deltaTclock
        ENDDO
       ENDDO
      ENDDO

C     Dump files and restart average computation if needed
      dumpFiles = .FALSE.
      IF ( myIter .NE. nIter0 ) THEN
        dumpFiles =
     &     DIFFERENT_MULTIPLE(SEAICE_taveFreq,myTime,deltaTClock)
#ifdef ALLOW_CAL
        IF ( useCAL ) THEN
          CALL CAL_TIME2DUMP( SEAICE_taveFreq, deltaTClock,
     U                        dumpFiles,
     I                        myTime, myIter, myThid )
        ENDIF
#endif
      ENDIF

      IF (dumpFiles) THEN
C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZ(FUtave   ,SEAICE_timeave, 1,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(FVtave   ,SEAICE_timeave, 1,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(EmPmRtave,SEAICE_timeave, 1,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(QNETtave ,SEAICE_timeave, 1,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(QSWtave  ,SEAICE_timeave, 1,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(UICEtave ,SEAICE_timeave, 1,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(VICEtave ,SEAICE_timeave, 1,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(HEFFtave ,SEAICE_timeave, 1,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(AREAtave ,SEAICE_timeave, 1,
     &                 bi,bj,myThid)
        ENDDO
       ENDDO
c      IF (myIter.EQ.10) WRITE(0,*) myThid, dumpFiles

       _BARRIER
#ifdef ALLOW_MNC
       IF (useMNC .AND. SEAICE_tave_mnc) THEN
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
       _BARRIER

C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(FUtave   ,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(FVtave   ,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(EmPmRtave,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(QNETtave ,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(QSWtave  ,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(UICEtave ,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(VICEtave ,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(HEFFtave ,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(AREAtave ,1,bi,bj,myThid)
         DO k=1,Nr
          SEAICE_TimeAve(k,bi,bj)=ZERO
         ENDDO
        ENDDO
       ENDDO

      ENDIF

#endif /* ALLOW_TIMEAVE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL DIAGNOSTICS_FILL(AREA(1-oLx,1-oLy,1,bi,bj),
     &          'SIarea  ',0,1 ,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(HEFF(1-oLx,1-oLy,1,bi,bj),
     &        'SIheff  ',0,1 ,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(UICE(1-oLx,1-oLy,1,bi,bj),
     &        'SIuice  ',0,1 ,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(VICE(1-oLx,1-oLy,1,bi,bj),
     &        'SIvice  ',0,1 ,2,bi,bj,myThid)
        ENDDO
       ENDDO
c
       CALL DIAGNOSTICS_FILL(HSNOW  ,'SIhsnow ',0,1 ,0,1,1,myThid)
c
       CALL DIAGNOSTICS_FILL(UWIND  ,'SIuwind ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(VWIND  ,'SIvwind ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(FU     ,'SIfu    ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(FV     ,'SIfv    ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(EmPmR  ,'SIempmr ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(Qnet   ,'SIqnet  ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(Qsw    ,'SIqsw   ',0,1 ,0,1,1,myThid)
       IF ( DIAGNOSTICS_IS_ON('SIsigI  ',myThid) .OR.
     &      DIAGNOSTICS_IS_ON('SIsigII ',myThid) .OR.
     &      DIAGNOSTICS_IS_ON('SIpress ',myThid) .OR.
     &      DIAGNOSTICS_IS_ON('SIzeta  ',myThid) .OR.
     &      DIAGNOSTICS_IS_ON('SIeta   ',myThid) ) THEN
C     Unfortunately, for this we have to recompute a few things
C     first make sure we have a current copy of ice velocities
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            uIceC(I,J,bi,bj)=uIce(I,J,1,bi,bj)
            vIceC(I,J,bi,bj)=vIce(I,J,1,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
C      compute strainrates, "viscosities" and modified pressure
        CALL SEAICE_CALC_STRAINRATES( 
     I       uIceC, vIceC,
     O       e11, e22, e12,
     I       myThid )
        
        CALL SEAICE_CALC_VISCOSITIES( 
     I       e11, e22, e12, zMin, zMax, hEffM, press0,
     O       eta, zeta, press, 
     I       myThid )
C     
        CALL DIAGNOSTICS_FILL(zeta   ,'SIzeta  ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(eta    ,'SIeta   ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(eta    ,'SIpress ',0,1 ,0,1,1,myThid)
       ENDIF
        
       IF ( DIAGNOSTICS_IS_ON('SIsigI  ',myThid) .OR.
     &      DIAGNOSTICS_IS_ON('SIsigII ',myThid) ) THEN
C     principle stress components
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            sig1  = 2.*zeta(I,J,bi,bj)
     &           * (e11(I,J,bi,bj) + e22(I,J,bi,bj)) 
     &           - PRESS(I,J,bi,bj)
            sig2  = 2.* eta(I,J,bi,bj)
     &           * (e11(I,J,bi,bj) - e22(I,J,bi,bj)) 
            sig12 = 2.*eta(I,J,bi,bj) * 0.25 _d 0 * 
     &           ( e12(I  ,J  ,bi,bj) + e12(I+1,J  ,bi,bj)
     &           + e12(I  ,J+1,bi,bj) + e12(I+1,J+1,bi,bj) )
            sigTmp = SQRT( sig2*sig2 + 4. _d 0*sig12*sig12 )
            recip_prs = 0. _d 0
            IF ( PRESS(I,J,  bi,bj) .GT. 1. _d -13 .AND. 
     &           AREA (I,J,1,bi,bj) .GT. A22 ) 
     &           recip_prs = 1./PRESS(I,J,bi,bj)
            arr(I,J,bi,bj) = 0.5*(sig1 + sigTmp)*recip_prs
            brr(I,J,bi,bj) = 0.5*(sig1 - sigTmp)*recip_prs
           ENDDO
          ENDDO
         ENDDO
        ENDDO
C     
        CALL DIAGNOSTICS_FILL(arr    ,'SIsigI  ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(brr    ,'SIsigII ',0,1 ,0,1,1,myThid)
       ENDIF
C     endif useDiagnostics
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   do SEAICE monitor output : print some statistics about seaice fields
      CALL SEAICE_MONITOR( myTime, myIter, myThid )



      RETURN
      END
