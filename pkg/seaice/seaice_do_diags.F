C $Header:

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

#ifdef ALLOW_SEAICE

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) suff
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER i, j, k, bi, bj
      _RS arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nSx,nSy)
         
      IF (SEAICEwriteState) THEN

         IF ( DIFFERENT_MULTIPLE(SEAICE_dumpFreq,myTime,
     &        myTime-deltaTClock) ) THEN
            
            WRITE(suff,'(I10.10)') myIter
            _BARRIER
            _BEGIN_MASTER( myThid )
            CALL WRITE_FLD_XY_RS( 'FU.',suff,fu,myIter,myThid)
            CALL WRITE_FLD_XY_RS( 'FV.',suff,fv,myIter,myThid)
            CALL WRITE_FLD_XY_RS( 'EmPmR.',suff,EmPmR,myIter,myThid)
            CALL WRITE_FLD_XY_RS( 'Qnet.',suff,Qnet,myIter,myThid)
            CALL WRITE_FLD_XY_RS( 'Qsw.',suff,Qsw,myIter,myThid)
            _END_MASTER( myThid )
            _BARRIER

            DO bj=myByLo(myThid),myByHi(myThid)
               DO bi=myBxLo(myThid),myBxHi(myThid)
                  DO j=1,sNy
                     DO i=1,sNx
                        arr(i,j,bi,bj)=UICE(i,j,1,bi,bj)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
            _BARRIER
            _BEGIN_MASTER( myThid )
            CALL WRITE_FLD_XY_RS( 'UICE.',suff,arr,myIter,myThid)
            _END_MASTER( myThid )
            _BARRIER
            
            DO bj=myByLo(myThid),myByHi(myThid)
               DO bi=myBxLo(myThid),myBxHi(myThid)
                  DO j=1,sNy
                     DO i=1,sNx
                        arr(i,j,bi,bj)=VICE(i,j,1,bi,bj)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
            _BARRIER
            _BEGIN_MASTER( myThid )
            CALL WRITE_FLD_XY_RS( 'VICE.',suff,arr,myIter,myThid)
            _END_MASTER( myThid )
            _BARRIER
            
            DO bj=myByLo(myThid),myByHi(myThid)
               DO bi=myBxLo(myThid),myBxHi(myThid)
                  DO j=1,sNy
                     DO i=1,sNx
                        arr(i,j,bi,bj)=HEFF(i,j,1,bi,bj)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
            _BARRIER
            _BEGIN_MASTER( myThid )
            CALL WRITE_FLD_XY_RS( 'HEFF.',suff,arr,myIter,myThid)
            _END_MASTER( myThid )
            _BARRIER
            
            DO bj=myByLo(myThid),myByHi(myThid)
               DO bi=myBxLo(myThid),myBxHi(myThid)
                  DO j=1,sNy
                     DO i=1,sNx
                        arr(i,j,bi,bj)=AREA(i,j,1,bi,bj)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
            _BARRIER
            _BEGIN_MASTER( myThid )
            CALL WRITE_FLD_XY_RS( 'AREA.',suff,arr,myIter,myThid)
            _END_MASTER( myThid )
            _BARRIER

#ifdef SEAICE_DEBUG
c       CALL PLOT_FIELD_XYRS( gairx,'Current gairx ', myIter, myThid )
c       CALL PLOT_FIELD_XYRS( gairy,'Current gairy ', myIter, myThid )
c       CALL PLOT_FIELD_XYRS( tair ,'Current tair ' , myIter, myThid )
c       CALL PLOT_FIELD_XYRS( qa   ,'Current qa '   , myIter, myThid )
c       CALL PLOT_FIELD_XYRS( flo  ,'Current flo '  , myIter, myThid )
c       CALL PLOT_FIELD_XYRS( fsh  ,'Current fsh '  , myIter, myThid )
       CALL PLOT_FIELD_XYRL( fu   ,'Current fu '   , myIter, myThid )
       CALL PLOT_FIELD_XYRL( fv   ,'Current fv '   , myIter, myThid )
       CALL PLOT_FIELD_XYRL( Qnet ,'Current Qnet ' , myIter, myThid )
#endif SEAICE_DEBUG

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
      IF ( myIter.NE.nIter0 .AND. 
     &     DIFFERENT_MULTIPLE(SEAICE_taveFreq,myTime,myTime-deltaTClock)
     &     ) THEN
         
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

#endif ALLOW_SEAICE
      
      RETURN
      END
