cswdcost -- add sunroutine ---
#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"


CStartOfInterFace
      SUBROUTINE DIC_DIAGS(
     I           myTime,myIter,myThid)

C     /==========================================================\
C     | SUBROUTINE DIC_COST  i                            |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS.h"
#include "GCHEM.h"
#include "DIC_ABIOTIC.h"
#ifdef DIC_BIOTIC
#include "DIC_BIOTIC.h"
#include "DIC_DIAGS.h"
#endif
#ifdef ALLOW_SEAICE
#include "ICE.h"
#endif

C     == Routine arguments ==
      INTEGER myIter
      _RL myTime
      INTEGER myThid

#ifdef ALLOW_DIC_COST

C     == Local variables ==
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER i, j, bi, bj, k, it
      _RL po4av(nR)
      _RL o2av(nR)
      _RL volvar(nR)
cswdmonth -add-
      _RL po4avm(12,4)
      _RL o2avm(12,4)
      _RL rdt
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm

cswddmonth -- end-
c
c initialize to zero
      IF ( myIter .EQ. nIter0+1 ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET(PO4ann,   Nr,  bi, bj, myThid)
          CALL TIMEAVE_RESET(O2ann,   Nr,  bi, bj, myThid)
          CALL TIMEAVE_RESET(PO4obs,   Nr,  bi, bj, myThid)
          CALL TIMEAVE_RESET(O2obs,   Nr,  bi, bj, myThid)
cswdmonth
          CALL TIMEAVE_RESET(PO4obsl1,   Nr,  bi, bj, myThid)
          CALL TIMEAVE_RESET(PO4obsl2,   Nr,  bi, bj, myThid)
          CALL TIMEAVE_RESET(PO4obsl3,   Nr,  bi, bj, myThid)
cQQ       CALL TIMEAVE_RESET(PO4obsl4,   Nr,  bi, bj, myThid)
          CALL TIMEAVE_RESET(O2obsl1,   Nr,  bi, bj, myThid)
          CALL TIMEAVE_RESET(O2obsl2,   Nr,  bi, bj, myThid)
          CALL TIMEAVE_RESET(O2obsl3,   Nr,  bi, bj, myThid)
cQQ       CALL TIMEAVE_RESET(O2obsl4,   Nr,  bi, bj, myThid)
          CALL TIMEAVE_RESET(PO4lev1,    12,  bi, bj, myThid)
          CALL TIMEAVE_RESET(PO4lev2,    12,  bi, bj, myThid)
          CALL TIMEAVE_RESET(PO4lev3,    12,  bi, bj, myThid)
cQQ       CALL TIMEAVE_RESET(PO4lev4,    12,  bi, bj, myThid)
          CALL TIMEAVE_RESET(O2lev1,    12,  bi, bj, myThid)
          CALL TIMEAVE_RESET(O2lev2,    12,  bi, bj, myThid)
          CALL TIMEAVE_RESET(O2lev3,    12,  bi, bj, myThid)
cQQ       CALL TIMEAVE_RESET(O2lev4,    12,  bi, bj, myThid)
cswdmonth -end-
          do k=1,Nr
            OBS_Timetave(bi,bj,k)=0.d0
            po4av(k)=0.d0
            o2av(k)=0.d0
            po4var(k)=0.d0
            o2var(k)=0.d0
            volvar(k)=0.d0
          enddo 
cswdmonth
          do k=1,3
           do it=1,12
            OBSM_Timetave(bi,bj,it)=0.d0
            po4avm(it,k)=0.d0
            o2avm(it,k)=0.d0
            po4varm(it,k)=0.d0
            o2varm(it,k)=0.d0
           enddo
          enddo
         ENDDO
        ENDDO 
        _BEGIN_MASTER( myThid )
        CALL READ_FLD_XYZ_RL( 'input/po4obs.bin', ' ', 
     &                          po4obs, 0, myThid )        
        CALL READ_FLD_XYZ_RL( 'input/o2obs.bin', ' ', 
     &                          o2obs, 0, myThid )
cswdmonth
        CALL READ_FLD_XYZ_RL( 'input/po4lev1.bin', ' ',
     &                          po4obsl1, 0, myThid )
        CALL READ_FLD_XYZ_RL( 'input/po4lev2.bin', ' ',
     &                          po4obsl2, 0, myThid )
        CALL READ_FLD_XYZ_RL( 'input/po4lev3.bin', ' ',
     &                          po4obsl3, 0, myThid )
cQQ     CALL READ_FLD_XYZ_RL( 'input/po4lev4.bin', ' ',
cQQ  &                          po4obsl4, 0, myThid )
        CALL READ_FLD_XYZ_RL( 'input/o2lev1.bin', ' ',
     &                          o2obsl1, 0, myThid )
        CALL READ_FLD_XYZ_RL( 'input/o2lev2.bin', ' ',
     &                          o2obsl2, 0, myThid )
        CALL READ_FLD_XYZ_RL( 'input/o2lev3.bin', ' ',
     &                          o2obsl3, 0, myThid )
cQQ     CALL READ_FLD_XYZ_RL( 'input/o2lev4.bin', ' ',
cQQ  &                          o2obsl4, 0, myThid )
cswdmonth -end-
       _END_MASTER(myThid)
       _EXCH_XYZ_R8(po4obs  , myThid )
       _EXCH_XYZ_R8(o2obs  , myThid )
cswdmonth -add-
       _EXCH_XYZ_R8(po4obsl1  , myThid )
       _EXCH_XYZ_R8(po4obsl2  , myThid )
       _EXCH_XYZ_R8(po4obsl3  , myThid )
cQQ    _EXCH_XYZ_R8(po4obsl4  , myThid )
       _EXCH_XYZ_R8(o2obsl1  , myThid )
       _EXCH_XYZ_R8(o2obsl2  , myThid )
       _EXCH_XYZ_R8(o2obsl3  , myThid )
cQQ    _EXCH_XYZ_R8(o2obsl4  , myThid )
cswdmonth -end-

        _BARRIER
c calculate layer means
        _BEGIN_MASTER( mythid )
        do k=1,Nr
         call tracer_meanarea(myIter,myTime,myThid,po4obs, k,
     &                    po4av(k))
         call tracer_meanarea(myIter,myTime,myThid,o2obs, k,
     &                    o2av(k))
c        print*,po4av(k), o2av(k)
        enddo
cswdmonth -add-
        do it=1,12
         call tracer_meanarea(myIter,myTime,myThid,po4obsl1,it,
     &                    po4avm(it,1))
         call tracer_meanarea(myIter,myTime,myThid,po4obsl2,it,
     &                    po4avm(it,2))
         call tracer_meanarea(myIter,myTime,myThid,po4obsl3,it,
     &                    po4avm(it,3))
cQQ      call tracer_meanarea(myIter,myTime,myThid,po4obsl4,it,
cQQ  &                    po4avm(it,4))
         call tracer_meanarea(myIter,myTime,myThid,o2obsl1,it,
     &                    o2avm(it,1))
         call tracer_meanarea(myIter,myTime,myThid,o2obsl2,it,
     &                    o2avm(it,2))
         call tracer_meanarea(myIter,myTime,myThid,o2obsl3,it,
     &                    o2avm(it,3))
cQQ      call tracer_meanarea(myIter,myTime,myThid,o2obsl4,it,
cQQ  &                    o2avm(it,4))

        enddo
        _END_MASTER(myThid)
c calculate layer variance
        _BEGIN_MASTER( mythid )
        DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          DO k=1,Nr
            volvar(k)=volvar(k)+
     &                rA(i,j,bi,bj)*drF(k)*maskC(i,j,k,bi,bj)
            po4var(k)=po4var(k)+(po4obs(i,j,k,bi,bj)-po4av(k))**2
     &                *rA(i,j,bi,bj)*drF(k)*maskC(i,j,k,bi,bj)
            o2var(k)=o2var(k)+(o2obs(i,j,k,bi,bj)-o2av(k))**2
     &                *rA(i,j,bi,bj)*drF(k)*maskC(i,j,k,bi,bj)
          ENDDO
cswdmonth -add-
          DO it=1,12
           po4varm(it,1)=po4varm(it,1)+
     &                (po4obsl1(i,j,it,bi,bj)-po4avm(it,1))**2
     &                *rA(i,j,bi,bj)*drF(1)*maskC(i,j,1,bi,bj)
           po4varm(it,2)=po4varm(it,2)+
     &                (po4obsl2(i,j,it,bi,bj)-po4avm(it,2))**2
     &                *rA(i,j,bi,bj)*drF(2)*maskC(i,j,2,bi,bj)
           po4varm(it,3)=po4varm(it,3)+
     &                (po4obsl3(i,j,it,bi,bj)-po4avm(it,3))**2
     &                *rA(i,j,bi,bj)*drF(3)*maskC(i,j,3,bi,bj)
cQQ        po4varm(it,4)=po4varm(it,4)+
cQQ  &                (po4obsl4(i,j,it,bi,bj)-po4avm(it,4))**2
cQQ  &                *rA(i,j,bi,bj)*drF(4)*maskC(i,j,4,bi,bj)
           o2varm(it,1)=o2varm(it,1)+
     &                (o2obsl1(i,j,it,bi,bj)-o2avm(it,1))**2
     &                *rA(i,j,bi,bj)*drF(1)*maskC(i,j,1,bi,bj)
           o2varm(it,2)=o2varm(it,2)+
     &                (o2obsl2(i,j,it,bi,bj)-o2avm(it,2))**2
     &                *rA(i,j,bi,bj)*drF(2)*maskC(i,j,2,bi,bj)
           o2varm(it,3)=o2varm(it,3)+
     &                (o2obsl3(i,j,it,bi,bj)-o2avm(it,3))**2
     &                *rA(i,j,bi,bj)*drF(3)*maskC(i,j,3,bi,bj)
cQQ        o2varm(it,4)=o2varm(it,4)+
cQQ  &                (o2obsl4(i,j,it,bi,bj)-o2avm(it,4))**2
cQQ  &                *rA(i,j,bi,bj)*drF(4)*maskC(i,j,4,bi,bj)

          ENDDO
         ENDDO
         ENDDO
        ENDDO
        ENDDO
        DO k=1,Nr
            po4var(k)=po4var(k)/volvar(k)
            o2var(k)=o2var(k)/volvar(k)
cQQ         print*,po4var(k),o2var(k)
        ENDDO
cswdmonth- add-
        DO k=1,3
         Do it=1,12
           po4varm(it,k)=po4varm(it,k)/volvar(k)
           o2varm(it,k)=o2varm(it,k)/volvar(k)
         ENDDO
        ENDDO
cswdmonth -end-
        _END_MASTER(myThid)
      ENDIF
C
c averages
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         OBS_timetave(bi,bj,k)=OBS_timetave(bi,bj,k)+
     &                         deltaTclock
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            po4ann(i,j,k,bi,bj)=po4ann(i,j,k,bi,bj)+
     &               PTRACER(i,j,k,bi,bj,3)*deltaTclock
            o2ann(i,j,k,bi,bj)=o2ann(i,j,k,bi,bj)+
     &               PTRACER(i,j,k,bi,bj,5)*deltaTclock
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
cswdmonth-add--
      rdt=1. _d 0 / deltaTclock
      nForcingPeriods=int(externForcingCycle/externForcingPeriod+0.5)
      Imytm=int(myTime*rdt+0.5)
      Ifprd=int(externForcingPeriod*rdt+0.5)
      Ifcyc=int(externForcingCycle*rdt+0.5)
      Iftm=mod( Imytm+Ifcyc ,Ifcyc)
      it=int(Iftm/Ifprd)+1
c     print*,'QQ timing check', mytime, myIter, it
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         OBSM_timetave(bi,bj,it)=OBSM_timetave(bi,bj,it)+
     &                         deltaTclock
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            po4lev1(i,j,it,bi,bj)=po4lev1(i,j,it,bi,bj)+
     &               PTRACER(i,j,1,bi,bj,3)*deltaTclock
            po4lev2(i,j,it,bi,bj)=po4lev2(i,j,it,bi,bj)+
     &               PTRACER(i,j,2,bi,bj,3)*deltaTclock
            po4lev3(i,j,it,bi,bj)=po4lev3(i,j,it,bi,bj)+
     &               PTRACER(i,j,3,bi,bj,3)*deltaTclock
cQQ         po4lev4(i,j,it,bi,bj)=po4lev4(i,j,it,bi,bj)+
cQQ  &               PTRACER(i,j,4,bi,bj,3)*deltaTclock
            o2lev1(i,j,it,bi,bj)=o2lev1(i,j,it,bi,bj)+
     &               PTRACER(i,j,1,bi,bj,5)*deltaTclock
            o2lev2(i,j,it,bi,bj)=o2lev2(i,j,it,bi,bj)+
     &               PTRACER(i,j,2,bi,bj,5)*deltaTclock
            o2lev3(i,j,it,bi,bj)=o2lev3(i,j,it,bi,bj)+
     &               PTRACER(i,j,3,bi,bj,5)*deltaTclock
cQQ         O2lev4(i,j,it,bi,bj)=O2lev4(i,j,it,bi,bj)+
cQQ  &               PTRACER(i,j,4,bi,bj,5)*deltaTclock
         ENDDO
        ENDDO
       ENDDO
      ENDDO

cswdmonth-end--


C     Dump files and restart average computation if needed
      IF ( DIFFERENT_MULTIPLE(
     &    externForcingCycle,myTime,myTime-deltaTClock)
     &     ) THEN
C
C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZ(PO4ann, OBS_timetave, nR ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(O2ann, OBS_timetave, nR ,
     &                 bi,bj,myThid)
cswdmonth-add
         CALL TIMEAVE_NORMALIZ(PO4lev1, OBSM_timetave, 12 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(PO4lev2, OBSM_timetave, 12 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(PO4lev3, OBSM_timetave, 12 ,
     &                 bi,bj,myThid)
cQQ      CALL TIMEAVE_NORMALIZ(PO4lev4, OBSM_timetave, 12 ,
cQQ  &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(O2lev1, OBSM_timetave, 12 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(O2lev2, OBSM_timetave, 12 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(O2lev3, OBSM_timetave, 12 ,
     &                 bi,bj,myThid)
cQQ      CALL TIMEAVE_NORMALIZ(O2lev4, OBSM_timetave, 12 ,
cQQ  &                 bi,bj,myThid)
cswdmonth -end-
        ENDDO 
       ENDDO
c
c calculate cost function
        _BEGIN_MASTER( mythid )
         call DIC_COST(myTime,myIter,myThid)
        _END_MASTER( mythid )
c
C Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(PO4ann,Nr,bi,bj,myThid)
         CALL TIMEAVE_RESET(O2ann,Nr,bi,bj,myThid)
         do k=1,Nr
           OBS_Timetave(bi,bj,k)=0.d0
         enddo
cswdmonth-add--
         CALL TIMEAVE_RESET(PO4lev1,12,bi,bj,myThid)
         CALL TIMEAVE_RESET(PO4lev2,12,bi,bj,myThid)
         CALL TIMEAVE_RESET(PO4lev3,12,bi,bj,myThid)
cQQ      CALL TIMEAVE_RESET(PO4lev4,12,bi,bj,myThid)
         CALL TIMEAVE_RESET(O2lev1,12,bi,bj,myThid)
         CALL TIMEAVE_RESET(O2lev2,12,bi,bj,myThid)
         CALL TIMEAVE_RESET(O2lev3,12,bi,bj,myThid)
cQQ      CALL TIMEAVE_RESET(O2lev4,12,bi,bj,myThid)
         do it=1,12
           OBSM_Timetave(bi,bj,it)=0.d0
         enddo
cswdmonth-end--
        ENDDO
       ENDDO
c
      ENDIF
#endif
c
      RETURN
      END
cswd -- end added subroutine --
