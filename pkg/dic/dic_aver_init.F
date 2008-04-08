C $Header: /u/gcmpack/MITgcm/pkg/dic/Attic/dic_aver_init.F,v 1.9 2008/04/08 20:21:35 dfer Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE DIC_AVER_INIT(
     I           myThid)

C     /==========================================================\
C     | SUBROUTINE DIC_AVER_INIT  i                            |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_FIELDS.h"
#include "DIC_VARS.h"
#ifdef DIC_BIOTIC
#include "DIC_DIAGS.h"
#include "DIC_COST.h"
#endif

C     == Routine arguments ==
      INTEGER myThid

#ifdef ALLOW_DIC_COST

C     == Local variables ==
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
        totcost=0.d0
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
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
         call tracer_meanarea(myThid,po4obs, k,
     &                    po4av(k))
         call tracer_meanarea(myThid,o2obs, k,
     &                    o2av(k))
c        print*,po4av(k), o2av(k)
        enddo
cswdmonth -add-
        do it=1,12
         call tracer_meanarea(myThid,po4obsl1,it,
     &                    po4avm(it,1))
         call tracer_meanarea(myThid,po4obsl2,it,
     &                    po4avm(it,2))
         call tracer_meanarea(myThid,po4obsl3,it,
     &                    po4avm(it,3))
cQQ      call tracer_meanarea(myThid,po4obsl4,it,
cQQ  &                    po4avm(it,4))
         call tracer_meanarea(myThid,o2obsl1,it,
     &                    o2avm(it,1))
         call tracer_meanarea(myThid,o2obsl2,it,
     &                    o2avm(it,2))
         call tracer_meanarea(myThid,o2obsl3,it,
     &                    o2avm(it,3))
cQQ      call tracer_meanarea(myThid,o2obsl4,it,
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
C
C Reset averages to zero
       print*,'QQ dic_aver_init, set to zero'
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(PO4ann,Nr,bi,bj,myThid)
         CALL TIMEAVE_RESET(O2ann,Nr,bi,bj,myThid)
         CALL TIMEAVE_RESET(PO4lev1,    12,  bi, bj, myThid)
         CALL TIMEAVE_RESET(PO4lev2,    12,  bi, bj, myThid)
          CALL TIMEAVE_RESET(PO4lev3,    12,  bi, bj, myThid)
cQQ       CALL TIMEAVE_RESET(PO4lev4,    12,  bi, bj, myThid)
         CALL TIMEAVE_RESET(O2lev1,    12,  bi, bj, myThid)
         CALL TIMEAVE_RESET(O2lev2,    12,  bi, bj, myThid)
         CALL TIMEAVE_RESET(O2lev3,    12,  bi, bj, myThid)
cQQ       CALL TIMEAVE_RESET(O2lev4,    12,  bi, bj, myThid)

         do k=1,Nr
           OBS_Timetave(bi,bj,k)=0.d0
         enddo
         do it=1,12
           OBSM_Timetave(bi,bj,it)=0.d0
         enddo
        ENDDO
       ENDDO
c
#endif
c
      RETURN
      END
cswd -- end added subroutine --
