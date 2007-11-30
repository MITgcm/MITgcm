C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_cost.F,v 1.7 2007/11/30 17:47:41 stephd Exp $
C $Name:  $

cswdcost -- add sunroutine ---
#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"


CStartOfInterFace
      SUBROUTINE DIC_COST (
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
c#include "PTRACERS_SIZE.h"
c#include "PTRACERS_PARAMS.h"
c#include "PTRACERS_FIELDS.h"
#include "GCHEM.h"
#include "DIC_ABIOTIC.h"
#ifdef DIC_BIOTIC
#include "DIC_BIOTIC.h"
#include "DIC_DIAGS.h"
#include "DIC_COST.h"
#endif
c#ifdef ALLOW_SEAICE
c#include "SEAICE.h"
c#endif

C     == Routine arguments ==
      INTEGER myIter
      _RL myTime
      INTEGER myThid

#ifdef ALLOW_DIC_COST

C     == Local variables ==
      INTEGER i, j, bi, bj, k, it
c
      _RL po4cost, o2cost, sumvol
      _RL po4costm, o2costm, sumvolm
c
c calculate costfunction

        _BEGIN_MASTER(myThid)

       sumvol= 0. _d 0
       po4cost= 0. _d 0
       o2cost=0.d0
cswdmonth-add--
       sumvolm=0.d0
       po4costm=0.d0
       o2costm=0.d0
cswdmonth -- end add --

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO i=1,sNx
          DO j=1,sNy
           do k=1,nR
            sumvol=sumvol+
     &            rA(i,j,bi,bj)*drF(k)*maskC(i,j,k,bi,bj)
            po4cost=po4cost+
     &          (po4ann(i,j,k,bi,bj)-po4obs(i,j,k,bi,bj))**2
     &          /po4var(k)
     &          *rA(i,j,bi,bj)*drF(k)*maskC(i,j,k,bi,bj)
            o2cost=o2cost+
     &          (o2ann(i,j,k,bi,bj)-o2obs(i,j,k,bi,bj))**2
     &          /o2var(k)
     &          *rA(i,j,bi,bj)*drF(k)*maskC(i,j,k,bi,bj)
           enddo
cswdmonth-add--
           do k=1,4
            sumvolm=sumvolm+
     &            rA(i,j,bi,bj)*drF(k)*maskC(i,j,k,bi,bj)
           enddo
           do it=1,12
              po4costm=po4costm+
     &          (po4lev1(i,j,it,bi,bj)-po4obsl1(i,j,it,bi,bj))**2
     &          /po4varm(it,1)
     &          *rA(i,j,bi,bj)*drF(1)*maskC(i,j,1,bi,bj)  +
     &          (po4lev2(i,j,it,bi,bj)-po4obsl2(i,j,it,bi,bj))**2
     &          /po4varm(it,2)
     &          *rA(i,j,bi,bj)*drF(2)*maskC(i,j,2,bi,bj) +
     &          (po4lev3(i,j,it,bi,bj)-po4obsl3(i,j,it,bi,bj))**2
     &          /po4varm(it,3)
     &          *rA(i,j,bi,bj)*drF(3)*maskC(i,j,3,bi,bj)
cQQ  &          (po4lev4(i,j,it,bi,bj)-po4obsl4(i,j,it,bi,bj))**2
cQQ  &          /po4varm(it,4)
cQQ  &          *rA(i,j,bi,bj)*drF(4)*maskC(i,j,4,bi,bj)
              o2costm=o2costm+
     &          (o2lev1(i,j,it,bi,bj)-o2obsl1(i,j,it,bi,bj))**2
     &          /o2varm(it,1)
     &          *rA(i,j,bi,bj)*drF(1)*maskC(i,j,1,bi,bj) +
     &          (o2lev2(i,j,it,bi,bj)-o2obsl2(i,j,it,bi,bj))**2
     &          /o2varm(it,2)
     &          *rA(i,j,bi,bj)*drF(2)*maskC(i,j,2,bi,bj) +
     &          (o2lev3(i,j,it,bi,bj)-o2obsl3(i,j,it,bi,bj))**2
     &          /o2varm(it,3)
     &          *rA(i,j,bi,bj)*drF(3)*maskC(i,j,3,bi,bj)
cQQ  &          (O2lev4(i,j,it,bi,bj)-o2obsl4(i,j,it,bi,bj))**2
cQQ  &          /o2varm(it,4)
cQQ  &          *rA(i,j,bi,bj)*drF(4)*maskC(i,j,4,bi,bj)

           enddo
cswdmonth -end-
          ENDDO
          ENDDO
         ENDDO
        ENDDO
        po4cost=po4cost/sumvol
        o2cost=o2cost/sumvol
cswdmonth-add
        po4costm=po4costm/sumvolm/12.d0
        o2costm=o2costm/sumvolm/12.d0
cswdmonth-end
        totcost=po4cost+o2cost+po4costm+o2costm

        print*,'COST ',totcost, po4cost, o2cost,po4costm,o2costm

       _END_MASTER(myThid)
c
#endif
      RETURN
      END
cswd -- end added subroutine --
