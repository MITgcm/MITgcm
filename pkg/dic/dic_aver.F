C $Header: /u/gcmpack/MITgcm/pkg/dic/Attic/dic_aver.F,v 1.10 2010/01/02 23:07:39 jmc Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE DIC_AVER(
     I           myTime,myIter,myThid)

C     *==========================================================*
C     | SUBROUTINE DIC_AVER
C     *==========================================================*
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
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIC_COST

C     == Local variables ==
      INTEGER i, j, bi, bj, k, it
      _RL rdt
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm

Cswddmonth -- end-

C averages
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        OBS_timetave(bi,bj) = OBS_timetave(bi,bj)+deltaTclock
        DO k=1,Nr
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
Cswdmonth-add--
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
         OBSM_timetave(it,bi,bj) = OBSM_timetave(it,bi,bj)+deltaTclock
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

Cswdmonth-end--

#endif /* ALLOW_DIC_COST */

      RETURN
      END
