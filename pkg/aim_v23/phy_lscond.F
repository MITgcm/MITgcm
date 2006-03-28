C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/phy_lscond.F,v 1.3 2006/03/28 04:26:21 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE LSCOND (PSA,dpFac,QA,QSAT,
     O                   PRECLS,DTLSC,DQLSC,
     I                   kGrd,bi,bj,myThid)
C--
C--   SUBROUTINE LSCOND (PSA,QA,QSAT,
C--  *                   PRECLS,DTLSC,DQLSC)
C--
C--   Purpose: Compute large-scale precipitation and
C--            associated tendencies of temperature and moisture
C--   Input:   PSA    = norm. surface pressure [p/p0]           (2-dim)
C              dpFac  = cell delta_P fraction                   (3-dim)
C--            QA     = specific humidity [g/kg]                (3-dim)
C--            QSAT   = saturation spec. hum. [g/kg]            (3-dim)
C--   Output:  PRECLS = large-scale precipitation [g/(m^2 s)]   (2-dim)
C--            DTLSC  = temperature tendency from l.s. cond     (3-dim)
C--            DQLSC  = hum. tendency [g/(kg s)] from l.s. cond (3-dim)
C    Input:    kGrd   = Ground level index              (2-dim)
C              bi,bj  = tile index
C              myThid = Thread number for this instance of the routine
C--

      IMPLICIT NONE

C     Resolution parameters

C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

#include "EEPARAMS.h"

C     Physical constants + functions of sigma and latitude

#include "com_physcon.h"

C     Large-scale condensation constants

#include "com_lsccon.h"

C-- Routine arguments:
      _RL PSA(NGP), dpFac(NGP,NLEV), QA(NGP,NLEV), QSAT(NGP,NLEV)
      _RL PRECLS(NGP), DTLSC(NGP,NLEV), DQLSC(NGP,NLEV)
      INTEGER  kGrd(NGP)
      INTEGER  bi,bj,myThid

#ifdef ALLOW_AIM

C-- Local variables:
      INTEGER J, K
      _RL PSA2(NGP)

C- jmc: declare all local variables:
      _RL RTLSC, TFACT, PRG
      _RL SIG2, RHREF, DQMAX, PFACT
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   1. Initialization

c_FM  QSMAX = 50.

      RTLSC = 1./(TRLSC*3600.)
      TFACT = ALHC/CP
      PRG = P0/GG

      DO J=1,NGP
        DTLSC(J,1) = 0.
        DQLSC(J,1) = 0.
        PRECLS(J)  = 0.
        PSA2(J)    = PSA(J)*PSA(J)
      ENDDO

C--   2. Tendencies of temperature and moisture
C        NB. A maximum heating rate is imposed to avoid
C            grid-point-storm instability

      DO K=2,NLEV
        SIG2=SIG(K)*SIG(K)
c_FM    RHREF = RHLSC+DRHLSC*(SIG2-1.)
c_FM    DQMAX = (1.1-RHREF)*QSMAX*SIG2*RTLSC
        DO J=1,NGP
          RHREF = RHLSC+DRHLSC*(SIG2/PSA2(J) - 1. _d 0)
          DQMAX = (1.1 _d 0-RHREF)*QSMAX*SIG2*RTLSC
          DQLSC(J,K) = MIN(0. _d 0,(RHREF*QSAT(J,K)-QA(J,K)))*RTLSC
c_FM      DTLSC(J,K) = TFACT*MIN(-DQLSC(J,K),DQMAX*PSA2(J))
c         DTLSC(J,K) = TFACT*MIN(-DQLSC(J,K),DQMAX)
C-jmc: the threshold on latent heat (above) breaks the energy conservation;
C      to fix this, apply the threshold directly to the condensation
          DQLSC(J,K) = MAX(-DQMAX, DQLSC(J,K) )
          DTLSC(J,K) = -TFACT*DQLSC(J,K)
        ENDDO
      ENDDO

C--   Scale LSC tendencies by dpFac (=Partial-Cell factor) (=> Mass Weighted)
C     to get homogenous units & scaling across all physics tendencies.
      DO K=2,NLEV
        DO J=1,NGP
          DQLSC(J,K) = DQLSC(J,K)*dpFac(J,K)
          DTLSC(J,K) = DTLSC(J,K)*dpFac(J,K)
        ENDDO
      ENDDO

C--   3. Large-scale precipitation

      DO J=1,NGP
        DO K=2,kGrd(J)
          PFACT = DSIG(K)*PRG
          PRECLS(J) = PRECLS(J)-PFACT*DQLSC(J,K)
        ENDDO
      ENDDO

c_FM  DO J=1,NGP
c_FM    PRECLS(J) = PRECLS(J)*PSA(J)
c_FM  ENDDO

C--
#endif /* ALLOW_AIM */

      RETURN
      END
