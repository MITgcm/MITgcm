C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/phy_lscond.F,v 1.4 2002/09/27 20:05:11 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE LSCOND (PSA,QA,QSAT,
     *                   PRECLS,DTLSC,DQLSC,myThid)
C--
C--   SUBROUTINE LSCOND (PSA,QA,QSAT,
C--  *                   PRECLS,DTLSC,DQLSC) 
C--
C--   Purpose: Compute large-scale precipitation and
C--            associated tendencies of temperature and moisture
C--   Input:   PSA    = norm. surface pressure [p/p0]           (2-dim)
C--            QA     = specific humidity [g/kg]                (3-dim)
C--            QSAT   = saturation spec. hum. [g/kg]            (3-dim)
C--   Output:  PRECLS = large-scale precipitation [g/(m^2 s)]   (2-dim)
C--            DTLSC  = temperature tendency from l.s. cond     (3-dim)
C--            DQLSC  = hum. tendency [g/(kg s)] from l.s. cond (3-dim)
C--

      IMPLICIT NONE

C     Resolution parameters

C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h" 

#include "EEPARAMS.h"

#include "AIM_GRID.h"

C     Physical constants + functions of sigma and latitude

#include "com_physcon.h"

C     Large-scale condensation constants

#include "com_lsccon.h"

C-- Routine arguments:
      INTEGER  myThid
      _RL PSA(NGP), QA(NGP,NLEV), QSAT(NGP,NLEV)
      _RL PRECLS(NGP), DTLSC(NGP,NLEV), DQLSC(NGP,NLEV)

#ifdef ALLOW_AIM

C-- Local variables:  
      INTEGER J, K

C- jmc: declare all local variables:
      _RL RTLSC, TFACT, PRG, PFACT
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   1. Initialization

      RTLSC = 1./(TRLSC*3600)
      TFACT = ALHC/CP
      PRG = P0/GG

      DO J=1,NGP
        DTLSC(J,1) = 0.
        DQLSC(J,1) = 0.
        PRECLS(J)  = 0.
      ENDDO

C--   2. Tendencies of temperature and moisture
C
      DO K=2,NLEV
        DO J=1,NGP
          DQLSC(J,K) = MIN(0. _d 0,(RHLSC*QSAT(J,K)-QA(J,K)))*RTLSC
          DTLSC(J,K) = -TFACT*DQLSC(J,K)
        ENDDO
      ENDDO

C--   3. Large-scale precipitation

      DO J=1,NGP
        DO K=2,NLEVxy(J,myThid)
          PFACT = DSIG(K)*PRG
          PRECLS(J) = PRECLS(J)-PFACT*DQLSC(J,K)
        ENDDO
      ENDDO

      DO J=1,NGP
        PRECLS(J) = PRECLS(J)*PSA(J)
      ENDDO

C--
#endif /* ALLOW_AIM */ 

      RETURN
      END
