C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/phy_lscond.F,v 1.3 2001/05/29 19:28:53 cnh Exp $
C $Name:  $

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


      IMPLICIT rEAL*8 (A-H,O-Z)
      INTEGER  myThid

C     Resolution parameters
C
#include "atparam.h"
#include "atparam1.h"
#include "EEPARAMS.h"
#include "Lev_def.h"
C
      INTEGER NLAT, NLON, NLEV, NGP
      PARAMETER ( NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT )

C     Physical constants + functions of sigma and latitude

#include "com_physcon.h"

C     Large-scale condensation constants

#include "com_lsccon.h"

      REAL PSA(NGP), QA(NGP,NLEV), QSAT(NGP,NLEV)

      REAL PRECLS(NGP), DTLSC(NGP,NLEV), DQLSC(NGP,NLEV)

      INTEGER J, K


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
          DQLSC(J,K) = MIN(0.,(RHLSC*QSAT(J,K)-QA(J,K)))*RTLSC
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
      RETURN
      END
