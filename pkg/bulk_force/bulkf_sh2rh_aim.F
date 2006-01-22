C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_sh2rh_aim.F,v 1.1 2006/01/22 15:50:37 jmc Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"

      SUBROUTINE BULKF_SH2RH_AIM(IMODE,NGP,TA,PS,SIG,QA,RH,QSAT,myThid)
C--
C--   SUBROUTINE SHTORH (IMODE,NGP,TA,PS,SIG,QA,RH,QSAT)
C--
C--   Purpose: compute saturation specific humidity and
C--            relative hum. from specific hum. (or viceversa)
C--   Input:   IMODE  : mode of operation
C--            NGP    : no. of grid-points
C--            TA     : abs. temperature
C--            PS     : normalized pressure   (=  p/1000_hPa) [if SIG < 0]
C--                   : normalized sfc. pres. (= ps/1000_hPa) [if SIG > 0]
C--            SIG    : sigma level
C--            QA     : specific humidity in g/kg [if IMODE = 1]
C--            RH     : relative humidity         [if IMODE < 0]
C--   Output:  RH     : relative humidity         [if IMODE = 1]
C--            QA     : specific humidity in g/kg [if IMODE < 0]
C--            QSAT   : saturation spec. hum. in g/kg
C--            RH     : d.Qsat/d.T  in g/kg/K     [if IMODE = 2]
C--

      IMPLICIT NONE

C-- Routine arguments:
      INTEGER IMODE, NGP
      INTEGER  myThid
c     _RL TA(NGP), PS(NGP), QA(NGP), RH(NGP), QSAT(NGP)
      _RL TA(NGP), PS(NGP), QSAT(NGP), QA(*), RH(*)

C- jmc: declare all routine arguments:
      _RL SIG

#ifdef ALLOW_BULK_FORCE

C-- Local variables:
      INTEGER  J

C- jmc: declare all local variables:
      _RL E0, C1, C2, T0, T1, T2, QS1, QS2
      _RL sigP, recT, tmpQ
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---  1. Compute Qsat (g/kg) from T (degK) and normalized pres. P (= p/1000_hPa)
C        If SIG > 0, P = Ps * sigma, otherwise P = Ps(1) = const.
C
      E0=  6.108 _d -3
      C1= 17.269 _d 0
      C2= 21.875 _d 0
      T0=273.16 _d 0
      T1= 35.86 _d 0
      T2=  7.66 _d 0
      QS1= 622. _d 0
      QS2= .378 _d 0


      IF (IMODE.EQ.2) THEN
C-    Compute Qsat and d.Qsat/d.T :
        DO J=1,NGP
         QSAT(J)=0.
         sigP = PS(1)
         IF (SIG.GT.0.0) sigP=SIG*PS(J)
         IF (TA(J).GE.T0) THEN
          tmpQ   = E0*EXP(C1*(TA(J)-T0)/(TA(J)-T1))
          QSAT(J)= QS1*tmpQ/(sigP-QS2*tmpQ)
          recT   = 1. _d 0 / (TA(J)-T1)
          RH(J)  = QSAT(J)*C1*(T0-T1)*recT*recT*sigP/(sigP-QS2*tmpQ)
         ELSE IF ( TA(J).GT.T2) THEN
          tmpQ   = E0*EXP(C2*(TA(J)-T0)/(TA(J)-T2))
          QSAT(J)= QS1*tmpQ/(sigP-QS2*tmpQ)
          recT   = 1. _d 0 / (TA(J)-T2)
          RH(J)  = QSAT(J)*C2*(T0-T2)*recT*recT*sigP/(sigP-QS2*tmpQ)
         ENDIF
        ENDDO
        RETURN
      ENDIF

      DO 110 J=1,NGP
        QSAT(J)=0.
        IF (TA(J).GE.T0) THEN
          QSAT(J)=E0*EXP(C1*(TA(J)-T0)/(TA(J)-T1))
        ELSE IF ( TA(J).GT.T2) THEN
          QSAT(J)=E0*EXP(C2*(TA(J)-T0)/(TA(J)-T2))
        ENDIF
  110 CONTINUE
C
      IF (SIG.LE.0.0) THEN
        DO 120 J=1,NGP
          QSAT(J)= QS1*QSAT(J)/( PS(1)   - QS2*QSAT(J))
  120   CONTINUE
      ELSE
        DO 130 J=1,NGP
          QSAT(J)= QS1*QSAT(J)/(SIG*PS(J)- QS2*QSAT(J))
  130   CONTINUE
      ENDIF
C
C---  2. Compute rel.hum. RH=Q/Qsat (IMODE>0), or Q=RH*Qsat (IMODE<0)
C
      IF (IMODE.GT.0) THEN
        DO 210 J=1,NGP
          IF(QSAT(J).NE.0.) then
            RH(J)=QA(J)/QSAT(J)
          ELSE
            RH(J)=0.
          ENDIF
  210   CONTINUE
      ELSE IF (IMODE.LT.0) THEN
        DO 220 J=1,NGP
          QA(J)=RH(J)*QSAT(J)
  220   CONTINUE
      ENDIF

#endif /* ALLOW_BULK_FORCE */
      RETURN
      END
