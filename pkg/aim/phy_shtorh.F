C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/phy_shtorh.F,v 1.4 2001/09/06 13:28:01 adcroft Exp $
C $Name:  $
 
      SUBROUTINE SHTORH (IMODE,NGP,TA,PS,SIG,QA,RH,QSAT,myThid)
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
C--            QA     : specific humidity in g/kg [if IMODE > 0]
C--            RH     : relative humidity         [if IMODE < 0]
C--            QSAT   : saturation spec. hum. in g/kg 
C--   Output:  RH     : relative humidity         [if IMODE > 0] 
C--            QA     : specific humidity in g/kg [if IMODE < 0]
C--        


      IMPLICIT rEAL*8 (A-H,O-Z)
      INTEGER IMODE,NGP,J
      INTEGER  myThid


CcnhDebugStarts
#include "SIZE.h"
CcnhDebugEnds
      REAL TA(NGP), PS(NGP), QA(NGP), RH(NGP), QSAT(NGP)
C
C---  1. Compute Qsat (g/kg) from T (degK) and normalized pres. P (= p/1000_hPa)
C        If SIG > 0, P = Ps * sigma, otherwise P = Ps(1) = const. 
C
      E0=  6.108 _d -3
      C1= 17.269 _d 0
      C2= 21.875 _d 0
      T0=273.16 _d 0
      T1= 35.86 _d 0
      T2=  7.66 _d 0
C 
      DO 110 J=1,NGP
        QSAT(J)=0.
        IF (TA(J).GE.T0) THEN
          QSAT(J)=E0*EXP(C1*(TA(J)-T0)/(TA(J)-T1))
        ELSE IF ( TA(J).GT.0.) then
          QSAT(J)=E0*EXP(C2*(TA(J)-T0)/(TA(J)-T2))
        ENDIF
  110 CONTINUE
C
      IF (SIG.LE.0.0) THEN
        DO 120 J=1,NGP
          QSAT(J)=622. _d 0*QSAT(J)/(PS(1)-0.378 _d 0*QSAT(J))
  120   CONTINUE
      ELSE
        DO 130 J=1,NGP
          QSAT(J)=622. _d 0*QSAT(J)/(SIG*PS(J)-0.378 _d 0*QSAT(J))
  130   CONTINUE
      ENDIF
chh      write(0,*) 'MAXVAL(QSAT)=',MAXVAL(QSAT)
chh      write(0,*) 'MINVAL(QSAT)=',MINVAL(QSAT)
C
C---  2. Compute rel.hum. RH=Q/Qsat (IMODE>0), or Q=RH*Qsat (IMODE<0)
C
      IF (IMODE.GT.0) THEN
        DO 210 J=1,NGP
          IF(QSAT(J).ne.0.) then
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
chh      write(0,*) 'MAXVAL(QA)=',MAXVAL(QA)
chh      write(0,*) 'MINVAL(QA)=',MINVAL(QA)
chh      write(0,*) 'MAXVAL(RH)=',MAXVAL(RH)
chh      write(0,*) 'MINVAL(RH)=',MINVAL(RH)
C				
      RETURN
      END

      SUBROUTINE ZMEDDY (NLON,NLAT,FF,ZM,EDDY)


      IMPLICIT rEAL*8 (A-H,O-Z)
      INTEGER NLON,NLAT,I,J


C
C *** Decompose a field into zonal-mean and eddy component
C
      REAL FF(NLON,NLAT), ZM(NLAT), EDDY(NLON,NLAT)
C
      RNLON=1./NLON
C
      DO 130 J=1,NLAT
C
        ZM(J)=0.
        DO 110 I=1,NLON
          ZM(J)=ZM(J)+FF(I,J)
 110    CONTINUE
        ZM(J)=ZM(J)*RNLON
C
        DO 120 I=1,NLON
          EDDY(I,J)=FF(I,J)-ZM(J)
 120    CONTINUE
C
 130  CONTINUE
C
C--
      RETURN
      END
C
