C $Header:

#include "SEAICE_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE BUDGET(UG, TICE, HICE1, FICE1, KOPEN, bi, bj)
C     /==========================================================\
C     | SUBROUTINE budget                                        |
C     | o Calculate ice growth rate                              |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_FFIELDS.h"

C     Subset of variables from SEAICE.h
      _RL HEFF       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL HSNOW      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL QNETO      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL QNETI      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL QSWO       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL QSWI       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      COMMON/TRANS/HEFF,HSNOW
      COMMON/QFLUX/QNETO,QNETI,QSWO,QSWI

C     === Routine arguments ===
      _RL UG         (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL TICE       (1-OLx:sNx+OLx, 1-OLy:sNy+OLy,  nSx,nSy)
      _RL HICE1      (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL FICE1       (1-OLx:sNx+OLx, 1-OLy:sNy+OLy,  nSx,nSy)
      INTEGER KOPEN
      INTEGER bi, bj
CEndOfInterface
 
#ifdef ALLOW_SEAICE

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j
      INTEGER ITER
      _RL  QS1, C1, C2, C3, C4, C5, TB, D1, D1W, D1I, D3
      _RL  TMELT, TMELTP, XKI, XKS, HCUT, ASNOW, XIO

      _RL HICE       (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL ALB        (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL A1         (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL A2         (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL A3         (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL B          (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)

C IF KOPEN LT 0 THAN DO OPEN WATER BUDGET
C NOW DEFINE ASSORTED CONSTANTS
C SATURATION VAPOR PRESSURE CONSTANT
      QS1=0.622 _d +00/1013.0 _d +00
C MAYKUTS CONSTANTS FOR SAT. VAP. PRESSURE TEMP. POLYNOMIAL
      C1=2.7798202 _d -06
      C2=-2.6913393 _d -03
      C3=0.97920849 _d +00
      C4=-158.63779 _d +00
      C5=9653.1925 _d +00
C FREEZING TEMPERATURE OF SEAWATER
      TB=271.2 _d +00
C SENSIBLE HEAT CONSTANT
      D1=SEAICE_sensHeat
C WATER LATENT HEAT CONSTANT
      D1W=SEAICE_latentWater
C ICE LATENT HEAT CONSTANT
      D1I=SEAICE_latentIce
C STEFAN BOLTZMAN CONSTANT TIMES 0.97 EMISSIVITY
      D3=SEAICE_emissivity
C MELTING TEMPERATURE OF ICE
      TMELT=273.16 _d +00
      TMELTP=273.159 _d +00
C ICE CONDUCTIVITY
      XKI=SEAICE_iceConduct
C SNOW CONDUCTIVITY
      XKS=SEAICE_snowConduct
C CUTOFF SNOW THICKNESS
      HCUT=SEAICE_snowThick
C PENETRATION SHORTWAVE RADIATION FACTOR
      XIO=SEAICE_shortwave

      DO J=1,sNy
       DO I=1,sNx
        TICE(I,J,bi,bj)=MIN(273.16 _d 0+MAX_TICE,TICE(I,J,bi,bj))
        TAIR(I,J,bi,bj)=MAX(273.16 _d 0+MIN_TAIR,TAIR(I,J,bi,bj))
        FLO(I,J,bi,bj)=MAX(MIN_FLO,FLO(I,J,bi,bj))
       ENDDO
      ENDDO

C NOW DECIDE IF OPEN WATER OR ICE
      IF(KOPEN.LE.0) THEN

C NOW DETERMINE OPEN WATER HEAT BUD. ASSUMING TICE=WATER TEMP.
C WATER ALBEDO IS ASSUMED TO BE 0.1                       
         DO J=1,sNy
          DO I=1,sNx
           ALB(I,J)=SEAICE_waterAlbedo                                
           A1(I,J)=(1.0 _d 0-ALB(I,J))*FSH(I,J,bi,bj)+FLO(I,J,bi,bj)
     &          +D1*UG(I,J)*TAIR(I,J,bi,bj)+D1W*UG(I,J)*QA(I,J,bi,bj)
           B(I,J)=QS1*6.11 _d +00*EXP(17.2694 _d +00
     &          *(TICE(I,J,bi,bj)-TMELT)
     &          /(TICE(I,J,bi,bj)-TMELT+237.3 _d +00))                   
           A2(I,J)=-D1*UG(I,J)*TICE(I,J,bi,bj)-D1W*UG(I,J)*B(I,J)
     &          -D3*(TICE(I,J,bi,bj)**4)
           FICE1(I,J,bi,bj)=-A1(I,J)-A2(I,J)    
           QSWO(I,J,bi,bj)=-(ONE-ALB(I,J))*FSH(I,J,bi,bj)
           QNETO(I,J,bi,bj)=FICE1(I,J,bi,bj)-QSWO(I,J,bi,bj)
          ENDDO
         ENDDO

      ELSE

C COME HERE IF ICE COVER                              
C FIRST PUT MINIMUM ON ICE THICKNESS               
         DO J=1,sNy
          DO I=1,sNx
           HICE(I,J)=MAX(HICE1(I,J),0.05 _d +00) 
           HICE(I,J)=MIN(HICE(I,J),9.0 _d +00)
          ENDDO
         ENDDO
C NOW DECIDE ON ALBEDO                       
         DO J=1,sNy
          DO I=1,sNx
           ALB(I,J)=0.75 _d +00
           IF(TICE(I,J,bi,bj).GT.TMELTP) ALB(I,J)=SEAICE_albedo
           ASNOW=SEAICE_drySnowAlb
           IF(TICE(I,J,bi,bj).GT.TMELTP) ASNOW=SEAICE_wetSnowAlb
           IF(HSNOW(I,J,bi,bj).GT.HCUT) THEN
            ALB(I,J)=ASNOW
           ELSE
            ALB(I,J)=ALB(I,J)+(HSNOW(I,J,bi,bj)/HCUT)*(ASNOW-ALB(I,J))
            IF(ALB(I,J).GT.ASNOW) ALB(I,J)=ASNOW
           END IF
          ENDDO
         ENDDO
C NOW DETERMINE FIXED FORCING TERM IN HEAT BUDGET
         DO J=1,sNy
          DO I=1,sNx
           IF(HSNOW(I,J,bi,bj).GT.0.0) THEN
C NO SW PENETRATION WITH SNOW
            A1(I,J)=(ONE-ALB(I,J))*FSH(I,J,bi,bj)+FLO(I,J,bi,bj)
     &       +D1*UG(I,J)*TAIR(I,J,bi,bj)+D1I*UG(I,J)*QA(I,J,bi,bj)
           ELSE
C SW PENETRATION UNDER ICE
            A1(I,J)=(ONE-ALB(I,J))*FSH(I,J,bi,bj)
     &       *(ONE-XIO*EXP(-1.5 _d 0*HICE(I,J)))+FLO(I,J,bi,bj)
     &       +D1*UG(I,J)*TAIR(I,J,bi,bj)+D1I*UG(I,J)*QA(I,J,bi,bj)
           ENDIF
          ENDDO
         ENDDO
C NOW COMPUTE OTHER TERMS IN HEAT BUDGET                        
C COME HERE AT START OF ITERATION

crg check wether a2 is needed in the list of variables
c$taf loop = iteration TICE,A2
         DO ITER=1,IMAX_TICE

          DO J=1,sNy
           DO I=1,sNx
            B(I,J)=QS1*(C1*TICE(I,J,bi,bj)**4+C2*TICE(I,J,bi,bj)**3
     ?            +C3*TICE(I,J,bi,bj)**2+C4*TICE(I,J,bi,bj)+C5)
            A2(I,J)=-D1*UG(I,J)*TICE(I,J,bi,bj)-D1I*UG(I,J)*B(I,J)
     ?             -D3*(TICE(I,J,bi,bj)**4)
            B(I,J)=XKS/(HSNOW(I,J,bi,bj)/HICE(I,J)+XKS/XKI)/HICE(I,J)
            A3(I,J)=4.0 _d +00*D3*(TICE(I,J,bi,bj)**3)+B(I,J)+D1*UG(I,J)
            B(I,J)=B(I,J)*(TB-TICE(I,J,bi,bj))
cdm
cdm   if(TICE(I,J,bi,bj).le.206.)
cdm  &           print '(A,3i4,f12.2)','### ITER,I,J,TICE',
cdm  &           ITER,I,J,TICE(I,J,bi,bj)
cdm
           ENDDO
          ENDDO
C NOW DECIDE IF IT IS TIME TO ESTIMATE GROWTH RATES
C NOW DETERMINE NEW ICE TEMPERATURE              
          DO J=1,sNy
           DO I=1,sNx
            TICE(I,J,bi,bj)=TICE(I,J,bi,bj)
     &                     +(A1(I,J)+A2(I,J)+B(I,J))/A3(I,J) 
            TICE(I,J,bi,bj)=MAX(273.16 _d 0+MIN_TICE,TICE(I,J,bi,bj))
           ENDDO
          ENDDO
C NOW DECIDE IF ITERATIONS ARE COMPLETE                  
C NOW SET ICE TEMP TO MIN OF TMELT/ITERATION RESULT  
c          IF(ITER.EQ.IMAX_TICE) THEN
            DO J=1,sNy
             DO I=1,sNx
              TICE(I,J,bi,bj)=MIN(TICE(I,J,bi,bj),TMELT)            
             ENDDO
            ENDDO
c          ENDIF

C END OF ITERATION
         ENDDO

         DO J=1,sNy
          DO I=1,sNx
           FICE1(I,J,bi,bj)=-A1(I,J)-A2(I,J)    
           IF(HSNOW(I,J,bi,bj).GT.0.0) THEN
C NO SW PENETRATION WITH SNOW
            QSWI(I,J,bi,bj)=ZERO
           ELSE
C SW PENETRATION UNDER ICE
            QSWI(I,J,bi,bj)=-(ONE-ALB(I,J))*FSH(I,J,bi,bj)
     &       *XIO*EXP(-1.5 _d 0*HICE(I,J))
           ENDIF
          ENDDO
         ENDDO

      END IF

#endif ALLOW_SEAICE

      RETURN
      END
