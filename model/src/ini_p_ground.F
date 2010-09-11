C $Header: /u/gcmpack/MITgcm/model/src/ini_p_ground.F,v 1.9 2010/09/11 21:24:52 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#undef CHECK_ANALYLIC_THETA

CBOP
C     !ROUTINE: INI_P_GROUND
C     !INTERFACE:
      SUBROUTINE INI_P_GROUND(selectMode,
     &                        Hfld, Pfld,
     I                        myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_P_GROUND
C     | o Convert Topography [m] to (reference) Surface Pressure
C     |   according to tRef profile,
C     |   using same discretisation as in calc_phi_hyd
C     |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     selectMode ::  > 0 = find Pfld from Hfld ; < 0 = compute Hfld from Pfld
C                   selectFindRoSurf = 0 : use Theta_Ref profile
C                   selectFindRoSurf = 1 : use analytic fct Theta(Lat,P)
C     Hfld (input/outp) :: Ground elevation [m]
C     Pfld (outp/input) :: reference Pressure at the ground [Pa]
      INTEGER selectMode
      _RS Hfld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS Pfld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     msgBuf :: Informational/error message buffer
C-
C       For an accurate definition of the reference surface pressure,
C       define a High vertical resolution (in P):
C     nLevHvR :: Number of P-level used for High vertical Resolution (HvR)
C     plowHvR :: Lower bound of the High vertical Resolution
C     dpHvR   :: normalized pressure increment (HvR)
C     pLevHvR :: normalized P-level of the High vertical Resolution
C     pMidHvR :: normalized mid point level (HvR)
C     thetaHvR :: potential temperature at mid point level (HvR)
C     PiHvR  :: Exner function at P-level
C     dPiHvR :: Exner function difference between 2 P-levels
C     recip_kappa :: 1/kappa = Cp/R
C     PiLoc, zLoc, dzLoc, yLatLoc, phiLoc :: hold on temporary values
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi,bj,i,j,K, Ks
      _RL Po_surf
      _RL hRef(2*Nr+1), rHalf(2*Nr+1)
      LOGICAL findPoSurf

      INTEGER nLevHvR
      PARAMETER ( nLevHvR = 60 )
      _RL plowHvR, dpHvR, pLevHvR(nLevHvR+1), pMidHvR(nLevHvR)
      _RL thetaHvR(nLevHvR), PiHvR(nLevHvR+1), dPiHvR(nLevHvR)
      _RL recip_kappa, PiLoc, zLoc, dzLoc, yLatLoc, phiLoc
      _RL  psNorm, rMidKp1
      _RL ratioRm(Nr), ratioRp(Nr)
      INTEGER kLev
#ifdef CHECK_ANALYLIC_THETA
      _RL tmpVar(nLevHvR,61)
#endif
CEOP

      IF ( selectFindRoSurf.LT.0 .OR. selectFindRoSurf.GT.1 ) THEN
        WRITE(msgBuf,'(A,I2,A)')
     &   'INI_P_GROUND: selectFindRoSurf =', selectFindRoSurf,
     &        ' <== bad value !'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'INI_P_GROUND'
      ENDIF

      DO K=1,Nr
        rHalf(2*K-1) = rF(K)
        rHalf(2*K)   = rC(K)
      ENDDO
       rHalf(2*Nr+1) = rF(Nr+1)

C- Reference Geopotential at Half levels :
C    Tracer level: hRef(2K)  ;  Interface_W level: hRef(2K+1)
C- Convert phiRef to Z unit :
      DO K=1,2*Nr+1
        hRef(K) = phiRef(K)*recip_gravity
      ENDDO

      IF (selectFindRoSurf.EQ.0 .AND. selectMode .GT. 0 ) THEN
C- Find Po_surf : Linear between 2 half levels :
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
           Ks = 1
           DO K=1,2*Nr
             IF (Hfld(i,j,bi,bj).GE.hRef(K)) Ks = K
           ENDDO
           Po_surf = rHalf(Ks) + (rHalf(Ks+1)-rHalf(Ks))*
     &       (Hfld(i,j,bi,bj)-hRef(Ks))/(hRef(Ks+1)-hRef(Ks))

c          IF (Hfld(i,j,bi,bj).LT.hRef(1)) Po_surf= rHalf(1)
c          IF (Hfld(i,j,bi,bj).GT.hRef(2*Nr+1)) Po_surf=rHalf(2*Nr+1)
           Pfld(i,j,bi,bj) = Po_surf
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- endif selectFindRoSurf=0 & selectMode > 0
      ENDIF

      IF ( selectFindRoSurf.EQ.1 ) THEN
C-- define high resolution Pressure discretization:

      recip_kappa = 1. _d 0 / atm_kappa
      plowHvR = 0.4 _d 0
      dpHvR = nLevHvR
      dpHvR = (1. - plowHvR) / dpHvR
        pLevHvR(1)= Ro_SeaLevel/atm_Po
        PiHvR(1) = atm_Cp*(pLevHvR(1)**atm_kappa)
      DO k=1,nLevHvR
        pLevHvR(k+1)= pLevHvR(1) - float(k)*dpHvR
        PiHvR(k+1)  = atm_Cp*(pLevHvR(k+1)**atm_kappa)
        pMidHvR(k)= (pLevHvR(k)+pLevHvR(k+1))*0.5 _d 0
        dPiHvR(k) = PiHvR(k) - PiHvR(k+1)
      ENDDO

C--   to modify pressure when using non fully linear relation between Phi & p
C       (Integr_GeoPot=2 & Tracer Point at middle between 2 interfaces)
      DO k=1,Nr
         ratioRm(k) = 1. _d 0
         ratioRp(k) = 1. _d 0
         IF (k.GT.1 ) ratioRm(k) = 0.5 _d 0*drC(k)/(rF(k)-rC(k))
         IF (k.LT.Nr) ratioRp(k) = 0.5 _d 0*drC(k+1)/(rC(k)-rF(k+1))
      ENDDO

#ifdef CHECK_ANALYLIC_THETA
      _BEGIN_MASTER( mythid )
      DO j=1,61
        yLatLoc =-90.+(j-1)*3.
        CALL ANALYLIC_THETA( yLatLoc , pMidHvR,
     &                       tmpVar(1,j), nLevHvR, mythid)
      ENDDO
      OPEN(88,FILE='analytic_theta',
     &      STATUS='unknown',FORM='unformatted')
      WRITE(88) tmpVar
      CLOSE(88)
      _END_MASTER( mythid )
      STOP 'CHECK_ANALYLIC_THETA'
#endif /* CHECK_ANALYLIC_THETA */

C-- endif selectFindRoSurf=1
      ENDIF

      IF ( selectFindRoSurf*selectMode .GT. 0) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C- Find Po_surf such as g*Hfld = Phi[Po_surf,theta(yLat,p)]:

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C- start bi,bj loop:

        DO j=1,sNy
         DO i=1,sNx
          IF ( Hfld(i,j,bi,bj) .LE. 0. _d 0) THEN
           Pfld(i,j,bi,bj) = Ro_SeaLevel
          ELSE
           yLatLoc  = yC(i,j,bi,bj)
           CALL ANALYLIC_THETA( yLatLoc , pMidHvR,
     &                       thetaHvR, nLevHvR, mythid)
           zLoc = 0.
           DO k=1,nLevHvR
            IF (zLoc.GE.0.) THEN
C-    compute  phi/g corresponding to next p_level:
             dzLoc = dPiHvR(k)*thetaHvR(k)*recip_gravity
             IF ( Hfld(i,j,bi,bj) .LE. zLoc+dzLoc ) THEN
C-    compute the normalized surf. Pressure psNorm
               PiLoc = PiHvR(k)
     &               - gravity*(Hfld(i,j,bi,bj)-zLoc)/thetaHvR(k)
               psNorm = (PiLoc/atm_Cp)**recip_kappa
C- use linear interpolation:
c              psNorm = pLevHvR(k)
c    &                - dpHvR*(Hfld(i,j,bi,bj)-zLoc)/dzLoc
               zLoc = -1.
             ELSE
               zLoc = zLoc + dzLoc
             ENDIF
            ENDIF
           ENDDO
           IF (zLoc.GE.0.) THEN
             WRITE(msgBuf,'(2A)')
     &        'INI_P_GROUND: FAIL in trying to find Pfld:',
     &        ' selectMode,i,j,bi,bj=',selectMode,i,j,bi,bj
             CALL PRINT_ERROR( msgBuf , myThid)
             WRITE(msgBuf,'(A,F7.1,2A,F6.4,A,F8.0)')
     &        'INI_P_GROUND: Hfld=', Hfld(i,j,bi,bj), ' exceeds',
     &        ' Zloc(lowest P=', pLevHvR(1+nLevHvR),' )=',zLoc
             CALL PRINT_ERROR( msgBuf , myThid)
             STOP 'ABNORMAL END: S/R INI_P_GROUND'
           ELSE
             Pfld(i,j,bi,bj) = psNorm*atm_Po
           ENDIF
          ENDIF
         ENDDO
        ENDDO

        IF (selectMode.EQ.2 .AND. integr_GeoPot.NE.1) THEN
C---------
C     Modify pressure to account for non fully linear relation between
C      Phi & p (due to numerical truncation of the Finite Difference scheme)
C---------
          DO j=1,sNy
           DO i=1,sNx
             Po_surf = Pfld(i,j,bi,bj)
              IF ( Po_surf.LT.rC(1) .AND. Po_surf.GT.rC(Nr) ) THEN
                findPoSurf = .TRUE.
                DO k=1,Nr
                  IF ( findPoSurf .AND. Po_surf.GE.rC(k) ) THEN
                    Po_surf = rC(k) + (Po_surf-rC(k))/ratioRm(k)
                    findPoSurf = .FALSE.
                  ENDIF
                  rMidKp1 = rF(k+1)
                  IF (k.LT.Nr) rMidKp1 = (rC(k)+rC(k+1))*0.5 _d 0
                  IF ( findPoSurf .AND. Po_surf.GE.rMidKp1 ) THEN
                    Po_surf = rC(k) + (Po_surf-rC(k))/ratioRp(k)
                    findPoSurf = .FALSE.
                  ENDIF
                ENDDO
                IF ( findPoSurf )
     &               STOP 'S/R INI_P_GROUND: Pb with selectMode=2'
              ENDIF
             Pfld(i,j,bi,bj) = Po_surf
           ENDDO
          ENDDO
C---------
        ENDIF

C- end bi,bj loop.
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- endif selectFindRoSurf*selectMode > 0
      ENDIF

      IF (selectMode .LT. 0) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--- Compute Hfld = Phi(Pfld)/g.

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C- start bi,bj loop:

C--  Compute Hfld from g*Hfld = hRef(Po_surf)
        DO j=1,sNy
         DO i=1,sNx
C-   compute phiLoc = hRef(Ro_surf):
          ks = kSurfC(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
C-   sigma coord. case (=> uniform kSurfC), find true ks:
           IF ( selectSigmaCoord.NE.0 ) THEN
             DO k=2,Nr
               IF ( Pfld(i,j,bi,bj).LT.rF(k) ) ks = k
             ENDDO
           ENDIF
           IF ( Pfld(i,j,bi,bj).GE.rC(ks) ) THEN
            phiLoc = hRef(2*ks)
     &       + (hRef(2*ks-1)-hRef(2*ks))
     &        *(Pfld(i,j,bi,bj)-rC(ks))/(rHalf(2*ks-1)-rHalf(2*ks))
           ELSE
            phiLoc = hRef(2*ks)
     &       + (hRef(2*ks+1)-hRef(2*ks))
     &        *(Pfld(i,j,bi,bj)-rC(ks))/(rHalf(2*ks+1)-rHalf(2*ks))
           ENDIF
           Hfld(i,j,bi,bj) = phiLoc
          ELSE
           Hfld(i,j,bi,bj) = 0.
          ENDIF
         ENDDO
        ENDDO

        IF (selectFindRoSurf.EQ.1) THEN
C-----
C  goal: evaluate phi0surf (used for computing geopotential_prime = Phi - PhiRef):
C   phi0surf = g*Ztopo-PhiRef(Ro_surf) if no truncation was applied to Ro_surf;
C  but because of hFacMin, surf.ref.pressure (=Ro_surf) is truncated and
C   phi0surf = Phi(Theta-Analytic,P=Ro_surf) - PhiRef(P=Ro_surf)
C-----
C--   Compute Hfld from g*Hfld = Phi[Po_surf,theta(yLat,p)]:
         DO j=1,sNy
          DO i=1,sNx
           zLoc = 0.
           IF ( Pfld(i,j,bi,bj) .LT. Ro_SeaLevel) THEN
            Po_surf = Pfld(i,j,bi,bj)
C---------
C     Modify pressure to account for non fully linear relation between
C      Phi & p (due to numerical truncation of the Finite Difference scheme)
             IF (selectMode.EQ.-2 .AND. integr_GeoPot.NE.1) THEN
              IF ( Po_surf.LT.rC(1) .AND. Po_surf.GT.rC(Nr) ) THEN
                findPoSurf = .TRUE.
                DO k=1,Nr
                  IF ( findPoSurf .AND. Po_surf.GE.rC(k) ) THEN
                    Po_surf = rC(k) + (Po_surf-rC(k))*ratioRm(k)
                    findPoSurf = .FALSE.
                  ENDIF
                  IF ( findPoSurf .AND. Po_surf.GE.rF(k+1) ) THEN
                    Po_surf = rC(k) + (Po_surf-rC(k))*ratioRp(k)
                    findPoSurf = .FALSE.
                  ENDIF
                ENDDO
              ENDIF
             ENDIF
C---------
            psNorm = Po_surf/atm_Po
            kLev = 1 + INT( (pLevHvR(1)-psNorm)/dpHvR )
            yLatLoc  = yC(i,j,bi,bj)
            CALL ANALYLIC_THETA( yLatLoc , pMidHvR,
     &                        thetaHvR, kLev, mythid)
C-    compute height at level pLev(kLev) just below Pfld:
            DO k=1,kLev-1
              dzLoc = dPiHvR(k)*thetaHvR(k)*recip_gravity
              zLoc = zLoc + dzLoc
            ENDDO
            dzLoc = ( PiHvR(kLev)-atm_Cp*(psNorm**atm_kappa) )
     &            * thetaHvR(kLev)*recip_gravity
            zLoc = zLoc + dzLoc
           ENDIF
C-    compute phi0surf = Phi[Po_surf,theta(yLat,p)] - PhiRef(Po_surf)
           phi0surf(i,j,bi,bj) = gravity*(zLoc - Hfld(i,j,bi,bj))
C-    save Phi[Po_surf,theta(yLat,p)] in Hfld (replacing PhiRef(Po_surf)):
           Hfld(i,j,bi,bj) = zLoc
          ENDDO
         ENDDO
C- endif selectFindRoSurf=1
        ENDIF

C- end bi,bj loop.
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- endif selectMode < 0
      ENDIF

      RETURN
      END

CBOP
C     !SUBROUTINE: ANALYLIC_THETA
C     !INTERFACE:
      SUBROUTINE ANALYLIC_THETA( yLat, pNlev,
     O                           thetaLev,
     I                           kSize,myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ANALYLIC_THETA
C     | o Conpute analyticaly the potential temperature Theta
C     |   as a function of Latitude (yLat) and normalized
C     |   pressure pNlev.
C     |   approximatively match the N-S symetric, zonal-mean and
C     |   annual-mean NCEP climatology in the troposphere.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     yLat   :: latitude (degre)
C     pNlev  :: normalized pressure levels
C     kSize  :: dimension of pNlev & ANALYLIC_THETA
C     myThid :: Thread number for this instance of the routine
      INTEGER kSize
      _RL  yLat
      _RL  pNlev  (kSize)
      _RL  thetaLev(kSize)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER k
      _RL  yyA, yyB, yyC, yyAd, yyBd, yyCd
      _RL  cAtmp, cBtmp, ttdC
      _RL  ppN0, ppN1, ppN2, ppN3a, ppN3b, ppN4
      _RL  ttp1, ttp2, ttp3, ttp4, ttp5
      _RL  yAtmp, yBtmp, yCtmp, yDtmp
      _RL  ttp2y, ttp4y, a1tmp
      _RL  ppl, ppm, pph, ppr

CEOP

      DATA yyA ,    yyB ,     yyC ,     yyAd ,   yyBd ,   yyCd
     &  / 45. _d 0, 65. _d 0, 65. _d 0, .9 _d 0, .9 _d 0, 10. _d 0 /
      DATA  cAtmp ,   cBtmp ,   ttdC
     &   /  2.6 _d 0, 1.5 _d 0, 3.3 _d 0 /
      DATA  ppN0  ,   ppN1  ,  ppN2  ,  ppN3a ,  ppN3b ,  ppN4
     &   / .1 _d 0, .19 _d 0, .3 _d 0, .9 _d 0, .7 _d 0, .925 _d 0 /
      DATA ttp1 ,     ttp2 ,     ttp3 ,     ttp4 ,     ttp5
     &   / 350. _d 0, 342. _d 0, 307. _d 0, 301. _d 0, 257. _d 0 /

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       yAtmp = ABS(yLat) - yyA
       yAtmp = yyA + MIN(0. _d 0,yAtmp/yyAd) + MAX(yAtmp, 0. _d 0)
       yAtmp = COS( deg2rad*MAX(yAtmp, 0. _d 0) )
       yBtmp = ABS(yLat) - yyB
       yBtmp = yyB + yBtmp/yyBd
       yBtmp = COS( deg2rad*MAX( 0. _d 0, MIN(yBtmp,90. _d 0) ) )
       yCtmp = ABS(yLat) - yyC
       yCtmp = MAX( 0. _d 0, 1. _d 0 - (yCtmp/yyCd)**2 )
       yDtmp = ppN3a +(ppN3b - ppN3a)*yCtmp
       ttp2y = ttp3 + (ttp2-ttp3)*yAtmp**cAtmp
       ttp4y = ttp5 + (ttp4-ttp5)*yBtmp**cBtmp
       a1tmp = (ttp1-ttp2y)*ppN1*ppN2/(ppN2-ppN1)
      DO k=1,kSize
       ppl = MIN(pNlev(k),ppN1)
       ppm = MIN(MAX(pNlev(k),ppN1),ppN2)
       pph = MAX(pNlev(k),ppN2)
       ppr =( ppN0 + ABS(ppl-ppN0) - ppN1 )/(ppN2-ppN1)
       thetaLev(k) =
     &       ( (1. _d 0 -ppr)*ttp1*ppN1**atm_kappa
     &        + ppr*ttp2y*ppN2**atm_kappa
     &       )*ppl**(-atm_kappa)
     &     + a1tmp*(1. _d 0 /ppm - 1. _d 0/ppN1)
     &     + (ttp4y-ttp2y)*(pph-ppN2)/(ppN4-ppN2)
     &     + (ttdC+yCtmp)*MAX(0. _d 0,pNlev(k)-yDtmp)/(1-yDtmp)
      ENDDO

C---------------------------------------------------
C matlab script, input: pN, yp=abs(yLat)
C pN0=.1; pN1=.19 ; pN2=.3; pN4=.925;
C pm=min(max(pN,pN1),pN2); pp=max(pN,pN2);
C pl=min(pN,pN1); pr=(pN0+abs(pl-pN0)-pN1)/(pN2-pN1);
C
C  yA=yp-45; yA=45+min(0,yA/.9)+max(0,yA); yA=max(0,yA); cosyA=cos(yA*rad) ;
C  yB=yp-65; yB=65+yB/.9; yB=min(max(0,yB),90); cosyB=cos(yB*rad) ;
C  tp1=350*ones(nyA,1);
C  tp2=307+(342-307)*cosyA.^2.6;
C  tp4=257+(301-257)*cosyB.^1.5;
C  a1=(tp1-tp2)*pN1*pN2/(pN2-pN1);
C  pF=max(0,1.-((yp-65)/10).^2); pT=.9+(0.7-.9)*pF;
C
C  tA0=((1-pr(k))*tp1(j)*pN1^kappa+pr(k)*tp2(j)*pN2^kappa)*pl(k)^-kappa;
C  tA1=a1(j)*(1./pm(k)-1./pN1);
C  tA2=(tp4(j)-tp2(j))*(pp(k)-pN2)/(pN4-pN2);
C  tA3=(3.3+pF(j))*max(0,pN(k)-pT(j))/(1-pT(j));
C  tAn(j,k)=tA0+tA1+tA2+tA3;
C---------------------------------------------------

      RETURN
      END
