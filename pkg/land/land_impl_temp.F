C $Header: /u/gcmpack/MITgcm/pkg/land/land_impl_temp.F,v 1.3 2004/06/21 16:50:21 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_IMPL_TEMP
C     !INTERFACE:
      SUBROUTINE LAND_IMPL_TEMP(
     I                land_frc, 
     I                dTskin, sFlx,
     O                dTsurf,
     I                bi, bj, myTime, myIter, myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_IMPL_TEMP
C     | o solve ground temp. and surface temp. implicitly 
C     *==========================================================*
C     | o account for snow layer (with no heat capacity) 
C     |   and ground water freezing/melting
C     | o surf. heat flux is linearly dependent on surf. temp.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C-- size for MITgcm & Land package :
#include "LAND_SIZE.h" 

#include "EEPARAMS.h"
#include "LAND_PARAMS.h"
#include "LAND_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     land_frc :: land fraction [0-1]
C     dTskin   :: temp. correction for daily-cycle heating [K]
C     sFlx     :: surf. heat flux (+=down) function of surface temp. Ts:
C                 0: Flx(Ts=0) ; 1: Flx(Ts=Ts^n) ; 2: d.Flx/dTs(Ts=Ts^n)
C     dTsurf   :: surf. temp adjusment: Ts^n+1 - Ts^n
C     bi,bj    :: Tile index
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     myThid   :: Number of this instance of the routine
      _RS land_frc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dTskin(sNx,sNy), sFlx(sNx,sNy,0:2) 
      _RL dTsurf(sNx,sNy)
      INTEGER bi, bj, myIter, myThid
      _RL myTime
CEOP

#ifdef ALLOW_LAND

C     == Local variables ==
C-  local variables used in solving the ground temp. implicitly :
C     aLoc         :: ground Conductivity * delT / delZ_12    [J/K]
C     bLoc         :: minus surf. flux derivative ./. Ts      [W/m2/K]
C     cLoc         :: temporary value for level.1 heat capacity [J/m2/K]
C     eLoc         :: temporary value for level.1 ground enthalpy [J/m2]
C     fLoc         :: temporary value for surface heat flux [W/m2]
C     alpha        :: snow thicknes / snow conductivity [m2.K/W]
C     beta         :: local coeff = 1/(1+alpha*bLoc)    [1]
C     tSurf        :: surf.  temperature   [oC]
C     tg           :: ground temperature   [oC]    (2 levels)
C     eg           :: ground enthalpy      [J/m2]  (2 levels)
C     cg           :: ground heat capacity [J/m2/K](2 levels)
C     mW           :: ground water mass    [kg/m2] (2 levels)
C     temp_af      :: ground temperature if above freezing
C     temp_bf      :: ground temperature if below freezing
C     mSnow        :: mass of snow         [kg/m2]
C     dMsn         :: mass of melting snow [kg/m2]
C     delT         :: time step            [s]
C     mSnEpsil     :: small snow mass      [kg/m2]
C     i,j,k        :: loop counters
C     msgBuf       :: Informational/error meesage buffer
C     tmpFlag      :: temp. flag, =.T. until found final groung temp
      _RL aLoc, bLoc, cLoc, eLoc, fLoc, alpha, beta
      _RL tSurf, tg(land_nLev), eg(land_nLev)
      _RL cg(land_nLev), mW(land_nLev)
      _RL temp_af, temp_bf
      _RL mSnow, dMsn, delT
      _RL mSnEpsil
      _RL tmp1, tmp2
      INTEGER i,j,k
      LOGICAL tmpFlag

#ifdef LAND_DEBUG
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL dBug, debugFlag
      INTEGER iprt,jprt,lprt
      DATA iprt, jprt , lprt / 19 , 20 , 6 /
      DATA debugFlag / .FALSE. /
 1010 FORMAT(A,I3,1P4E11.3)
#endif

      DATA    mSnEpsil / 1. _d -6 /

C-------------------------------------------------------------------------
C  solve implicitly the coupled 3 eq. (time level n+1 omitted) :
C    1a : if hs=0 : Flx = Flx^o + d.F/dT*(Ts - Ts^n) & Ts=Tg1
C    1b : if hs>0 : Flx = (Ts-Tg1)*Ks/hs =< Flx^o + d.F/dT*(Ts - Ts^n)
C         & difference used to melt the snow, maintaining Ts=0 
C    2  : Eg1 - Eg1^n  = delT*Flx - (lambda*delT/delZ_12)*(Tg1-Tg2)
C    3  : Eg2 - Eg2^n  = (lambda*delT/delZ_12)*(Tg1-Tg2)
C    were  lambda = ground Conductivity ; Ks = snow Conductivity
C          k=1,2: Eg_k = Cg_k * Tg_k - Lfreez * mIce_k
C
C  using local variables:
C   a = lambda*delT/delZ_12 ; b = - d.F/dT ;  f = Flx^o + b*Ts^n
C                         alpha = hs/Ks ;  beta = 1/(1+alpha*b)
C  3.eq system becomes: 
C   o if Ts*hs =< 0
C     1a,b:  Ts = ( Tg1 + alpha*F)*beta
C      2  : Eg1 + a*(Tg1-Tg2) + (b*delT)*beta*Tg1 = Eg1^n + delT*f*beta
C      3  : Eg2 + a*(Tg2-Tg1) = Eg2^n
C   o else: 
C     1.b : Ts=0 , f = Flx(ts=0) ; snowMelt = (f + Tg1/alpha)/Lfreez
C      2  : Eg1 + a*(Tg1-Tg2) + (delT/alpha)*Tg1 = Eg1^n
C      3  : Eg2 + a*(Tg2-Tg1) = Eg2^n
C-------------------------------------------------------------------------

C---  Solve implicitely for ground temp. & surface temp

      delT = land_deltaT
      aLoc = land_grdLambda*land_deltaT*land_rec_dzC(2)
      DO j=1,sNy
       DO i=1,sNx
        IF ( land_frc(i,j,bi,bj).GT.0. ) THEN

C--   initialise local variables
          tmpFlag = .TRUE.
          tSurf = land_skinT(i,j,bi,bj)
          mSnow = land_rhoSnow*land_hSnow(i,j,bi,bj)
          bLoc  = -sFlx(i,j,2)
          fLoc  = sFlx(i,j,1)+bLoc*tSurf
          alpha = land_hSnow(i,j,bi,bj)/diffKsnow
          beta  = 1. _d 0/(1. _d 0+alpha*bLoc)
          DO k=1,land_nLev
            eg(k) = land_dzF(k)*land_enthalp(i,j,k,bi,bj)
            mW(k) = land_dzF(k)*land_groundW(i,j,k,bi,bj)
     &                         *land_waterCap*land_rhoLiqW
            mW(k) = MAX( mW(k), 0. _d 0 )
            cg(k) = land_dzF(k)*land_heatCs + mW(k)*land_CpWater
            tg(k) = land_groundT(i,j,k,bi,bj)
          ENDDO
#ifdef LAND_DEBUG
          dBug = bi.eq.lprt .AND. i.EQ.iprt .AND. j.EQ.jprt
          IF (dBug) write(6,1010)
     &         'LAND_IMPL_TEMP: 0 , ts,tg1&2,mSw=',0,tSurf,tg,mSnow
          IF (dBug) write(6,1010)
     &         'LAND_IMPL_TEMP: 0 , sFlx=', 0,(sFlx(i,j,k),k=0,2)
#endif

C---   Solve for temp as if no freezing/melting was occuring :
          tg(1) = ( cg(1)*tg(1) + fLoc*delT*beta
     &                          + cg(2)*tg(2)*aLoc/(cg(2)+aLoc)
     &            )
     &          / ( cg(1) + aLoc + bLoc*delT*beta
     &                           - aLoc*aLoc/(cg(2)+aLoc) 
     &            )
          tg(2) = ( cg(2)*tg(2) + aLoc*tg(1) ) / (cg(2)+aLoc)
          tSurf = ( tg(1) + alpha*fLoc ) * beta
          
#ifdef LAND_DEBUG
          IF (dBug) write(6,1010)
     &         'LAND_IMPL_TEMP: 1 , ts,tg1&2,mW1=',1,tSurf,tg,mW(1)
#endif
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---   If melting/freezing (top of snow layer, ground water level 1 or 2)
C      set corresponding temp to freezing point and update enthalpy 
C--------------

          IF ( tg(2)*land_groundT(i,j,2,bi,bj) .LE. 0. _d 0 
     &         .AND. tmpFlag .AND. tSurf*mSnow .LE. 0. _d 0 ) THEN
C--    freezing/melting in level 2: set Tg2 to freezing point
           tmp1 = tg(1)
           tmp2 = tg(2)
           tg(2) = 0.
           eLoc = eg(1) + fLoc*delT*beta
           cLoc = cg(1) + aLoc + bLoc*delT*beta
           temp_bf = (eLoc+land_Lfreez*mW(1))/cLoc
           temp_af =  eLoc/cLoc
           tg(1) = MIN( temp_bf, MAX(temp_af, 0. _d 0) )
           tSurf = ( tg(1) + alpha*fLoc ) * beta
           IF ( tSurf*mSnow .LE. 0. _d 0 ) THEN
             tmpFlag = .FALSE.
             eg(1) = eLoc - (aLoc + bLoc*delT*beta)*tg(1)
             eg(2) = eg(2) + aLoc*tg(1) 
#ifdef LAND_DEBUG
           ELSEIF ( debugFlag ) THEN
             WRITE(msgBuf,'(A,2I4,2I3,I10)')
     &             'LAND_IMPL_TEMP: i,j,bi,bj,Iter=',
     &             i,j,bi,bj,myIter
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
             WRITE(msgBuf,'(A,1P4E12.4)')
     &             'LAND_IMPL_TEMP: groundT,t2,ts=',
     &             land_groundT(i,j,1,bi,bj),land_groundT(i,j,2,bi,bj),
     &             tmp2,(tmp1+alpha*fLoc)*beta
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
             WRITE(msgBuf,'(A,1P4E12.4)')
     &             'LAND_IMPL_TEMP: Tg,tSurf,mSnw=',
     &              tg,tSurf,mSnow
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
             WRITE(msgBuf,'(A,1P4E14.6)')
     &             'LAND_IMPL_TEMP: eg,mW=', eg,mW
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif
           ENDIF

C-  if tg2_new*tg2_old < 0 : end
          ENDIF

C--------------

          IF ( tg(1)*land_groundT(i,j,1,bi,bj) .LE. 0. _d 0 
     &         .AND. tmpFlag .AND. tSurf*mSnow .LE. 0. _d 0 ) THEN
C--    freezing/melting in level 1: set Tg1 to freezing point
           tmp1 = tg(1)
           tg(1) = 0.
           tg(2) = cg(2)*tg(2) / (cg(2)+aLoc)
           tSurf = alpha*fLoc * beta
           IF ( tSurf*mSnow .LE. 0. _d 0 ) THEN
             tmpFlag = .FALSE.
             eg(2) = eg(2) - aLoc*tg(2) 
             eg(1) = eg(1) + aLoc*tg(2) + fLoc*delT*beta
             IF ( eg(1)*mSnow .GT. 0. _d 0 ) THEN
C-           melt snow from bottom
              dMsn = MIN( mSnow, eg(1)*recip_Lfreez )
              land_Pr_m_Ev(i,j,bi,bj) = dMsn/delT
              land_hSnow(i,j,bi,bj) = (mSnow - dMsn)/land_rhoSnow 
              eg(1) = eg(1) - dMsn*land_Lfreez
#ifdef LAND_DEBUG
              IF (dBug) write(6,1010)
     &         'LAND_IMPL_TEMP: Bot-Melt : dMsn,dEg1,eg1=',
     &         1, dMsn, -dMsn*land_Lfreez, eg(1) 
#endif
             ENDIF
#ifdef LAND_DEBUG
           ELSEIF ( debugFlag ) THEN
             WRITE(msgBuf,'(A,2I4,2I3,I10)')
     &             'LAND_IMPL_TEMP: i,j,bi,bj,Iter=',
     &             i,j,bi,bj,myIter
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
             WRITE(msgBuf,'(A,4F11.6)')
     &             'LAND_IMPL_TEMP: groundT,t1,ts=',
     &             land_groundT(i,j,1,bi,bj),land_groundT(i,j,2,bi,bj),
     &             tmp1,(tmp1+alpha*fLoc)*beta
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
             WRITE(msgBuf,'(A,4F12.7)')
     &             'LAND_IMPL_TEMP: Tg,tSurf,mSnow=',
     &             tg,tSurf,mSnow
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
             WRITE(msgBuf,'(A,1P4E14.6)')
     &             'LAND_IMPL_TEMP: eg,mW=', eg,mW
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
             WRITE(msgBuf,'(A)')  
     &             'LAND_IMPL_TEMP: snow with ts > 0 ! but continue'
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif
           ENDIF

C-  if tg1_new*tg1_old < 0 : end
          ENDIF

C--------------

          IF ( tmpFlag .AND. tSurf*mSnow .GT. 0. _d 0 ) THEN
C--    snow is melting at the surface: set ts=0 & use fixed heat flux Flx(ts=0)
#ifdef LAND_DEBUG
              IF (dBug) write(6,1010)
     &         'LAND_IMPL_TEMP: Top-Melt : fx0, fx1, fx1-b*Ts =',
     &         1, sFlx(i,j,0), fLoc, fLoc-bLoc*tSurf
#endif
           tSurf = 0. _d 0
           fLoc = sFlx(i,j,0)
           dTsurf(i,j) = 1000.
           tg(1) = land_groundT(i,j,1,bi,bj)
           tg(2) = land_groundT(i,j,2,bi,bj)

           eLoc = cg(1)*tg(1)
     &          + delT*fLoc - land_Lfreez*mSnow + aLoc*tg(2)
           IF ( eLoc .GT. 0. _d 0 .OR. mSnow.LT.mSnEpsil ) THEN
C-     all snow melt: do not solve diffusion of heat in snow layer
C      but put surf. heat flux directly to 1rst level and set tg1=0
             dMsn = mSnow
             tg(1) = 0. _d 0
             tg(2) = cg(2)*tg(2) / (cg(2)+aLoc)
           ELSE
C-     solve diffusion of heat in snow layer ; heat flux difference 
C      (surf.Flx - diffusion.Flx) is used to melt the snow from top.
            tg(1) = ( cg(1)*tg(1) + cg(2)*tg(2)*aLoc/(cg(2)+aLoc) )
     &       / ( cg(1)+aLoc + delT/alpha - aLoc*aLoc/(cg(2)+aLoc) )
            tg(2) = ( cg(2)*tg(2) + aLoc*tg(1) ) / (cg(2)+aLoc)
            IF ( tg(2)*land_groundT(i,j,2,bi,bj).LE.0. _d 0 ) THEN
              tg(2) = 0.
              tg(1) = cg(1)*tg(1) / ( cg(1)+aLoc + delT/alpha )
            ELSEIF ( tg(1)*land_groundT(i,j,1,bi,bj).LE.0. _d 0 ) THEN
              tg(1) = 0.
              tg(2) = cg(2)*tg(2) / (cg(2)+aLoc)
            ENDIF
            dMsn = ( fLoc+tg(1)/alpha )*delT*recip_Lfreez
#ifdef LAND_DEBUG
              IF (dBug) write(6,1010)
     &         'LAND_IMPL_TEMP: Surf-Melt: dMsn,fLoc,tg1/alpha=',
     &          2, dMsn, fLoc,tg(1)/alpha
#endif
C-  note: Fx0 < -tg(1)/alpha can happen (due to non-linearity in Fx(Ts)), 
C-     => do not melt nor accumulate snow but put d.Flx in Eg1
            dMsn = MIN( MAX(dMsn, 0. _d 0), mSnow )
           ENDIF
           tmpFlag = .FALSE.
           eg(2) = eg(2) + aLoc*(tg(1)-tg(2))
           eg(1) = eg(1) - aLoc*(tg(1)-tg(2))
     &           + delT*fLoc - land_Lfreez*dMsn
           land_Pr_m_Ev(i,j,bi,bj) = dMsn/delT
           land_hSnow(i,j,bi,bj) = (mSnow - dMsn)/land_rhoSnow 

C-  if ts*hSnow > 0 , else:
          ELSEIF ( tmpFlag ) THEN
C--   snow is not melting & no freezing/melting in ground level 1 & 2
           eg(2) = eg(2) + aLoc*(tg(1)-tg(2))
           eg(1) = eg(1) - aLoc*(tg(1)-tg(2))
     &           + delT*(fLoc-bLoc*Tsurf)
           tmpFlag = .FALSE.
          ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---  Save new values :
          IF ( dTsurf(i,j) .LE. 999. ) 
     &         dTsurf(i,j) = tSurf - land_skinT(i,j,bi,bj)
          land_skinT(i,j,bi,bj) = tSurf
          DO k=1,land_nLev
            land_enthalp(i,j,k,bi,bj) = eg(k)/land_dzF(k)
            land_groundT(i,j,k,bi,bj) = tg(k)
          ENDDO

#ifdef LAND_DEBUG
          IF (dBug) write(6,1010) 'LAND_IMPL_TEMP: 9, ts,tg1&2,dTs=',9,
     &         tSurf, tg, dTsurf(i,j)
          IF (dBug) write(6,1010) 'LAND_IMPL_TEMP: 9, Eg1,Eg2,mPmE=',9,
     &       (land_enthalp(i,j,k,bi,bj),k=1,2), land_Pr_m_Ev(i,j,bi,bj)
#endif

        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_LAND */

      RETURN
      END
