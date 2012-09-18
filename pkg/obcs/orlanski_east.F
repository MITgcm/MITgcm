C $Header: /u/gcmpack/MITgcm/pkg/obcs/orlanski_east.F,v 1.13 2012/09/18 20:09:17 jmc Exp $
C $Name:  $
cc

#include "OBCS_OPTIONS.h"

      SUBROUTINE ORLANSKI_EAST( bi, bj, futureTime,
     I                      uVel, vVel, wVel, theta, salt,
     I                      myThid )
C     /==========================================================\
C     | SUBROUTINE ORLANSKI_EAST                                 |
C     | o Calculate future boundary data at open boundaries      |
C     |   at time = futureTime by applying Orlanski radiation    |
C     |   conditions.                                            |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"
#include "ORLANSKI.h"

C SPK 6/2/00: Added radiative OBCs for salinity.
C SPK 6/6/00: Changed calculation of OB*w. When K=1, the
C             upstream value is used. For example on the eastern OB:
C                IF (K.EQ.1) THEN
C                   OBEw(J,K,bi,bj)=wVel(I_obc-1,J,K,bi,bj)
C                ENDIF
C
C SPK 7/7/00: 1) Removed OB*w fix (see above).
C             2) Added variable CMAX. Maximum diagnosed phase speed is now
C                clamped to CMAX. For stability of AB-II scheme (CFL) the
C                (non-dimensional) phase speed must be <0.5
C             3) (Sonya Legg) Changed application of uVel and vVel.
C                uVel on the western OB is actually applied at I_obc+1
C                while vVel on the southern OB is applied at J_obc+1.
C             4) (Sonya Legg) Added templates for forced OBs.
C
C SPK 7/17/00: Non-uniform resolution is now taken into account in diagnosing
C              phase speeds and time-stepping OB values. CL is still the
C              non-dimensional phase speed; CVEL is the dimensional phase
C              speed: CVEL = CL*(dx or dy)/dt, where dx and dy is the
C              appropriate grid spacings. Note that CMAX (with which CL
C              is compared) remains non-dimensional.
C
C SPK 7/18/00: Added code to allow filtering of phase speed following
C              Blumberg and Kantha. There is now a separate array
C              CVEL_**, where **=Variable(U,V,T,S,W)Boundary(E,W,N,S) for
C              the dimensional phase speed. These arrays are initialized to
C              zero in ini_obcs.F. CVEL_** is filtered according to
C              CVEL_** = fracCVEL*CVEL(new) + (1-fracCVEL)*CVEL_**(old).
C              fracCVEL=1.0 turns off filtering.
C
C SPK 7/26/00: Changed code to average phase speed. A new variable
C              'cvelTimeScale' was created. This variable must now be
C              specified. Then, fracCVEL=deltaT/cvelTimeScale.
C              Since the goal is to smooth out the 'singularities' in the
C              diagnosed phase speed, cvelTimeScale could be picked as the
C              duration of the singular period in the unfiltered case. Thus,
C              for a plane wave cvelTimeScale might be the time take for the
C              wave to travel a distance DX, where DX is the width of the region
C              near which d(phi)/dx is small.
C
C JBG 4/10/03: Fixed phase speed at western boundary (as suggested by
C              Dale Durran in his MWR paper). Fixed value (in m/s) is
C              passed in as variable CFIX in data.obcs.
C              also now allow choice of Orlanski or fixed wavespeed
C              (by means of new booleans useFixedCEast and
C              useFixedCWest) without having to recompile each time
C

C     == Routine arguments ==
      INTEGER bi, bj
      _RL futureTime
      _RL uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_ORLANSKI
#ifdef ALLOW_OBCS_EAST

C     == Local variables ==
      INTEGER J, K, I_obc
      _RL CL, ab1, ab2, fracCVEL, f1, f2

      ab1   =  1.5 _d 0 + abEps /* Adams-Bashforth coefficients */
      ab2   = -0.5 _d 0 - abEps
      /* CMAX is maximum allowable phase speed-CFL for AB-II */
      /* cvelTimeScale is averaging period for phase speed in sec. */

      fracCVEL = deltaT/cvelTimeScale /* fraction of new phase speed used*/
      f1 = fracCVEL /* dont change this. Set cvelTimeScale */
      f2 = 1.0-fracCVEL   /* dont change this. set cvelTimeScale */

C     Eastern OB (Orlanski Radiation Condition)
      DO K=1,Nr
         DO J=1-OLy,sNy+OLy
            I_obc=OB_Ie(J,bi,bj)
            IF ( I_obc.NE.OB_indexNone ) THEN
C              uVel
               IF ((UE_STORE_2(J,K,bi,bj).eq.0.).and.
     &            (UE_STORE_3(J,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                  CL=-(uVel(I_obc-1,J,K,bi,bj)-UE_STORE_1(J,K,bi,bj))/
     &          (ab1*UE_STORE_2(J,K,bi,bj) + ab2*UE_STORE_3(J,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               IF (useFixedCEast) THEN
C                Fixed phase speed (ignoring all of that painstakingly
C                saved data...)
                 CVEL_UE(J,K,bi,bj) = CFIX
               ELSE
                 CVEL_UE(J,K,bi,bj) = f1*(CL*dxF(I_obc-2,J,bi,bj)/deltaT
     &              )+f2*CVEL_UE(J,K,bi,bj)
               ENDIF
C              update OBC to next timestep
               OBEu(J,K,bi,bj)=uVel(I_obc,J,K,bi,bj)-
     &           CVEL_UE(J,K,bi,bj)*(deltaT*recip_dxF(I_obc-1,J,bi,bj))*
     &           (ab1*(uVel(I_obc,J,K,bi,bj)-uVel(I_obc-1,J,K,bi,bj)) +
     &           ab2*(UE_STORE_4(J,K,bi,bj)-UE_STORE_1(J,K,bi,bj)))
C              vVel
               IF ((VE_STORE_2(J,K,bi,bj).eq.0.).and.
     &            (VE_STORE_3(J,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                  CL=-(vVel(I_obc-1,J,K,bi,bj)-VE_STORE_1(J,K,bi,bj))/
     &          (ab1*VE_STORE_2(J,K,bi,bj) + ab2*VE_STORE_3(J,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               IF (useFixedCEast) THEN
C                Fixed phase speed (ignoring all of that painstakingly
C                saved data...)
                 CVEL_VE(J,K,bi,bj) = CFIX
               ELSE
                 CVEL_VE(J,K,bi,bj) = f1*(CL*dxV(I_obc-1,J,bi,bj)
     $                 /deltaT)+f2*CVEL_VE(J,K,bi,bj)
               ENDIF
C              update OBC to next timestep
               OBEv(J,K,bi,bj)=vVel(I_obc,J,K,bi,bj)-
     &           CVEL_VE(J,K,bi,bj)*(deltaT*recip_dxV(I_obc,J,bi,bj))*
     &           (ab1*(vVel(I_obc,J,K,bi,bj)-vVel(I_obc-1,J,K,bi,bj)) +
     &           ab2*(VE_STORE_4(J,K,bi,bj)-VE_STORE_1(J,K,bi,bj)))
C              Temperature
               IF ((TE_STORE_2(J,K,bi,bj).eq.0.).and.
     &            (TE_STORE_3(J,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                  CL=-(theta(I_obc-1,J,K,bi,bj)-TE_STORE_1(J,K,bi,bj))/
     &          (ab1*TE_STORE_2(J,K,bi,bj) + ab2*TE_STORE_3(J,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               IF (useFixedCEast) THEN
C                Fixed phase speed (ignoring all of that painstakingly
C                saved data...)
                 CVEL_TE(J,K,bi,bj) = CFIX
               ELSE
                 CVEL_TE(J,K,bi,bj) = f1*(CL*dxC(I_obc-1,J,bi,bj)
     $                 /deltaT)+f2*CVEL_TE(J,K,bi,bj)
               ENDIF
C              update OBC to next timestep
               OBEt(J,K,bi,bj)=theta(I_obc,J,K,bi,bj)-
     &           CVEL_TE(J,K,bi,bj)*(deltaT*recip_dxC(I_obc,J,bi,bj))*
     &           (ab1*(theta(I_obc,J,K,bi,bj)-theta(I_obc-1,J,K,bi,bj))+
     &           ab2*(TE_STORE_4(J,K,bi,bj)-TE_STORE_1(J,K,bi,bj)))
C              Salinity
               IF ((SE_STORE_2(J,K,bi,bj).eq.0.).and.
     &            (SE_STORE_3(J,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                  CL=-(salt(I_obc-1,J,K,bi,bj)-SE_STORE_1(J,K,bi,bj))/
     &          (ab1*SE_STORE_2(J,K,bi,bj) + ab2*SE_STORE_3(J,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               IF (useFixedCEast) THEN
C                Fixed phase speed (ignoring all of that painstakingly
C                saved data...)
                 CVEL_SE(J,K,bi,bj) = CFIX
               ELSE
                 CVEL_SE(J,K,bi,bj) = f1*(CL*dxC(I_obc-1,J,bi,bj)
     $                 /deltaT)+f2*CVEL_SE(J,K,bi,bj)
               ENDIF
C              update OBC to next timestep
               OBEs(J,K,bi,bj)=salt(I_obc,J,K,bi,bj)-
     &           CVEL_SE(J,K,bi,bj)*(deltaT*recip_dxC(I_obc,J,bi,bj))*
     &           (ab1*(salt(I_obc,J,K,bi,bj)-salt(I_obc-1,J,K,bi,bj))+
     &           ab2*(SE_STORE_4(J,K,bi,bj)-SE_STORE_1(J,K,bi,bj)))
#ifdef ALLOW_NONHYDROSTATIC
             IF ( nonHydrostatic ) THEN
C              wVel
               IF ((WE_STORE_2(J,K,bi,bj).eq.0.).and.
     &            (WE_STORE_3(J,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                  CL=-(wVel(I_obc-1,J,K,bi,bj)-WE_STORE_1(J,K,bi,bj))/
     &          (ab1*WE_STORE_2(J,K,bi,bj)+ab2*WE_STORE_3(J,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               IF (useFixedCEast) THEN
C                Fixed phase speed (ignoring all of that painstakingly
C                saved data...)
                 CVEL_WE(J,K,bi,bj) = CFIX
               ELSE
                 CVEL_WE(J,K,bi,bj)=f1*(CL*dxC(I_obc-1,J,bi,bj)/deltaT)
     &                   + f2*CVEL_WE(J,K,bi,bj)
               ENDIF
C              update OBC to next timestep
               OBEw(J,K,bi,bj)=wVel(I_obc,J,K,bi,bj)-
     &           CVEL_WE(J,K,bi,bj)*(deltaT*recip_dxC(I_obc,J,bi,bj))*
     &           (ab1*(wVel(I_obc,J,K,bi,bj)-wVel(I_obc-1,J,K,bi,bj))+
     &           ab2*(WE_STORE_4(J,K,bi,bj)-WE_STORE_1(J,K,bi,bj)))
             ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
C              update/save storage arrays
C              uVel
C              copy t-1 to t-2 array
               UE_STORE_3(J,K,bi,bj)=UE_STORE_2(J,K,bi,bj)
C              copy (current time) t to t-1 arrays
               UE_STORE_2(J,K,bi,bj)=uVel(I_obc-1,J,K,bi,bj) -
     &         uVel(I_obc-2,J,K,bi,bj)
               UE_STORE_1(J,K,bi,bj)=uVel(I_obc-1,J,K,bi,bj)
               UE_STORE_4(J,K,bi,bj)=uVel(I_obc,J,K,bi,bj)
C              vVel
C              copy t-1 to t-2 array
               VE_STORE_3(J,K,bi,bj)=VE_STORE_2(J,K,bi,bj)
C              copy (current time) t to t-1 arrays
               VE_STORE_2(J,K,bi,bj)=vVel(I_obc-1,J,K,bi,bj) -
     &         vVel(I_obc-2,J,K,bi,bj)
               VE_STORE_1(J,K,bi,bj)=vVel(I_obc-1,J,K,bi,bj)
               VE_STORE_4(J,K,bi,bj)=vVel(I_obc,J,K,bi,bj)
C              Temperature
C              copy t-1 to t-2 array
               TE_STORE_3(J,K,bi,bj)=TE_STORE_2(J,K,bi,bj)
C              copy (current time) t to t-1 arrays
               TE_STORE_2(J,K,bi,bj)=theta(I_obc-1,J,K,bi,bj) -
     &         theta(I_obc-2,J,K,bi,bj)
               TE_STORE_1(J,K,bi,bj)=theta(I_obc-1,J,K,bi,bj)
               TE_STORE_4(J,K,bi,bj)=theta(I_obc,J,K,bi,bj)
C              Salinity
C              copy t-1 to t-2 array
               SE_STORE_3(J,K,bi,bj)=SE_STORE_2(J,K,bi,bj)
C              copy (current time) t to t-1 arrays
               SE_STORE_2(J,K,bi,bj)=salt(I_obc-1,J,K,bi,bj) -
     &         salt(I_obc-2,J,K,bi,bj)
               SE_STORE_1(J,K,bi,bj)=salt(I_obc-1,J,K,bi,bj)
               SE_STORE_4(J,K,bi,bj)=salt(I_obc,J,K,bi,bj)
#ifdef ALLOW_NONHYDROSTATIC
             IF ( nonHydrostatic ) THEN
C              wVel
C              copy t-1 to t-2 array
               WE_STORE_3(J,K,bi,bj)=WE_STORE_2(J,K,bi,bj)
C              copy (current time) t to t-1 arrays
               WE_STORE_2(J,K,bi,bj)=wVel(I_obc-1,J,K,bi,bj) -
     &         wVel(I_obc-2,J,K,bi,bj)
               WE_STORE_1(J,K,bi,bj)=wVel(I_obc-1,J,K,bi,bj)
               WE_STORE_4(J,K,bi,bj)=wVel(I_obc,J,K,bi,bj)
             ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
            ENDIF
         ENDDO
      ENDDO

#endif
#endif /* ALLOW_ORLANSKI */
      RETURN
      END
