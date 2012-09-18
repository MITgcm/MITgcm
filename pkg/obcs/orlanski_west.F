C $Header: /u/gcmpack/MITgcm/pkg/obcs/orlanski_west.F,v 1.13 2012/09/18 20:09:17 jmc Exp $
C $Name:  $
cc

#include "OBCS_OPTIONS.h"

      SUBROUTINE ORLANSKI_WEST( bi, bj, futureTime,
     I                      uVel, vVel, wVel, theta, salt,
     I                      myThid )
C     /==========================================================\
C     | SUBROUTINE ORLANSKI_WEST                                 |
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
C JBG 3/24/03: Fixed phase speed at western boundary (as suggested by
C              Dale Durran in his MWR paper). Fixed value (in m/s) is
C              passed in as variable CFIX in data.obcs.
C
C JBG 4/10/03: allow choice of Orlanski or fixed wavespeed (by means of new
C              booleans useFixedCEast and useFixedCWest) without
C              having to recompile each time
c SAL 1/7/03:  Fixed bug: implementation for salinity was incomplete
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
#ifdef ALLOW_OBCS_WEST

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

C      Western OB (Orlanski Radiation Condition)
       DO K=1,Nr
         DO J=1-OLy,sNy+OLy
            I_obc=OB_Iw(J,bi,bj)
            IF ( I_obc.NE.OB_indexNone ) THEN
C              uVel (to be applied at I_obc+1)
               IF ((UW_STORE_2(J,K,bi,bj).eq.0.).and.
     &            (UW_STORE_3(J,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                  CL=(uVel(I_obc+2,J,K,bi,bj)-UW_STORE_1(J,K,bi,bj))/
     &          (ab1*UW_STORE_2(J,K,bi,bj) + ab2*UW_STORE_3(J,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               IF (useFixedCWest) THEN
C                Fixed phase speed (ignoring all of that painstakingly
C                saved data...)
                 CVEL_UW(J,K,bi,bj) = CFIX
               ELSE
                 CVEL_UW(J,K,bi,bj) = f1*(CL*dxF(I_obc+2,J,bi,bj)/deltaT
     &              )+f2*CVEL_UW(J,K,bi,bj)
               ENDIF
C              update OBC to next timestep
               OBWu(J,K,bi,bj)=uVel(I_obc+1,J,K,bi,bj)+
     &          CVEL_UW(J,K,bi,bj)*(deltaT*recip_dxF(I_obc+1,J,bi,bj))*
     &          (ab1*(uVel(I_obc+2,J,K,bi,bj)-uVel(I_obc+1,J,K,bi,bj))+
     &          ab2*(UW_STORE_1(J,K,bi,bj)-UW_STORE_4(J,K,bi,bj)))
C              vVel
               IF ((VW_STORE_2(J,K,bi,bj).eq.0.).and.
     &            (VW_STORE_3(J,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                  CL=(vVel(I_obc+1,J,K,bi,bj)-VW_STORE_1(J,K,bi,bj))/
     &          (ab1*VW_STORE_2(J,K,bi,bj) + ab2*VW_STORE_3(J,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               IF (useFixedCWest) THEN
C                Fixed phase speed (ignoring all of that painstakingly
C                saved data...)
                 CVEL_VW(J,K,bi,bj) = CFIX
               ELSE
                 CVEL_VW(J,K,bi,bj) = f1*(CL*dxV(I_obc+2,J,bi,bj)/deltaT
     &              )+f2*CVEL_VW(J,K,bi,bj)
               ENDIF
C              update OBC to next timestep
               OBWv(J,K,bi,bj)=vVel(I_obc,J,K,bi,bj)+
     &           CVEL_VW(J,K,bi,bj)*(deltaT*recip_dxV(I_obc+1,J,bi,bj))*
     &           (ab1*(vVel(I_obc+1,J,K,bi,bj)-vVel(I_obc,J,K,bi,bj))+
     &           ab2*(VW_STORE_1(J,K,bi,bj)-VW_STORE_4(J,K,bi,bj)))
C              Temperature
               IF ((TW_STORE_2(J,K,bi,bj).eq.0.).and.
     &            (TW_STORE_3(J,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                  CL=(theta(I_obc+1,J,K,bi,bj)-TW_STORE_1(J,K,bi,bj))/
     &          (ab1*TW_STORE_2(J,K,bi,bj) + ab2*TW_STORE_3(J,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               IF (useFixedCWest) THEN
C                Fixed phase speed (ignoring all of that painstakingly
C                saved data...)
                 CVEL_TW(J,K,bi,bj) = CFIX
               ELSE
                 CVEL_TW(J,K,bi,bj) = f1*(CL*dxC(I_obc+2,J,bi,bj)/deltaT
     &              )+f2*CVEL_TW(J,K,bi,bj)
               ENDIF
C              update OBC to next timestep
               OBWt(J,K,bi,bj)=theta(I_obc,J,K,bi,bj)+
     &          CVEL_TW(J,K,bi,bj)*(deltaT*recip_dxC(I_obc+1,J,bi,bj))*
     &          (ab1*(theta(I_obc+1,J,K,bi,bj)-theta(I_obc,J,K,bi,bj))+
     &          ab2*(TW_STORE_1(J,K,bi,bj)-TW_STORE_4(J,K,bi,bj)))
C              Salinity
               IF ((SW_STORE_2(J,K,bi,bj).eq.0.).and.
     &            (SW_STORE_3(J,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                  CL=(salt(I_obc+1,J,K,bi,bj)-SW_STORE_1(J,K,bi,bj))/
     &          (ab1*SW_STORE_2(J,K,bi,bj) + ab2*SW_STORE_3(J,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               IF (useFixedCWest) THEN
C                Fixed phase speed (ignoring all of that painstakingly
C                saved data...)
                 CVEL_SW(J,K,bi,bj) = CFIX
               ELSE
                 CVEL_SW(J,K,bi,bj) = f1*(CL*dxC(I_obc+2,J,bi,bj)/deltaT
     &              )+f2*CVEL_SW(J,K,bi,bj)
               ENDIF
C              update OBC to next timestep
               OBWs(J,K,bi,bj)=salt(I_obc,J,K,bi,bj)+
     &           CVEL_SW(J,K,bi,bj)*(deltaT*recip_dxC(I_obc+1,J,bi,bj))*
     &           (ab1*(salt(I_obc+1,J,K,bi,bj)-salt(I_obc,J,K,bi,bj))+
     &           ab2*(SW_STORE_1(J,K,bi,bj)-SW_STORE_4(J,K,bi,bj)))
#ifdef ALLOW_NONHYDROSTATIC
             IF ( nonHydrostatic ) THEN
C              wVel
               IF ((WW_STORE_2(J,K,bi,bj).eq.0.).and.
     &            (WW_STORE_3(J,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                  CL=(wVel(I_obc+1,J,K,bi,bj)-WW_STORE_1(J,K,bi,bj))/
     &          (ab1*WW_STORE_2(J,K,bi,bj)+ab2*WW_STORE_3(J,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               IF (useFixedCWest) THEN
C                Fixed phase speed (ignoring all of that painstakingly
C                saved data...)
                 CVEL_WW(J,K,bi,bj) = CFIX
               ELSE
                 CVEL_WW(J,K,bi,bj)=f1*(CL*dxC(I_obc+2,J,bi,bj)/deltaT)
     &                   + f2*CVEL_WW(J,K,bi,bj)
               ENDIF
C              update OBC to next timestep
               OBWw(J,K,bi,bj)=wVel(I_obc,J,K,bi,bj)+
     &           CVEL_WW(J,K,bi,bj)*(deltaT*recip_dxC(I_obc+1,J,bi,bj))*
     &            (ab1*(wVel(I_obc+1,J,K,bi,bj)-wVel(I_obc,J,K,bi,bj))+
     &            ab2*(WW_STORE_1(J,K,bi,bj)-WW_STORE_4(J,K,bi,bj)))
             ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
C              update/save storage arrays
C              uVel
C              copy t-1 to t-2 array
               UW_STORE_3(J,K,bi,bj)=UW_STORE_2(J,K,bi,bj)
C              copy (current time) t to t-1 arrays
               UW_STORE_2(J,K,bi,bj)=uVel(I_obc+3,J,K,bi,bj) -
     &         uVel(I_obc+2,J,K,bi,bj)
               UW_STORE_1(J,K,bi,bj)=uVel(I_obc+2,J,K,bi,bj)
               UW_STORE_4(J,K,bi,bj)=uVel(I_obc+1,J,K,bi,bj)
C              vVel
C              copy t-1 to t-2 array
               VW_STORE_3(J,K,bi,bj)=VW_STORE_2(J,K,bi,bj)
C              copy (current time) t to t-1 arrays
               VW_STORE_2(J,K,bi,bj)=vVel(I_obc+2,J,K,bi,bj) -
     &         vVel(I_obc+1,J,K,bi,bj)
               VW_STORE_1(J,K,bi,bj)=vVel(I_obc+1,J,K,bi,bj)
               VW_STORE_4(J,K,bi,bj)=vVel(I_obc,J,K,bi,bj)
C              Temperature
C              copy t-1 to t-2 array
               TW_STORE_3(J,K,bi,bj)=TW_STORE_2(J,K,bi,bj)
C              copy (current time) t to t-1 arrays
               TW_STORE_2(J,K,bi,bj)=theta(I_obc+2,J,K,bi,bj) -
     &         theta(I_obc+1,J,K,bi,bj)
               TW_STORE_1(J,K,bi,bj)=theta(I_obc+1,J,K,bi,bj)
               TW_STORE_4(J,K,bi,bj)=theta(I_obc,J,K,bi,bj)
c              Salinity
C              copy t-1 to t-2 array
               SW_STORE_3(J,K,bi,bj)=SW_STORE_2(J,K,bi,bj)
C              copy (current time) t to t-1 arrays
               SW_STORE_2(J,K,bi,bj)=salt(I_obc+2,J,K,bi,bj) -
     &         salt(I_obc+1,J,K,bi,bj)
               SW_STORE_1(J,K,bi,bj)=salt(I_obc+1,J,K,bi,bj)
               SW_STORE_4(J,K,bi,bj)=salt(I_obc,J,K,bi,bj)
#ifdef ALLOW_NONHYDROSTATIC
             IF ( nonHydrostatic ) THEN
C              wVel
C              copy t-1 to t-2 array
               WW_STORE_3(J,K,bi,bj)=WW_STORE_2(J,K,bi,bj)
C              copy (current time) t to t-1 arrays
               WW_STORE_2(J,K,bi,bj)=wVel(I_obc+2,J,K,bi,bj) -
     &         wVel(I_obc+1,J,K,bi,bj)
               WW_STORE_1(J,K,bi,bj)=wVel(I_obc+1,J,K,bi,bj)
               WW_STORE_4(J,K,bi,bj)=wVel(I_obc,J,K,bi,bj)
             ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
            ENDIF
         ENDDO
      ENDDO

#endif
#endif /* ALLOW_ORLANSKI */
      RETURN
      END
