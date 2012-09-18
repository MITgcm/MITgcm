C $Header: /u/gcmpack/MITgcm/pkg/obcs/orlanski_north.F,v 1.9 2012/09/18 20:09:17 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE ORLANSKI_NORTH( bi, bj, futureTime,
     I                      uVel, vVel, wVel, theta, salt,
     I                      myThid )
C     /==========================================================\
C     | SUBROUTINE OBCS_RADIATE                                  |
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
#ifdef ALLOW_OBCS_NORTH

C     == Local variables ==
      INTEGER  I, K, J_obc
      _RL CL, ab1, ab2, fracCVEL, f1, f2

      ab1   =  1.5 _d 0 + abEps /* Adams-Bashforth coefficients */
      ab2   = -0.5 _d 0 - abEps
      /* CMAX is maximum allowable phase speed-CFL for AB-II */
      /* cvelTimeScale is averaging period for phase speed in sec. */

      fracCVEL = deltaT/cvelTimeScale /* fraction of new phase speed used*/
      f1 = fracCVEL /* dont change this. Set cvelTimeScale */
      f2 = 1.0-fracCVEL   /* dont change this. set cvelTimeScale */

C     Northern OB (Orlanski Radiation Condition)
       DO K=1,Nr
         DO I=1-OLx,sNx+OLx
            J_obc=OB_Jn(I,bi,bj)
            IF ( J_obc.NE.OB_indexNone ) THEN
C              uVel
               IF ((UN_STORE_2(I,K,bi,bj).eq.0.).and.
     &            (UN_STORE_3(I,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                  CL=-(uVel(I,J_obc-1,K,bi,bj)-UN_STORE_1(I,K,bi,bj))/
     &          (ab1*UN_STORE_2(I,K,bi,bj) + ab2*UN_STORE_3(I,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               CVEL_UN(I,K,bi,bj) = f1*(CL*dyU(I,J_obc-1,bi,bj)/deltaT)+
     &                f2*CVEL_UN(I,K,bi,bj)
C              update OBC to next timestep
               OBNu(I,K,bi,bj)=uVel(I,J_obc,K,bi,bj)-
     &           CVEL_UN(I,K,bi,bj)*deltaT*recip_dyU(I,J_obc,bi,bj)*
     &           (ab1*(uVel(I,J_obc,K,bi,bj)-uVel(I,J_obc-1,K,bi,bj)) +
     &           ab2*(UN_STORE_4(I,K,bi,bj)-UN_STORE_1(I,K,bi,bj)))
C              vVel
               IF ((VN_STORE_2(I,K,bi,bj).eq.0.).and.
     &            (VN_STORE_3(I,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                  CL=-(vVel(I,J_obc-1,K,bi,bj)-VN_STORE_1(I,K,bi,bj))/
     &          (ab1*VN_STORE_2(I,K,bi,bj) + ab2*VN_STORE_3(I,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               CVEL_VN(I,K,bi,bj) = f1*(CL*dyF(I,J_obc-2,bi,bj)/deltaT)+
     &                f2*CVEL_VN(I,K,bi,bj)
C              update OBC to next timestep
               OBNv(I,K,bi,bj)=vVel(I,J_obc,K,bi,bj)-
     &           CVEL_VN(I,K,bi,bj)*deltaT*recip_dyF(I,J_obc-1,bi,bj)*
     &           (ab1*(vVel(I,J_obc,K,bi,bj)-vVel(I,J_obc-1,K,bi,bj)) +
     &           ab2*(VN_STORE_4(I,K,bi,bj)-VN_STORE_1(I,K,bi,bj)))
C              Temperature
               IF ((TN_STORE_2(I,K,bi,bj).eq.0.).and.
     &            (TN_STORE_3(I,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                  CL=-(theta(I,J_obc-1,K,bi,bj)-TN_STORE_1(I,K,bi,bj))/
     &          (ab1*TN_STORE_2(I,K,bi,bj) + ab2*TN_STORE_3(I,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               CVEL_TN(I,K,bi,bj) = f1*(CL*dyC(I,J_obc-1,bi,bj)/deltaT)+
     &                f2*CVEL_TN(I,K,bi,bj)
C              update OBC to next timestep
               OBNt(I,K,bi,bj)=theta(I,J_obc,K,bi,bj)-
     &          CVEL_TN(I,K,bi,bj)*deltaT*recip_dyC(I,J_obc,bi,bj)*
     &          (ab1*(theta(I,J_obc,K,bi,bj)-theta(I,J_obc-1,K,bi,bj))+
     &          ab2*(TN_STORE_4(I,K,bi,bj)-TN_STORE_1(I,K,bi,bj)))
C              Salinity
               IF ((SN_STORE_2(I,K,bi,bj).eq.0.).and.
     &            (SN_STORE_3(I,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                  CL=-(salt(I,J_obc-1,K,bi,bj)-SN_STORE_1(I,K,bi,bj))/
     &          (ab1*SN_STORE_2(I,K,bi,bj) + ab2*SN_STORE_3(I,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               CVEL_SN(I,K,bi,bj) = f1*(CL*dyC(I,J_obc-1,bi,bj)/deltaT)+
     &                f2*CVEL_SN(I,K,bi,bj)
C              update OBC to next timestep
               OBNs(I,K,bi,bj)=salt(I,J_obc,K,bi,bj)-
     &          CVEL_SN(I,K,bi,bj)*deltaT*recip_dyC(I,J_obc,bi,bj)*
     &          (ab1*(salt(I,J_obc,K,bi,bj)-salt(I,J_obc-1,K,bi,bj)) +
     &          ab2*(SN_STORE_4(I,K,bi,bj)-SN_STORE_1(I,K,bi,bj)))
#ifdef ALLOW_NONHYDROSTATIC
             IF ( nonHydrostatic ) THEN
C              wVel
               IF ((WN_STORE_2(I,K,bi,bj).eq.0.).and.
     &            (WN_STORE_3(I,K,bi,bj).eq.0.)) THEN
                  CL=0.
               ELSE
                 CL=-(wVel(I,J_obc-1,K,bi,bj)-WN_STORE_1(I,K,bi,bj))/
     &          (ab1*WN_STORE_2(I,K,bi,bj)+ab2*WN_STORE_3(I,K,bi,bj))
               ENDIF
               IF (CL.lt.0.) THEN
                  CL=0.
               ELSEIF (CL.gt.CMAX) THEN
                  CL=CMAX
               ENDIF
               CVEL_WN(I,K,bi,bj)=f1*(CL*dyC(I,J_obc-1,bi,bj)/deltaT)
     &                + f2*CVEL_WN(I,K,bi,bj)
C              update OBC to next timestep
               OBNw(I,K,bi,bj)=wVel(I,J_obc,K,bi,bj)-
     &           CVEL_WN(I,K,bi,bj)*deltaT*recip_dyC(I,J_obc,bi,bj)*
     &           (ab1*(wVel(I,J_obc,K,bi,bj)-wVel(I,J_obc-1,K,bi,bj))+
     &           ab2*(WN_STORE_4(I,K,bi,bj)-WN_STORE_1(I,K,bi,bj)))
             ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
C              update/save storage arrays
C              uVel
C              copy t-1 to t-2 array
               UN_STORE_3(I,K,bi,bj)=UN_STORE_2(I,K,bi,bj)
C              copy (current time) t to t-1 arrays
               UN_STORE_2(I,K,bi,bj)=uVel(I,J_obc-1,K,bi,bj) -
     &         uVel(I,J_obc-2,K,bi,bj)
               UN_STORE_1(I,K,bi,bj)=uVel(I,J_obc-1,K,bi,bj)
               UN_STORE_4(I,K,bi,bj)=uVel(I,J_obc,K,bi,bj)
C              vVel
C              copy t-1 to t-2 array
               VN_STORE_3(I,K,bi,bj)=VN_STORE_2(I,K,bi,bj)
C              copy (current time) t to t-1 arrays
               VN_STORE_2(I,K,bi,bj)=vVel(I,J_obc-1,K,bi,bj) -
     &         vVel(I,J_obc-2,K,bi,bj)
               VN_STORE_1(I,K,bi,bj)=vVel(I,J_obc-1,K,bi,bj)
               VN_STORE_4(I,K,bi,bj)=vVel(I,J_obc,K,bi,bj)
C              Temperature
C              copy t-1 to t-2 array
               TN_STORE_3(I,K,bi,bj)=TN_STORE_2(I,K,bi,bj)
C              copy (current time) t to t-1 arrays
               TN_STORE_2(I,K,bi,bj)=theta(I,J_obc-1,K,bi,bj) -
     &         theta(I,J_obc-2,K,bi,bj)
               TN_STORE_1(I,K,bi,bj)=theta(I,J_obc-1,K,bi,bj)
               TN_STORE_4(I,K,bi,bj)=theta(I,J_obc,K,bi,bj)
C              Salinity
C              copy t-1 to t-2 array
               SN_STORE_3(I,K,bi,bj)=SN_STORE_2(I,K,bi,bj)
C              copy (current time) t to t-1 arrays
               SN_STORE_2(I,K,bi,bj)=salt(I,J_obc-1,K,bi,bj) -
     &         salt(I,J_obc-2,K,bi,bj)
               SN_STORE_1(I,K,bi,bj)=salt(I,J_obc-1,K,bi,bj)
               SN_STORE_4(I,K,bi,bj)=salt(I,J_obc,K,bi,bj)
#ifdef ALLOW_NONHYDROSTATIC
             IF ( nonHydrostatic ) THEN
C              wVel
C              copy t-1 to t-2 array
               WN_STORE_3(I,K,bi,bj)=WN_STORE_2(I,K,bi,bj)
C              copy (current time) t to t-1 arrays
               WN_STORE_2(I,K,bi,bj)=wVel(I,J_obc-1,K,bi,bj) -
     &         wVel(I,J_obc-2,K,bi,bj)
               WN_STORE_1(I,K,bi,bj)=wVel(I,J_obc-1,K,bi,bj)
               WN_STORE_4(I,K,bi,bj)=wVel(I,J_obc,K,bi,bj)
             ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
            ENDIF
         ENDDO
       ENDDO

#endif /* ALLOW_OBCS_NORTH */
#endif /* ALLOW_ORLANSKI */
      RETURN
      END
