#include "BLING_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
      SUBROUTINE BLING_MIXEDLAYER(
     U           sumMLDepth,
     I           bi, bj, imin, imax, jmin, jmax,
     I           myTime, myIter, myThid)

C     =================================================================
C     | subroutine bling_mixedlayer
C     | o Calculate mixed layer depth based on density criterion
C     |   (default: second derivative criterion; optional: threshold)
C     =================================================================

      IMPLICIT NONE

C     === Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "BLING_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif
C     === Routine arguments ===
C     bi,bj         :: tile indices
C     iMin,iMax     :: computation domain: 1rst index range
C     jMin,jMax     :: computation domain: 2nd  index range
C     myTime        :: current time
C     myIter        :: current timestep
C     myThid        :: thread Id. number
      INTEGER bi, bj, imin, imax, jmin, jmax
      INTEGER myThid
      INTEGER myIter
      _RL     myTime
C     === Output ===
C      sumMLDepth   :: mixed layer depth
      _RL sumMLDepth(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     === Local variables ===
#ifdef BLING_USE_THRESHOLD_MLD
      _RL dens_surf (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dens_z    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL delta_dens(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#else
      _RL blg_stra   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL rhoKm1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoKp1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL blg_minstra(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL blg_str2   (Nr)
      _RL blg_str2max
      INTEGER blgI,blgJ
#endif
      INTEGER i,j,k
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ijkkey, ikey, kkey
#endif
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      ikey = (act1 + 1) + act2*max1
     &                  + act3*max1*max2
     &                  + act4*max1*max2*max3
      ikey = (ikey-1)*Nr
#endif /* ALLOW_AUTODIFF_TAMC */
# ifndef BLING_USE_THRESHOLD_MLD
c ---------------------------------------------------------------------
c  Mixed layer depth initialization

      DO j=jmin,jmax
        DO i=imin,imax
          sumMLDepth(i,j) = drF(1)
          rhoKm1 (i,j)   = 0. _d 0
          rhoKp1 (i,j)   = 0. _d 0
          blg_minstra(i,j) = 0. _d 0
        ENDDO
      ENDDO

      DO k=1,Nr
        DO j=jmin,jmax
          DO i=imin,imax
            blg_stra(i,j,k) = 0. _d 0
          ENDDO
        ENDDO
        blg_str2(k) = 0
      ENDDO

c  get drhdr
      DO k=1,Nr-1
       DO j=jmin,jmax
        DO i=imin,imax
          rhoKm1(i,j) = rhoKp1(i,j)
        ENDDO
       ENDDO
       CALL FIND_RHO_2D(
     I        1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1,
     I        theta(1-OLx,1-OLy,k+1,bi,bj), salt(1-OLx,1-OLy,k+1,bi,bj),
     O        rhoKp1,
     I        k+1, bi, bj, myThid )

       IF (k.EQ.1) THEN
        DO j=jmin,jmax
         DO i=imin,imax
          blg_stra(i,j,k)= 0. _d 0
         ENDDO
        ENDDO
       ELSE
        DO j=jmin,jmax
         DO i=imin,imax
          blg_stra(i,j,k)= maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
     &                *recip_drC(k)*rkSign
     &                *(rhoKp1(i,j)-rhoKm1(i,j))
         ENDDO
        ENDDO
       ENDIF
      ENDDO
CMM Strat at k is on bottom of cell

C  % 1. find min(drho/dz)
      DO k = 1,Nr
#ifdef ALLOW_AUTODIFF_TAMC
       kkey = ikey + k
CADJ STORE blg_minstra = comlev1_bibj_k, key = kkey
#endif
       DO j=jmin,jmax
        DO i=imin,imax
         IF(blg_stra(i,j,k).LT.blg_minstra(i,j))
     &        blg_minstra(i,j) = blg_stra(i,j,k)
        ENDDO
       ENDDO
      ENDDO

CMM NOW LOOP TO GET MLD
        DO j=jmin,jmax
         DO i=imin,imax
C  % 2. check that we have at least 3 grid cells of water
          IF (hFacC(i,j,3,bi,bj).GT. 0. _d 0)  THEN
C  %  check that we have stable strat
           IF (blg_minstra(i,j).LT. 0. _d 0) THEN
C  % 3. find the index of minimum stratification
            blgI = 3
            DO k = Nr,1,-1
             IF (blg_stra(i,j,k).EQ.blg_minstra(i,j))  blgI = k
            ENDDO
C        %if deep enough see strat starts before pynocline
            IF (blgI.GT.3) THEN
C % 4. compute the second derivative (d2rho/dz2)..
CMM Only look up
            blg_str2max = -999
            DO k = 3,blgI
#ifdef ALLOW_AUTODIFF_TAMC
             ijkkey = ikey * (2*OLx+sNx) * (2*OLy+sNy)
     &            + ( (j-1)*(2*OLx+sNx) + (i-1) ) + k
CADJ STORE blg_str2max = comlev1_bibj_ijk, key = ijkkey
#endif
CMM blg_str2 will be in cell center
             blg_str2(k) = (blg_stra(i,j,k-1) -blg_stra(i,j,k))
     &                                             *recip_drF(k)
C     &                                             *blg_invdz(k)
Cblg_invdz should be DRF
             IF (blg_str2(k).GT.blg_str2max)
     &            blg_str2max = blg_str2(k)
            ENDDO
            DO k = Nr,1,-1
             IF (blg_str2(k).EQ.blg_str2max)  blgJ = k
            ENDDO

CMM %TAKE WHERE STRAT IS FASTEST INCREASING ABOVE PYCNOCLINE
            IF (blgJ.LT.blgI)  blgI = blgJ

            IF ( (blg_str2(blgI).LT. 0. _d 0) .AND. (blgI.EQ.blgJ) )
CMM; %error: strat should increase or be flat
     &       print*,'error: strat should increase or be flat'

            sumMLDepth(i,j) = -RF(blgI)
           ENDIF
          ENDIF
         ENDIF
        ENDDO
       ENDDO

#else /* BLING_USE_THRESHOLD_MLD */
c ---------------------------------------------------------------------
c  Mixed layer depth

      DO j=jmin,jmax
        DO i=imin,imax
          SumMLDepth(i,j) = drf(1)
        ENDDO
      ENDDO

c  Surface density
      CALL FIND_RHO_2D(
     I     1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1,
     I     theta(1-OLx,1-OLy,1,bi,bj), salt(1-OLx,1-OLy,1,bi,bj),
     O     dens_surf,
     I     1, bi, bj, myThid )

      DO k=1,Nr
        DO j=jmin,jmax
          DO i=imin,imax
             if (k.eq.1) then
              delta_dens(i,j,1) = 0. _d 0
             else
              delta_dens(i,j,k) = 9999. _d 0
             endif
          ENDDO
        ENDDO
      ENDDO

      DO k = 2,Nr

c  Potential density
         CALL FIND_RHO_2D(
     I        1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        dens_z,
     I        k, bi, bj, myThid )

        DO j=jmin,jmax
          DO i=imin,imax

c           SumMLDepth(i,j) = 0. _d 0

           IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN
            delta_dens(i,j,k) = dens_z(i,j)-dens_surf(i,j)
            IF (delta_dens(i,j,k) .LT. 0.03 _d 0) THEN
             SumMLDepth(i,j) = SumMLDepth(i,j)+drF(k)
            ENDIF
           ENDIF

          ENDDO
        ENDDO
      ENDDO

#endif /* BLING_USE_THRESHOLD_MLD */

      RETURN
      END
