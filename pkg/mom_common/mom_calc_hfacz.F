C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_calc_hfacz.F,v 1.2 2006/06/07 01:55:14 heimbach Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_CALC_HFACZ

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_CALC_HFACZ(
     I        bi,bj,k,
     O        hFacZ,r_hFacZ,
     I        myThid)

C !DESCRIPTION:
C Calculates the fractional thickness at vorticity points

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "EEPARAMS.h"
# include "tamc.h"
# include "tamc_keys.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  myThid               :: thread number
      INTEGER bi,bj,k
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  hFacZ                :: fractional thickness at vorticity points
C  r_hFacZ              :: reciprocal
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER I,J
#ifdef ALLOW_DEPTH_CONTROL
      _RL hFacZOpen(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hFacZOpenI(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hFacZOpenJ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
# ifdef USE_SMOOTH_MIN
      _RS      smoothMin_R4
      EXTERNAL smoothMin_R4
# endif /* USE_SMOOTH_MIN */
#else
      _RL hFacZOpen
#endif /* ALLOW_DEPTH_CONTROL */
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_DEPTH_CONTROL
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
          kkey = (ikey-1)*Nr + k
#endif /* ALLOW_DEPTH_CONTROL */
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Calculate open water fraction at vorticity points

#ifdef ALLOW_DEPTH_CONTROL
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        hFacZ(i,j)     =0.
        r_hFacZ(i,j)   =0.
        hFacZOpen(i,j) =0.
        hFacZOpenJ(i,j)=0.
        hFacZOpenJ(i,j)=0.
       ENDDO
      ENDDO

#ifdef    ALLOW_AUTODIFF_TAMC
CADJ STORE      hFacZ(:,:) = comlev1_bibj_k , key=kkey, byte=isbyte
CADJ STORE    r_hFacZ(:,:) = comlev1_bibj_k , key=kkey, byte=isbyte
#endif    /* ALLOW_AUTODIFF_TAMC */
      DO j=2-Oly,sNy+Oly
       DO i=2-Olx,sNx+Olx
        hFacZOpenJ(i,j)=
#ifdef USE_SMOOTH_MIN
     &       smoothMin_R4(_hFacW(i  ,j  ,k,bi,bj),
#else
     &                MIN(_hFacW(i  ,j  ,k,bi,bj),
#endif /* USE_SMOOTH_MIN */
     &                    _hFacW(i  ,j-1,k,bi,bj))
     &       *maskW(i,j,k,bi,bj)*maskW(i,j-1,k,bi,bj)
        hFacZOpenI(i,j)=
#ifdef USE_SMOOTH_MIN
     &       smoothMin_R4(_hFacS(i  ,j  ,k,bi,bj),
#else
     &                MIN(_hFacS(i  ,j  ,k,bi,bj),
#endif /* USE_SMOOTH_MIN */
     &                    _hFacS(i-1,j  ,k,bi,bj))
     &         *maskS(i,j,k,bi,bj)*maskS(i-1,j,k,bi,bj)
       ENDDO
      ENDDO
#ifdef    ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_DEPTH_CONTROL
CADJ STORE hFacZOpenI(:,:) = comlev1_bibj_k , key=kkey, byte=isbyte
CADJ STORE hFacZOpenJ(:,:) = comlev1_bibj_k , key=kkey, byte=isbyte
#endif /* ALLOW_DEPTH_CONTROL */
#endif    /* ALLOW_AUTODIFF_TAMC */
      DO j=2-Oly,sNy+Oly
       DO i=2-Olx,sNx+Olx
        hFacZ(i,j) =
#ifdef USE_SMOOTH_MIN
     &       smoothMin_R4(hFacZOpenI(i,j),hFacZOpenJ(i,j))
#else
     &                MIN(hFacZOpenI(i,j),hFacZOpenJ(i,j))
#endif /* USE_SMOOTH_MIN */
     &         *maskW(i,j,k,bi,bj)*maskW(i,j-1,k,bi,bj)
     &         *maskS(i,j,k,bi,bj)*maskS(i-1,j,k,bi,bj)
       ENDDO
      ENDDO
#ifdef    ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_DEPTH_CONTROL
CADJ STORE hFacZ(:,:) = comlev1_bibj_k , key=kkey, byte=isbyte
#endif /* ALLOW_DEPTH_CONTROL */
#endif    /* ALLOW_AUTODIFF_TAMC */
      DO j=2-Oly,sNy+Oly
       DO i=2-Olx,sNx+Olx
        IF (hFacZ(i,j).EQ.0.) THEN
         r_hFacZ(i,j)=0.
        ELSE
         r_hFacZ(i,j)=1./hFacZ(i,j)
        ENDIF
       ENDDO
      ENDDO
#ifdef    ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_DEPTH_CONTROL
CADJ STORE    r_hFacZ(:,:) = comlev1_bibj_k , key=kkey, byte=isbyte
#endif /* ALLOW_DEPTH_CONTROL */
#endif    /* ALLOW_AUTODIFF_TAMC */

#else /* not ALLOW_DEPTH_CONTROL */

      DO i=1-Olx,sNx+Olx
       hFacZ(i,1-Oly)=0.
       r_hFacZ(i,1-Oly)=0.
      ENDDO

      DO j=2-Oly,sNy+Oly
       hFacZ(1-Olx,j)=0.
       r_hFacZ(1-Olx,j)=0.
       DO i=2-Olx,sNx+Olx
        hFacZOpen=min(_hFacW(i,j,k,bi,bj),
     &                _hFacW(i,j-1,k,bi,bj))
        hFacZOpen=min(_hFacS(i,j,k,bi,bj),hFacZOpen)
        hFacZOpen=min(_hFacS(i-1,j,k,bi,bj),hFacZOpen)
        hFacZ(i,j)=hFacZOpen
        IF (hFacZ(i,j).EQ.0.) THEN
         r_hFacZ(i,j)=0.
        ELSE
         r_hFacZ(i,j)=1./hFacZ(i,j)
        ENDIF
       ENDDO
      ENDDO
#endif /* ALLOW_DEPTH_CONTROL */

      RETURN
      END
