C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_hdissip.F,v 1.35 2015/02/06 21:42:58 jmc Exp $
C $Name:  $

#include "MOM_VECINV_OPTIONS.h"

      SUBROUTINE MOM_VI_HDISSIP(
     I        bi, bj, k,
     I        hDiv, vort3, dStar, zStar, hFacZ,
     I        viscAh_Z, viscAh_D, viscA4_Z, viscA4_D,
     I        harmonic, biharmonic, useVariableViscosity,
     O        uDissip, vDissip,
     I        myThid )

      IMPLICIT NONE

C     Calculate horizontal dissipation terms
C     [del^2 - del^4] (u,v)

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi, bj, k
      _RL hDiv (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dStar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL zStar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL harmonic, biharmonic, useVariableViscosity
      _RL uDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i, j
      _RL Zip, Zij, Zpj, Dim, Dij, Dmj, uD2, vD2, uD4, vD4
      _RL Zip1, Zij1, Zpj1

C     - Laplacian  terms
      IF (harmonic) THEN
C This bit scales the harmonic dissipation operator to be proportional
C to the grid-cell area over the time-step. viscAh is then non-dimensional
C and should be less than 1/8, for example viscAh=0.01
       IF (useVariableViscosity) THEN
        DO j=2-OLy,sNy+OLy-1
         DO i=2-OLx,sNx+OLx-1

          Dij=hDiv( i , j )*viscAh_D(i,j)
          Dim=hDiv( i ,j-1)*viscAh_D(i,j-1)
          Dmj=hDiv(i-1, j )*viscAh_D(i-1,j)
          Zij=hFacZ( i , j )*vort3( i , j )*viscAh_Z(i,j)
          Zip=hFacZ( i ,j+1)*vort3( i ,j+1)*viscAh_Z(i,j+1)
          Zpj=hFacZ(i+1, j )*vort3(i+1, j )*viscAh_Z(i+1,j)

          uD2 = (
     &               cosFacU(j,bi,bj)*( Dij-Dmj )*recip_DXC(i,j,bi,bj)
     &     -_recip_hFacW(i,j,k,bi,bj)*( Zip-Zij )*recip_DYG(i,j,bi,bj) )
#ifdef ISOTROPIC_COS_SCALING
     &                                           *cosFacU(j,bi,bj)
#endif /* ISOTROPIC_COS_SCALING */
          vD2 = (
     &      _recip_hFacS(i,j,k,bi,bj)*( Zpj-Zij )*recip_DXG(i,j,bi,bj)
     &                                           *cosFacV(j,bi,bj)
     &                               +( Dij-Dim )*recip_DYC(i,j,bi,bj) )
#ifdef ISOTROPIC_COS_SCALING
     &                                           *cosFacV(j,bi,bj)
#endif /* ISOTROPIC_COS_SCALING */

          uDissip(i,j) = uD2
          vDissip(i,j) = vD2

         ENDDO
        ENDDO
       ELSE
        DO j=2-OLy,sNy+OLy-1
         DO i=2-OLx,sNx+OLx-1

          Dim=hDiv( i ,j-1)
          Dij=hDiv( i , j )
          Dmj=hDiv(i-1, j )
          Zip=hFacZ( i ,j+1)*vort3( i ,j+1)
          Zij=hFacZ( i , j )*vort3( i , j )
          Zpj=hFacZ(i+1, j )*vort3(i+1, j )

          uD2 = viscAhD*
     &               cosFacU(j,bi,bj)*( Dij-Dmj )*recip_DXC(i,j,bi,bj)
     &        - viscAhZ*_recip_hFacW(i,j,k,bi,bj)*
     &                                ( Zip-Zij )*recip_DYG(i,j,bi,bj)
#ifdef ISOTROPIC_COS_SCALING
     &                                           *cosFacU(j,bi,bj)
#endif /* ISOTROPIC_COS_SCALING */
          vD2 = viscAhZ*_recip_hFacS(i,j,k,bi,bj)*
     &               cosFacV(j,bi,bj)*( Zpj-Zij )*recip_DXG(i,j,bi,bj)
     &        + viscAhD*              ( Dij-Dim )*recip_DYC(i,j,bi,bj)
#ifdef ISOTROPIC_COS_SCALING
     &                                           *cosFacV(j,bi,bj)
#endif /* ISOTROPIC_COS_SCALING */

          uDissip(i,j) = uD2
          vDissip(i,j) = vD2

         ENDDO
        ENDDO
       ENDIF
      ELSE
       DO j=2-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1
         uDissip(i,j) = 0.
         vDissip(i,j) = 0.
        ENDDO
       ENDDO
      ENDIF

C     - Bi-harmonic terms
      IF (biharmonic) THEN

C This bit scales the harmonic dissipation operator to be proportional
C to the grid-cell area over the time-step. viscAh is then non-dimensional
C and should be less than 1/8, for example viscAh=0.01
       IF (useVariableViscosity) THEN
        DO j=2-OLy,sNy+OLy-1
         DO i=2-OLx,sNx+OLx-1

#ifdef MOM_VI_ORIGINAL_VISCA4
          Dim=dyF( i ,j-1,bi,bj)*dStar( i ,j-1)
          Dij=dyF( i , j ,bi,bj)*dStar( i , j )
          Dmj=dyF(i-1, j ,bi,bj)*dStar(i-1, j )

          Zip1=dxV( i ,j+1,bi,bj)*hFacZ( i ,j+1)*zStar( i ,j+1)
          Zij1=dxV( i , j ,bi,bj)*hFacZ( i , j )*zStar( i , j )
          Zpj1=dxV(i+1, j ,bi,bj)*hFacZ(i+1, j )*zStar(i+1, j )
#else
          Dim=dStar( i ,j-1)
          Dij=dStar( i , j )
          Dmj=dStar(i-1, j )

          Zip1=hFacZ( i ,j+1)*zStar( i ,j+1)
          Zij1=hFacZ( i , j )*zStar( i , j )
          Zpj1=hFacZ(i+1, j )*zStar(i+1, j )
#endif
          Dij=Dij*viscA4_D(i,j)
          Dim=Dim*viscA4_D(i,j-1)
          Dmj=Dmj*viscA4_D(i-1,j)
          Zij=Zij1*viscA4_Z(i,j)
          Zip=Zip1*viscA4_Z(i,j+1)
          Zpj=Zpj1*viscA4_Z(i+1,j)

#ifdef MOM_VI_ORIGINAL_VISCA4
          uD4 = recip_rAw(i,j,bi,bj)*(
     &                             ( (Dij-Dmj)*cosFacU(j,bi,bj) )
     &  -_recip_hFacW(i,j,k,bi,bj)*( Zip-Zij )
# ifdef ISOTROPIC_COS_SCALING
     &                                        *cosFacU(j,bi,bj)
# endif /* ISOTROPIC_COS_SCALING */
     &         )
          vD4 = recip_rAs(i,j,bi,bj)*(
     &   _recip_hFacS(i,j,k,bi,bj)*( (Zpj-Zij)*cosFacV(j,bi,bj) )
     &   +                         ( Dij-Dim )
# ifdef ISOTROPIC_COS_SCALING
     &                                        *cosFacV(j,bi,bj)
# endif /* ISOTROPIC_COS_SCALING */
     &         )
#else /* MOM_VI_ORIGINAL_VISCA4 */
          uD4 = (
     &               cosFacU(j,bi,bj)*( Dij-Dmj )*recip_DXC(i,j,bi,bj)
     &     -_recip_hFacW(i,j,k,bi,bj)*( Zip-Zij )*recip_DYG(i,j,bi,bj) )
# ifdef ISOTROPIC_COS_SCALING
     &                                           *cosFacU(j,bi,bj)
# endif /* ISOTROPIC_COS_SCALING */
          vD4 = (
     &      _recip_hFacS(i,j,k,bi,bj)*( Zpj-Zij )*recip_DXG(i,j,bi,bj)
     &                                           *cosFacV(j,bi,bj)
     &                               +( Dij-Dim )*recip_DYC(i,j,bi,bj) )
# ifdef ISOTROPIC_COS_SCALING
     &                                           *cosFacV(j,bi,bj)
# endif /* ISOTROPIC_COS_SCALING */
#endif  /* MOM_VI_ORIGINAL_VISCA4 */

          uDissip(i,j) = uDissip(i,j) - uD4
          vDissip(i,j) = vDissip(i,j) - vD4

         ENDDO
        ENDDO
       ELSE
        DO j=2-OLy,sNy+OLy-1
         DO i=2-OLx,sNx+OLx-1

#ifdef MOM_VI_ORIGINAL_VISCA4
          Dim=dyF( i ,j-1,bi,bj)*dStar( i ,j-1)
          Dij=dyF( i , j ,bi,bj)*dStar( i , j )
          Dmj=dyF(i-1, j ,bi,bj)*dStar(i-1, j )

          Zip1=dxV( i ,j+1,bi,bj)*hFacZ( i ,j+1)*zStar( i ,j+1)
          Zij1=dxV( i , j ,bi,bj)*hFacZ( i , j )*zStar( i , j )
          Zpj1=dxV(i+1, j ,bi,bj)*hFacZ(i+1, j )*zStar(i+1, j )
#else
          Dim=dStar( i ,j-1)
          Dij=dStar( i , j )
          Dmj=dStar(i-1, j )

          Zip1=hFacZ( i ,j+1)*zStar( i ,j+1)
          Zij1=hFacZ( i , j )*zStar( i , j )
          Zpj1=hFacZ(i+1, j )*zStar(i+1, j )
#endif
          Zij=Zij1
          Zip=Zip1
          Zpj=Zpj1

#ifdef MOM_VI_ORIGINAL_VISCA4
          uD4 = recip_rAw(i,j,bi,bj)*(
     &                             viscA4D*( Dij-Dmj )*cosFacU(j,bi,bj)
     &  -_recip_hFacW(i,j,k,bi,bj)*viscA4Z*( Zip-Zij )
# ifdef ISOTROPIC_COS_SCALING
     &                                                *cosFacU(j,bi,bj)
# endif /* ISOTROPIC_COS_SCALING */
     &                               )
          vD4 = recip_rAs(i,j,bi,bj)*(
     &   _recip_hFacS(i,j,k,bi,bj)*viscA4Z*( Zpj-Zij )*cosFacV(j,bi,bj)
     &   +                         viscA4D*( Dij-Dim )
# ifdef ISOTROPIC_COS_SCALING
     &                                                *cosFacV(j,bi,bj)
# endif /* ISOTROPIC_COS_SCALING */
     &                               )
#else /* MOM_VI_ORIGINAL_VISCA4 */
          uD4 = viscA4D*
     &               cosFacU(j,bi,bj)*( Dij-Dmj )*recip_DXC(i,j,bi,bj)
     &        - viscA4Z*_recip_hFacW(i,j,k,bi,bj)*
     &                                ( Zip-Zij )*recip_DYG(i,j,bi,bj)
# ifdef ISOTROPIC_COS_SCALING
     &                                           *cosFacU(j,bi,bj)
# endif /* ISOTROPIC_COS_SCALING */
          vD4 = viscA4Z*_recip_hFacS(i,j,k,bi,bj)*
     &               cosFacV(j,bi,bj)*( Zpj-Zij )*recip_DXG(i,j,bi,bj)
     &        + viscA4D*              ( Dij-Dim )*recip_DYC(i,j,bi,bj)
# ifdef ISOTROPIC_COS_SCALING
     &                                           *cosFacV(j,bi,bj)
# endif /* ISOTROPIC_COS_SCALING */
#endif /* MOM_VI_ORIGINAL_VISCA4 */

          uDissip(i,j) = uDissip(i,j) - uD4
          vDissip(i,j) = vDissip(i,j) - vD4

         ENDDO
        ENDDO
       ENDIF
      ENDIF

      IF ( harmonic .OR. biharmonic ) THEN
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
         uDissip(i,j) = uDissip(i,j)*maskW(i,j,k,bi,bj)
         vDissip(i,j) = vDissip(i,j)*maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
