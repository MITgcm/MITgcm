#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_CORIOLIS

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_CORIOLIS(
     I        bi,bj,k,vFld,
     U        uCoriolisTerm,
     I        myThid)

C !DESCRIPTION:
C Calculates the horizontal Coriolis term in the zonal equation:
C \begin{equation*}
C \overline{f}^i \overline{v}^{ij}
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#if (defined (ALLOW_GGL90) && defined (ALLOW_GGL90_LANGMUIR))
# include "GGL90_OPTIONS.h"
# include "FFIELDS.h"
# include "GGL90.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  vFld                 :: meridional flow
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uCoriolisTerm        :: Coriolis term
      _RL uCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
      _RS one
      PARAMETER( one = 1. _d 0 )
#if (defined (ALLOW_GGL90) && defined (ALLOW_GGL90_LANGMUIR))
      _RL Vsfc, recip_Lasq
      _RL stokesV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
CEOP

#if (defined (ALLOW_GGL90) && defined (ALLOW_GGL90_LANGMUIR))
      IF (useLANGMUIR) THEN
       recip_Lasq = 1 / LC_num
       recip_Lasq = recip_Lasq * recip_Lasq
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx
         Vsfc = SIGN(SQRT(ABS(surfaceForcingV(i,j,bi,bj))),
     &               surfaceForcingV(i,j,bi,bj)) * recip_Lasq
         stokesV(i,j) = Vsfc * exp(4*PI/LC_lambda*rC(k))
        ENDDO
       ENDDO
      ENDIF
#endif

      IF ( selectCoriScheme.GE.2 ) THEN
C Energy conserving discretization
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx
         uCoriolisTerm(i,j) =
     &     0.5*( _fCori( i ,j,bi,bj)
     &          *0.5*( vFld( i ,j)+vFld( i ,j+1) )
     &         + _fCori(i-1,j,bi,bj)
     &          *0.5*( vFld(i-1,j)+vFld(i-1,j+1) ) )
#if (defined (ALLOW_GGL90) && defined (ALLOW_GGL90_LANGMUIR))
         IF (useLANGMUIR) THEN
          uCoriolisTerm(i,j) = uCoriolisTerm(i,j)
     &      +0.5*( _fCori( i ,j,bi,bj)
     &           *0.5*( stokesV( i ,j)+stokesV( i ,j+1) )
     &          + _fCori(i-1,j,bi,bj)
     &           *0.5*( stokesV(i-1,j)+stokesV(i-1,j+1) ) )
         ENDIF
#endif
        ENDDO
       ENDDO
      ELSE
C Original discretization
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx
         uCoriolisTerm(i,j) =
     &     0.5*( _fCori( i ,j,bi,bj) +
     &           _fCori(i-1,j,bi,bj)  )
     &   *0.25*(
     &     vFld( i ,j)+vFld( i ,j+1)
     &    +vFld(i-1,j)+vFld(i-1,j+1)
     &         )
#if (defined (ALLOW_GGL90) && defined (ALLOW_GGL90_LANGMUIR))
         IF (useLANGMUIR) THEN
          uCoriolisTerm(i,j) = uCoriolisTerm(i,j)
     &     +0.5*( _fCori( i ,j,bi,bj) +
     &            _fCori(i-1,j,bi,bj)  )
     &    *0.25*(
     &      stokesV( i ,j)+stokesV( i ,j+1)
     &     +stokesV(i-1,j)+stokesV(i-1,j+1)
     &          )
         ENDIF
#endif
        ENDDO
       ENDDO
      ENDIF

      IF ( selectCoriScheme.EQ.1 .OR. selectCoriScheme.EQ.3 ) THEN
C Scale term so that only "wet" points are used
C Due to: Jamart and Ozer, 1986, JGR 91 (C9), 10,621-10,631
C         "Numerical Boundary Layers and Spurious Residual Flows"
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx
         uCoriolisTerm(i,j) = uCoriolisTerm(i,j)
     &   *4. _d 0/MAX( one,
     &                 maskS( i , j ,k,bi,bj)+maskS( i ,j+1,k,bi,bj)
     &                +maskS(i-1, j ,k,bi,bj)+maskS(i-1,j+1,k,bi,bj) )
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
