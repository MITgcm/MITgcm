#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#undef SEAICE_ALTERNATIVE_MODULUS_U_AND_V

CBOP
C     !ROUTINE: SEAICE_SIDEDRAG
C     !INTERFACE:
      SUBROUTINE SEAICE_SIDEDRAG_STRESS(
     I     uIceArg, vIceArg,
     I     coastRoughUarg, coastRoughVarg, AREAarg,
     O     sideDragUarg,sideDragVarg,
     I     iStep, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_BOTTOMDRAG_COEFFS
C     | o Compute the side drag stress for ice-side drag,
C     |    as a parameterization for island-drag fastice
C     |    (Liu et al 2022)
C     *==========================================================*
C     | written by Yuqing Liu, Jul 2020
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     u/vIceArg      :: local copies of the current ice velocity
C     coastRoughUarg :: local copy of normalized coast length
C     coastRoughVarg :: local copy of normalized coast length
C     SideDraXarg    :: side drag in x direction
C     SideDraYarg    :: side drag in y direction
C     iStep     :: current sub-time step iterate
C     myTime    :: Simulation time
C     myIter    :: Simulation timestep number
C     myThid    :: my Thread Id. number
      _RL uIceArg       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceArg       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL AREAarg       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sideDragUarg  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sideDragVarg  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL coastRoughUarg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL coastRoughVarg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      INTEGER iStep
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef SEAICE_ALLOW_SIDEDRAG
C     === local variables ===
C     i,j,bi,bj,ksrf :: loop indices
      INTEGER i,j,bi,bj
      _RL    tmpx, tmpy
      _RL    uSpd(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    u_0

      u_0 = 0.0005 _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     initialize fields
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          sideDragUarg(i,j,bi,bj) = 0. _d 0
          sideDragVarg(i,j,bi,bj) = 0. _d 0
          uSpd(i,j)               = 0. _d 0
         ENDDO
        ENDDO

        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          uSpd(i,j) = 0.5 _d 0 *
     &          SQRT((uIceArg(i,  j,bi,bj)+uIceArg(i+1,j,  bi,bj))**2
     &              +(vIceArg(i,  j,bi,bj)+vIceArg(i,  j+1,bi,bj))**2)
         ENDDO
        ENDDO
C     calculate the sidedrag coeff for landfast ice parameterisations
C     (Liu et al 2022 use SEAICEsideDrag 2e-4)
        IF (SEAICEsideDrag .GT. 0. _d 0 ) THEN
         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
           IF ( AREAarg(i,j,bi,bj) .GT. 0.01 _d 0 ) THEN
# ifndef SEAICE_ALTERNATIVE_MODULUS_U_AND_V
            tmpx = 0.5 _d 0 * ( uSpd(i,j) + uSpd(i-1,j) )
            tmpy = 0.5 _d 0 * ( uSpd(i,j) + uSpd(i,j-1) )
# else /* ifdef SEAICE_ALTERNATIVE_MODULUS_U_AND_V */
C     A different way to compute |u|, |v|, deprecated
            tmpx = sqrt(uIceArg(i,j,bi,bj)**2+ (0.25 _d 0*
     &           (vIceArg(i,  j,  bi,bj)+vIceArg(i-1,j,bi,bj)+
     &            vIceArg(i-1,j+1,bi,bj)+vIceArg(i,  j+1,bi,bj)))**2)
            tmpy = sqrt(vIceArg(i,j,bi,bj)**2+ (0.25 _d 0*
     &           (uIceArg(i,  j,  bi,bj)+uIceArg(i,  j-1,bi,bj)+
     &            uIceArg(i+1,j-1,bi,bj)+uIceArg(i+1,j,bi,bj)))**2)
#endif

            sideDragUarg(i,j,bi,bj) =
     &           SEAICEsideDrag/(tmpx + u_0) * coastRoughUarg(i,j,bi,bj)
     &                                       *    seaiceMassU(i,j,bi,bj)
            sideDragVarg(i,j,bi,bj) =
     &           SEAICEsideDrag/(tmpy + u_0) * coastRoughVarg(i,j,bi,bj)
     &                                       *    seaiceMassV(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ELSEIF ( SEAICEsideDrag .LT. 0. _d 0 ) THEN
C     this is the quadratic drag law version, deprecated
         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
           IF ( AREAarg(i,j,bi,bj) .GT. 0.01 _d 0 ) THEN
# ifndef SEAICE_ALTERNATIVE_MODULUS_U_AND_V
            tmpx = 0.5 _d 0 * ( uSpd(i,j) + uSpd(i-1,j) )
            tmpy = 0.5 _d 0 * ( uSpd(i,j) + uSpd(i,j-1) )
# else /* ifdef SEAICE_ALTERNATIVE_MODULUS_U_AND_V */
C     A different way to compute |u|, |v|, deprecated
            tmpx = sqrt(uIceArg(i,j,bi,bj)**2+ (0.25 _d 0*
     &           (vIceArg(i,  j,  bi,bj)+vIceArg(i-1,j,bi,bj)+
     &            vIceArg(i-1,j+1,bi,bj)+vIceArg(i,  j+1,bi,bj)))**2)
            tmpy = sqrt(vIceArg(i,j,bi,bj)**2+ (0.25 _d 0*
     &           (uIceArg(i,  j,  bi,bj)+uIceArg(i,  j-1,bi,bj)+
     &            uIceArg(i+1,j-1,bi,bj)+uIceArg(i+1,j,bi,bj)))**2)
# endif

            sideDragUarg(i,j,bi,bj) =
     &           - SEAICEsideDrag * tmpx * coastRoughUarg(i,j,bi,bj)
     &                                   *    seaiceMassU(i,j,bi,bj)
            sideDragVarg(i,j,bi,bj) =
     &           - SEAICEsideDrag * tmpy * coastRoughVarg(i,j,bi,bj)
     &                                   *    seaiceMassV(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_SIDEDRAG */

      RETURN
      END
