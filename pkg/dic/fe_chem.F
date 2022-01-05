#include "DIC_OPTIONS.h"

CBOP
C     !ROUTINE: Fe_CHEM
C     !INTERFACE:
      SUBROUTINE Fe_CHEM(
     I           bi,bj, iMin,iMax,jMin,jMax,
     I           fe,
     O           freefe,
     I           myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE Fe_CHEM
C     | o Calculate L,FeL,Fe concentration
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj              :: current tile indices
C     iMin,iMax,jMin,jMax :: Range of points for which calculation is performed.
C     myThid              :: my Thread Id number
      INTEGER bi,bj
      INTEGER iMin,iMax,jMin,jMax
      _RL     fe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL freefe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_FE
C     !LOCAL VARIABLES:
      INTEGER i,j,k
      _RL  lig, FeL
      _RL  tmpfe
#if ( defined MINFE && defined AD_SAFE )
      _RL thx, thy, theps
#endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  ADAPTED FROM PAYAL
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          freefe(i,j,k) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

C ligand balance in surface layer
C in surface layer

       DO k=1,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          IF ( maskC(i,j,k,bi,bj).GT.0. ) THEN

#ifdef DIC_NO_NEG
              tmpfe =MAX( 0. _d 0 , fe(i,j,k) )
#else
              tmpfe = fe(i,j,k)
#endif

C   Ligand,FeL,Fe calculation
              lig=(-ligand_stab*tmpfe +
     &              ligand_stab*ligand_tot-1. _d 0
     &             +((ligand_stab*tmpfe
     &                -ligand_stab*ligand_tot+1. _d 0)**2
     &               +4. _d 0*ligand_stab*ligand_tot)**0.5 _d 0
     &            )/(2. _d 0*ligand_stab)

              FeL = ligand_tot-lig
              IF (tmpfe.NE.0. _d 0) THEN
                freefe(i,j,k) = tmpfe -FeL
              ELSE
                freefe(i,j,k) = 0. _d 0
              ENDIF
#ifdef MINFE
#ifdef AD_SAFE
              thx=freefe(i,j,k)
              thy=freefemax
              theps=1. _d -8
              freefe(i,j,k) =
     &                 ( 1. _d 0 - tanh((thx-thy)/theps) ) * thx/2.+
     &                 ( 1. _d 0 + tanh((thx-thy)/theps) ) * thy/2.

#else
              freefe(i,j,k) = MIN(freefe(i,j,k),freefemax)
#endif
              fe(i,j,k) = FeL+freefe(i,j,k)
#endif
          ENDIF
         ENDDO
        ENDDO
       ENDDO

#endif /* ALLOW_FE */
      RETURN
      END
