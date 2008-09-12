C $Header: /u/gcmpack/MITgcm/pkg/shelfice/Attic/shelfice_update_masks.F,v 1.1 2008/09/12 07:21:55 mlosch Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"

CBOP
C     !ROUTINE: SHELFICE_INI_DEPTH
C     !INTERFACE:
      SUBROUTINE SHELFICE_INI_DEPTH( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_INI_DEPTH
C     | o define R_shelfIce ( the Lower (in r sense) boundary of 
C     |   floating shelfice )
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif /* ALLOW_SHELFICE */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of SHELFICE_INI_DEPTH
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_SHELFICE
C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Tile indices
C     I,J    - Loop counters
      INTEGER bi, bj
      INTEGER  I, J
CEOP

C     initialize R_shelfIce
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          R_shelfIce(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     
      IF ( SHELFICEtopoFile .NE. ' ' ) THEN
C Read the shelfIce draught using the mid-level I/O pacakage read_write_rec
C The 0 is the "iteration" argument. The 1 is the record number.
       CALL READ_REC_XY_RS( SHELFICEtopoFile, R_shelfIce, 
     &      1, 0, myThid )
C Read the shelfIce draught using the mid-level I/O pacakage read_write_fld
C The 0 is the "iteration" argument. The ' ' is an empty suffix
C      CALL READ_FLD_XY_RS( SHELFICEtopoFile, ' ', R_shelfIce, 
C    &      0, myThid )
C Read the selfIce draught using the low-level I/O package
c      CALL MDSREADFIELD( SHELFICEtopoFile, readBinaryPrec,
c    &                     'RS', 1, R_selfIce, 1, myThid )
      ENDIF
C- end setup R_shelfIce in the interior

C- fill in the overlap (+ BARRIER):
      _EXCH_XY_R4(R_shelfIce, myThid )
#endif /* ALLOW_SHELFICE */

      RETURN
      END

CBOP
C     !ROUTINE: SHELFICE_UPDATE_MASKS
C     !INTERFACE:
      SUBROUTINE SHELFICE_UPDATE_MASKS( 
     I     rF, recip_drF,
     U     hFacC, 
     I     myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_UPDATE_MASKS
C     | o modify topography factor hFacC according to ice shelf
C     |   topography
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif /* ALLOW_SHELFICE */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     rF        :: R-coordinate of face of cell (units of r).
C     recip_drF :: Recipricol of cell face separation along Z axis ( units of r ).
C     hFacC     :: Fraction of cell in vertical which is open (see GRID.h)
C     myThid    :: Number of this instance of SHELFICE_UPDATE_MASKS
      _RS rF        (1:Nr+1)
      _RS recip_drF (1:Nr)
      _RS hFacC     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_SHELFICE
C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj   :: tile indices
C     I,J,K   :: Loop counters
      INTEGER bi, bj
      INTEGER I, J, K
      _RL hFacCtmp
      _RL hFacMnSz
CEOP

C     initialize R_shelfIce
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          R_shelfIce(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     
      IF ( SHELFICEtopoFile .NE. ' ' ) THEN
C Read the shelfIce draught using the mid-level I/O pacakage read_write_rec
C The 0 is the "iteration" argument. The 1 is the record number.
       CALL READ_REC_XY_RS( SHELFICEtopoFile, R_shelfIce,
     &      1, 0, myThid )
C Read the shelfIce draught using the mid-level I/O pacakage read_write_fld
C The 0 is the "iteration" argument. The ' ' is an empty suffix
C      CALL READ_FLD_XY_RS( SHELFICEtopoFile, ' ', R_shelfIce, 
C    &      0, myThid )
C Read the selfIce draught using the low-level I/O package
c      CALL MDSREADFIELD( SHELFICEtopoFile, readBinaryPrec,
c    &                     'RS', 1, R_selfIce, 1, myThid )
      ENDIF
C- end setup R_shelfIce in the interior

C- fill in the overlap (+ BARRIER):
      _EXCH_XY_R4(R_shelfIce, myThid )

C--   Calculate lopping factor hFacC : Remove part outside of the domain
C     taking into account the Reference (=at rest) Surface Position Ro_shelfIce
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)

C--   compute contributions of shelf ice to looping factors
        DO K=1, Nr
         hFacMnSz=max( hFacMin, min(hFacMinDr*recip_drF(k),1. _d 0) )
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
C      o Non-dimensional distance between grid boundary and model surface
           hFacCtmp = (rF(k)-R_shelfIce(I,J,bi,bj))*recip_drF(K)
C      o Reduce the previous fraction : substract the outside part.
           hFacCtmp = hFacC(I,J,K,bi,bj) - max( hFacCtmp, 0. _d 0)
C      o set to zero if empty Column :
           hFacCtmp = max( hFacCtmp, 0. _d 0)
C      o Impose minimum fraction and/or size (dimensional)
           IF (hFacCtmp.LT.hFacMnSz) THEN
            IF (hFacCtmp.LT.hFacMnSz*0.5) THEN
             hFacC(I,J,K,bi,bj)=0.
            ELSE
             hFacC(I,J,K,bi,bj)=hFacMnSz
            ENDIF
           ELSE
             hFacC(I,J,K,bi,bj)=hFacCtmp
           ENDIF
          ENDDO
         ENDDO
        ENDDO
C - end bi,bj loops.
       ENDDO
      ENDDO
#endif /* ALLOW_SHELFICE */


      RETURN
      END
