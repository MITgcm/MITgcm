C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/zonal_filt_postsmooth.F,v 1.3 2001/02/04 14:38:51 cnh Exp $
C $Name:  $

#include "ZONAL_FILT_OPTIONS.h"

      SUBROUTINE ZONAL_FILT_POSTSMOOTH( 
     I                           holeMask, 
     U                           field, 
     O                           avgField,
     I                           lField,
     I                           myThid )
C     /==========================================================\
C     | S/R ZONAL_FILT_POSTSMOOTH                                |
C     | o After FFT filtering processing.                        |
C     |==========================================================|
C     | Restore the mean value that was removed prior to FFT.    |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     holeMask - Array with 0 for holes and != 0 for valid data.
C     lField   - Length of field to smooth (assumed periodic)
C     field    - Field smoothed.
C     avgField - Mean value of the field.
C     myThid   - Thread number of this instance of FFT_PRESMOOTH_IN_X
      INTEGER lField
      Real*8  holeMask(lField)
      Real*8  field(lField)
      Real*8  avgField
      INTEGER myThid

#ifdef ALLOW_ZONAL_FILT

C     == Local variables ====
C     I         - Loop counter
C 
      INTEGER I

C
C     Add fields mean value back in.
      DO I=1,lField
       IF ( holeMask(I) .NE. 0. ) THEN 
        field(I) = field(I) + avgField
       ELSE
        field(I) = 0.
       ENDIF
      ENDDO

#endif /* ALLOW_ZONAL_FILT */

      RETURN
      END
