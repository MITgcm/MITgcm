#include "ATM2D_OPTIONS.h"
#ifdef ATM2D_MPI_ON

CStartOfInterface
      SUBROUTINE CPL_SEND_OCN_ATMCONFIG
C     /==========================================================\
C     | SUBROUTINE CPL_SEND_OCN_ATMCONFIG                        |
C     | o Routine for sending atmos. config to ocean component.  |
C     |==========================================================|
C     | This version talks to the MITgcm general circulation     |
C     | model. For now just send atmosphere depths.              |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "OCNSIZE.h"
#include "OCNVARS.h"
#include "OCNIDS.h"

C     == Routine arguments ==

C     == Local variables ==
      CHARACTER*(10) atmDepthName
      PARAMETER( atmDepthName        = 'ATM Depths'     )
      INTEGER i,j

CEndOfInterface

      DO i=1,Nx_ocn
        DO j=1,Ny_ocn
          AtmDepths_ocn(i,j)=1. _d 5   !ocean code checks for this
        ENDDO
      ENDDO

   
C     Send atmos. config to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, atmDepthName, Nx_ocn, Ny_ocn, 
     &                       AtmDepths_ocn )

      RETURN
      END

#endif
