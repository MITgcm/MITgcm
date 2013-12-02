C $Header: /u/gcmpack/MITgcm/pkg/atm2d/cpl_send_ocn_atmconfig.F,v 1.3 2013/12/02 23:54:43 jmc Exp $
C $Name:  $

#include "ATM2D_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_SEND_OCN_ATMCONFIG

C !INTERFACE:
      SUBROUTINE CPL_SEND_OCN_ATMCONFIG

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_SEND_OCN_ATMCONFIG
C     | o Routine for sending atmos. config to ocean component.
C     *==========================================================*
C     | This version talks to the MITgcm general circulation
C     | model. For now just send atmosphere land/sea mask.
C     *==========================================================*

#ifdef ATM2D_MPI_ON
C !USES:
      IMPLICIT NONE
C     == Global variables ==
c#include "ATMSIZE.h"
c#include "ATMVARS.h"
#include "ATMIDS.h"
#include "OCNSIZE.h"
#include "OCNVARS.h"
#include "OCNIDS.h"

C !INPUT/OUTPUT PARAMETERS:
C !LOCAL VARIABLES:
      INTEGER i,j
CEOP

C     Map atmos. depth map onto ocean grid
c     CALL ATM_TO_OCN_MAPXYR8(
c    I     Nx_atm, Ny_atm,
c    I     landMask_atm,
c    I     Nx_ocn, Ny_ocn,
c    O     landMask_ocn)
      DO i=1,Nx_ocn
        DO j=1,Ny_ocn
          landMask_ocn(i,j)= 0.5  !ocean code checks for this
        ENDDO
      ENDDO

C     Send atmos. config to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, atmLandName, Nx_ocn, Ny_ocn,
     &                       landMask_ocn )

#endif /* ATM2D_MPI_ON */

      RETURN
      END
