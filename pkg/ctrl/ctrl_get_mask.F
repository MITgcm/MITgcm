#include "CTRL_OPTIONS.h"

CBOP
C     !ROUTINE: CTRL_GET_MASK3D
C     !INTERFACE:
      SUBROUTINE CTRL_GET_MASK3D( 
     I      xx_filename, 
     U      mask3D, 
     I      myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_GET_MASK3D
C     | o A simple routine to return the correct 3D mask for each ctrl
C     |   variable
C     | o Only important for a few cases: e.g. 
C     |   - uvel/vvel need maskW/S rather than maskC
C     |   - in shelfice domain, need special 2D masks
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      CHARACTER*(MAX_LEN_FNAM) xx_filename
      _RL mask3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == local variables ==
      integer i, j, bi, bj

CEOP

C --- Initialize to zero (covers overlap region)
      call ctrl_zero(mask3D, Nr, zeroRL, myThid)

C --- Set to maskC by default
      call ctrl_cprsrl(maskC, Nr, mask3D, Nr, myThid)
      
C --- Test special cases
#if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
      if (xx_filename(1:7).EQ.'xx_uvel') then
        call ctrl_cprsrl(maskW,Nr,mask3D,Nr,myThid)
      else if (xx_filename(1:7).EQ.'xx_vvel') then
        call ctrl_cprsrl(maskS,Nr,mask3D,Nr,myThid)
      endif
#endif

C --- Get overlaps
      if ((xx_filename(1:7).ne.'xx_uvel') .and.
     &    (xx_filename(1:7).ne.'xx_vvel')) then
       CALL EXCH_XYZ_RL( mask3D, myThid )
      endif


      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     !ROUTINE: CTRL_GET_MASK2D
C     !INTERFACE:
      SUBROUTINE CTRL_GET_MASK2D( 
     I      xx_filename, 
     U      mask2D, 
     I      myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_GET_MASK2D
C     | o A simple routine to return the correct 2D mask for each ctrl
C     |   variable.
C     | o 2D mask is in XY plane
C     | o Only important for a few cases: e.g. 
C     |   - uvel/vvel need maskW/S rather than maskC
C     |   - in shelfice domain, need special 2D masks
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_SHELFICE
# ifdef ALLOW_SHI2D_CONTROL
#  include "SHELFICE.h"
# endif
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      CHARACTER*(MAX_LEN_FNAM) xx_filename
      _RL mask2D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == local variables ==
      _RL mask3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      integer i, j, bi, bj

CEOP

C --- Initialize to zero (covers overlap region)
      call ctrl_zero(mask3D, Nr, zeroRL, myThid)

C --- Set to maskC by default
      call ctrl_cprsrl(maskC, Nr, mask3D, Nr, myThid)
      
C --- Test special cases
#ifdef ALLOW_SHI2D_CONTROL
      if (xx_filename(1:11).EQ.'xx_shicoeff') then
        call ctrl_cprsrl(maskSHI,Nr,mask3D,Nr,myThid)
      endif
#endif

C --- Set 2D mask for output
      do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)
        do j = 1,sNy
         do i = 1,sNx
          mask2D(i,j,bi,bj) = mask3D(i,j,1,bi,bj)
         enddo
        enddo
       enddo
      enddo

C --- Get overlaps
      CALL EXCH_XY_RL( mask2D, myThid )
      

      RETURN
      END
