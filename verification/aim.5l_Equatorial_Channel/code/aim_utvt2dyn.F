C $Header: /u/gcmpack/MITgcm/verification/aim.5l_Equatorial_Channel/code/Attic/aim_utvt2dyn.F,v 1.2 2001/03/06 19:40:59 jmc Exp $
C     $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE AIM_UTVT2DYN( bi, bj, myThid )
C     /==================================================================\
C     | S/R AIM_UTVT2DYN                                                 |
C     |==================================================================|
C     | Interface between atmospheric physics package and the            |
C     | dynamical model. This routine copies AIM velocity tendencies     |
C     | into dynamics conforming array. The tendencies are left at the   |
C     | P-point locations in the dynamics conforming array.              |
C     \==================================================================/

C     === Global variables ===
C     Physics package
#include "atparam.h"
#include "atparam1.h"
      INTEGER NGP
      INTEGER NLON
      INTEGER NLAT
      INTEGER NLEV
      PARAMETER ( NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT )
#include "com_physvar.h"
#include "com_forcing1.h"
#include "Lev_def.h"
#include "AIM_DDT.h"
C     MITgcm
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

C     === Routine arguments ===
      INTEGER bi, bj
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_AIM

C     === Local variables ===
C     Loop counters
      INTEGER I, J, kLev
      INTEGER I2, kAtm

      DO kLev = 1, Nr
       DO J=1,sNy
        DO I=1,sNx
         I2 = sNx*(J-1)+I
         Katm = _KD2KA( Klev )
         AIM_UT(i,j,kLev,bi,bj) = UT_PBL(I2,katm)
         AIM_VT(i,j,kLev,bi,bj) = VT_PBL(I2,katm)
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
