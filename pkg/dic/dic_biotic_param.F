
#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: DIC_BIOTIC_PARAM

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_BIOTIC_PARAM( myThid )

C !DESCRIPTION:
C Set parameters needed for needed for biotic pump

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef DIC_BIOTIC
#include "DIC_BIOTIC.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j,k,bi,bj

#ifdef DIC_BIOTIC


C fraction of new production going to DOP
      DOPfraction = 0.67
C DOP remineralization rate (s)
      KDOPRemin = 1.0/(0.5*360.*86400.)
C remin power law coeff
       Kremin = 0.9
C critical depth (m)
       zcrit = 50.0
       nlev=10
ccritical oxygen level (mol m-3)
       O2crit = 4.0d-3
C set stoichiometric ratios
       R_op = -170.0
       R_cp =  117.0
       R_np =   16.0
       R_fep=  0.000468 
C set scale depth for CaCO3 remineralization (m)
       zca = 3500.0
cn parameters for light/nutrient limited bioac
       k0=0.02   !light attentuation coefficient
       lit0=30    ! half saturation light constant (W/m2)
       Kpo4 = 5.d-4 ! half saturation phosphate constant (mol/m3)
       Kfe = 0.00000012 ! half saturation fe constant (mol/m3)
       Kfe = 1.2e-7     ! half saturation fe constant (mol/m3)
c iron chemisty values
       alpfe = 0.01    ! solubility of aeolian fe
       freefemax = 3.e-7   ! max solubility of free iron (mol/m3)
c      KScav =  0.19/(360.*86400.)  ! scavenging rate QQ
c      ligand_stab= 1.d8  ! ligand-fre iron stability constant (m3/mol)
c      ligand_tot = 0.000001  ! total free ligand  (mol/m3)
c
c
c control variables
       KScav =  0.19/(360.*86400.)  ! iron scavenging rate QQ
       ligand_stab= 1.d8  ! ligand-free iron stability constant (m3/mol)
       ligand_tot = 0.000001  ! total free ligand  (mol/m3)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
C timescape for biological activity
             alpha(i,j,bi,bj)= 2d-3/(24*60*60*360)
C set inorganic/organic carbon rain ratio
             rain_ratio(i,j,bi,bj)=0.07
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       
#endif

      RETURN
      END
