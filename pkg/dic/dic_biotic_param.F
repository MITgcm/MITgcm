C $Header: /u/gcmpack/MITgcm/pkg/dic/Attic/dic_biotic_param.F,v 1.10 2008/04/04 21:37:05 dfer Exp $
C $Name:  $

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
#include "DIC_VARS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j,k,bi,bj

#ifdef DIC_BIOTIC


C fraction of new production going to DOP
       DOPfraction = 0.67 _d 0
C DOP remineralization rate (s) = 1/(6 month)
       KDOPRemin = 1. _d 0/(6. _d 0*30. _d 0*86400. _d 0)
C remin power law coeff
       Kremin = 0.9 _d 0
C Depth (m) over which biological productivity (BIOac)
C is computed --> determines nlev as the indice of the
C first layer deeper than -zcrit
       zcrit = 500. _d 0
#ifndef AD_SAFE
       k = 1
       DO WHILE ( k .LT. Nr .AND. rC(k) .GE. -zcrit )
          k = k+1
       ENDDO
       nlev = k
#else
       nlev=Nr
#endif

ccritical oxygen level (mol m-3)
       O2crit = 4. _d -3
C set stoichiometric ratios
       R_OP = -170. _d 0
       R_CP =  117. _d 0
       R_NP =   16. _d 0
       R_FeP=  0.000468 _d 0
C set scale depth for CaCO3 remineralization (m)
       zca = 3500. _d 0
cn parameters for light/nutrient limited bioac
       parfrac=0.4 _d 0 !fraction of Qsw that is PAR
       k0=0.02 _d 0     !light attentuation coefficient
       lit0=30. _d 0    ! half saturation light constant (W/m2)
       Kpo4 = 5. _d -4  ! half saturation phosphate constant (mol/m3)
       Kfe = 1.2 _d -7  ! half saturation fe constant (mol/m3)
c iron chemisty values
       alpfe = 0.01 _d 0    ! solubility of aeolian fe
       freefemax = 3. _d -7 ! max solubility of free iron (mol/m3)
c
c control variables
       KScav =  0.19 _d 0/(360. _d 0*86400. _d 0)  ! iron scavenging rate QQ
       ligand_stab= 1. _d 8   ! ligand-free iron stability constant (m3/mol)
       ligand_tot = 1. _d -6  ! total free ligand  (mol/m3)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
C timescape for biological activity
             alpha(i,j,bi,bj)= 2. _d -3/(360. _d 0 * 86400. _d 0)
C set inorganic/organic carbon rain ratio
             rain_ratio(i,j,bi,bj)=7. _d -2
          ENDDO
         ENDDO
        ENDDO
       ENDDO

#endif

      RETURN
      END
