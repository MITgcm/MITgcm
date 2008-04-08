C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_readparms.F,v 1.2 2008/04/08 20:21:35 dfer Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

CBOP
C !ROUTINE: DIC_READPARMS
C !INTERFACE: ==========================================================
      SUBROUTINE DIC_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R DIC_READPARMS
C     | o Initialise and read dic package parameters
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
#include "DIC_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid    :: My Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIC

C     === Local variables ===
C     msgBuf    :: Informational/error message buffer
C     iUnit     :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER i,j,k,bi,bj
      _RL alphaloc, rain_ratioloc

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Abiotic dic parameters:
C permil   :: set carbon mol/m3 <---> mol/kg conversion factor
C             default permil = 1024.5 kg/m3 
C Pa2Atm   :: Conversion factor for atmospheric pressure pLoad (when coupled
C             to atmospheric model) into Atm. Default assumes pLoad in Pascal
C             1 Atm = 1.01325e5 Pa = 1013.25 mb

      NAMELIST /ABIOTIC_PARMS/ permil, Pa2Atm

#ifdef DIC_BIOTIC

C-- Biotic dic parameters:
C   DOPfraction :: fraction of new production going to DOP
C   KDOPRemin   :: DOP remineralization rate (s) = 1/(6 month)
C   KRemin      :: remin power law coeff
C   zcrit       :: Minimum Depth (m) over which biological activity
C                  is computed --> determines nlev as the indice of the
C                  first layer deeper than -zcrit
C   O2crit      :: critical oxygen level (mol/m3)
C   R_OP, R_CP  :: stochiometric ratios
C   R_NP, R_FeP
C   zca         :: scale depth for CaCO3 remineralization (m)
CC Parameters for light/nutrient limited bioac
C   parfrac     :: fraction of Qsw that is PAR
C   k0          :: light attentuation coefficient (1/m)
C   lit0        :: half saturation light constant (W/m2)
C   KPO4        :: half saturation phosphate constant (mol/m3)
C   KFE         :: half saturation fe constant (mol/m3)
CC Iron chemisty values
C   alpfe       :: solubility of aeolian fe
C   freefemax   :: max solubility of free iron (mol/m3)
CC Control variables
C   KScav       :: iron scavenging rate QQ 
C   ligand_stab :: ligand-free iron stability constant (m3/mol)
C   ligand_tot  :: total free ligand  (mol/m3)
C   alpha       :: timescape for biological activity
C                  read in alphaloc and filled in 2d array alpha
C   rain_ratio  :: inorganic/organic carbon rain ratio
C                  read in rain_ratioloc and filled in 2d array rain_ratio

      NAMELIST /BIOTIC_PARMS/
     & DOPfraction, KDOPRemin, KRemin, zcrit,
     & O2crit, R_OP, R_CP, R_NP, R_FeP, zca,
     & parfrac, k0, lit0, KPO4, KFE,
     & alpfe, freefemax,
     & KScav, ligand_stab, ligand_tot,
     & alphaloc, rain_ratioloc
#endif

      NAMELIST /DIC_FORCING/
     &          WindFileDic, AtmospFileDic, IceFileDic,
     &          IronFileDic, SilicaFileDic,
     &          dic_ForcingPeriod, dic_ForcingCycle,
     &          dic_int1, dic_int2, dic_int3, dic_int4, dic_pCO2

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

       permil      = 1. _d 0 / 1024.5 _d 0
       Pa2Atm      = 1.01325 _d 5
#ifdef DIC_BIOTIC
       DOPfraction = 0.67 _d 0
       KDOPRemin   = 1. _d 0/(6. _d 0*30. _d 0*86400. _d 0)
       KRemin      = 0.9 _d 0
       zcrit       = 500. _d 0
       O2crit      = 4. _d -3
       R_OP        =-170. _d 0
       R_CP        = 117. _d 0
       R_NP        = 16. _d 0
       R_FeP       = 0.000468 _d 0
       zca         = 3500. _d 0
       parfrac     = 0.4 _d 0
       k0          = 0.02 _d 0
       lit0        = 30. _d 0
       KPO4        = 5. _d -4
       KFE         = 1.2 _d -7
       alpfe       = 0.01 _d 0
       freefemax   = 3. _d -7
       KScav       = 0.19 _d 0/(360. _d 0*86400. _d 0)
       ligand_stab = 1. _d 8
       ligand_tot  = 1. _d -6
       alphaloc    = 2. _d -3/(360. _d 0 * 86400. _d 0)
       rain_ratioloc = 7. _d -2
#endif
       WindFileDic  = ' '
       AtmospFileDic= ' '
       IceFileDic   = ' '
       IronFileDic  = ' '
       SilicaFileDic= ' '
       dic_int1    = 0
       dic_int2    = 0
       dic_int3    = 0
       dic_int4    = 0
       dic_pCO2    = 0. _d 0
c default periodic forcing to same as for physics
       dic_ForcingPeriod = externForcingPeriod
       dic_ForcingCycle  = externForcingCycle

      WRITE(msgBuf,'(A)') ' DIC_READPARMS: opening data.dic'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

      CALL OPEN_COPY_DATA_FILE( 'data.dic', 'DIC_READPARMS',
     O                          iUnit, myThid )

C--   Read parameters from open data file:

C-    Abiotic parameters
      READ(UNIT=iUnit,NML=ABIOTIC_PARMS)

#ifdef DIC_BIOTIC
C-    Biotic parameters
      READ(UNIT=iUnit,NML=BIOTIC_PARMS)
#endif

C-    forcing filenames and parameters
      READ(UNIT=iUnit,NML=DIC_FORCING)

      WRITE(msgBuf,'(A)')
     &   ' DIC_READPARMS: finished reading data.dic'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

C--   Close the open data file
      CLOSE(iUnit)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    derive other parameters:

#ifdef DIC_BIOTIC

#ifndef AD_SAFE
       k = 1
       DO WHILE ( k .LT. Nr .AND. rC(k) .GE. -zcrit )
          k = k+1
       ENDDO
       nlev = k
#else
       nlev=Nr
#endif

#endif /* DIC_BIOTIC */

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#ifdef DIC_BIOTIC
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            alpha(i,j,bi,bj)     = alphaloc
            rain_ratio(i,j,bi,bj)= rain_ratioloc
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#endif /* DIC_BIOTIC */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_DIC */

      RETURN
      END
