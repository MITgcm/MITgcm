C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_ini_forcing.F,v 1.4 2005/10/13 16:25:12 stephd Exp $
C $Name:  $

#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_FORCING_INT

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_INI_FORCING( myThid )

C !DESCRIPTION:
c initializes forcing fields to zero

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#include "GCHEM.h"
#include "DIC_ABIOTIC.h"
#ifdef DIC_BIOTIC
#include "DIC_BIOTIC.h"
#include "DIC_LOAD.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS
c
C First call requires that we initialize everything to zero for safety
      CALL LEF_ZERO( wspeed0,myThid )
      CALL LEF_ZERO( wspeed1,myThid )
      CALL LEF_ZERO( atmosp0,myThid )
      CALL LEF_ZERO( atmosp1,myThid )
      CALL LEF_ZERO( silica0,myThid )
      CALL LEF_ZERO( silica1,myThid )
      CALL LEF_ZERO( ice0,myThid )
      CALL LEF_ZERO( ice1,myThid )
#ifdef READ_PAR
      CALL LEF_ZERO( par0,myThid )
      CALL LEF_ZERO( par1,myThid )
#endif
# ifdef ALLOW_FE
      CALL LEF_ZERO( feinput0,myThid )
      CALL LEF_ZERO( feinput1,myThid )
# endif

       _EXCH_XY_R4(wspeed0, myThid )
       _EXCH_XY_R4(wspeed1, myThid )
       _EXCH_XY_R4(atmosp0, myThid )
       _EXCH_XY_R4(atmosp1, myThid )
       _EXCH_XY_R4(silica0, myThid )
       _EXCH_XY_R4(silica1, myThid )
       _EXCH_XY_R4(ice0, myThid )
       _EXCH_XY_R4(ice1, myThid )
#ifdef READ_PAR
       _EXCH_XY_R4(par0, myThid )
       _EXCH_XY_R4(par1, myThid )
#endif
# ifdef ALLOW_FE
       _EXCH_XY_R4(feinput0, myThid )
       _EXCH_XY_R4(feinput1, myThid )
# endif

#endif
      RETURN
      END
