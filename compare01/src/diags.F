C $Id: diags.F,v 1.1 1998/05/25 20:21:04 cnh Exp $
C     /---------------------------------------------------------------\
C     |+| MIT General Circulation Modeling Package (GCMPACK)        |+|
C     |+|                                                           |+|
C     |+|     Copyright (c) 1993, 1994, 1995, 1996, 1997            |+|
C     |+|                                                           |+|
C     |+|     All rights reserved                                   |+|
C     |+|                                                           |+|
C     |+|  This software is provided with absolutely NO WARRANTY.   |+|
C     |+|                                                           |+|
C     |+|  Permission is given to use this software for any         |+|
C     |+|  non-commercial purpose provided that                     |+|
C     |+|   o Publications acknowledge any use of GCMPACK.          |+|
C     |+|   o Alterations to the software are made freely and       |+|
C     |+|     unconditionally available to all and any of GCMPACK   |+|
C     |+|     authors without prejudice.                            |+|
C     |+|                                                           |+|
C     |+|  All other uses, including redistribution in whole or     |+|
C     |+|   in part, are forbidden.                                 |+|
C     |+|                                                           |+|
C     |+|  Chris Hill cnh@plume.mit.edu                             |+|
C     \---------------------------------------------------------------/
C
C     /---------------------------------------------------------------\
C     |||     **************************************************    |||
C     |||     * General Circulation Modeling Package (GCMPACK) *    |||
C     |||     **************************************************    |||
C     |||                                                           |||
C     |||         MIT Ocean-Atmosphere Diagnostics Library          |||
C     |||         ========================================          |||
C     |||                                                           |||
C     |||     Diagnostic routines that are compatible with the MIT  |||
C     |||     Ocean-Atmosphere Circulation Model. The diagnostics   |||
C     |||     use the same gridding and other conventions as the    |||
C     |||     Ocean-Atmosphere Model. They can be used in a stand   |||
C     |||     alone mode reading output from the Ocean-Atmosphere   |||
C     |||     Model or invoked with calls to the library routines   |||
C     |||     can be made during a dynamical run.                   |||
C     \---------------------------------------------------------------/
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"

C |============|
C | diags.F    |
C |============|
C o Contents 
C   DIAGS_ADD_BASIN             
C   DIAGS_CALC_AVE_T_S_AND_RHO
C   DIAGS_CALC_MERID_PSI
C   DIAGS_CALC_MLD
C   DIAGS_CONTROL            
C   DIAGS_DUMP_MLD           
C   DIAGS_MIXED_LAYER_DEPTH
C   DIAGS_MERIDIONAL_FLUX
C   DIAGS_PRINT_AVE_T_S_AND_RHO
C   DIAGS_READ_BASIN_MASK                      
C   DIAGS_SHOW                                 
C   DIAGS_SHOW_BASINS                          

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_ADD_BASIN                                   |||
C|||===============================================================|||
C|||        Function: Add a basin definition to the diagnostic     |||
C|||                  objects set.                                 |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_ADD_BASIN( bName, iErr )
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | bName   - Name of the basin.                                 |
C     | iErr    - Error flag.                                        |
C     \--------------------------------------------------------------/
      CHARACTER*(*) bName        
      INTEGER       iErr          
      REAL          X1, X2, X3
CEndofinterface

      CALL DIAGS_CONTROL (
     &                    'ADD_BASIN', bName, X1, X2, X3, iErr )

      RETURN
      END

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_CALC_AVE_T_S_AND_RHO                        |||
C|||===============================================================|||
C|||        Function: Calculate the basin and whole domain average |||
C|||                  temperature, salinity and density.           |||
C|||                  Average is calculated for each depth in the  |||
C|||                  model.                                       |||
C|||        Comments: Result is stored in internal COMMON block.   |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_CALC_AVE_T_S_AND_RHO(
     I             T, S )
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | T       - Model temperature field ( oC ).                    |
C     | S       - Model salinity field (ppt).                        |
C     \--------------------------------------------------------------/
      REAL    T (_I3(Nz,Nx,Ny))
      REAL    S (_I3(Nz,Nx,Ny))
CEndofinterface
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | Nb, LEV - Loop counters.                                     |
C     | I, J, K - Loop counters.                                     |
C     | rho     - Array to hold density                              |
C     | basinVol- Accumulates volume of water in basin levels.       |
C     | basinT  - Accumulates volume integrated T in basin levels.   |
C     | basinS  - Accumulates volume integrated S in basin levels.   |
C     | basinRho- Accumulates volume integrated Rho in basin levels. |
C     | vol     - Temporary scalar holding cell volume.              |
C     \--------------------------------------------------------------/
      INTEGER Nb, K, I, J
      INTEGER LEV
      REAL    rho(_I3(Nz,Nx,Ny))
      REAL    basinVol
      REAL    basinT  
      REAL    basinS  
      REAL    basinRho 
      REAL    vol
C     Calculate sigmaTheta.
      DO K=1,Nz
       LEV=K
       CALL UPDATE_RHO(T,S,LEV,'LINEAR','ABS_LOCAL', rho )
      ENDDO
C     Calculate basin averages.
      DO Nb = 1, numberOfBasins
       bAveTime(Nb) = currentTime
       DO K=1,Nz
        basinVol = 0.
        basinT   = 0.
        basinS   = 0.
        basinRho = 0.
        DO J=1,Ny
         DO I=1,Nx
          vol = ZA(_I3(K,I,J))*delps(K)
          basinVol = basinVol+vol*basinMask(I,J,Nb)*PMASK(_I3(K,I,J))
          basinT   = basinT  +  T(_I3(K,I,J))*vol*basinMask(I,J,Nb)*PMASK(_I3(K,I,J))
          basinS   = basinS  +  S(_I3(K,I,J))*vol*basinMask(I,J,Nb)*PMASK(_I3(K,I,J))
          basinRho = basinRho+RHO(_I3(K,I,J))*vol*basinMask(I,J,Nb)*PMASK(_I3(K,I,J))
         ENDDO
        ENDDO
        IF ( basinVol .NE. 0. ) THEN
         bAveT  (K,Nb) =   basinT/basinVol
         bAveS  (K,Nb) =   basinS/basinVol
         bAveRho(K,Nb) = basinRho/basinVol
        ELSE
         bAveT  (K,Nb) = 0.
         bAveS  (K,Nb) = 0.
         bAveRho(K,Nb) = 0.
        ENDIF
       ENDDO
      ENDDO
      RETURN
      END

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_CALC_MERID_PSI                              |||
C|||===============================================================|||
C|||        Function: Calculate the meridional overturning         |||
C|||        Comments: Result is stored in internal COMMON block.   |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_CALC_MERID_PSI(
     I             V    )
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | V       - Meridional velocity ( m/s ).                       |
C     \--------------------------------------------------------------/
      REAL    V (_I3(Nz,Nx,Ny))
CEndofinterface
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | I, J, K, Nb - Loop counters.                                 |
C     | depth       - Temporary scalar holding depth.                |
C     \--------------------------------------------------------------/
      INTEGER I, J, K, Nb
      REAL    depth   
      REAL    totalFlux
      REAL    openArea
C     /--------------------------------------------------------------\
C     | Calculate psi YZ                                             |
C     \--------------------------------------------------------------/
      DO Nb =1,numberOfBasins
       DO K=Nz,1,-1
        DO J=1,Ny
         totalFlux = 0.
         DO I=1,Nx
          openArea  = 
     &    basinMask(I,J,Nb)*vMask(_I3(K,I,J))*yA(_I3(K,I,J))/g/ronil
          totalFlux = totalFlux+openArea*V(_I3(K,I,J))
         ENDDO
         IF ( K .NE. Nz ) THEN
          meridPsi(J,K,Nb) = meridPsi(J,K+1,Nb)+totalFlux
         ELSE
          meridPsi(J,K,Nb) = totalFlux
         ENDIF
        ENDDO
       ENDDO
      ENDDO
C
      RETURN
      END

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_CALC_MLD                                    |||
C|||===============================================================|||
C|||        Function: Calculate the mixed layer depth.             |||
C|||        Comments: Result is stored in internal COMMON block.   |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_CALC_MLD(
     I             T, S )
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | T       - Model temperature field ( oC ).                    |
C     | S       - Model salinity field (ppt).                        |
C     \--------------------------------------------------------------/
      REAL    T (_I3(Nz,Nx,Ny))
      REAL    S (_I3(Nz,Nx,Ny))
CEndofinterface
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | I, J, K - Loop counters.                                     |
C     | rho     - Array to hold density                              |
C     | depth   - Temporary scalar holding depth.                    |
C     \--------------------------------------------------------------/
      INTEGER K, I, J, LEV
      REAL    rho(_I3(Nz,Nx,Ny))
      REAL    depth   
C     Calculate sigma0.
      DO K=1,Nz
       LEV=K
       CALL UPDATE_RHO(T,S,LEV,'LINEAR','ABS_SURFACE', rho )
      ENDDO
C     Calculate mixed layer depth.             
      depth = -delps(1)/G/RONIL/2
      DO J=1,Ny
       DO I=1,Nx
        MLD(I,J)      = depth*PMASK(_I3(1,I,J))
        MLDIndex(I,J) = 1
       ENDDO
      ENDDO
      DO K=2,Nz
       depth = depth -delps(K-1)/G/RONIL/2 -delps(K)/G/RONIL/2
       DO J=1,Ny
        DO I=1,Nx
         IF ( rho(_I3(K,I,J)) - rho(_I3(1,I,J)) .LE. MixedLayerDensityJump .AND.
     &        PMASK(_I3(K,I,J)) .EQ. WATER ) THEN
          MLD     (I,J) = depth
          MLDIndex(I,J) = K
         ENDIF
        ENDDO
       ENDDO
      ENDDO
      RETURN
      END

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_CONTROL                                     |||
C|||===============================================================|||
C|||        Function: Central interface to the DIAGS configuration |||
C|||                  routines. Maintains global state associated  |||
C|||                  with DIAGS routines.                         |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_CONTROL ( 
     I                           op, oper01, oper02, oper03, oper04,
     U                           iErr )
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
#include "EXTERNAL.h"
CEndofinterface
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | op     - Operation to be performed.                          |
C     |          Supported operations are                            |
C     |          o ADD_BASIN                                         |
C     |            oper01 - CHARACTER string holding basin name      |
C     |                     String also identifies file from which   |
C     |                     basin map will be read.                  |
C     |            oper02 - Ignored                                  |
C     |            oper03 - Ignored                                  |
C     |            oper04 - Ignored                                  |
C     |          o PRINT_BASIN                                       |
C     |            oper01 - Ignored                                  |
C     |            oper02 - Ignored                                  |
C     |            oper03 - Ignored                                  |
C     |            oper04 - Ignored                                  |
C     |          o ADD_ZONAL_SECTION                                 |
C     |            oper01 - CHARACTER string identifying section.    |
C     |            oper02 - Y coordinate of section.                 |
C     |            oper03 - X starting coordinate of section.        |
C     |            oper04 - X ending coordinate of section.          |
C     |          o ADD_MERIDIONAL_SECTION                            |
C     |            oper01 - CHARACTER string identifying section.    |
C     |            oper02 - X coordinate of section.                 |
C     |            oper03 - Y starting coordinate of section.        |
C     |            oper04 - Y ending coordinate of section.          |
C     | oper01 - Argument for operation op.                          |
C     | oper02 - Argument for operation op.                          |
C     | oper03 - Argument for operation op.                          |
C     | oper04 - Argument for operation op.                          |
C     | iErr      - Error flag.                                      |
C     \--------------------------------------------------------------/
      CHARACTER*(*) OP
      CHARACTER*(*) OPER01
      REAL    OPER02
      REAL    OPER03
      REAL    OPER04
      INTEGER iErr
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | CALL1 - Flag used to cause initialisation.                   |
C     | fName - Holds constructed file name for basin mask.          |
C     | loc   - Index used to move through fName string.             |
C     | s1, s2- Substring start and end indices.                     |
C     | bNumber - Active basin number.                               |
C     \--------------------------------------------------------------/
      LOGICAL CALL1
      SAVE    CALL1
      DATA    CALL1 /.TRUE./
      CHARACTER*(MAXFN) fName
      INTEGER loc
      INTEGER s1
      INTEGER s2
      INTEGER bNumber
C     /--------------------------------------------------------------\
C     | Initialise if not already done.                              |
C     \--------------------------------------------------------------/
      IF ( CALL1 ) THEN
       numberOfBasins = 0
       CALL1 = .FALSE.
      ENDIF
      IF ( op .EQ. 'ADD_BASIN' ) THEN
C      /--------------------------------------------------------------\
C      | Load basin definition.                                       |
C      \--------------------------------------------------------------/
       bNumber = numberOfBasins + 1
       IF ( bNumber .GT. DIAGS_MAX_NUMBER_OF_BASINS ) GOTO 999
C      /--------------------------------------------------------------\
C      | Determine size of the basin name                             |
C      \--------------------------------------------------------------/
       loc = 1
       s1 = IFNBLNK(oper01)
       s2 = ILNBLNK(oper01)
       IF ( loc+s2-s1 .LE. MAXFN ) 
     &  fName(loc:loc+s2-s1) = oper01(s1:s2)
       loc = loc+s2-s1+1
       s1 = IFNBLNK(bMapSuffix)
       s2 = ILNBLNK(bMapSuffix)
       IF ( loc+s2-s1 .LE. MAXFN ) fName(loc:loc+s2-s1) = bMapSuffix(s1:s2)
       loc = loc+s2-s1
       IF ( loc .GT. MAXFN ) GOTO 998
       CALL GET_MAP(fName(1:loc), dUnit,
     O              basinMask,
     I              Nx, Ny, DIAGS_MAX_NUMBER_OF_BASINS, bNumber, 
     O              iErr)
       IF ( iErr .EQ. 0 ) THEN
        numberOfBasins = numberOfBasins+1
        basinList(numberOfBasins) = oper01
       ENDIF
      ELSEIF ( op .EQ. 'PRINT_BASINS' ) THEN
       CALL DIAGS_SHOW_BASINS
      ELSE
       GOTO 997
      ENDIF
 1000 CONTINUE
      RETURN
  999 CONTINUE
       iErr = 1  ! Too many basins.
      GOTO 1000
  998 CONTINUE
       iErr = 2  ! Name too long.
      GOTO 1000
  997 CONTINUE
       iErr = 3  ! Unrecognised operation.
      GOTO 1000
      END

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_DUMP_MERID_PSI                              |||
C|||===============================================================|||
C|||        Function: Dump out the meridional overturning.         |||
C|||        Comments: PSI is stored in internal COMMON block.      |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_DUMP_MERID_PSI( IOUNIT )
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | IOUNIT - Unit to which data will be written.                 |
C     \--------------------------------------------------------------/
      INTEGER IOUNIT            
CEndofinterface
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | I, J, K, Nb - Loop counters.                                 |
C     | meridPsiGrid- Temporary array holding grid for psi.          |
C     | yCoord      - Temporary array v point y coordinates.         |
C     | zCoord      - Temporary array v point z coordinates.         |
C     \--------------------------------------------------------------/
      INTEGER I, J, K, Nb
      REAL    meridPsiGrid(Ny,Nz)
      REAL    yCoord(Ny)
      REAL    zCoord(Nz)
      REAL    timePeriod
C     This is really dumb here!!!!!
      REAL delY
      REAL sbLat

      sbLat = -80.D0
      delY  = 4.0
C
C     Dump the grid.
      yCoord(1)=sbLat
      DO J=2,Ny
       yCoord(J)=yCoord(J-1)+delY
      ENDDO
      zCoord(1)=-delps(1)/g/ronil/2
      DO K=2,Nz
       zCoord(K)=zCoord(K-1)-delps(K-1)/g/ronil/2-delps(K)/g/ronil/2
      ENDDO
      DO K=1,Nz
       DO J=1,Ny
        meridPsiGrid(J,K)=yCoord(J)
       ENDDO
      ENDDO
      WRITE(IOUNIT) meridPsiGrid
      DO K=1,Nz
       DO J=1,Ny
        meridPsiGrid(J,K)=zCoord(J)
       ENDDO
      ENDDO
      WRITE(IOUNIT) meridPsiGrid
C     Dump each basin.
      DO Nb =1,numberOfBasins
       WRITE(IOUNIT) ((meridPsi(J,K,Nb),J=1,Ny),K=1,Nz)
      ENDDO
      timePeriod = currentTime-sumMeridPsiTime0+delT
      DO Nb =1,numberOfBasins
       WRITE(IOUNIT) 
     & ((sumMeridPsi(J,K,Nb)/timePeriod,J=1,Ny),K=1,Nz)
      ENDDO
C
      RETURN
      END

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_DUMP_MLD                                    |||
C|||===============================================================|||
C|||        Function: Write out the mixed layer depth.             |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_DUMP_MLD(
     I             IOUNIT )
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | IOUNIT  - Unit number for writing dump.                      |
C     \--------------------------------------------------------------/
      INTEGER IOUNIT             
      REAL    timePeriod
CEndofinterface
      WRITE(IOUNIT) mld
      timePeriod = currentTime-sumMldTime0+delT
      WRITE(IOUNIT) sumMld/timePeriod
      WRITE(IOUNIT) minMld
      WRITE(IOUNIT) maxMld
      RETURN
      END

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_GET_MLD                                     |||
C|||===============================================================|||
C|||        Function: Pass current mixed layer out.                |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_GET_MLD(
     I             mldArr, mldIndexArr )
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | mldArr - Array for passing mixed layer depth                 |
C     | mldIndexArr - Array for passing mixed layer base index.      |
C     \--------------------------------------------------------------/
      REAL    mldArr     (Nx,Ny)
      INTEGER mldIndexArr(Nx,Ny)
CEndofinterface
      mldArr      = mld
      mldIndexArr = mldIndex
C
      RETURN
      END

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_READ_BASIN_MASK                             |||
C|||===============================================================|||
C|||        Function: Controls loading of "basin" masks from       |||
C|||                  external file.                               |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_READ_BASIN_MASK( bNumber )
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "EXTERNAL.h"
CEndofinterface
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | bNumber - Ordinal index of basin mask to be loaded.          |
C     \--------------------------------------------------------------/
      INTEGER bNumber
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | fName - Holds constructed file name for basin mask.          |
C     | loc   - Index used to move through fName string.             |
C     | s1, s2- Substring start and end indices.                     |
C     \--------------------------------------------------------------/
      CHARACTER*(MAXFN) fName
      INTEGER loc
      INTEGER s1
      INTEGER s2
C     /--------------------------------------------------------------\
C     | Determine size of the basin name                             |
C     \--------------------------------------------------------------/
      loc = 1
      s1 = IFNBLNK(basinList(bNumber))
      s2 = ILNBLNK(basinList(bNumber))
      IF ( loc+s2-s1+1 .LE. MAXFN ) fName(loc:loc+s2-s1+1) = basinList(bNumber)(s1:s2)
      loc = loc+s2-s1+1
      s1 = IFNBLNK(bMapSuffix)
      s2 = ILNBLNK(bMapSuffix)
      IF ( loc+s2-s1+1 .LE. MAXFN ) fName(loc:loc+s2-s1+1) = bMapSuffix(s1:s2)
      loc = loc+s2-s1+1
      IF ( loc .LE. MAXFN ) THEN
      ENDIF
      RETURN
      END
C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_PRINT_AVE_T_S_AND_RHO                       |||
C|||===============================================================|||
C|||        Function: Request tabulation of current diagnostics.   |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_PRINT_AVE_T_S_AND_RHO(
     I                                        IOUNIT )
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
#include "EXTERNAL.h"
CEndofinterface
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | IOUNIT - Unit number on which output will be written.        |
C     \--------------------------------------------------------------/
      INTEGER IOUNIT
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | depth - Holds layer depth                                    |
C     | K, Nb - Loop counter                                         |
C     \--------------------------------------------------------------/
      REAL    depth
      INTEGER K, Nb
      WRITE(IOUNIT,*) 'numberOfBasins=', numberOfBasins
      DO Nb = 1, numberOfBasins
       WRITE(IOUNIT,*) 'basinName=',basinList(Nb), ',timeOfAverage=',
     &  bAveTime(Nb), ',Nz=',Nz
       WRITE (IOUNIT,*) 'Temp Salt Rho Depth '
       depth = -delps(1)/2/G/ronil
       DO K = 1, Nz
        IF ( K.NE. 1 ) THEN
         depth = depth - delps(K-1)/2/G/ronil - delps(K)/2/G/ronil
        ENDIF
        WRITE (IOUNIT,*) bAveT(K,Nb), bAveS(K,Nb), bAveRho(K,Nb), depth
       ENDDO
      ENDDO
C
      RETURN
      END

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_SHOW                                        |||
C|||===============================================================|||
C|||        Function: Request tabulation of current diagnostics.   |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_SHOW
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
#include "EXTERNAL.h"
CEndofinterface
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | ** NONE **                                                   |
C     \--------------------------------------------------------------/
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | ** NONE **                                                   |
C     \--------------------------------------------------------------/
      REAL X1, X2, X3
      INTEGER iErr

      CALL DIAGS_CONTROL ('PRINT_BASINS', ' ', X1, X2, X3, iErr )

      RETURN
      END

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_SHOW_BASINS                                 |||
C|||===============================================================|||
C|||        Function: Print maps of basins relative to coastline.  |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_SHOW_BASINS
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
#include "EXTERNAL.h"
CEndofinterface
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | ** NONE **                                                   |
C     \--------------------------------------------------------------/
      INTEGER IOUNIT   
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | I      - Loop counter.                                       |
C     | tmpArr - Work array for print routine.                       |
C     \--------------------------------------------------------------/
      REAL    tmpArr(Nx,Ny)
      INTEGER I
C
      DO I = 1, numberOfBasins
       tmpArr = basinMask(:,:,I)+PMASK(_I3(1,:,:))
       tmpArr = tmpArr*PMASK(_I3(1,:,:))
       CALL PLOT_FIELD(tmpArr,Nx,Ny)
      ENDDO
C
      RETURN
      END

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_RESET_MLD_STATS                            |||
C|||===============================================================|||
C|||        Function: Reset the mixed layer depth statistics.      |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_RESET_MLD_STATS
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
#include "EXTERNAL.h"
CEndofinterface
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | ** NONE **                                                   |
C     \--------------------------------------------------------------/
      INTEGER IOUNIT   
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | I      - Loop counter.                                       |
C     | CALL1  - Initialisation flag.                                |
C     \--------------------------------------------------------------/
      REAL    tmpArr(Nx,Ny)
      INTEGER I
      LOGICAL CALL1
      DATA    CALL1 /.TRUE./
      SAVE    CALL1
C
      maxMld = mld
      minMld = mld
      sumMld = 0.
      sumMldTime0 = currentTime
C
      RETURN
      END

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_RESET_PSI_STATS                             |||
C|||===============================================================|||
C|||        Function: Reset the meridional overturning statistics. |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_RESET_PSI_STATS 
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
#include "EXTERNAL.h"
CEndofinterface
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | ** NONE **                                                   |
C     \--------------------------------------------------------------/
C
      sumMeridPsi = 0.
      sumMeridPsiTime0 = currentTime
C
      RETURN
      END

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_STORE_MLD_STATS                             |||
C|||===============================================================|||
C|||        Function: Store the mixed layer depth statistics.      |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_STORE_MLD_STATS
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
#include "EXTERNAL.h"
CEndofinterface
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | ** NONE **                                                   |
C     \--------------------------------------------------------------/
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | CALL1  - Initialisation flag.                                |
C     \--------------------------------------------------------------/
      LOGICAL CALL1
      DATA    CALL1 /.TRUE./
      SAVE    CALL1
C
      IF ( CALL1 ) THEN
       maxMld = mld
       minMld = mld
       sumMld = 0.
       sumMldTime0 = currentTime
       CALL1  = .FALSE.
      ENDIF
C
      WHERE ( mld .GT. minMld ) minMld = mld
      WHERE ( mld .LT. maxMld ) maxMld = mld
      sumMld = sumMld + mld*delT
C
      RETURN
      END

C/-------------------------------------------------------------------\
C|||  Procedure: DIAGS_STORE_PSI_STATS                             |||
C|||===============================================================|||
C|||        Function: Store the meridional overturning statistics. |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE DIAGS_STORE_PSI_STATS 
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "DIAGS.h"
#include "MASKS.h"
#include "EXTERNAL.h"
CEndofinterface
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | ** NONE **                                                   |
C     \--------------------------------------------------------------/
      INTEGER IOUNIT   
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | I      - Loop counter.                                       |
C     | CALL1  - Initialisation flag.                                |
C     \--------------------------------------------------------------/
      INTEGER I
      LOGICAL CALL1
      DATA CALL1 /.TRUE./
      SAVE CALL1
C
      IF ( CALL1 ) THEN
       sumMeridPsi = 0.
       sumMeridPsiTime0 = currentTime
       CALL1 = .FALSE.
      ENDIF
C
      sumMeridPsi = sumMeridPsi+meridPsi*delT
C
      RETURN
      END
