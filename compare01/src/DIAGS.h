C $Id: DIAGS.h,v 1.1 1998/05/25 20:21:06 cnh Exp $
C     Ultimately the diagnostics library should allow a set of diagnostic "objects"
C     to be defined. Diagnostic objects will have a type such as a basin
C     map or a section or a path. The model will call a DIAG_OBJECT_ADD
C     routine to create any number of diagnostic objects each having a unique
C     user defined name such as "NA_BASIN" or "SEC_I10" or "PATH_A1" and
C     of a specific type. Diagnostic operations will then be allowed using 
C     these diagnostic objects such as along path integrals or basin averages
C     etc... Diagnostic objects, the quantities to monitor for each object etc..
C     will be defined in the 
C     #DIAGNOSTICS
C     #END DIAGNOSTICS 
C     section of the input data file.
C     For now however something much simpler will be used!

C     /--------------------------------------------------------------\
C     | Constants for diagnostic utilities                           |
C     \--------------------------------------------------------------/
C     DIAGS_MAX_NUMBER_OF_BASINS - Maximum number of different
C                                  basins that can be defined.
C     bMapSuffix       - Suffix appended to name of a basin to get filename
C                        for the basin XY map.
      INTEGER DIAGS_MAX_NUMBER_OF_BASINS
      PARAMETER ( DIAGS_MAX_NUMBER_OF_BASINS = 10 )
      CHARACTER*(*) bMapSuffix
      PARAMETER    (bMapSuffix       =      '_map.txt'  )
      REAL    MixedLayerDensityJump
      PARAMETER ( MixedLayerDensityJump = 0.05 )

C     /--------------------------------------------------------------\
C     | Parameters for diagnostic utilities                          |
C     \--------------------------------------------------------------/
C     Floating point parameters
C     basinMask         - XY map(s) defining extent of each basin.
      COMMON /DIAGS_F/
     &                basinMask,
     &                bAveT, bAveS, bAveRho, bAveTime,
     &                meridPsi, mld,
     &                sumMeridPsi, sumMeridPsiTime0,
     &                maxMld, sumMld, sumMldTime0, minMld
      REAL basinMask(Nx,Ny,DIAGS_MAX_NUMBER_OF_BASINS)
      REAL bAveT  (Nz,DIAGS_MAX_NUMBER_OF_BASINS)
      REAL bAveS  (Nz,DIAGS_MAX_NUMBER_OF_BASINS)
      REAL bAveRho(Nz,DIAGS_MAX_NUMBER_OF_BASINS)
      REAL bAveTime(DIAGS_MAX_NUMBER_OF_BASINS)
      REAL meridPsi(Ny,Nz,DIAGS_MAX_NUMBER_OF_BASINS)
      REAL sumMeridPsi(Ny,Nz,DIAGS_MAX_NUMBER_OF_BASINS)
      REAL sumMeridPsiTime0
      REAL mld(Nx,Ny)
      REAL maxMld(Nx,Ny)
      REAL sumMld(Nx,Ny)
      REAL sumMldTime0
      REAL minMld(Nx,Ny)

C     Integer parameters
C     numberOfBasins - Number of basins actually defined.
C     mldIndex       - Level number of last cell within the mixed
C                      layer.
      COMMON /DIAGS_I/
     &                numberOfBasins, mldIndex
      INTEGER numberOfBasins
      INTEGER mldIndex(Nx,Ny)
C     Character parameters
C     basinList      - Name associated with each basin.
      COMMON /DIAGS_C/
     &                basinList
      CHARACTER*10    basinList(DIAGS_MAX_NUMBER_OF_BASINS)
