C $Id: initialise.F,v 1.3 1998/06/12 19:33:31 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
#define SPHERICAL
#define _DB0 0.D0
C ================
C | initialise.F |
C ================
C Contents
C o INITIALISE                 Controlling procedure
C o INITALISE_CARTESIAN_GRID   Initialise geometric terms in cartesian form
C o INITIALISE_CORIOLIS        Initialise coriolis parameter
C o INITIALISE_DIAGS           Initialise diagnostics.          
C o INITIALISE_EOS             Initialise equation of state operators
C o INITIALISE_FIELDS          Set initial values of model fields
C o INITIALISE_GRIDDING        Control setting up of model grid
C o INITIALISE_MASKS           Set land-water masks
C o INITIALISE_METRIC          Set curvilinear grid metric term operators
C o INITIALISE_OPERATORS       Set finite difference operators
C o INITIALISE_S               Set initial salinity field
C o INITIALISE_SPHERICAL_POLAR Initialise geometric terms in spherical polar form
C o INITIALISE_T               Set inital temperature field
C o INITIALISE_U               Set inital U velocity field
C o INITIALISE_V               Set inital V velocity field

C================================================================================
C     Procedure name: INITIALISE                                                |
C           Function: Controls initialisation of model fields and discrete      |
C                     operators.                                                |
C           Comments:                                                           |
C================================================================================
CStartofinterface
      SUBROUTINE INITIALISE (
     O                         U, V, W, T, S, PH, PS )
      IMPLICIT NONE
C     /-------------------------------------------------------------------------\
C     | Global variable declarations                                            |
C     \-------------------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "STRINGS.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "OLDG.h"
#include "MASKS.h"
#include "CG2DA.h"
#include "CG2DZ.h"
#include "AJAINF.h"
#include "FORCING.h"
C     /-------------------------------------------------------------------------\
C     |  Routine argument declarations                                          |
C     |=========================================================================|
C     | U, V, W - X,Y,Z Velocity ( m/s, m/s, Pa/s ).                            |
C     | T       - Potential temperature (oC).                                   |
C     | S       - Salinity (ppt).                                               |
C     | PH      - Hydrostatic pressure perturbation (m).                        |
C     | PS      - Surface pressure (m).                                         |
C     \-------------------------------------------------------------------------/
      REAL    U (_I3(nz,nx,ny))
      REAL    V (_I3(nz,nx,ny))
      REAL    W (_I3(nz,nx,ny))
      REAL    T (_I3(nz,nx,ny))
      REAL    S (_I3(nz,nx,ny))
      REAL    PH(_I3(nz,nx,ny))
      REAL    PS(nx, ny       )
CEndofinteface
      CALL INITIALISE_SPHERICAL_POLAR_GRID
      CALL INITIALISE_CORIOLIS
      CALL INITIALISE_FIELDS ( U, V, W, T, S, PH, PS )
C     CALL INITIALISE_EOS
      CALL INITIALISE_DIAGS
C 
      RETURN
      END

C================================================================================
C     Procedure name: INITIALISE_SPHERICAL_POLAR_GRID                           |
C           Function: Iniitialise volumes and areas for a spherical polar       |
C                     latitude-longitude grid. Basin geometry is specified in   |
C                     terms of a southern boundary latitude (at V(j=1) ) and    |
C                     latitudinal and longitudinal grid spacing in degrees.     |
C           Comments:                                                           |
C================================================================================
      SUBROUTINE INITIALISE_SPHERICAL_POLAR_GRID
      IMPLICIT NONE
C     /-------------------------------------------------------------------------\
C     | Global declarations                                                     |
C     \-------------------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "STRINGS.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "CG2DA.h"
#include "CG2DZ.h"
#include "MASKS.h"
#include "FORCING.h"
C     /-------------------------------------------------------------------------\
C     | Local declarations                                                      |
C     |=========================================================================|
C     | sbLat   - Latitude (in degress ) of southern boundary                   |
C     |           -90.0 = South Pole                                            |
C     |             0.0 = Equator                                               |
C     |           +90.0 = North Pole                                            |
C     | delX    - X grid spacing in degress longitude                           |
C     | delY    - Y grid spacing in degress latitude                            |
C     | latSouth- Holds active latitude (degrees)                               |
C     | latNorth- Holds active latitude (degrees)                               |
C     | longWest- Holds active longitude (degrees)                              |
C     | longEast- Holds active longitude (degrees)                              |
C     | I,J,K   - Loop counters                                                 |
C     \-------------------------------------------------------------------------/
      REAL sbLat
      REAL delX
      REAL delY
      REAL latSouth, latNorth
      REAL longWest, longEast
      INTEGER I, J, K
      REAL dist
      REAL tmpVal
      REAL tmpMsk( _I3(nz,0:nx+1,0:ny+1) )
      REAL aC( 1:nx,1:ny )
      LOGICAL landHere, waterHere
      LOGICAL landSouth, waterSouth
      LOGICAL landWest, waterWest
      LOGICAL waterAbove, landAbove
      LOGICAL TOP_LAYER, BOTTOM_LAYER
C
      delX  =   dTheta
      delY  =   dPhi   
      sbLat =   phiMin
C
C
      rZero    = radiusEarth
#ifdef SPHERICAL
      rPVolHat = 0.
      DO k=1, nz
       latNorth = sbLat
       DO j =1, ny
        latSouth = latNorth
        latNorth = latNorth+delY
        longEast = 0.
        DO i =1, nx
         longWest = longEast
         longEast = longWest+delX
         XA( _I3(k,i,j) )   = rZero*delY*deg2rad*delps(k)
         YA( _I3(k,i,j) )   = rZero*delX*deg2rad*COS(latSouth*deg2rad)*delps(k)
         ZA( _I3(k,i,j) )   = rZero*delX*deg2rad
     &                        *(SIN(latNorth*deg2rad)-SIN(latSouth*deg2rad))
     &                        *rZero
         rPVol( _I3(k,i,j) )= 1.D0/( ZA( _I3(k,i,j) )*delps(k) )
         rPVolHat(i,j)      = rPVolHat(i,j) +  ZA( _I3(k,i,j) )*delps(k)
         dist = rZero*delX*deg2rad*COS( 0.5D0*deg2rad*(latNorth+latSouth) )
         uDdxOp( _I3(k,i,j) ) = 1.D0/( dist )
         pDdxOp( _I3(k,i,j) ) = 1.D0/( dist )
         dist = rZero*delX*deg2rad*COS( deg2rad*latSouth )
         vDdxOp( _I3(k,i,j) ) = 1.D0/( dist )
         dist = rZero*delY*deg2rad
         uDdyOp( _I3(k,i,j) ) = 1.D0/( dist )
         vDdyOp( _I3(k,i,j) ) = 1.D0/( dist )
         pDdyOp( _I3(k,i,j) ) = 1.D0/( dist )
C        fCorV( _I3(k,i,j) )  = 2.D0*omega*SIN(latSouth*deg2rad)
         fCorU( _I3(k,i,j) )  = 2.D0*omega*SIN( 0.5D0*deg2rad*(latNorth+latSouth) )
         uTPhiOp( _I3(k,i,j) ) = TAN(0.5D0*deg2rad*(latNorth+latSouth) )
         vTPhiOp( _I3(k,i,j) ) = TAN(deg2rad*(latSouth) )
         uTPhiOp( _I3(k,i,j) ) = 0.
         vTPhiOp( _I3(k,i,j) ) = 0.
        ENDDO
       ENDDO
      ENDDO
      fCorV = fCorV * _DB0
      fCorV = 0.5D0*(fCorU+CSHIFT(fCorU,-1,2) )
C     fCorU = 0.5*(fCorV+CSHIFT(fCorV,+1,2) )
      fCorW = fCorU

C     fCorU = 1.d-4
C     fCorV = 1.d-4

      WRITE(0,*) ' fcoru '
      CALL PLOT_FIELD(fCorU(_I3(1,1:nx,1:ny)), Nx, Ny)
 
      DO j=1, ny
       DO i=1, nx
        rPVolHat(i,j) = 1.D0/rPVolHat(i,j)
       ENDDO
      ENDDO
#endif
      rDxatU = pDdxOp(_I3(1,:,:))
      rDyatV = pDdyOp(_I3(1,:,:))
C
      DO k=1, nz
       DO j=1, ny
        rUVol( _I3(k,1,j) ) = ZA( _I3(k,1  ,j) )*delps(k)
     &                       +ZA( _I3(k,nx ,j) )*delps(k)
        rUVol( _I3(k,1,j) ) = 2.D0/rUVol( _I3(k,1,j) )
        DO i=2, nx
         rUVol( _I3(k,i,j) ) = ZA( _I3(k,i  ,j) )*delps(k)
     &                        +ZA( _I3(k,i-1,j) )*delps(k)
         rUVol( _I3(k,i,j) ) = 2.D0/rUVol( _I3(k,i,j) )
        ENDDO
       ENDDO
      ENDDO
C
      DO k=1, nz
       DO j=2, ny
        DO i=1, nx
         rVVol( _I3(k,i,j) ) = ZA( _I3(k,i,j  ) )*delps(k)
     &                        +ZA( _I3(k,i,j-1) )*delps(k)
         rVVol( _I3(k,i,j) ) = 2.D0/rVVol( _I3(k,i,j) )
        ENDDO
       ENDDO
      ENDDO
      DO k=1, nz
       DO i=1, nx
        rVVol( _I3(k,i,1) ) = ZA( _I3(k,i,1  ) )*delps(k)
     &                       +ZA( _I3(k,i,ny ) )*delps(k)
        rVVol( _I3(k,i,1) ) = 2.D0/rVVol( _I3(k,i,1) )
       ENDDO
      ENDDO
C
      UMASK = WATER
      VMASK = WATER
      tmpMsk( _I3(:,1:Nx,1:Ny) ) = PMASK
      tmpMsk( _I3(:,0   ,1:Ny) ) = tmpMsk( _I3(:,Nx,1:Ny) )
      tmpMsk( _I3(:,Nx+1,1:Ny) ) = tmpMsk( _I3(:, 1,1:Ny) )
      tmpMsk( _I3(:,   :,0   ) ) = tmpMsk( _I3(:, :,Ny  ) )
      tmpMsk( _I3(:,   :,Ny+1) ) = tmpMsk( _I3(:, :,1   ) )
      DO k=1,nz
       DO j=1,ny
        DO i=1,nx
C        du/dy boundary conditions.
         WaterHere  = tmpMsk( _I3(k,i  ,j) )*tmpMsk( _I3(k,i-1,j) )     .GT. 0.5
         LandHere   = .NOT. WaterHere 
         WaterSouth = tmpMsk( _I3(k,i  ,j-1) )*tmpMsk( _I3(k,i-1,j-1) ) .GT. 0.5
         LandSouth  = .NOT. WaterSouth
         IF ( LandHere ) UMASK( _I3(k,i,j) ) = LAND
         IF     ( WaterHere .AND. LandSouth  ) THEN
C         uDdYop( _I3(k,i,j) ) = 0.                       ! Free slip southern boundaries
          uDdYop( _I3(k,i,j) ) = +2.*uDdYop( _I3(k,i,j) ) ! No slip southern boundaries
         ELSEIF ( LandHere  .AND. WaterSouth ) THEN
C         uDdYop( _I3(k,i,j) ) = 0.                       ! Free slip northern boundaries
          uDdYop( _I3(k,i,j) ) = +2.*uDdYop( _I3(k,i,j) ) ! No slip northern boundaries
         ELSEIF ( LandHere  .AND. LandSouth  ) THEN
          uDdYop( _I3(k,i,j) ) = 0.            
         ENDIF
C        dv/dx boundary conditions.
         WaterHere  = tmpMsk( _I3(k,i,j  ) )*tmpMsk( _I3(k,i,j-1) )     .GT. 0.5
         LandHere   = .NOT. WaterHere 
         WaterWest  = tmpMsk( _I3(k,i-1,j  ) )*tmpMsk( _I3(k,i-1,j-1) ) .GT. 0.5
         LandWest   = .NOT. WaterWest   
         IF ( LandHere ) VMASK( _I3(k,i,j) ) = LAND
         IF     ( WaterHere .AND. LandWest   ) THEN
C         vDdXop( _I3(k,i,j) ) = 0.                       ! Free slip western boundaries
          vDdXop( _I3(k,i,j) ) = +2.*vDdXop( _I3(k,i,j) ) ! No slip western boundaries
         ELSEIF ( LandHere  .AND. WaterWest  ) THEN
C         vDdXop( _I3(k,i,j) ) = 0.                       ! Free slip eastern boundaries
          vDdXop( _I3(k,i,j) ) = +2.*vDdXop( _I3(k,i,j) ) ! No slip eastern boundaries
         ELSEIF ( LandHere  .AND. LandWest  ) THEN
          vDdXop( _I3(k,i,j) ) = 0.            
         ENDIF
C        dp/dx and dp/dy boundary conditions
         WaterHere  = tmpMsk( _I3(k,i  ,j  ) ) .GT. 0.5
         LandHere   = .NOT. WaterHere
         WaterSouth = tmpMsk( _I3(k,i  ,j-1) ) .GT. 0.5
         LandSouth  = .NOT. WaterSouth
         WaterWest  = tmpMsk( _I3(k,i-1,j  ) ) .GT. 0.5
         LandWest   = .NOT. WaterWest
         IF   ( LandHere ) THEN
          pDdxOp( _I3(k,i,j) ) = 0.
          pDdyOp( _I3(k,i,j) ) = 0.
         ELSE
          IF ( LandWest  ) THEN
           pDdxOp( _I3(k,i,j) ) = 0.
          ENDIF
          IF ( LandSouth ) THEN
           pDdyOp( _I3(k,i,j) ) = 0.
          ENDIF
         ENDIF
        ENDDO
       ENDDO
C      du/dz, dv/dz and dp/dz boundary conditions.
       TOP_LAYER    = K .EQ. 1
       BOTTOM_LAYER = K .EQ. Nz
C      Can skip first and last layers. These are handled in-line
C      by special case code. The in-line TOP_LAYER, BOTTOM_LAYER
C      code needs to be altered if the boundary conditions are altered.
C      Only the land below case is coded up. This assumes that there 
C      are no "overhangs" in the topography.
       IF ( .NOT. TOP_LAYER ) THEN
        DO j=1,ny
         DO i=1,nx
C         du/dz = 0.
          LandHere   = tmpMsk( _I3(k  ,i,j) )*tmpMsk( _I3(k  ,i-1,j) ) .LT. 0.5
          WaterAbove = tmpMsk( _I3(k-1,i,j) )*tmpMsk( _I3(k-1,i-1,j) ) .GT. 0.5
          IF ( LandHere .AND. WaterAbove ) THEN
           uDdzOp( _I3(k,i,j) ) = 0.              ! Free Slip
           uDdzOp( _I3(k,i,j) ) = rDzAtW(K)*2.D0  ! No Slip
          ENDIF
C         dv/dz = 0.
          LandHere   = tmpMsk( _I3(k  ,i,j) )*tmpMsk( _I3(k  ,i,j-1) ) .LT. 0.5
          WaterAbove = tmpMsk( _I3(k-1,i,j) )*tmpMsk( _I3(k-1,i,j-1) ) .GT. 0.5
          IF ( LandHere .AND. WaterAbove ) THEN
           vDdzOp( _I3(k,i,j) ) = 0.              ! Free Slip
           vDdzOp( _I3(k,i,j) ) = rDzAtW(K)*2.D0  ! No Slip
          ENDIF
C         dt/dz = ds/dz = 0.
          LandHere   = tmpMsk( _I3(k  ,i,j) ) .LT. 0.5
          WaterHere  = tmpMsk( _I3(k  ,i,j) ) .GT. 0.5
          WaterAbove = tmpMsk( _I3(k-1,i,j) ) .GT. 0.5
          LandAbove  = tmpMsk( _I3(k-1,i,j) ) .LT. 0.5
          IF ( LandHere .AND. WaterAbove ) THEN
           pDdzOp( _I3(k,i,j) ) = 0.    ! Insulating
          ENDIF
C         Error overhanging shelf.
          IF ( WaterHere .AND. LandAbove ) THEN
           WRITE(scrUnitError,*) 'Land above water at I,J,K = ',I,J,K
           initialisationError = .TRUE.
          ENDIF
         ENDDO
        ENDDO
        DO j=1,ny
         DO i=1,nx
          wMask(_I3(K,i,j)) = pMask(_I3(K,i,j))*pMask(_I3(K-1,i,j))
         ENDDO
        ENDDO
       ENDIF
      ENDDO
      wMask(_I3(1,:,:)) = 0.
C
      DO k=1, nz
       DO j=1, ny
        DO i=1, nx
         rUVol( _I3(k,i,j) ) = rUVol( _I3(k,i,j) )*UMASK( _I3(k,i,j) )
         rVVol( _I3(k,i,j) ) = rVVol( _I3(k,i,j) )*VMASK( _I3(k,i,j) )
         rPVol( _I3(k,i,j) ) = rPVol( _I3(k,i,j) )*PMASK( _I3(k,i,j) )
        ENDDO
       ENDDO
      ENDDO
      XA = XA*UMASK
      YA = YA*VMASK
      ZA = ZA*PMASK
      rPVolHat = rPVolHat*PMASK(_I3(1,:,:))
C
C     Elliptic equation solver variables.
      pC            = 1.
      pX            = 0.
      pY            = 0.
      aX            = 0.
      aY            = 0.
      DO K = 1, nz
       aX(1:nx,1:ny) = aX(1:nx,1:ny)+XA(_I3(K,:,:))*pDdxOp(_I3(k,:,:))/RONIL
       aY(1:nx,1:ny) = aY(1:nx,1:ny)+YA(_I3(K,:,:))*pDdyOp(_I3(k,:,:))/RONIL
      ENDDO
      aX(0   ,1:ny) = aX(nx,1:ny)
      aX(nx+1,1:ny) = aX( 1,1:ny)
      aY(1:nx,0   ) = aY(1:nx,ny+1)
      aY(1:nx,ny+1) = aY(1:nx,   1)
      aNorm2d = MAXVAL(ABS(aX))
      tmpVal  = MAXVAL(ABS(aY))
      aNorm2d = MAX(aNorm2d,tmpVal)
      DO J=1,ny
       DO I=1,nx
        pC(I,J) = -aX(I,J)-aX(I+1,J)
     &            -aY(I,J)-aY(I,J+1)
     &            -freeSurfFac*ZA(_I3(1,I,J))/DELT/DELT
        aC(I,J) = pC(I,J)
        IF ( pC(I,J) .NE. 0. ) pC(I,J) =  1.0/pC(I,J)
CcnhDebugStarts
C       IF ( pC(I,J) .NE. 0. ) pC(I,J) = 1.D0
CcnhDebugEnds
       ENDDO
      ENDDO
      aX = aX/aNorm2d
      aY = aY/aNorm2d
      aC = aC/aNorm2d
      WHERE ( aC + CSHIFT(aC,-1,1) .NE. 0. )
       pX(1:Nx,1:Ny) = -aX(1:Nx,1:Ny)/( (0.51*(aC + CSHIFT(aC,-1,1)))**2 )
      ENDWHERE
      WHERE ( aC + CSHIFT(aC,-1,2) .NE. 0. )
       pY(1:Nx,1:Ny) = -aY(1:Nx,1:Ny)/( (0.51*(aC + CSHIFT(aC,-1,2)))**2 )
      ENDWHERE
CcnhDebugStarts
      pC = pC*aNorm2d
CcnhDebugEnds
C
CcnhDebugStarts
      WRITE(0,*) ' aX = '
      CALL PLOT_FIELD(aX(1:Nx,1:Ny), Nx, Ny)
      WRITE(0,*) ' aY = '
      CALL PLOT_FIELD(aY(1:Nx,1:Ny), Nx, Ny)
CcnhDebugEnds

      RETURN
      END
      SUBROUTINE INITIALISE_CORIOLIS
      IMPLICIT NONE
      RETURN
      END
C/-------------------------------------------------------------------\
C|||  Procedure: INITIALISE_DIAGS                                  |||
C|||===============================================================|||
C|||        Function: Set up quantities that will be diagnosed.    |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE INITIALISE_DIAGS
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "DIAGS.h"
CEndofinterface
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | ** NONE ***                                                  |
C     \--------------------------------------------------------------/
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | I - Loop counter                                             |
C     \--------------------------------------------------------------/
      INTEGER iErr
C     /--------------------------------------------------------------\
C     | Set basins.                                                  |
C     \--------------------------------------------------------------/
C     iErr = 0
C     CALL DIAGS_ADD_BASIN('GLB',iErr)
C     IF ( iErr .NE. 0 ) THEN
C      WRITE(scrUnitError,*) 
C    &  '*** Error: Basin GLB was not defined correctly.'
C      initialisationError = .TRUE.
C     ENDIF
C     CALL DIAGS_ADD_BASIN('NA' ,iErr)
C     IF ( iErr .NE. 0 ) THEN
C      WRITE(scrUnitError,*) 
C    &  '*** Error: Basin NA was not defined correctly.'
C      initialisationError = .TRUE.
C     ENDIF
C     CALL DIAGS_ADD_BASIN('IO' ,iErr)
C     IF ( iErr .NE. 0 ) THEN
C      WRITE(scrUnitError,*) 
C    &  '*** Error: Basin IO was not defined correctly.'
C      initialisationError = .TRUE.
C     ENDIF

      CALL DIAGS_SHOW

      RETURN
      END

      SUBROUTINE INITIALISE_FIELDS(
     U                             U, V, W, T, S, PH, PS )
      IMPLICIT NONE
C     /-------------------------------------------------------------------------\
C     | Global variable declarations                                            |
C     \-------------------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "STRINGS.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "OLDG.h"
#include "MASKS.h"
#include "CG2DA.h"
#include "CG2DZ.h"
#include "AJAINF.h"
#include "FORCING.h"
C     /-------------------------------------------------------------------------\
C     |  Routine argument declarations                                          |
C     |=========================================================================|
C     | U, V, W - X,Y,Z Velocity ( m/s, m/s, Pa/s ).                            |
C     | T       - Potential temperature (oC).                                   |
C     | S       - Salinity (ppt).                                               |
C     | PH      - Hydrostatic pressure perturbation (m).                        |
C     | PS      - Surface pressure (m).                                         |
C     \-------------------------------------------------------------------------/
      REAL    U (_I3(nz,nx,ny))
      REAL    V (_I3(nz,nx,ny))
      REAL    W (_I3(nz,nx,ny))
      REAL    T (_I3(nz,nx,ny))
      REAL    S (_I3(nz,nx,ny))
      REAL    PH(_I3(nz,nx,ny))
      REAL    PS(nx, ny       )
CEndofinteface
C     /-------------------------------------------------------------------------\
C     | Local variables                                                         |
C     |=========================================================================|
C     | tmp  Single precision I/O buffer                                        |
C     | iErr Error flag                                                         |
C     | K    Loop counter                                                       |
C     \-------------------------------------------------------------------------/
      Real*4  tmp(Nx,Ny,Nz)
      INTEGER iErr
      INTEGER K
      IF   ( PickupRun  ) THEN
C      /------------------------------------------------------------------------\
C      | Run starts from previously saved model state.                          |
C      |========================================================================|
C      | This block handles "pickup" runs that are used to continue integrations|
C      | over a series of computational jobs.                                   |
C      \------------------------------------------------------------------------/
       iErr = 0
CcnhDEbugStarts
       WRITE(0,*) ' Calling READ_DUMP'
CcnhDebugEnds
       CALL READ_DUMP_3D(U    ,Nx,Ny,Nz,'U.'    ,puSuffix,iErr)
CcnhDebugStarts
       WRITE(0,*) MAXVAL(U)
       WRITE(0,*) MINVAL(U)
CcnhDEbugEnds
       CALL READ_DUMP_3D(V    ,Nx,Ny,Nz,'V.'    ,puSuffix,iErr)
       CALL READ_DUMP_3D(uAja ,Nx,Ny,Nz,'UAJA.' ,puSuffix,iErr)
       CALL READ_DUMP_3D(vAja ,Nx,Ny,Nz,'VAJA.' ,puSuffix,iErr)
       CALL READ_DUMP_3D(W    ,Nx,Ny,Nz,'W.'    ,puSuffix,iErr)
CcnhDebugStarts
       CALL READ_DUMP_3D(T    ,Nx,Ny,Nz,'T.'    ,puSuffix,iErr)
       CALL READ_DUMP_3D(S    ,Nx,Ny,Nz,'S.'    ,puSuffix,iErr)
CcnhDebugEnds
       CALL READ_DUMP_2D(PS   ,Nx,Ny,   'PS.'   ,puSuffix,iErr)
       CALL READ_DUMP_3D(PH   ,Nx,Ny,Nz,'PH.'   ,puSuffix,iErr)
       CALL READ_DUMP_3D(UNM1 ,Nx,Ny,Nz,'UNM1.' ,puSuffix,iErr)
       CALL READ_DUMP_3D(VNM1 ,Nx,Ny,Nz,'VNM1.' ,puSuffix,iErr)
CcnhDebugStarts
C      Added cos. old code contains uBxBYNM1 etc which is not 
C      saved here.
       UNM1 = 0.
       VNM1 = 0.
CcnhDebugEnds
       CALL READ_DUMP_2D(PSNM1,Nx,Ny,   'PSNM1.',puSuffix,iErr)
       CALL READ_DUMP_3D(PHNM1,Nx,Ny,Nz,'PHNM1.',puSuffix,iErr)
       CALL READ_DUMP_3D(GUNM1,Nx,Ny,Nz,'GUNM1.',puSuffix,iErr)
       CALL READ_DUMP_3D(GVNM1,Nx,Ny,Nz,'GVNM1.',puSuffix,iErr)
CcnhDebugStarts
       CALL READ_DUMP_3D(GTNM1,Nx,Ny,Nz,'GTNM1.',puSuffix,iErr)
       CALL READ_DUMP_3D(GSNM1,Nx,Ny,Nz,'GSNM1.',puSuffix,iErr)
CcnhDebugEnds
       IF ( iErr .NE. 0 ) THEN
C       /----------------------------------------------------------------------\
C       | Stop if there was error loading pickup data.                         |
C       \----------------------------------------------------------------------/
        CALL PRINT_ERRORS
        initialisationError = .TRUE.
       ENDIF
      ELSE
C      /------------------------------------------------------------------------\
C      | Run starts from some externally generated state.                       |
C      |========================================================================|
C      | This block handles "startup" runs that typically start from t=0 and    |
C      | initialise the model fields from a climatology or with some predefined |
C      | stratification and/or velcoity profile.                                |
C      \------------------------------------------------------------------------/
C      /------------------------------------------------------------------------\
C      | Initialise velocity field.                                             |
C      |========================================================================|
C      | Initial conditions set velocity field to zero.                         |
C      | If non-zero velocity profile is used to initialise the model it needs  |
C      | to satisfy continuity to machine accuracy.                             |
C      \------------------------------------------------------------------------/
       U = 0.
       V = 0.
       W = 0.
C      /------------------------------------------------------------------------\
C      | Load potential temperaturea and salinity.                              |
C      |========================================================================|
C      | This example reads data derived from Levitus climatological average.   |
C      | Temperature is in degrees centigrade referenced to the surface.        |
C      | Salinity is in ppt.                                                    |
C      \------------------------------------------------------------------------/
C      /------------------------------------------------------------------------\
C      | Temperature                                                            |
C      \------------------------------------------------------------------------/
       OPEN ( 11, FILE='LevCli.ptmp.sun.b', STATUS='old' , FORM='unformatted')
       READ (11) tmp
       CLOSE(11)
       do K = 1, Nz
        T( _I3(K,:,:) ) = tmp(:,:,K)
       enddo
       T = T*pMask
       WRITE(0,*) 'T LOADED FROM FILE'
C      /-----------------------------------------------------------------------\
C      | Print character contour map of surface and deep temperature field.    |
C      \-----------------------------------------------------------------------/
       CALL PLOT_FIELD(T(_I3(1,:,: )), Nx, Ny)
       CALL PLOT_FIELD(T(_I3(20,:,:)), Nx, Ny)
C      STOP
C      /-----------------------------------------------------------------------\
C      | Salinity                                                              |
C      \-----------------------------------------------------------------------/
       OPEN ( 11, FILE='LevCli.salt.sun.b', STATUS='old' , FORM='unformatted')
       READ (11) tmp
       CLOSE(11)
       do K = 1, Nz
        S( _I3(K,:,:) ) = tmp(:,:,K)
       enddo
       S = S*pMask
C      /-----------------------------------------------------------------------\
C      | Print character contour map of surface and deep salinity field.       |
C      \-----------------------------------------------------------------------/
C      CALL PLOT_FIELD(S(_I3(1,:,: )), Nx, Ny)
C      CALL PLOT_FIELD(S(_I3(20,:,:)), Nx, Ny)
      ENDIF
C
      RETURN
      END
      SUBROUTINE INITIALISE_EOS
      IMPLICIT NONE
#include "SIZE.h"
#include "PARAMS.h"
#include "POLYEOS.h"
C     sD - Salinity anomaly from reference
C     tD - Temperature anomaly from reference
      REAL sD
      REAL tD
C     I,J,K - Loop counters
      INTEGER I, J, K
C       Set polynomial parameters
        OPEN(  dUnit, FILE='polyeos_coeffs', STATUS='OLD' )
C       READ ( dUnit, * ) tRef
C       READ ( dUnit, * ) sRef
C       READ ( dUnit, * ) C
C       CLOSE( dUnit )
        DO K = 1, Nz+1
         READ(dUnit,*) tRef(K)
        ENDDO
        DO K = 1, Nz+1
         READ(dUnit,*) sRef(K)
        ENDDO
        DO K = 1, Nz+1
        DO I = 1, 9
         READ(dUnit,*) C(K,I)
        ENDDO
        ENDDO
        DO K = 1, Nz+1
         READ(dUnit,*) sig0(K)
        ENDDO
        CLOSE(dUnit)
C       Evaluate perturbation for T and S background state.
        DO 10 K = 1, Nz
          tD =  ths(K)-tRef(K)
          sD = (ssppt(K)-35.)/1000.-sRef(K)
          rprmInit(K) = 1000.*(
     &     C(K,1)*tD   +C(K,2)*sD
     &    +C(K,3)*tD**2+C(K,4)*tD*sD   +C(K,5)*sD**2
     &    +C(K,6)*tD**3+C(K,7)*tD*sD**2+C(K,8)*sD*tD**2+C(K,9)*sD**3 )
   10   CONTINUE

      RETURN
      END
