C $Header: /u/gcmpack/MITgcm/verification/vero/code/Attic/FFIELDS.h,v 1.1 2001/03/25 22:33:55 heimbach Exp $
C
C     /==========================================================\
C     | FFIELDS.h                                                |
C     | o Model forcing fields                                   |
C     |==========================================================|
C     | The arrays here will need changing and customising for a |
C     | particular experiment.                                   |
C     \==========================================================/
C
C--   For a classical "gyre" type experiment just one term is needed.
C
C     fu     - Zonal surface wind stress
C                Units are           N/m^2 (>0 from East to West)
C
C     fv     - Meridional surface wind stress
C                Units are           N/m^2 (>0 from North to South))
C
C     EmPmR  - Evaporation - Precipitation - Runoff
C                Units are           m/s (>0 for ocean salting)
C
C     Qnet   - Upward surface heat flux
C                Units are           W/m^2=kg/s^3 (>0 for ocean cooling)
C
C     Qsw    - Upward short-wave surface heat flux
C                Units are          W/m^2=kg/s^3 (>0 for ocean cooling)
C
C     dQdT   - Thermal relaxation coefficient 
C                                 (W/m^2/degrees -> degrees/second)
C     SST    - Sea surface temperature (degrees) for relaxation
C     SSS    - Sea surface salinity (psu) for relaxation

      COMMON /FFIELDS/
     &                 fu,
     &                 fv,
     &                 Qnet,
     &                 EmPmR,
     &                 SST,
     &                 SSS,
     &                 Qsw
      _RS  fu       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  fv       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qnet     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  EmPmR    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qsw      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  dQdT     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SST      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     surfaceTendencyU       (units are  m/s^2)
C                -> usage in gU:     gU = gU + surfaceTendencyU[m/s^2]
C
C     surfaceTendencyV       (units are  m/s^2)
C                -> usage in gV:     gV = gV + surfaceTendencyV[m/s^2]
C
C     surfaceTendencyS       (units are  psu/s)
C            - EmPmR plus salinity relaxation term
C                -> calculate        -lambda*(S(model)-S(clim))
C                -> usage in gS:     gS = gS + surfaceTendencyS[psu/s]
C
C     surfaceTendencyT       (units are  degrees/s)
C            - Qnet plus temp. relaxation
C                -> calculate        -lambda*(T(model)-T(clim))
C            >>> Qnet assumed to be total flux minus s/w rad. <<<
C                -> usage in gT:     gT = gT + surfaceTendencyT[K/s]
C
      COMMON /TENDENCY_FORCING/
     &                         surfaceTendencyU,
     &                         surfaceTendencyV,
     &                         surfaceTendencyT,
     &                         surfaceTendencyS, 
     &                         tempQsw
      _RS  surfaceTendencyU  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  surfaceTendencyV  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  surfaceTendencyT  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  surfaceTendencyS  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  tempQsw           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_COST_TEST

C   there are eight different forcing fields
C   each has a specific index

      integer    maxforcing
      parameter( maxforcing = 8 )

      integer    fuindex      , fvindex
      parameter( fuindex   = 1, fvindex     = 2 )
      integer    SSTindex     , SSSindex
      parameter( SSTindex  = 3, SSSindex    = 4 )
      integer    Qnetindex    , EmPmRindex
      parameter( Qnetindex = 5, EmPmRindex  = 6 )
      integer    dQdTindex    , Qswindex
      parameter( dQdTindex = 7, Qswindex    = 8 )

C   The follwing values define the forcing file content.
C   Each file can have a different frequency of forcing
C   and they can contain more records than necessary.
C   But at least as much as required by startTime and endTime of model
C   must be in the file.
C
C   fstarttime     : time of first record on file
C   fendtime       : time of lasst record on file
C   finterval      : time interval between two records
C   factrec        : record number of forcing field currently in dot file

      _RS     fstarttime(maxforcing)
      _RS     finterval (maxforcing)
      _RS     fendtime  (maxforcing)

      integer factrec   (2,maxforcing)
      INTEGER fuHandle
      INTEGER fvHandle
      INTEGER SSTHandle
      INTEGER SSSHandle
      INTEGER QnetHandle
      INTEGER EmPmRHandle
      INTEGER dQdTHandle
      INTEGER QswHandle

      common /forcing_rs/ fstarttime, finterval, fendtime

      common /forcing_in/ factrec
     &                  , fuHandle, fvHandle
     &                  , SSTHandle, SSSHandle
     &                  , QnetHandle,EmPmRHandle
     &                  , dQdTHandle, QswHandle 

#endif
