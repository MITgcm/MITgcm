C $Header: /u/gcmpack/MITgcm/pkg/sbo/sbo_calc.F,v 1.17 2014/05/30 17:10:15 jmc Exp $
C $Name:  $

#include "SBO_OPTIONS.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif

CBOP
C !ROUTINE: SBO_CALC

C !INTERFACE: ==========================================================
      SUBROUTINE SBO_CALC( myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SBO_CALC                                      |
C     | o Do SBO diagnostic output.                              |
C     *==========================================================*
C     | NOTE: The following subtleties are ignored for time      |
C     | being but may need revisiting at some point in time.     |
C     | 1) The model is volume-preserving and Boussinesq so      |
C     |    quantities like oceanic mass need to be interpreted   |
C     |    with some care.  We remove spurious mass variations   |
C     |    using the Greatbatch correction. Real freshwater      |
C     |    fluxes retained in mass load.                         |
C     | 2) The sea surface height variable etaN might lag other  |
C     |    prognostic variables by half a time step.  This lag   |
C     |    is ignored in SBO computations.                       |
C     | 3) OAM due to currents assumes constant density          |
C     |    (=rhoConst), rms differences using variable density   |
C     |    is less than 1%, assuming rhoConst is a good measure  |
C     |    of the actual mean density                            |
C     | 4) Seaice motion added to OAMC.  Seaice mass is in OAMP  |
C     |    and COM.   Net freshwater flux is between atmosphere  |
C     |    and liquid ocean plus seaice.  I.e. changes in seaice |
C     |    mass due to melt/freeze with liquid ocean do not      |
C     |    change net freshwater flux.                           |
C     *==========================================================*

C=======================================================================
C
C     Based on ftp://euler.jpl.nasa.gov/sbo/software/calc_sbo2.f
C     Written  by Richard Gross (Richard.Gross@jpl.nasa.gov)
C     Reference
C           Gross, R. S., F. O. Bryan, Y. Chao, J. O. Dickey, S. L. Marcus,
C           R. M. Ponte, and R. Tokmakian, The IERS Special Bureau for the
C           Oceans, in IERS Technical Note on the IERS Global Geophysical
C           Fluids Center, edited by B. Chao, in press, Observatoire de Paris,
C           Paris, France, 2002.
C
C     June 10, 2001: Modified for online computations in MIT GCM UV
C              by Dimitris Menemenlis (Menemenlis@jpl.nasa.gov)
C     Jan 7, 2014: Modified for real freshwater flux and coordinates other
C              than spherical polar by Katy Quinn (kquinn@aer.com)
C
C     Purpose
C           calc_sbo calculates the core products of the IERS Special Bureau
C           for the Oceans including oceanic mass, center-of-mass, and angular
C           momentum.
C
C=======================================================================
C \ev

C !USES: ===============================================================
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SBO.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     == Routine arguments ==
C     myTime  :: Current time of simulation ( s )
C     myIter  :: Iteration number
C     myThid  :: Number of this instance of SBO_CALC
      _RL     myTime
      INTEGER myIter, myThid

#ifdef ALLOW_SBO

C !LOCAL VARIABLES: ====================================================
C     bi, bj    :: array indices
C     i         :: index over longitude grid points
C     j         :: index over latitude  grid points
C     k         :: index over layers
C     lat       :: latitude  of grid point (radians)
C     lon       :: longitude of grid point (radians)
C     darea     :: element of surface area (m**2)
C     dvolume   :: element of volume (m**3)
C     ae        :: mean radius of Earth (m) (PREM value)
C     sbo_omega :: mean angular velocity of Earth (rad/s)
C     UE,VN     :: geographic (east,north) ocean velocities at cell centers (m/s)
C     UEice,VNice  :: geographic (east,north) seaice velocities at cell centers (m/s)
C     Mload     :: total mass load (kg/m**2)
C     GCload    :: mass load for Greatbatch correction (kg/m**2)
C     FWLoad    :: real freshwater flux mass load (kg/m**2)
      integer bi, bj, i, j, k
      _RL lat, lon, darea, dvolume
      _RL ae, sbo_omega
      PARAMETER ( ae        = 6.3710 _d 6    )
      PARAMETER ( sbo_omega = 7.292115 _d -5 )
      _RL UE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL VN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL UEice(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VNice(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Mload(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL GCload, FWload
C Tiled global sums
      _RL tile_FWload(nSx,nSy)
      _RL tile_sboarea(nSx,nSy)
      _RL tile_GCload(nSx,nSy)
      _RL tile_mass(nSx,nSy)
      _RL tile_xcom(nSx,nSy)
      _RL tile_ycom(nSx,nSy)
      _RL tile_zcom(nSx,nSy)
      _RL tile_xoamc(nSx,nSy)
      _RL tile_yoamc(nSx,nSy)
      _RL tile_zoamc(nSx,nSy)
      _RL tile_xoamp(nSx,nSy)
      _RL tile_yoamp(nSx,nSy)
      _RL tile_zoamp(nSx,nSy)
      _RL tile_xoamc_si(nSx,nSy)
      _RL tile_yoamc_si(nSx,nSy)
      _RL tile_zoamc_si(nSx,nSy)
      _RL tile_mass_si(nSx,nSy)
      _RL tile_mass_fw(nSx,nSy)
      _RL tile_xcom_fw(nSx,nSy)
      _RL tile_ycom_fw(nSx,nSy)
      _RL tile_zcom_fw(nSx,nSy)
      _RL tile_xoamp_fw(nSx,nSy)
      _RL tile_yoamp_fw(nSx,nSy)
      _RL tile_zoamp_fw(nSx,nSy)
      _RL tile_mass_gc (nSx,nSy)
C Pre-computed cos(lat), sin(lat), cos(lon), sin(lon)
      _RL COSlat(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SINlat(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL COSlon(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SINlon(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CEOP

C Initialize variables to be computed---------------------------------
C-    note: only done once (by master thread) for var in common block
      _BEGIN_MASTER(myThid)

      xoamc = 0.0
      yoamc = 0.0
      zoamc = 0.0
      xoamp = 0.0
      yoamp = 0.0
      zoamp = 0.0
      mass  = 0.0
      xcom  = 0.0
      ycom  = 0.0
      zcom  = 0.0
      sboarea = 0.0

      xoamc_si = 0.0
      yoamc_si = 0.0
      zoamc_si = 0.0
      mass_si  = 0.0

      xoamp_fw = 0.0
      yoamp_fw = 0.0
      zoamp_fw = 0.0
      mass_fw  = 0.0
      xcom_fw  = 0.0
      ycom_fw  = 0.0
      zcom_fw  = 0.0

      mass_gc  = 0.0

      _END_MASTER(myThid)

C Get geographic (East,North) velocities------------------------------

      CALL ROTATE_UV2EN_RL(
     U          uVel, vVel,
     U          UE, VN,
     I          .TRUE., .TRUE., .FALSE., Nr, mythid )

#ifdef ALLOW_SEAICE
      IF ( useSEAICE ) THEN
        CALL ROTATE_UV2EN_RL(
     U          UICE, VICE,
     U          UEice, VNice,
     I          .TRUE., .TRUE., .FALSE., 1, mythid )
      ELSE
#else /* ALLOW_SEAICE */
      IF ( .TRUE. ) THEN
#endif /* ALLOW_SEAICE */
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             UEice(i,j,bi,bj) = 0.
             VNice(i,j,bi,bj) = 0.
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

C Calculate mass load-------------------------------------------------
C     Calculate mass load (Mload), Greatbatch correction for
C     spurious mass but spatial mean freshwater flux retained.
C     Mload *needs* to be total mass (for center of mass), so add
C     back missing time invariant term: -R_low*rhoConst

c Calculate freshwater load
C Calculate Greatbatch correction load over global ocean volume
c Note: no halo regions in i,j loops, do not want to double book sums
      FWload = 0.0
      GCload = 0.0
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        tile_FWload(bi,bj) = 0.0
        tile_GCload(bi,bj) = 0.0
        tile_sboarea(bi,bj) = 0.0
        DO j = 1, sNy
         DO i = 1, sNx
            darea = rA(i,j,bi,bj)*maskC(i,j,1,bi,bj)
            tile_sboarea(bi,bj) = tile_sboarea(bi,bj) + darea
            tile_FWload(bi,bj) =  tile_FWload(bi,bj) +
     &           rhoConst*etaN(i,j,bi,bj)*darea +
     &           sIceLoad(i,j,bi,bj)*darea
            DO k = 1, Nr
               dvolume = rA(i,j,bi,bj)*drF(k)*hFacC(i,j,k,bi,bj)
               tile_GCload(bi,bj) = tile_GCload(bi,bj) +
     &              rhoInSitu(i,j,k,bi,bj) * dvolume
            ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL GLOBAL_SUM_TILE_RL( tile_FWload  , FWload  , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_sboarea , sboarea , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_GCload  , GCload  , myThid )
      FWload = FWload/sboarea
      GCload = -1.0 * GCload/sboarea

c Total mass load with freshwater flux
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
            Mload(i,j,bi,bj) =  rhoConst*etaN(i,j,bi,bj) +
     &           sIceLoad(i,j,bi,bj) +
     &           GCload - R_low(i,j,bi,bj)*rhoConst
            DO k = 1, Nr
               Mload(i,j,bi,bj) = Mload(i,j,bi,bj) +
     &           rhoInSitu(i,j,k,bi,bj)*drF(k)*hFacC(i,j,k,bi,bj)
            ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

c Pre-compute cos(lat), sin(lat), cos(lon), sin(lon)
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
            lat = yC(i,j,bi,bj) * deg2rad
            lon = xC(i,j,bi,bj) * deg2rad
            COSlat(i,j,bi,bj) = COS(lat)
            SINlat(i,j,bi,bj) = SIN(lat)
            COSlon(i,j,bi,bj) = COS(lon)
            SINlon(i,j,bi,bj) = SIN(lon)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C Main loops----------------------------------------------------------
C     loop over all grid points, accumulating mass, com, oam
C     Note: no halo regions in i,j loops, do not want to double book sums

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C Initialize tile sums---------------------------------
        tile_xoamc(bi,bj) = 0.0
        tile_yoamc(bi,bj) = 0.0
        tile_zoamc(bi,bj) = 0.0
        tile_xoamp(bi,bj) = 0.0
        tile_yoamp(bi,bj) = 0.0
        tile_zoamp(bi,bj) = 0.0
        tile_mass(bi,bj) = 0.0
        tile_xcom(bi,bj) = 0.0
        tile_ycom(bi,bj) = 0.0
        tile_zcom(bi,bj) = 0.0

        tile_xoamc_si(bi,bj) = 0.0
        tile_yoamc_si(bi,bj) = 0.0
        tile_zoamc_si(bi,bj) = 0.0
        tile_mass_si(bi,bj) = 0.0

        tile_xoamp_fw(bi,bj) = 0.0
        tile_yoamp_fw(bi,bj) = 0.0
        tile_zoamp_fw(bi,bj) = 0.0
        tile_mass_fw(bi,bj) = 0.0
        tile_xcom_fw(bi,bj) = 0.0
        tile_ycom_fw(bi,bj) = 0.0
        tile_zcom_fw(bi,bj) = 0.0

        tile_mass_gc(bi,bj) = 0.0

        DO j = 1, sNy
         DO i = 1, sNx

           IF ( maskC(i,j,1,bi,bj) .NE. 0. ) THEN

C     horizontal area
              darea = rA(i,j,bi,bj)*maskC(i,j,1,bi,bj)

C     accumulate mass of oceans, Greatbatch correction, seaice
              tile_mass(bi,bj) = tile_mass(bi,bj) +
     &          Mload(i,j,bi,bj)*darea
              tile_mass_gc(bi,bj) = tile_mass_gc(bi,bj) +
     &          GCload*darea
              tile_mass_si(bi,bj) = tile_mass_si(bi,bj) +
     &          sIceLoad(i,j,bi,bj)*darea

C     accumulate center-of-mass of oceans (need to divide by total mass at end)
              tile_xcom(bi,bj) = tile_xcom(bi,bj) +
     &          Mload(i,j,bi,bj)*COSlat(i,j,bi,bj)*COSlon(i,j,bi,bj)
     &          * ae * darea
              tile_ycom(bi,bj) = tile_ycom(bi,bj) +
     &          Mload(i,j,bi,bj)*COSlat(i,j,bi,bj)*SINlon(i,j,bi,bj)
     &          * ae * darea
              tile_zcom(bi,bj) = tile_zcom(bi,bj) +
     &          Mload(i,j,bi,bj)*SINlat(i,j,bi,bj)
     &          * ae * darea

C     accumulate oceanic angular momentum due to currents (need depth integral too)
C     Note: depth integral goes from k=1,Nr.  hFacC takes care of R_low and etaN (as per JMC)
              DO k = 1, Nr
                 dvolume = rA(i,j,bi,bj)*drF(k)
     &             * maskC(i,j,k,bi,bj)*hFacC(i,j,k,bi,bj)
                 tile_xoamc(bi,bj) = tile_xoamc(bi,bj) +
     &             ( VN(i,j,k,bi,bj)*SINlon(i,j,bi,bj) -
     &             UE(i,j,k,bi,bj)*
     &             SINlat(i,j,bi,bj)*COSlon(i,j,bi,bj) )
     &             * rhoConst * ae * dvolume
                 tile_yoamc(bi,bj) = tile_yoamc(bi,bj) +
     &             (-VN(i,j,k,bi,bj)*COSlon(i,j,bi,bj) -
     &             UE(i,j,k,bi,bj)*
     &             SINlat(i,j,bi,bj)*SINlon(i,j,bi,bj) )
     &             * rhoConst * ae * dvolume
                 tile_zoamc(bi,bj) = tile_zoamc(bi,bj) +
     &             UE(i,j,k,bi,bj)*COSlat(i,j,bi,bj)
     &             * rhoConst * ae * dvolume
              ENDDO

C     accumulate sea angular momentum due to motion (one layer, so no depth integral needed)
              tile_xoamc_si(bi,bj) = tile_xoamc_si(bi,bj) +
     &          ( VNice(i,j,bi,bj)*SINlon(i,j,bi,bj) -
     &          UEice(i,j,bi,bj)*
     &          SINlat(i,j,bi,bj)*COSlon(i,j,bi,bj) )
     &          * sIceLoad(i,j,bi,bj) * ae * darea
              tile_yoamc_si(bi,bj) = tile_yoamc_si(bi,bj) +
     &          (-VNice(i,j,bi,bj)*COSlon(i,j,bi,bj) -
     &          UEice(i,j,bi,bj)*
     &          SINlat(i,j,bi,bj)*SINlon(i,j,bi,bj) )
     &          * sIceLoad(i,j,bi,bj) * ae * darea
              tile_zoamc_si(bi,bj) = tile_zoamc_si(bi,bj) +
     &          UEice(i,j,bi,bj)*COSlat(i,j,bi,bj)
     &          * sIceLoad(i,j,bi,bj) * ae * darea

C     accumulate oceanic angular momentum due to pressure
              tile_xoamp(bi,bj) = tile_xoamp(bi,bj) -
     &          SINlat(i,j,bi,bj)*COSlat(i,j,bi,bj)*COSlon(i,j,bi,bj)
     &          * sbo_omega * Mload(i,j,bi,bj) * ae*ae * darea
              tile_yoamp(bi,bj) = tile_yoamp(bi,bj) -
     &          SINlat(i,j,bi,bj)*COSlat(i,j,bi,bj)*SINlon(i,j,bi,bj)
     &          * sbo_omega * Mload(i,j,bi,bj) * ae*ae * darea
              tile_zoamp(bi,bj) = tile_zoamp(bi,bj) +
     &          COSlat(i,j,bi,bj) * COSlat(i,j,bi,bj)
     &          * sbo_omega * Mload(i,j,bi,bj) * ae*ae * darea

C     accumulate mass of real freshwater flux
              tile_mass_fw(bi,bj) = tile_mass_fw(bi,bj) +
     &          FWload * darea

C     accumulate center-of-mass of real freshwater flux (need to divide by total FW mass at end)
              tile_xcom_fw(bi,bj) = tile_xcom_fw(bi,bj) +
     &          FWload * COSlat(i,j,bi,bj) * COSlon(i,j,bi,bj)
     &          * ae * darea
              tile_ycom_fw(bi,bj) = tile_ycom_fw(bi,bj) +
     &          FWload * COSlat(i,j,bi,bj) * SINlon(i,j,bi,bj)
     &          * ae * darea
              tile_zcom_fw(bi,bj) = tile_zcom_fw(bi,bj) +
     &          FWload * SINlat(i,j,bi,bj)
     &          * ae * darea

C     accumulate oceanic angular momentum due to real freshwater flux
              tile_xoamp_fw(bi,bj) = tile_xoamp_fw(bi,bj) -
     &          SINlat(i,j,bi,bj)*COSlat(i,j,bi,bj)*COSlon(i,j,bi,bj)
     &          * sbo_omega * FWload * ae*ae * darea
              tile_yoamp_fw(bi,bj) = tile_yoamp_fw(bi,bj) -
     &          SINlat(i,j,bi,bj)*COSlat(i,j,bi,bj)*SINlon(i,j,bi,bj)
     &          * sbo_omega * FWload * ae*ae * darea
              tile_zoamp_fw(bi,bj) = tile_zoamp_fw(bi,bj) +
     &          COSlat(i,j,bi,bj) * COSlat(i,j,bi,bj)
     &          * sbo_omega * FWload * ae*ae * darea

C     end if over ocean
           ENDIF
C     end loop over i,j
         ENDDO
        ENDDO

C     end loop over bi,bj
       ENDDO
      ENDDO

C     sum all global values across model tiles
      CALL GLOBAL_SUM_TILE_RL( tile_mass  , mass  , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_xcom  , xcom  , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_ycom  , ycom  , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_zcom  , zcom  , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_xoamc , xoamc , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_yoamc , yoamc , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_zoamc , zoamc , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_xoamp , xoamp , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_yoamp , yoamp , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_zoamp , zoamp , myThid )

      CALL GLOBAL_SUM_TILE_RL( tile_xoamc_si , xoamc_si , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_yoamc_si , yoamc_si , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_zoamc_si , zoamc_si , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_mass_si , mass_si , myThid )

      CALL GLOBAL_SUM_TILE_RL( tile_mass_fw  , mass_fw  , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_xcom_fw  , xcom_fw  , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_ycom_fw  , ycom_fw  , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_zcom_fw  , zcom_fw  , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_xoamp_fw , xoamp_fw , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_yoamp_fw , yoamp_fw , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_zoamp_fw , zoamp_fw , myThid )
      CALL GLOBAL_SUM_TILE_RL( tile_mass_gc  , mass_gc  , myThid )

C     finish calculating center-of-mass of oceans
C-    note: only master thread updates/modifies var in common block
      _BEGIN_MASTER(myThid)

      IF ( mass.NE.zeroRL ) THEN
        xcom = xcom / mass
        ycom = ycom / mass
        zcom = zcom / mass
      ENDIF

      IF ( mass_fw.NE.zeroRL ) THEN
        xcom_fw = xcom_fw / mass_fw
        ycom_fw = ycom_fw / mass_fw
        zcom_fw = zcom_fw / mass_fw
      ENDIF

C     Add seaice OAMC to total OAMC
      xoamc = xoamc + xoamc_si
      yoamc = yoamc + yoamc_si
      zoamc = zoamc + zoamc_si

      _END_MASTER(myThid)

#endif /* ALLOW_SBO */

      RETURN
      END
