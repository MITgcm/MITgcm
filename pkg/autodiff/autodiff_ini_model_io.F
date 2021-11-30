C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif
#include "MDSIO_OPTIONS.h"

CBOP
C     !ROUTINE: AUTODIFF_INI_MODEL_IO
C     !INTERFACE:
      SUBROUTINE AUTODIFF_INI_MODEL_IO( myThid )

C     !DESCRIPTION: \bv
C     autodiff_ini_model_io() is where run-time/experiment specific data are
C     passed to any I/O packages ready that will be used for I/O of model state
C     variables.
C     This is specifically for setting up once only information such as
C     shape/size of variables, units, etc... and is only for state variables.
C     \ev

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif
#include "GRID.h"
#include "AUTODIFF.h"
#ifdef ALLOW_WHIO
#include "MDSIO_BUFF_WH.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid  :: my Thread Id number
      INTEGER myThid

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
#ifndef HAVE_SYSTEM
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
      CHARACTER*(MAX_LEN_FNAM) namBuf
      INTEGER iL, pIL
#ifdef ALLOW_AUTODIFF_WHTAPEIO
      INTEGER myLev
#endif
CEOP

c     initialize ad dump record number (used only if dumpAdByRec is true)
      dumpAdRecMn=0
      dumpAdRecDy=0
      dumpAdRecSi=0
      dumpAdRecEt=0

      _BEGIN_MASTER( myThid )

      IF ( adTapeDir .NE. ' ' ) THEN
       iL = ILNBLNK( adTapeDir )
C      append "/", if necessary
       IF ( iL.LT.MAX_LEN_FNAM .AND. adTapeDir(iL:iL).NE.'/' ) THEN
        namBuf(1:iL) = adTapeDir(1:iL)
        WRITE(adTapeDir(1:iL+1),'(2A)') namBuf(1:iL),'/'
       ENDIF
#ifdef HAVE_SYSTEM
C      create directory
       iL = ILNBLNK( adTapeDir ) -1
       WRITE(namBuf,'(2A)') ' mkdir -p ', adTapeDir(1:iL)
       pIL = 1 + ILNBLNK( namBuf )
       WRITE(standardMessageUnit,'(3A)')
     &  '==> SYSTEM CALL (from AUTODIFF_INI_MODEL_IO): >',
     &                                 namBuf(1:pIL), '<'
       CALL SYSTEM( namBuf(1:pIL) )
#else
       WRITE(msgBuf,'(2A)') '** WARNING ** AUTODIFF_INI_MODEL_IO: ',
     &       'cannot call mkdir -> please create adTapeDir manually'
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
#endif
      ENDIF

      _END_MASTER( myThid )

#ifdef ALLOW_AUTODIFF_WHTAPEIO

      tapeFileUnit=0
      do myLev=1,4
        tapeFileUnitS(myLev)=0
      enddo

      tapeFileCounter=0
      tapeMaxCounter=nWh

      tapeConcatIO=.TRUE.
      tapeSingleCpuIO=useSingleCpuIO
      tapeBufferIO=.FALSE.

#ifdef EXCLUDE_WHIO_GLOBUFF_2D
      tapeSingleCpuIO=.FALSE.
#endif
#ifdef ALLOW_WHIO_3D
#ifndef INCLUDE_WHIO_GLOBUFF_3D
      tapeSingleCpuIO=.FALSE.
#endif
#endif

#endif /* ALLOW_AUTODIFF_WHTAPEIO */

#if (defined (ALLOW_MNC) && defined (ALLOW_AUTODIFF_MONITOR))

      IF (useMNC) THEN

      CALL MNC_CW_ADD_VNAME('adU', 'U_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adU','units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adU','long_name',
     &     'adjoint zonal velocity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adU',
     &     'coordinates','XU YU RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adV', 'V_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adV','units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adV','long_name',
     &     'adjoint merid. velocity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adV',
     &     'coordinates','XV YV RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adT', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adT','units','[cost]/[degC]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adT','long_name',
     &     'adjoint potential_temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adT',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adS', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adS','units','[cost]/[g/kg]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adS','long_name',
     &     'adjoint salinity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adS',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adEta', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEta','units','[cost]/[m]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEta','long_name',
     &     'adjoint free-surface_r-anomaly', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEta',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adW', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adW','units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adW','long_name',
     &     'adjoint vertical velocity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adW',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adQnet', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adQnet',
     &     'units','[cost]/[W/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adQnet','long_name',
     &     'adjoint net upward heat flux', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adQnet',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME('adEmpmr', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEmpmr',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEmpmr','long_name',
     &     'adjoint net upward freshwater flux', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adEmpmr',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME(    'adFu', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adFu',
     &     'units','[cost]/[N/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adFu','long_name',
     &     'adjoint zonal wind stress', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adFu',
     &     'coordinates','XC YC RC iter', myThid)

      CALL MNC_CW_ADD_VNAME(    'adFv', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adFv',
     &     'units','[cost]/[N/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adFv','long_name',
     &     'adjoint zonal meridional stress', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adFv',
     &     'coordinates','XC YC RC iter', myThid)

#ifdef ALLOW_SST0_CONTROL
      CALL MNC_CW_ADD_VNAME(    'adSST', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSST',
     &     'units','[cost]/[degC]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSST','long_name',
     &     'adjoint sea_surface_temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSST',
     &     'coordinates','XC YC RC iter', myThid)
#endif

#ifdef ALLOW_SSS0_CONTROL
      CALL MNC_CW_ADD_VNAME(    'adSSS', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSSS',
     &     'units','[cost]/[g/kg]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSSS','long_name',
     &     'adjoint sea_surface_salinity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adSSS',
     &     'coordinates','XC YC RC iter', myThid)
#endif

#ifdef ALLOW_BOTTOMDRAG_CONTROL
      CALL MNC_CW_ADD_VNAME('adBottomDrag', 'Cen_xy_Hn__-__t',
     &                       3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adBottomDrag',
     &     'units','[cost]/[N/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adBottomDrag','long_name',
     &     'adjoint bottom drag', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adBottomDrag',
     &     'coordinates','XC YC RC iter', myThid)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
      CALL MNC_CW_ADD_VNAME('adDiffkr', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adDiffkr',
     & 'units','[cost]/[m^2/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adDiffkr',
     & 'long_name','adjoint vertical diffusion', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adDiffkr',
     &     'coordinates','XC YC RC iter', myThid)
#endif

#ifdef ALLOW_KAPGM_CONTROL
      CALL MNC_CW_ADD_VNAME('adkapgm', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adkapgm',
     & 'units','[cost]/[m^2/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adkapgm',
     & 'long_name','adjoint isopycnal diffusion', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adkapgm',
     &     'coordinates','XC YC RC iter', myThid)
#endif

#ifdef ALLOW_KAPREDI_CONTROL
      CALL MNC_CW_ADD_VNAME('adkapredi', 'Cen_xy_Hn__C__t', 4,5, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adkapredi',
     & 'units','[cost]/[m^2/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adkapredi',
     & 'long_name','adjoint isopycnal diffusion', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adkapredi',
     &     'coordinates','XC YC RC iter', myThid)
#endif

#ifdef ALLOW_EXF
c
      CALL MNC_CW_ADD_VNAME('adustress', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adustress',
     &     'units','[cost]/[N/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adustress','long_name',
     &     'adjoint zonal wind stress', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adustress',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('advstress', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advstress',
     &     'units','[cost]/[N/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advstress','long_name',
     &     'adjoint meridional wind stress', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advstress',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('adhflux', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adhflux',
     &     'units','[cost]/[W/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adhflux','long_name',
     &     'adjoint net upward heat flux', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adhflux',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('adsflux', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adsflux',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adsflux','long_name',
     &     'adjoint net upward freshwater flux', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adsflux',
     &     'coordinates','XC YC RC iter', myThid)
c
# ifdef ALLOW_ATM_TEMP
      CALL MNC_CW_ADD_VNAME('adatemp', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adatemp',
     &     'units','[cost]/[degK]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adatemp','long_name',
     &     'adjoint surface air temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adatemp',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('adaqh', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adaqh',
     &     'units','[cost]/[kg/kg]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adaqh','long_name',
     &     'adjoint specific humidity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adaqh',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('adprecip', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adprecip',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adprecip','long_name',
     &     'adjoint precipitation', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adprecip',
     &     'coordinates','XC YC RC iter', myThid)
# endif
# ifdef ALLOW_RUNOFF
      CALL MNC_CW_ADD_VNAME('adrunoff', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adrunoff',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adrunoff','long_name',
     &     'adjoint runoff', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adrunoff',
     &     'coordinates','XC YC RC iter', myThid)
# endif
# ifdef ALLOW_ATM_WIND
      IF ( useAtmWind ) THEN
      CALL MNC_CW_ADD_VNAME('aduwind', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aduwind',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aduwind','long_name',
     &     'adjoint zonal wind speed', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aduwind',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('advwind', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advwind',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advwind','long_name',
     &     'adjoint meridional wind speed', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advwind',
     &     'coordinates','XC YC RC iter', myThid)
      ENDIF
# endif
# ifdef ALLOW_DOWNWARD_RADIATION
      CALL MNC_CW_ADD_VNAME('adswdown', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adswdown',
     &     'units','[cost]/[W/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adswdown','long_name',
     &     'adjoint downward shortwave radiation', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adswdown',
     &     'coordinates','XC YC RC iter', myThid)
      CALL MNC_CW_ADD_VNAME('adlwdown', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adlwdown',
     &     'units','[cost]/[W/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adlwdown','long_name',
     &     'adjoint downward wave radiation', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adlwdown',
     &     'coordinates','XC YC RC iter', myThid)
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
      CALL MNC_CW_ADD_VNAME('adclimsst', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adclimsst',
     &     'units','[cost]/[degC]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adclimsst','long_name',
     &     'adjoint sea surface temperature', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adclimsst',
     &     'coordinates','XC YC RC iter', myThid)
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
      CALL MNC_CW_ADD_VNAME('adclimsss', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adclimsss',
     &     'units','[cost]/[g/kg]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adclimsss','long_name',
     &     'adjoint sea surface salinity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adclimsss',
     &     'coordinates','XC YC RC iter', myThid)
# endif
c
#endif

#ifdef ALLOW_SEAICE
c
      CALL MNC_CW_ADD_VNAME('adarea', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adarea',
     &     'units','[cost]/[m^2/m^2]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adarea','long_name',
     &     'adjoint fractional ice-covered area', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adarea',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('adheff', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adheff',
     &     'units','[cost]/[m]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adheff','long_name',
     &     'adjoint effective ice thickness', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adheff',
     &     'coordinates','XC YC RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('adhsnow', 'Cen_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adhsnow',
     &     'units','[cost]/[m]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adhsnow','long_name',
     &     'adjoint snow thickness', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('adhsnow',
     &     'coordinates','XC YC RC iter', myThid)
c
# ifdef SEAICE_ALLOW_DYNAMICS
      CALL MNC_CW_ADD_VNAME('aduice', 'U_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aduice',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aduice','long_name',
     &     'adjoint zonal ice velocity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aduice',
     &     'coordinates','XG YG RC iter', myThid)
c
      CALL MNC_CW_ADD_VNAME('advice', 'V_xy_Hn__-__t', 3,4, myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advice',
     &     'units','[cost]/[m/s]', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advice','long_name',
     &     'adjoint meridional ice velocity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('advice',
     &     'coordinates','XG YG RC iter', myThid)
# endif
c
#endif

CC     Write coordinates to "adstate" file
C      CALL MNC_CW_SET_UDIM('adstate', 0, myThid)
C      CALL MNC_CW_RS_W('R','adstate',0,0,'XC',xC, myThid)
C      CALL MNC_CW_RS_W('R','adstate',0,0,'YC',yC, myThid)
C      CALL MNC_CW_RS_W('R','adstate',0,0,'XU',xG, myThid)
C      CALL MNC_CW_RS_W('R','adstate',0,0,'YU',yC, myThid)
C      CALL MNC_CW_RS_W('R','adstate',0,0,'XV',xC, myThid)
C      CALL MNC_CW_RS_W('R','adstate',0,0,'YV',yG, myThid)
C      CALL MNC_CW_RS_W('R','adstate',0,0,'XG',xG, myThid)
C      CALL MNC_CW_RS_W('R','adstate',0,0,'YG',yG, myThid)
C      CALL MNC_CW_RS_W('R','adstate',0,0,'RC',rC, myThid)
C      CALL MNC_CW_RS_W('R','adstate',0,0,'RF',rF, myThid)
C
C#ifdef ALLOW_EXF
C      CALL MNC_CW_SET_UDIM('adexf', 0, myThid)
C      CALL MNC_CW_RS_W('R','adexf',0,0,'XC',xC, myThid)
C      CALL MNC_CW_RS_W('R','adexf',0,0,'YC',yC, myThid)
C      CALL MNC_CW_RS_W('R','adexf',0,0,'XU',xG, myThid)
C      CALL MNC_CW_RS_W('R','adexf',0,0,'YU',yC, myThid)
C      CALL MNC_CW_RS_W('R','adexf',0,0,'XV',xC, myThid)
C      CALL MNC_CW_RS_W('R','adexf',0,0,'YV',yG, myThid)
C      CALL MNC_CW_RS_W('R','adexf',0,0,'XG',xG, myThid)
C      CALL MNC_CW_RS_W('R','adexf',0,0,'YG',yG, myThid)
C      CALL MNC_CW_RS_W('R','adexf',0,0,'RC',rC, myThid)
C      CALL MNC_CW_RS_W('R','adexf',0,0,'RF',rF, myThid)
C#endif
C
C#ifdef ALLOW_SEAICE
C      CALL MNC_CW_SET_UDIM('adseaice', 0, myThid)
C      CALL MNC_CW_RS_W('R','adseaice',0,0,'XC',xC, myThid)
C      CALL MNC_CW_RS_W('R','adseaice',0,0,'YC',yC, myThid)
C      CALL MNC_CW_RS_W('R','adseaice',0,0,'XU',xG, myThid)
C      CALL MNC_CW_RS_W('R','adseaice',0,0,'YU',yC, myThid)
C      CALL MNC_CW_RS_W('R','adseaice',0,0,'XV',xC, myThid)
C      CALL MNC_CW_RS_W('R','adseaice',0,0,'YV',yG, myThid)
C      CALL MNC_CW_RS_W('R','adseaice',0,0,'XG',xG, myThid)
C      CALL MNC_CW_RS_W('R','adseaice',0,0,'YG',yG, myThid)
C      CALL MNC_CW_RS_W('R','adseaice',0,0,'RC',rC, myThid)
C      CALL MNC_CW_RS_W('R','adseaice',0,0,'RF',rF, myThid)
C#endif

      ENDIF
#endif /* ALLOW_MNC and ALLOW_AUTODIFF_MONITOR */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
