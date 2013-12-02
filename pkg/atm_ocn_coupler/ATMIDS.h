C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/ATMIDS.h,v 1.3 2013/12/02 23:21:47 jmc Exp $
C $Name:  $

  These lines are here to deliberately cause a compile-time error.
  If you see these lines in your .F files or the compiler shows them
    as an error then it means you have not placed your coupler
    configuration files in the appropriate place.
  You need: a) to place you own copy of ATMIDS.h in a directory
    (e.g.: shared_code) which is both in the include path of the
    coupler and in the include path of the atmospheric component ;
  and b) to comment out these lines.

C     *==========================================================*
C     | ATMIDS.h Declare symbolic constants holding strings that
C     |          are used to identify this component and the
C     |          fields it exchanges with other components.
C     *==========================================================*
      CHARACTER*(8)  atmCompName
      PARAMETER( atmCompName         = 'UV-Atmos'       )

      CHARACTER*(8)  atmLandName
      PARAMETER( atmLandName         = 'ATM Land'       )
      CHARACTER*(10) atmMxlDName
      PARAMETER( atmMxlDName         = 'ATM ocMxlD'     )
      CHARACTER*(7)  atmSSTName
      PARAMETER( atmSSTName          = 'ATM SST'        )
      CHARACTER*(7)  atmSSSName
      PARAMETER( atmSSSName          = 'ATM SSS'        )
      CHARACTER*(9)  atmSSVsqName
      PARAMETER( atmSSVsqName        = 'ATM SSVsq'      )
      CHARACTER*(8)  atmFCO2Name
      PARAMETER( atmFCO2Name         = 'ATM FCO2'       )

      CHARACTER*(8)  atmSLPrName
      PARAMETER( atmSLPrName         = 'ATM SLPr'       )
      CHARACTER*(12) atmHeatFluxName
      PARAMETER( atmHeatFluxName     = 'ATM HeatFlux'   )
      CHARACTER*(14) atmQshortWaveName
      PARAMETER( atmQshortWaveName   = 'ATM Qshortwave' )
c     CHARACTER*(13) atmQlongWaveName
c     PARAMETER( atmQlongWaveName    = 'ATM Qlongwave'  )
c     CHARACTER*(11) atmQlatentName
c     PARAMETER( atmQlatentName      = 'ATM Qlatent'    )
c     CHARACTER*(13) atmQsensibleName
c     PARAMETER( atmQsensibleName    = 'ATM Qsensible'  )
c     CHARACTER*(14) atmUvelgroundName
c     PARAMETER( atmUvelgroundName   = 'ATM Uvelground' )
c     CHARACTER*(14) atmVvelgroundName
c     PARAMETER( atmVvelgroundName   = 'ATM Vvelground' )
      CHARACTER*(8)  atmTauXName
      PARAMETER( atmTauXName         = 'ATM tauX'       )
      CHARACTER*(8)  atmTauYName
      PARAMETER( atmTauYName         = 'ATM tauY'       )
      CHARACTER*(9) atmEvMPrName
      PARAMETER( atmEvMPrName        = 'ATM Ev-Pr'      )
      CHARACTER*(10) atmRunOffName
      PARAMETER( atmRunOffName       = 'ATM RunOff'     )
      CHARACTER*(12) atmROEnFxName
      PARAMETER( atmROEnFxName       = 'ATM RO.EnFlx'   )
      CHARACTER*(12) atmSaltFxName
      PARAMETER( atmSaltFxName       = 'ATM SaltFlux'   )
      CHARACTER*(10) atmSeaIceName
      PARAMETER( atmSeaIceName       = 'ATM SeaIce'     )
      CHARACTER*(10) atmAirCO2Name
      PARAMETER( atmAirCO2Name       = 'ATM AirCO2'     )
      CHARACTER*(8) atmWSpdName
      PARAMETER( atmWSpdName         = 'ATM WSpd'       )
      CHARACTER*(8) atmFIceName
      PARAMETER( atmFIceName         = 'ATM FIce'       )

C     Pass carbon/DIC variables only if flag set below
      LOGICAL atmCpl_exchange_DIC
      PARAMETER( atmCpl_exchange_DIC = .FALSE.)

