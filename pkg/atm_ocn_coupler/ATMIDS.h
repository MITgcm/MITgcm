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

      CHARACTER*(13) atmCplParamsName
      PARAMETER( atmCplParamsName    = 'ATM CplParams'  )
      INTEGER    atmParSize
      PARAMETER( atmParSize = 6 )

C--   fields sent from OCN to ATM:
      CHARACTER*(10) atmMxlDName
      PARAMETER( atmMxlDName         = 'ATM ocMxlD'     )
      CHARACTER*(7)  atmSSTName
      PARAMETER( atmSSTName          = 'ATM SST'        )
      CHARACTER*(7)  atmSSSName
      PARAMETER( atmSSSName          = 'ATM SSS'        )
      CHARACTER*(9)  atmSSVsqName
      PARAMETER( atmSSVsqName        = 'ATM SSVsq'      )
      CHARACTER*(11) atmCO2FluxName
      PARAMETER( atmCO2FluxName      = 'ATM CO2Flux'    )

C--   fields that can be sent in both direction:
      CHARACTER*(12) atmSIceFracName
      PARAMETER( atmSIceFracName     = 'ATM sIceFrac'   )
      CHARACTER*(13) atmSIceThickName
      PARAMETER( atmSIceThickName    = 'ATM sIceThick'  )
      CHARACTER*(13) atmSIceSnowName
      PARAMETER( atmSIceSnowName     = 'ATM sIceSnowH'  )
      CHARACTER*(10) atmSIceQ1Name
      PARAMETER( atmSIceQ1Name       = 'ATM sIceQ1'     )
      CHARACTER*(10) atmSIceQ2Name
      PARAMETER( atmSIceQ2Name       = 'ATM sIceQ2'     )

C--   fields sent from ATM to OCN:
      CHARACTER*(8)  atmLandName
      PARAMETER( atmLandName         = 'ATM Land'       )
      CHARACTER*(8)  atmSLPrName
      PARAMETER( atmSLPrName         = 'ATM SLPr'       )
      CHARACTER*(12) atmHeatFluxName
      PARAMETER( atmHeatFluxName     = 'ATM HeatFlux'   )
      CHARACTER*(14) atmQshortWaveName
      PARAMETER( atmQshortWaveName   = 'ATM Qshortwave' )
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
      CHARACTER*(12) atmSIceMassName
      PARAMETER( atmSIceMassName     = 'ATM sIceMass'   )
      CHARACTER*(14) atmSaltPlmFlxName
      PARAMETER( atmSaltPlmFlxName   = 'ATM SaltPlmFlx' )
      CHARACTER*(10) atmAirCO2Name
      PARAMETER( atmAirCO2Name       = 'ATM AirCO2'     )
      CHARACTER*(11) atmWindSpdName
      PARAMETER( atmWindSpdName      = 'ATM WindSpd'    )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
