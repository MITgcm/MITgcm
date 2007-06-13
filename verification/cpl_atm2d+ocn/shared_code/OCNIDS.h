C $Header: /u/gcmpack/MITgcm/verification/cpl_atm2d+ocn/shared_code/OCNIDS.h,v 1.2 2007/06/13 15:23:10 jscott Exp $
C $Name:  $

C     /==========================================================\
C     | OCNIDS.h Declare symbolic constants holding strings that |
C     |          are used to identify this component and the     |
C     |          fields it exchanges with other components.      |
C     \==========================================================/
      CHARACTER*(8)  ocnCompName 
      PARAMETER( ocnCompName         = 'UV-Ocean'       )

      CHARACTER*(10) ocnMxlDName
      PARAMETER( ocnMxlDName         = 'OCN ocMxlD'     )
      CHARACTER*(7)  ocnSSTName
      PARAMETER( ocnSSTName          = 'OCN SST'        )
      CHARACTER*(7)  ocnSSSName
      PARAMETER( ocnSSSName          = 'OCN SSS'        )
      CHARACTER*(9)  ocnSSVsqName
      PARAMETER( ocnSSVsqName        = 'OCN SSVsq'      )
      CHARACTER*(8)  ocnFCO2Name
      PARAMETER( ocnFCO2Name         = 'OCN FCO2'       )

      CHARACTER*(8)  ocnSLPrName
      PARAMETER( ocnSLPrName         = 'OCN SLPr'       )
      CHARACTER*(12) ocnHeatFluxName
      PARAMETER( ocnHeatFluxName     = 'OCN HeatFlux'   )
      CHARACTER*(14) ocnQshortWaveName
      PARAMETER( ocnQshortWaveName   = 'OCN Qshortwave' )
c     CHARACTER*(13) ocnQlongWaveName
c     PARAMETER( ocnQlongWaveName    = 'OCN Qlongwave'  )
c     CHARACTER*(11) ocnQlatentName
c     PARAMETER( ocnQlatentName      = 'OCN Qlatent'    )
c     CHARACTER*(13) ocnQsensibleName
c     PARAMETER( ocnQsensibleName    = 'OCN Qsensible'  )
c     CHARACTER*(14) ocnUvelgroundName
c     PARAMETER( ocnUvelgroundName   = 'OCN Uvelground' )
c     CHARACTER*(14) ocnVvelgroundName
c     PARAMETER( ocnVvelgroundName   = 'OCN Vvelground' )
      CHARACTER*(8)  ocnTauXName
      PARAMETER( ocnTauXName         = 'OCN tauX'       )
      CHARACTER*(8)  ocnTauYName
      PARAMETER( ocnTauYName         = 'OCN tauY'       )
      CHARACTER*(10) ocnFWFluxName
      PARAMETER( ocnFWFluxName       = 'OCN FWFlux'     )
      CHARACTER*(12) ocnSaltFxName
      PARAMETER( ocnSaltFxName       = 'OCN SaltFlux'   )
      CHARACTER*(10) ocnSeaIceName
      PARAMETER( ocnSeaIceName       = 'OCN SeaIce'     )
      CHARACTER*(10) ocnAirCO2Name
      PARAMETER( ocnAirCO2Name       = 'OCN AirCO2'     )
      CHARACTER*(8) ocnWSpdName
      PARAMETER( ocnWSpdName         = 'OCN WSpd'       )
      CHARACTER*(8) ocnFIceName
      PARAMETER( ocnFIceName         = 'OCN FIce'       )
C
C     Pass carbon/DIC variables only if flag set below
      LOGICAL ocnCpl_exchange_DIC
      PARAMETER( ocnCpl_exchange_DIC = .FALSE.)
