C $Header: /u/gcmpack/MITgcm/verification/cpl_aim+ocn/shared_code/ATMIDS.h,v 1.1 2003/12/15 21:07:25 jmc Exp $
C $Name:  $

C     /==========================================================\
C     | ATMIDS.h Declare symbolic constants holding strings that |
C     |          are used to identify this component and the     |
C     |          fields it exchanges with other components.      |
C     \==========================================================/
      CHARACTER*(8)  atmCompName 
      PARAMETER( atmCompName          = 'UV-Atmos'        )
      CHARACTER*(10) atmDepthName
      PARAMETER( atmDepthName         = 'ATM Depths'      )
      CHARACTER*(7)  atmSSTName
      PARAMETER( atmSSTName           = 'ATM SST'         )
      CHARACTER*(8)  atmTauXName
      PARAMETER( atmTauXName          = 'ATM tauX'        )
      CHARACTER*(8)  atmTauYName
      PARAMETER( atmTauYName          = 'ATM tauY'        )
      CHARACTER*(12) atmHeatFluxName
      PARAMETER( atmHeatFluxName      = 'ATM HeatFlux'    )
      CHARACTER*(9) atmEvMPrName
      PARAMETER( atmEvMPrName         = 'ATM Ev-Pr'       )
      CHARACTER*(10) atmRunOffName
      PARAMETER( atmRunOffName        = 'ATM RunOff'      )
      CHARACTER*(13) atmQlongwaveName
      PARAMETER( atmQlongwaveName     = 'ATM Qlongwave'   )
      CHARACTER*(14) atmQshortwaveName
      PARAMETER( atmQshortwaveName    = 'ATM Qshortwave'  )
      CHARACTER*(11) atmQlatentName
      PARAMETER( atmQlatentName       = 'ATM Qlatent'     )
      CHARACTER*(13) atmQsensibleName
      PARAMETER( atmQsensibleName     = 'ATM Qsensible'   )
      CHARACTER*(14) atmUvelgroundName
      PARAMETER( atmUvelgroundName    = 'ATM Uvelground'  )
      CHARACTER*(14) atmVvelgroundName
      PARAMETER( atmVvelgroundName    = 'ATM Vvelground'  )
