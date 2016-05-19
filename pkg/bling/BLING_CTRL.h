C $Header: /u/gcmpack/MITgcm/pkg/bling/Attic/BLING_CTRL.h,v 1.1 2016/05/19 20:29:26 mmazloff Exp $
C $Name:  $

C     ==========================================================
C     | BLING_CTRL.h
C     | o Control of Biological Carbon Variables
C     ==========================================================

      integer    bling_n_control
      parameter( bling_n_control = 2 )
      COMMON /BLING_XX_R/ xx_bling
      _RL               xx_bling(bling_n_control)

