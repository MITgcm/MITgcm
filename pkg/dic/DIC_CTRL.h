C $Header: /u/gcmpack/MITgcm/pkg/dic/DIC_CTRL.h,v 1.2 2010/03/15 22:55:51 jmc Exp $
C $Name:  $

C     *==========================================================*
C     | DIC_XX.h
C     | o Control of Biological Carbon Variables
C     *==========================================================*

      integer    dic_n_control
      parameter( dic_n_control = 5 )
      COMMON /DIC_XX_R/ xx_dic
      _RL               xx_dic(dic_n_control)

