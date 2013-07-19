#! /usr/bin/env bash

#  $Header: /u/gcmpack/MITgcm/verification/lab_sea/input_ad/dorun.sh,v 1.2 2013/07/19 13:14:20 jmc Exp $
#  $Name:  $

rm -f costf* divided.ctrl snapshot*

./mitgcmuv_ad > output_adm.txt.p1
./mitgcmuv_ad > output_adm.txt.p2
./mitgcmuv_ad > output_adm.txt.p3
./mitgcmuv_ad > output_adm.txt.p4
./mitgcmuv_ad > output_adm.txt

