# -*- coding: utf-8 -*-
"""
Created on Thu Apr 13 08:34:30 2023

@author: EGavilan Pascual-Ahuir
"""
import MITgcmutils as mit

def test_blanklist():
    """Test blanklist generator
    """
    bathy=mit.readbin('./bathy_test.bin', [510,510])

    # Test

    print('Test 1: Output blanklist without tilemap')
    print('gen_blanklist(bathy, 51,51, tilemap=False)')

    blank=mit.gen_blanklist(bathy, 51,51, tilemap=False)
    print(blank[0:3],'..',blank[-1])

    print('Test 2: Output blanklist with tilemap')
    print('mit.gen_blanklist(bathy, 51,51, tilemap=True)')

    [blank,fig1]=mit.gen_blanklist(bathy, 51,51, tilemap=True)
    print(blank[0:3],'..',blank[-1])
    
def test_tilemap():
    """Test tilemap plot distribution
    """
    bathy=mit.readbin('./bathy_test.bin', [510,510])

    # Test

    print('Test 1: Output tilemap without specific tile')
    print('tilecmap(bathy, 51, 51)')

    mit.tilecmap(bathy, 51, 51)

    print('Test 2: Output tilemap without specific tile')
    print('mit.tilecmap(bathy, 51, 51)')

    mit.tilecmap(bathy, 51, 51,80,sel_zoom=5)
   