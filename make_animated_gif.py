# -*- coding: utf-8 -*-
"""
Created on Tue Sep  3 22:02:29 2019

@author: gccar
"""

import imageio
import os, glob

path_to_img = "E:/STAT685/Graphs/"

images = []
for filename in glob.glob(path_to_img + "TexasHisp*.png"):
    images.append(imageio.imread(filename))
imageio.mimsave(path_to_img+'TexasHisp.gif', images,duration=2)

images = []
for filename in glob.glob(path_to_img + "a1_all_rs*.png"):
    images.append(imageio.imread(filename))
imageio.mimsave(path_to_img+'a1_all_rs.gif', images,duration=2)