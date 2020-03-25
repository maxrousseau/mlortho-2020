#!/usr/bin/env python
#-*- coding: utf-8 -*-
import sys
sys.path.append('.')
import numpy as np

class Linear(object):
    """This class is a linear mathematical function"""

    def __init__(self, coords_ax, coords_ay, coords_bx, coords_by):
        """Linear mathematical function object
        Initialize with two coordinates to determine the constant and slope.

        Contains method to determine Y given X.

        Parameters
        ----------
        coords_ax : numpy array
            x coordinate for first landmark
        coords_ay : numpy array
            y coordinate for first landmark
        coords_bx : numpy array
            x coordinate for second landmark
        coords_bx : numpy array
            y coordinate for second landmark

        Returns
        ------
        None
        """

        self.x1 = coords_ax
        self.y1 = coords_ay
        self.x2 = coords_bx
        self.y2 = coords_by

        y_diff = self.y2-self.y1
        x_diff = self.x2-self.x1

        self.slopes = y_diff/x_diff
        self.constants = self.y1 - (self.slopes*self.x1)

    def solve(self, input_slope, input_constant):
        """calculates Y coordinate for a given X

        Parameters
        ----------
        input_slope : int
           value of the slope of the function we want to compare
        input_constant : int
           value of the constant of the function we want to compare

        Returns
        ------
        output_y : int
            resulting X and Y from the arithmetic computations
        """
        input_slope = np.array(input_slope)
        input_constant = np.array(input_constant)

        x_inter = (input_constant - self.constants) / (self.slopes - input_slope)
        y_inter = (self.slopes*x_inter) + self.constants

        return x_inter, y_inter

    def euc_dist(self):
        """compute the Euclidean distance between 2 landmarks
        Calculates the Euclidean distance between 2 landmarks and returns it as
        output.

        Parameters
        ----------
        NONE

        Returns
        -------
        distance : numpy array
            Euclidean between the two landmarks
        """
        height = self.y1 - self.y2
        width = self.x1 - self.x2
        distance = np.sqrt((np.square(height)+np.square(width)))

        return distance


