#!/usr/bin/env python3

import sympy
from sympy import *


I = Identity(1)

Matrix([[I]])

##################################################################################
import sympy
from sympy import *

W = MatrixSymbol("W", 1, 1)
y = BlockMatrix([[Identity(1), ZeroMatrix(1, 1)], [W, Identity(1)]])
y


