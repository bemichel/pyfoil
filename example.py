#  f90wrap: F90 to Python interface generator with derived type support
#
#  Copyright James Kermode 2011-2018
#
#  This file is part of f90wrap
#  For the latest version see github.com/jameskermode/f90wrap
#
#  f90wrap is free software: you can redistribute it and/or modify
#  it under the terms of the GNU Lesser General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  f90wrap is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public License
#  along with f90wrap. If not, see <http://www.gnu.org/licenses/>.
#
#  If you would like to license the source code under different terms,
#  please contact James Kermode, james.kermode@gmail.com
from __future__ import print_function
import pymain
import numpy as np

# options = vardefmodule.vardef.options_type()

options = pymain.vardef.options_type()
pymain.main.set_options(4, b'toto', options)
options.imax = 100
options.jmax = 200
pymain.main.print_options(options)

tt = pymain.main.mytype()
tt.val = 17
b=np.array(17)

print('********* BEFORE')
print('b=',b)
# print('tt=',tt)
pymain.main.mysubroutine(4,b,tt)
pymain.main.allouemonarraycheri(4,tt)
print(tt.consvar)
print('********* AFTER')
print('b=',b)
print('tt=',tt)
