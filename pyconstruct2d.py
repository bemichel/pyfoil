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

import math
import numpy as np
import matplotlib.pyplot as plt

import pymain

def greeting(version):
  print('This is Construct2D, the structured grid generator for airfoils')
  print(f'Version: {version}')

def airfoil_naca0012(x):
  return 0.6*(0.2969*np.sqrt(x) - 0.1260*x - 0.3516*x*x + 0.2843*x*x*x - 0.1015*x*x*x*x)

def airfoil_naca0012_sharp(x):
  return 0.6*(0.2969*np.sqrt(x) - 0.1260*x - 0.3516*x*x + 0.2843*x*x*x - 0.1036*x*x*x*x)

def create_airfoil(nsize, airfoil_func):
  xfoil = np.vectorize(airfoil_func)

  x1 = np.zeros(nsize, dtype=np.float64, order='F')
  coef = 0.92*np.arccos(-1.)/2.
  rap = 1./np.sin(coef)**3
  x1[0] = 0.
  for i in range(1, nsize-1):
     x1[i] = rap*np.sin(coef*float(i)/float(nsize-1))**3
  x1[nsize-1] = 1.

  x2 = np.flip(x1)
  y1 = -xfoil(x1)
  y2 = xfoil(x2)

  x = np.concatenate([x2, x1[1:]])
  y = np.concatenate([y2, y1[1:]])

  return x, y

if __name__ == "__main__":
  greeting('2.1.4')

  nsize = 100
  sharp = True
  if sharp:
    x, y = create_airfoil(nsize, airfoil_naca0012_sharp)
    y[0] =  y[-1] = 0.
  else:
    x, y = create_airfoil(nsize, airfoil_naca0012)
  # plt.plot(x, y, 'o')
  # plt.show()
  # with open("_naca0012.arf", "w") as f:
  #   f.write("NACA 0012\n")
  #   for i in range(x.shape[0]):
  #       f.write(f" {x[i]} {y[i]}\n")

  npoints = x.shape[0]
  print(f"npoints = {npoints}")

  surf = pymain.vardef.airfoil_surface_type()
  surf.npoints = npoints
  print('********* surf_allocation')
  pymain.memory.surf_allocation(surf)
  pymain.util.init_airfoil(x,y,surf)

  project_name = "mynaca12"
  options = pymain.vardef.options_type()
  pymain.util.set_project_name_options(len(project_name), project_name.encode('utf-8'), options)
  pymain.util.set_defaults(surf, options)
  print(f"surf = {surf}")
  print(f"options = {options}")
  pymain.main.compute_grid(b'SMTH', surf, options)

  sys.exit(1)

  print(f"dir(pymain) = {dir(pymain)}")

  grid = pymain.vardef.srf_grid_type()
  grid.imax = 100
  grid.jmax = 200

  print('********* grid_allocation')
  pymain.memory.grid_allocation(grid)
  # print('grid=',grid)

  tt = pymain.main.mytype()
  tt.val = 17
  b=np.zeros(17, dtype=np.float64, order='F')

  print('********* BEFORE')
  print('b=',b)
  # print('tt=',tt)
  pymain.main.mysubroutine(4,b,tt)
  pymain.main.allouemonarraycheri(4,tt)
  print(tt.consvar)
  print('********* AFTER')
  print('b=',b)
  print('tt=',tt)


  print('********* grid_deallocation')
  pymain.memory.grid_deallocation(grid)
  print('********* surf_deallocation')
  pymain.memory.surf_deallocation(surf)
