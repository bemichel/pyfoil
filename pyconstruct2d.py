import math
import numpy as np
# import matplotlib.pyplot as plt

import pymain
import Converter.Internal as I
import Converter.PyTree   as C
import Connector.PyTree   as X

def greeting(version):
  print(f'Use Version: {version} of Construct2D, a structured grid generator for airfoils')

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
    project_name = "naca12-sharp"
    x, y = create_airfoil(nsize, airfoil_naca0012_sharp)
    y[0] =  y[-1] = 0.
  else:
    project_name = "naca12"
    x, y = create_airfoil(nsize, airfoil_naca0012)
  # plt.plot(x, y, 'o')
  # plt.show()
  # with open("_naca0012.arf", "w") as f:
  #   f.write("NACA 0012\n")
  #   for i in range(x.shape[0]):
  #       f.write(f" {x[i]} {y[i]}\n")

  npoints = x.shape[0]

  surf = pymain.vardef.airfoil_surface_type()
  surf.npoints = npoints
  pymain.memory.surf_allocation(surf)
  pymain.util.init_airfoil(x,y,surf)

  options = pymain.vardef.options_type()
  pymain.util.set_project_name_options(len(project_name), project_name.encode('utf-8'), options)
  pymain.util.set_defaults(surf, options)
  print(f"options = {options}")

  grid   = pymain.vardef.srf_grid_type()
  qstats = pymain.vardef.grid_stats_type()
  pymain.main.compute_grid(b'SMTH', surf, options, grid, qstats)

  create2d = False
  if create2d:
    shape2d = list(grid.x.shape)
    xm = grid.x
    ym = grid.y
    zm = np.zeros(shape2d, dtype=np.float64, order='F')
    dims = [[shape2d[0],shape2d[0]-1,0], [shape2d[1],shape2d[1]-1,0]]
  else:
    shape3d = list(grid.x.shape)+[2]
    xm = np.zeros(shape3d, dtype=np.float64, order='F')
    ym = np.zeros(shape3d, dtype=np.float64, order='F')
    zm = np.zeros(shape3d, dtype=np.float64, order='F')
    for i in range(2):
      xm[:,:,i] = grid.x
      ym[:,:,i] = grid.y
      zm[:,:,i] = 0.1*i
    dims = [[shape3d[0],shape3d[0]-1,0], [shape3d[1],shape3d[1]-1,0], [2,2,0]]
    skewang = np.zeros(shape3d, dtype=np.float64, order='F')
    growthz = np.zeros(shape3d, dtype=np.float64, order='F')
    growthn = np.zeros(shape3d, dtype=np.float64, order='F')
    for i in range(2):
      skewang[:,:,i] = qstats.skewang
      growthz[:,:,i] = qstats.growthz
      growthn[:,:,i] = qstats.growthn

  # Create CGNS Tree
  t = I.newCGNSTree()
  b = I.newCGNSBase('Base', 3, 3, parent=t)
  z = I.newZone('Zone', dims, 'Structured', parent=b)
  g = I.newGridCoordinates(parent=z)
  I.newDataArray('CoordinateX', value=xm, parent=g)
  I.newDataArray('CoordinateY', value=ym, parent=g)
  I.newDataArray('CoordinateZ', value=zm, parent=g)
  f = I.newFlowSolution(name='FlowSolution', gridLocation='Vertex', parent=z)
  I.newDataArray('SkewAngle', value=skewang, parent=f)
  I.newDataArray('GrowthZ', value=growthz, parent=f)
  I.newDataArray('GrowthN', value=growthn, parent=f)

  # Create Family
  fwall = I.newFamily(name='Wall', parent=b)
  fnref = I.newFamily(name='NRef', parent=b)
  fsym  = I.newFamily(name='Sym', parent=b)

  # Create join
  t = X.connectMatch(t, tol=1.e-9)

  # Create BC
  if sharp:
    C._addBC2Zone(t, 'nref',  'FamilySpecified:NRef',  'imin')
    C._addBC2Zone(t, 'nref',  'FamilySpecified:NRef',  'imax')
    C._addBC2Zone(t, 'wall',  'FamilySpecified:Wall',  'jmin')
    C._addBC2Zone(t, 'nref',  'FamilySpecified:NRef',  'jmax')
  else:
    C._addBC2Zone(t, 'wall',  'FamilySpecified:Wall',  'jmin')
    C._addBC2Zone(t, 'nref',  'FamilySpecified:NRef',  'jmax')

  C._addBC2Zone(t, 'sym',   'FamilySpecified:Sym',   'kmin')
  C._addBC2Zone(t, 'sym',   'FamilySpecified:Sym',   'kmax')

  I.printTree(t)
  C.convertPyTree2File(t, f'{project_name}.hdf', format='bin_hdf')
  C.convertPyTree2File(t, f'{project_name}.cgns', format='bin_adf')

  pymain.memory.qstats_deallocation(qstats)
  pymain.memory.grid_deallocation(grid)
  pymain.memory.surf_deallocation(surf)
