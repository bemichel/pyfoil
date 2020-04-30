import numpy as np

import pymain

class PyFoil(object):
  """
   An arifoil 2D mesh generator based on a python wrapping of Construct2D thanks to
    f90wrap.

   The 2D strucutred mesh and associted statictics computed by Construct2D core is
    transformed in a Python/CGNS tree in memory thanks to Cassiopee.

   Grid connectivity and BC can be added to the resulting Python/CGNS tree and
    convert to an NGon unstructured mesh thanks to Cassiopee.

   * construct2d :  https://sourceforge.net/projects/construct2d/
   * f90wrap     : https://github.com/jameskermode/f90wrap
   * Cassiopee   : http://elsa.onera.fr/Cassiopee/
   """
  def __init__(self, x, y, name):
    self.x = x
    self.y = y
    self.name = name

  def initialize(self):
    self.surf = pymain.vardef.airfoil_surface_type()
    # 'airfoil_surface_type' creation and initialization
    self.surf.npoints = self.x.shape[0]
    pymain.memory.surf_allocation(self.surf)
    pymain.util.init_airfoil(self.x, self.y, self.surf)

    # 'options_type' creation and initialization
    self.options = pymain.vardef.options_type()
    pymain.util.set_project_name_options(len(self.name),
                                         self.name.encode('utf-8'),
                                         self.options)
    pymain.util.set_defaults(self.surf, self.options)
    print(f"self.options = {self.options}")

  def compute(self, grid='SMTH'):
    """
      Mesh and associated statictics computation
    """
    # 'srf_grid_type' and 'grid_stats_type' creation
    self.grid   = pymain.vardef.srf_grid_type()
    self.qstats = pymain.vardef.grid_stats_type()
    pymain.main.compute_grid(grid.encode('utf-8'),
                             self.surf,
                             self.options,
                             self.grid,
                             self.qstats)

  def mesh(self, dz=0.1):
    """
      Mesh and associated statictics transfert
    """
    if dz > 0.:
      shape3d = list(self.grid.x.shape)+[2]
      xm = np.zeros(shape3d, dtype=np.float64, order='F')
      ym = np.zeros(shape3d, dtype=np.float64, order='F')
      zm = np.zeros(shape3d, dtype=np.float64, order='F')
      for i in range(2):
        xm[:,:,i] = self.grid.x
        ym[:,:,i] = self.grid.y
        zm[:,:,i] = dz*i
    else:
      shape2d = list(self.grid.x.shape)
      xm = self.grid.x
      ym = self.grid.y
      zm = np.zeros(shape2d, dtype=np.float64, order='F')
    return xm, ym, zm

  def statistics(self, dz=0.1):
    """
      Mesh and associated statictics transfert
    """
    if dz > 0.:
      shape3d = list(self.grid.x.shape)+[2]
      skewang = np.zeros(shape3d, dtype=np.float64, order='F')
      growthz = np.zeros(shape3d, dtype=np.float64, order='F')
      growthn = np.zeros(shape3d, dtype=np.float64, order='F')
      for i in range(2):
        skewang[:,:,i] = self.qstats.skewang
        growthz[:,:,i] = self.qstats.growthz
        growthn[:,:,i] = self.qstats.growthn
    else:
      skewang = self.qstats.skewang
      growthz = self.qstats.growthz
      growthn = self.qstats.growthn
    return skewang, growthz, growthn

  def release(self):
    """
      Release allocated Fortran memory
    """
    pymain.memory.qstats_deallocation(self.qstats)
    pymain.memory.grid_deallocation(self.grid)
    pymain.memory.surf_deallocation(self.surf)
