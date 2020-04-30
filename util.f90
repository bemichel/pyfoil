!  This file is part of Construct2D.

!  Construct2D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.

!  Construct2D is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.

!  You should have received a copy of the GNU General Public License
!  along with Construct2D.  If not, see <http://www.gnu.org/licenses/>.

!  Copyright (C) 2013 -- 2018 Daniel Prosser

module util

! Contains subroutines to perform utility-type operations for the grid
! generator (reading/writing files, parsing CLOs, etc)

  implicit none

  contains

!=============================================================================80
!
! Prints out greeting
!
!=============================================================================80
subroutine greeting(version)

  character(*), intent(in) :: version

  write(*,*)
  write(*,*) 'This is Construct2D, the structured grid generator for airfoils'
  write(*,*) 'Version: '//trim(version)

end subroutine greeting

!=============================================================================80
!
! Subroutine to read command line options and set airfoil file name
!
!=============================================================================80
subroutine read_clo(filename)

  character(*), intent(inout) :: filename

  filename = ''

  call getarg(1, filename)

end subroutine read_clo

!=============================================================================80
!
! Subroutine to read airfoil file name from user input
!
!=============================================================================80
subroutine read_airfoil_name(filename)

  character(*), intent(inout) :: filename

  write(*,*)
  write(*,*) 'Enter name of airfoil to load (XFOIL labeled format)'
  write(*,*) ' or QUIT to close the program:'
  write(*,1001)
  read(*,'(A)') filename

  if (trim(filename) == 'QUIT' .or. trim(filename) == 'Quit' .or.              &
      trim(filename) == 'quit') stop

1001 format(/' Input > ',$)

end subroutine read_airfoil_name

!=============================================================================80
!
! Subroutine to read airfoil dimensions (XFoil labeled format)
!
!=============================================================================80
subroutine read_airfoil_size(filename, dimensions, ioerror)

  character(*), intent(inout) :: filename
  integer, intent(inout) :: dimensions, ioerror

  integer iunit

! Open file

  iunit = 12
  open(iunit, file=filename, status='old', iostat=ioerror)

  if (ioerror /= 0) then

    write(*,*)
    write(*,*) 'Error: airfoil file '//trim(filename)//' could not be found.'
    filename = ''

  else

!   Read number of points on surface

    dimensions = 0
    read(iunit,*)         ! Skip past header
    do
      read(iunit,*,end=500)
      dimensions = dimensions + 1
    end do

  end if

500 close(iunit)

end subroutine read_airfoil_size

!=============================================================================80
!
! Subroutine to read airfoil data (XFoil labeled format)
!
!=============================================================================80
subroutine read_airfoil(filename, npoints, x, y)

  character(*), intent(in) :: filename
  integer, intent(in) :: npoints
  double precision, dimension(npoints), intent(out) :: x, y

  integer i, iunit
  double precision, dimension(npoints) :: xtmp, ytmp

! Open file

  iunit = 13
  open(iunit, file=filename, status='old')

! Read data

  read(iunit,*)                     ! Skip past header
  do i = 1, npoints
    read(iunit,*) x(i), y(i)
  end do

! Flip direction if needed to ensure counterclockwise ordering

  if (y(2) < y(npoints-1)) then

    write(*,*)
    write(*,*) 'Changing point ordering to counter-clockwise ...'
    xtmp = x
    ytmp = y
    do i = 1, npoints
      x(i) = xtmp(npoints-i+1)
      y(i) = ytmp(npoints-i+1)
    end do

  end if

! Close file and print message

  close(iunit)
  write(*,*)
  write(*,*) 'Successfully loaded airfoil file '//trim(filename)
  write(*,*) ' Number of points:', npoints

end subroutine read_airfoil

!=============================================================================80
!
! Subroutine to init airfoil data (XFoil labeled format)
!
!=============================================================================80
subroutine init_airfoil(x, y, surf)
  Use vardef, only : airfoil_surface_type
  implicit none
  double precision, dimension(:), intent(in) :: x
  double precision, dimension(:), intent(in) :: y
  type(airfoil_surface_type), intent(inout) :: surf

  print *,'Running init_airfoil'
  surf%x = x
  surf%y = y

end subroutine init_airfoil

!=============================================================================80
!
! Flips the order of the characters in a string
!
!=============================================================================80
function flip_text(str) result(backstr)

  character(*), intent(in) :: str

  character(len(str)) :: backstr, trimstr
  integer i, n

  trimstr = trim(str)
  n = len(trimstr)
  do i = 1, n
    backstr(i:i) = str(n-i+1:n-i+1)
  end do

end function flip_text


!=============================================================================80
subroutine set_project_name_options(n, project_name, options)
  Use vardef, only : options_type
  implicit none
  integer(4), intent(in)   :: n
  character(n), intent(in) :: project_name
  type(options_type)       :: options

  options%project_name = project_name
end subroutine set_project_name_options

!=============================================================================80
!
! Set project name based on airfoil file name, and determine trailing edge info
!
!=============================================================================80
subroutine setup_airfoil_data(options, surf)
  Use vardef, only : airfoil_surface_type, options_type

  type(options_type), intent(inout) :: options
  type(airfoil_surface_type), intent(inout) :: surf

  integer npoints, loc1, loc2
  character(300) backname
  double precision, parameter :: my_tiny = tiny(1.d0)

! Determine if airfoil has blunt trailing edge

  npoints = surf%npoints
  if (abs((surf%x(1) - surf%x(npoints))) <= my_tiny .and.                      &
      abs((surf%y(1) - surf%y(npoints))) <= my_tiny) then

    surf%tegap = .false.
    options%topology = 'CGRD'
    write(*,*) ' Sharp trailing edge: C-grid topology is recommended.'

  elseif (abs((surf%y(1) - surf%y(npoints))) <= my_tiny .and.                  &
          abs((surf%x(1) - surf%x(npoints))) > my_tiny) then

!   Funky geometry fix

    surf%x(npoints) = surf%x(1)
    surf%tegap = .false.
    options%topology = 'CGRD'
    write(*,*) ' Sharp trailing edge: C-grid topology is recommended.'

  else

    surf%tegap = .true.
    options%topology = 'OGRD'
    write(*,*) ' Blunt trailing edge: O-grid topology is recommended.'

  end if

end subroutine setup_airfoil_data

!=============================================================================80
!
! Subroutine to set default program options
!
!=============================================================================80
subroutine set_defaults(surf, options)
  Use vardef, only : airfoil_surface_type, options_type

  type(airfoil_surface_type), intent(inout) :: surf
  type(options_type), intent(inout) :: options

  integer iunit
  logical filecheck
  character input

  integer nsrf, tept, jmax, nwke, stp1, stp2, nrmt, nrmb, gdim, npln, asmt
  double precision lesp, tesp, radi, ypls, recd, fdst, fwkl, fwki, dpln, alfa, &
                   epsi, epse, funi, uni, cfrc
  logical f3dm
  character(4) :: topo, slvr
  namelist /SOPT/ nsrf, lesp, tesp, radi, nwke, fdst, fwkl, fwki
  namelist /VOPT/ jmax, slvr, topo, ypls, recd, stp1, stp2, nrmt, nrmb,  &
                  alfa, epsi, epse, funi, asmt, cfrc
  namelist /OOPT/ gdim, npln, dpln, f3dm

! Initial project and airfoil setup

  call setup_airfoil_data(options, surf)

! Set default values

  nsrf = 250
  tept = 13
  fdst = 1.d0
  fwkl = 1.d0
  fwki = 10.d0
  jmax = 100
  slvr = 'HYPR'
  topo = options%topology
  radi = 15.d0
  nwke = 50
  ypls = 0.9d0
  recd = 1D+06
  cfrc = 0.5d0
  stp1 = 1000
  stp2 = 20
  nrmt = 1
  nrmb = 1
  gdim = 2
  npln = 2
  dpln = 1.d0
  alfa = 1.0d0
  epsi = 15.d0
  epse = 0.0d0
  funi = 0.20d0
  asmt = 20
  f3dm = .false.

  uni = 2.d0/dble(nsrf)
  lesp = uni / 2.d0
  if (surf%tegap) then
    tesp = (surf%y(1) - surf%y(surf%npoints)) / 10.d0
  else
    tesp = uni / 1.5d0
  end if

! Allow input-file setting of parameters

  iunit = 12
  inquire(file='grid_options.in', exist=filecheck)
  if (filecheck) then
    write(*,*)
    write(*,*) 'Reading settings from user-supplied file grid_options.in'
    open(iunit, file='grid_options.in', status='old')
    read(iunit,nml=SOPT)
    read(iunit,nml=VOPT)
    read(iunit,nml=OOPT)
    close(12)
  end if

! Set default number of trailing edge gap points and total points on surface

  options%ntedefault = tept
  options%nsrfdefault = nsrf
  if (surf%tegap) then
    options%nte = options%ntedefault
    options%nsrf = surf%npoints + options%nte + 1
  else
    options%nte = 0
    options%nsrf = surf%npoints
  end if

! Other options
  options%jmax = jmax
  options%lesp = lesp
  options%tesp = tesp
  options%fdst = fdst
  options%fwkl = fwkl
  options%fwki = fwki
  options%yplus = ypls
  options%Re = recd
  options%cfrac = cfrc
  options%maxsteps = stp1
  options%radi = radi
  options%fsteps = stp2
  options%nwake = nwke
  options%nrmt = nrmt
  options%nrmb = nrmb
  if (gdim == 2 .or. gdim == 3) then
    options%griddim = gdim
  else
    options%griddim = 2
  end if
  if (npln >= 1) then
    options%nplanes = npln
  else
    options%nplanes = 2
  end if
  if (dpln /= 0.d0) then
    options%plane_delta = dpln
  else
    options%plane_delta = 1.d0
  end if
  if (slvr == 'HYPR' .or. slvr == 'Hypr' .or. slvr == 'hypr') then
    options%slvr = 'HYPR'
  elseif (slvr == 'ELLP' .or. slvr == 'Ellp' .or. slvr == 'ellp') then
    options%slvr = 'ELLP'
  end if
  if (alfa >= 0.5d0) then
    options%alfa = alfa
  end if
  if (epsi >= 0.d0) then
    options%epsi = epsi
  end if
  if (epse >= 0.d0) then
    options%epse = epse
  end if
  if (funi >= 0.d0 .and. funi <= 1.d0) then
    options%funi = funi
  end if
  if (asmt >= 0) then
    options%asmt = asmt
  end if
  options%f3d_compat = f3dm

! Adjust settings based on whether FUN3D compatibility mode is enabled

  if (options%f3d_compat) then
    options%griddim = 3
  end if

! Topology

  if (topo == 'OGRD' .or. topo == 'Ogrd' .or. topo == 'ogrd') then
    if (options%topology == 'CGRD') then
      write(*,*)
      write(*,*) ' Warning: grid_options.in has topo = OGRD, but this airfoil'
      write(*,*) '  has a sharp trailing edge.  Do you really want to use the'
      write(*,*) '  O-grid topology instead of the recommended C-grid (y/n)?'
      write(*,999)
      read(*,*) input
      if (input == 'Y' .or. input == 'y') options%topology = 'OGRD'
    end if
  elseif (topo == 'CGRD' .or. topo == 'Cgrd' .or. topo == 'cgrd') then
    if (options%topology == 'OGRD') then
      write(*,*)
      write(*,*) ' Warning: grid_options.in has topo = CGRD, but this airfoil'
      write(*,*) '  has a blunt trailing edge.  Do you really want to use the'
      write(*,*) '  C-grid topology instead of the recommended O-grid (y/n)?'
      write(*,999)
      read(*,*) input
      if (input == 'Y' .or. input == 'y') options%topology = 'CGRD'
    end if
  end if

  if (options%topology == 'CGRD') then
    options%funi = 0.01d0
  end if

999 format(/' Input > ',$)

end subroutine set_defaults

!=============================================================================80
!
! Subroutine to write program options to a file
!
!=============================================================================80
subroutine write_options(options)

  Use vardef, only : options_type

  type(options_type), intent(in) :: options

  character(300) :: suggname, filename
  character :: input
  logical woptdone, writefile, filecheck
  integer iunit

  woptdone = .false.
  writefile = .false.

  do while (.not. woptdone)

    suggname = trim(options%project_name)//'_settings.nml'
    write(*,*)
    write(*,*) 'Suggested file name: '//trim(suggname)
    write(*,*)
    write(*,*) 'Enter 1 to accept suggested name, 2 to enter new name, or'
    write(*,*) ' 3 to return to the main menu:'
    write(*,1002)
    read(*,*) input

!   Get user input

    select case (input)

      case ('1')

        filename = suggname

      case ('2')

        write(*,*)
        write(*,*) 'Enter file name to write current program options:'
        write(*,1002)
        read(*,*) filename

      case ('3')

        woptdone = .true.

      case default

        write(*,*)
        write(*,*) 'Error: input '//trim(input)//' not recognized.'
        write(*,*)
        woptdone = .true.

    end select

    if (.not. woptdone) then

!     Check for existence of file

      inquire(file=filename, exist=filecheck)
      if (filecheck) then
        write(*,*)
        write(*,*) 'Warning: '//trim(filename)//' already exists. Do you want'
        write(*,*) ' to replace it (y/n)?'
        write(*,1002)
        read(*,*) input
        if (input == 'y') then
          woptdone = .true.
          writefile = .true.
        else
          writefile = .false.
        end if
      else
        woptdone = .true.
        writefile = .true.
      end if

    end if

!   Write out options

    if (writefile) then

      write(*,*)
      write(*,*) 'Writing current program options to '//trim(filename)//' ...'

      iunit = 12
      open(iunit, file=filename, status='replace')
      call write_options_file(iunit, options)
      close(iunit)
      woptdone = .true.

    end if

  end do

1002 format(/' Input > ',$)

end subroutine write_options

!=============================================================================80
!
! Subroutine to write out file containing current program options
!
!=============================================================================80
subroutine write_options_file(iunit, options)

  Use vardef, only : options_type

  type(options_type), intent(in) :: options
  integer, intent(in) :: iunit

! Surface options

  write(iunit,'(A)') '&SOPT'
  write(iunit,*) ' nsrf = ', options%nsrfdefault
  write(iunit,*) ' lesp = ', options%lesp
  write(iunit,*) ' tesp = ', options%tesp
  write(iunit,*) ' radi = ', options%radi
  write(iunit,*) ' nwke = ', options%nwake
  write(iunit,*) ' fdst = ', options%fdst
  write(iunit,*) ' fwkl = ', options%fwkl
  write(iunit,*) ' fwki = ', options%fwki
  write(iunit,'(A)') '/'

! Volume grid options

  write(iunit,'(A)') '&VOPT'
  write(iunit,*) " name = '"//trim(options%project_name)//"'"
  write(iunit,*) ' jmax = ', options%jmax
  write(iunit,*) " slvr = '"//trim(options%slvr)//"'"
  write(iunit,*) " topo = '"//options%topology//"'"
  write(iunit,*) ' ypls = ', options%yplus
  write(iunit,*) ' recd = ', options%Re
  write(iunit,*) ' cfrc = ', options%cfrac
  write(iunit,*) ' stp1 = ', options%maxsteps
  write(iunit,*) ' stp2 = ', options%fsteps
  write(iunit,*) ' nrmt = ', options%nrmt
  write(iunit,*) ' nrmb = ', options%nrmb
  write(iunit,*) ' alfa = ', options%alfa
  write(iunit,*) ' epsi = ', options%epsi
  write(iunit,*) ' epse = ', options%epse
  write(iunit,*) ' funi = ', options%funi
  write(iunit,*) ' asmt = ', options%asmt
  write(iunit,'(A)') '/'

! Grid output options

  write(iunit,'(A)') '&OOPT'
  write(iunit,*) ' gdim = ', options%griddim
  write(iunit,*) ' npln = ', options%nplanes
  write(iunit,*) ' dpln = ', options%plane_delta
  write(iunit,*) ' f3dm = ', options%f3d_compat
  write(iunit,'(A)') '/'

end subroutine write_options_file

!=============================================================================80
!
! Subroutine to write out surface grid in tecplot format
!
!=============================================================================80
subroutine write_srf_grid_tecplot(iunit, grid, griddim, nplanes, deltplane)

  Use vardef, only : srf_grid_type

  type(srf_grid_type), intent(in) :: grid
  integer, intent(in) :: iunit, griddim, nplanes
  double precision, intent(in) :: deltplane

  integer imax, jmax, i, j, k, kmax
  logical threed

  imax = grid%imax
  jmax = grid%jmax
  kmax = nplanes

  threed = .false.
  if (griddim == 3 .and. nplanes > 1) threed = .true.

! Write Tecplot format to output file

  if (threed) then
    write(iunit,*)  'TITLE = "Generated by Construct2D"'
    write(iunit,*)  'VARIABLES = "CoordinateX" "CoordinateY" "CoordinateZ"'
    write(iunit,*)  'ZONE T="naca", I=',imax,', J=',jmax,', K=', jmax,', F=BLOCK'
    do k=1,kmax
      do j=1,jmax
        do i=1,imax
          write(iunit,'(es17.8)') grid%x(i,j)
        enddo
      enddo
    enddo
    do k=1,kmax
      do j=1,jmax
        do i=1,imax
          write(iunit,'(es17.8)') grid%y(i,j)
        enddo
      enddo
    enddo
    do k=1,kmax
      do j=1,jmax
        do i=1,imax
          write(iunit,'(es17.8)') dble(k-1)*deltplane
        enddo
      enddo
    enddo
  else
    write(iunit,*)  'TITLE = "Generated by Construct2D"'
    write(iunit,*)  'VARIABLES = "CoordinateX" "CoordinateY"'
    write(iunit,*)  'ZONE T="naca", I=',imax,', J=',jmax,', F=BLOCK'
    do j=1,jmax
      do i=1,imax
        WRITE (iunit,'(es17.8)') grid%x(i,j)
      enddo
    enddo
    do j=1,jmax
      do i=1,imax
        WRITE (iunit,'(es17.8)') grid%y(i,j)
      enddo
    enddo
  endif

end subroutine write_srf_grid_tecplot

!=============================================================================80
!
! Subroutine to write out surface grid in plot3d format
!
!=============================================================================80
subroutine write_srf_grid(iunit, grid, griddim, nplanes, deltplane)

  Use vardef, only : srf_grid_type

  type(srf_grid_type), intent(in) :: grid
  integer, intent(in) :: iunit, griddim, nplanes
  double precision, intent(in) :: deltplane

  integer imax, jmax, i, j, k, kmax
  logical threed

  imax = grid%imax
  jmax = grid%jmax
  kmax = nplanes

  threed = .false.
  if (griddim == 3 .and. nplanes > 1) threed = .true.

! Write header to output file

  if (threed) then
    write(iunit,*) 1
    write(iunit,*) imax, kmax, jmax
  else
    write(iunit,*) imax, jmax
  end if

! Write out grid - in reverse i direction to preserve positive volumes

  if (threed) then

    do j = 1, jmax
    do k = 1, kmax
    do i = imax, 1, -1
      write(iunit,'(es17.8)') grid%x(i,j)
    end do
    end do
    end do

    do j = 1, jmax
    do k = 1, kmax
    do i = imax, 1, -1
      write(iunit,'(es17.8)') dble(k-1)*deltplane
    end do
    end do
    end do

    do j = 1, jmax
    do k = 1, kmax
    do i = imax, 1, -1
      write(iunit,'(es17.8)') grid%y(i,j)
    end do
    end do
    end do

  else

    write(iunit,'(es17.8)')                                                    &
         ((grid%x(i,j), i=imax,1,-1), j=1,jmax),                               &
         ((grid%y(i,j), i=imax,1,-1), j=1,jmax)

  end if

end subroutine write_srf_grid

!=============================================================================80
!
! Subroutine to write out grid quality stats in plot3d format
!
!=============================================================================80
subroutine write_quality_stats(iunit, qstats, griddim, nplanes)

  Use vardef, only : grid_stats_type

  type(grid_stats_type), intent(in) :: qstats
  integer, intent(in) :: iunit, griddim, nplanes

  integer imax, jmax, kmax, i, j, k
  logical threed

  imax = size(qstats%ang1,1)
  jmax = size(qstats%ang1,2)
  kmax = nplanes

  threed = .false.
  if (griddim == 3 .and. nplanes > 1) threed = .true.

! Write comments giving title and variables

  write(iunit,'(A)') '#Grid quality information'
  write(iunit,'(A)') '#skew angle, xi-growth, eta-growth'

! Write Plot3D header

  if (threed) then
    write(iunit,*) imax, kmax, jmax, 3
  else
    write(iunit,*) imax, jmax, 1, 3
  end if

! Write out grid stats - in reverse order to preserve positive volumes

  if (threed) then

    do j = 1, jmax
    do k = 1, kmax
    do i = imax, 1, -1
      write(iunit,'(es17.8)') qstats%skewang(i,j)
    end do
    end do
    end do

    do j = 1, jmax
    do k = 1, kmax
    do i = imax, 1, -1
      write(iunit,'(es17.8)') qstats%growthz(i,j)
    end do
    end do
    end do

    do j = 1, jmax
    do k = 1, kmax
    do i = imax, 1, -1
      write(iunit,'(es17.8)') qstats%growthn(i,j)
    end do
    end do
    end do

  else

    write(iunit,'(es17.8)')                                                    &
         ((qstats%skewang(i,j), i=imax,1,-1), j=1,jmax),                       &
         ((qstats%growthz(i,j), i=imax,1,-1), j=1,jmax),                       &
         ((qstats%growthn(i,j), i=imax,1,-1), j=1,jmax)

  end if

end subroutine write_quality_stats

!=============================================================================80
!
! Subroutine to write boundary conditions file (.nmf format)
!
!=============================================================================80
subroutine write_bc_file(iunit, options)

  Use vardef, only : options_type

  integer, intent(in) :: iunit
  type(options_type), intent(in) :: options

  integer :: imax, jmax, kmax, i

  character(90) :: text1
  logical :: threed

  threed = .false.
  if (options%griddim == 3 .and. options%nplanes > 1) threed = .true.

! Determine grid dimensions

  if (threed) then
    imax = options%imax
    jmax = options%nplanes
    kmax = options%jmax
  else
    imax = options%imax
    jmax = options%jmax
    kmax = 1
  end if

! Write main header

  write(iunit,'(A)'), '# ==================== '//                              &
                      'Neutral Map File generated by Construct2D '//           &
                      '===================='
  write(iunit,'(A)'), '# ==================== '//                              &
                      '========================================= '//           &
                      '===================='
  write(iunit,'(A)'), '# Block#   IDIM   JDIM   KDIM'
  write(iunit,'(A)'), '# ---------------------'//                              &
                      '------------------------------------------'//           &
                      '--------------------'

! Write grid dimensions

  do i = 1, 90
    text1(i:i) = ' '
  end do
  call place_integer_in_string(1, text1, 8)
  write(iunit,'(A)'), trim(text1)
  write(iunit,'(A)')
  do i = 1, 90
    text1(i:i) = ' '
  end do
  call place_integer_in_string(1, text1, 8)
  call place_integer_in_string(imax, text1, 15)
  call place_integer_in_string(jmax, text1, 22)
  call place_integer_in_string(kmax, text1, 29)
  write(iunit,'(A)'), trim(text1)
  write(iunit,'(A)')

! Write second header
  write(iunit,'(A)'), '# ====================='//                              &
                      '=========================================='//           &
                      '===================='
  do i = 3, 90
    text1(i:i) = ' '
  end do
  text1(1:2) = '# '
  call place_substring_in_string('Type', text1, 6)
  call place_substring_in_string('B1', text1, 17)
  call place_substring_in_string('F1', text1, 21)
  call place_substring_in_string('S1', text1, 28)
  call place_substring_in_string('E1', text1, 33)
  call place_substring_in_string('S2', text1, 40)
  call place_substring_in_string('E2', text1, 45)
  call place_substring_in_string('B2', text1, 51)
  call place_substring_in_string('F2', text1, 55)
  call place_substring_in_string('S1', text1, 62)
  call place_substring_in_string('E1', text1, 67)
  call place_substring_in_string('S2', text1, 74)
  call place_substring_in_string('E2', text1, 79)
  call place_substring_in_string('Swap', text1, 85)
  write(iunit,'(A)'), trim(text1)
  do i = 3, 90
    text1(i:i) = ' '
  end do
  if (.not. options%f3d_compat) then
    text1(1:2) = '# '
    call place_substring_in_string('Compute forces (walls)', text1, 85)
    write(iunit,'(A)'), trim(text1)
  end if
  write(iunit,'(A)'), '# ---------------------'//                              &
                      '------------------------------------------'//           &
                      '--------------------'

! Write boundary conditions

  do i = 1, 90
    text1(i:i) = ' '
  end do

  dimensions: if (threed) then

    topology_3d: if (options%topology == 'OGRD') then

!     Face number 1: k = kmin

      if (.not. options%f3d_compat) then
        text1(1:7) = 'VISCOUS'
      else
        text1(1:13) = 'viscous_solid'
      end if
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(1, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(imax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(jmax, text1, 45)
      if (.not. options%f3d_compat) then
        call place_substring_in_string('TRUE', text1, 85)
      end if
      write(iunit,'(A)'), trim(text1)

!     Face number 2: k = kmax

      do i = 1, 90
        text1(i:i) = ' '
      end do

      if (.not. options%f3d_compat) then
        text1(1:8) = 'FARFIELD'
      else
        text1(1:13) = 'farfield_riem'
      end if
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(2, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(imax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(jmax, text1, 45)
      write(iunit,'(A)'), trim(text1)

!     Face number 3: i = imin (one_to_one with face 4)

      do i = 1, 90
        text1(i:i) = ' '
      end do

      text1(1:10) = 'ONE_TO_ONE'
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(3, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(jmax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(kmax, text1, 45)
      call place_integer_in_string(1, text1, 51)
      call place_integer_in_string(4, text1, 55)
      call place_integer_in_string(1, text1, 62)
      call place_integer_in_string(jmax, text1, 67)
      call place_integer_in_string(1, text1, 74)
      call place_integer_in_string(kmax, text1, 79)
      call place_substring_in_string('FALSE', text1, 85)
      write(iunit,'(A)'), trim(text1)

!     Face number 5: j = jmin

      do i = 1, 90
        text1(i:i) = ' '
      end do

      if (.not. options%f3d_compat) then
        text1(1:10) = 'SYMMETRY-Y'
      else
        text1(1:10) = 'symmetry_y'
      end if
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(5, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(kmax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(imax, text1, 45)
      write(iunit,'(A)'), trim(text1)

!     Face number 6: j = jmax

      do i = 1, 90
        text1(i:i) = ' '
      end do

      if (.not. options%f3d_compat) then
        text1(1:10) = 'SYMMETRY-Y'
      else
        text1(1:10) = 'symmetry_y'
      end if
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(6, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(kmax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(imax, text1, 45)
      write(iunit,'(A)'), trim(text1)

    else topology_3d

!     Face number 1: k = kmin, (one_to_one mapping at cut)

      text1(1:10) = 'ONE_TO_ONE'
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(1, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(options%nwake+1, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(jmax, text1, 45)
      call place_integer_in_string(1, text1, 51)
      call place_integer_in_string(1, text1, 55)
      call place_integer_in_string(imax, text1, 62)
      call place_integer_in_string(imax-options%nwake, text1, 67)
      call place_integer_in_string(1, text1, 74)
      call place_integer_in_string(jmax, text1, 79)
      call place_substring_in_string('FALSE', text1, 85)
      write(iunit,'(A)'), trim(text1)

!     Face number 1: k = kmin, airfoil surface

      do i = 1, 90
        text1(i:i) = ' '
      end do

      if (.not. options%f3d_compat) then
        text1(1:7) = 'VISCOUS'
      else
        text1(1:13) = 'viscous_solid'
      end if
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(1, text1, 21)
      call place_integer_in_string(options%nwake+1, text1, 28)
      call place_integer_in_string(imax-options%nwake, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(jmax, text1, 45)
      if (.not. options%f3d_compat) then
        call place_substring_in_string('TRUE', text1, 85)
      end if
      write(iunit,'(A)'), trim(text1)

!     Face number 2: k = kmax

      do i = 1, 90
        text1(i:i) = ' '
      end do

      if (.not. options%f3d_compat) then
        text1(1:8) = 'FARFIELD'
      else
        text1(1:13) = 'farfield_riem'
      end if
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(2, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(imax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(jmax, text1, 45)
      write(iunit,'(A)'), trim(text1)

!     Face number 3: i = imin (outlet)

      do i = 1, 90
        text1(i:i) = ' '
      end do

      if (.not. options%f3d_compat) then
        text1(1:8) = 'FARFIELD'
      else
        text1(1:13) = 'farfield_riem'
      end if
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(3, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(jmax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(kmax, text1, 45)
      write(iunit,'(A)'), trim(text1)

!     Face number 4: i = imax (outlet)

      do i = 1, 90
        text1(i:i) = ' '
      end do

      if (.not. options%f3d_compat) then
        text1(1:8) = 'FARFIELD'
      else
        text1(1:13) = 'farfield_riem'
      end if
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(4, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(jmax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(kmax, text1, 45)
      write(iunit,'(A)'), trim(text1)

!     Face number 5: j = jmin

      do i = 1, 90
        text1(i:i) = ' '
      end do

      if (.not. options%f3d_compat) then
        text1(1:10) = 'SYMMETRY-Y'
      else
        text1(1:10) = 'symmetry_y'
      end if
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(5, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(kmax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(imax, text1, 45)
      write(iunit,'(A)'), trim(text1)

!     Face number 6: j = jmax

      do i = 1, 90
        text1(i:i) = ' '
      end do

      if (.not. options%f3d_compat) then
        text1(1:10) = 'SYMMETRY-Y'
      else
        text1(1:10) = 'symmetry_y'
      end if
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(6, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(kmax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(imax, text1, 45)
      write(iunit,'(A)'), trim(text1)

    end if topology_3d

  else dimensions

    topology_2d: if (options%topology == 'OGRD') then

!     Face number 1: i = imin (one_to_one with face 2)

      text1(1:10) = 'ONE_TO_ONE'
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(1, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(jmax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(kmax, text1, 45)
      call place_integer_in_string(1, text1, 51)
      call place_integer_in_string(2, text1, 55)
      call place_integer_in_string(1, text1, 62)
      call place_integer_in_string(jmax, text1, 67)
      call place_integer_in_string(1, text1, 74)
      call place_integer_in_string(kmax, text1, 79)
      call place_substring_in_string('FALSE', text1, 85)
      write(iunit,'(A)'), trim(text1)

!     Face number 3: j = jmin

      do i = 1, 90
        text1(i:i) = ' '
      end do

      text1(1:7) = 'VISCOUS'
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(3, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(imax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(kmax, text1, 45)
      call place_substring_in_string('TRUE', text1, 85)
      write(iunit,'(A)'), trim(text1)

!     Face number 4: j = jmax

      do i = 1, 90
        text1(i:i) = ' '
      end do

      text1(1:8) = 'FARFIELD'
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(4, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(imax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(kmax, text1, 45)
      write(iunit,'(A)'), trim(text1)

    else topology_2d

!     Face number 1: i = imin (outlet)

      text1(1:8) = 'FARFIELD'
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(1, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(jmax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(kmax, text1, 45)
      write(iunit,'(A)'), trim(text1)

!     Face number 2: i = imax (outlet)

      do i = 1, 90
        text1(i:i) = ' '
      end do

      text1(1:8) = 'FARFIELD'
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(2, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(jmax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(kmax, text1, 45)
      write(iunit,'(A)'), trim(text1)

!     Face number 3: j = jmin (one_to_one mapping at cut)

      do i = 1, 90
        text1(i:i) = ' '
      end do

      text1(1:10) = 'ONE_TO_ONE'
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(3, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(options%nwake+1, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(kmax, text1, 45)
      call place_integer_in_string(1, text1, 51)
      call place_integer_in_string(3, text1, 55)
      call place_integer_in_string(imax, text1, 62)
      call place_integer_in_string(imax-options%nwake, text1, 67)
      call place_integer_in_string(1, text1, 74)
      call place_integer_in_string(kmax, text1, 79)
      call place_substring_in_string('FALSE', text1, 85)
      write(iunit,'(A)'), trim(text1)

!     Face number 3: j = jmin (airfoil surface)

      do i = 1, 90
        text1(i:i) = ' '
      end do

      text1(1:7) = 'VISCOUS'
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(3, text1, 21)
      call place_integer_in_string(options%nwake+1, text1, 28)
      call place_integer_in_string(imax-options%nwake, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(kmax, text1, 45)
      call place_substring_in_string('TRUE', text1, 85)
      write(iunit,'(A)'), trim(text1)

!     Face number 4: j = jmax (farfield)

      do i = 1, 90
        text1(i:i) = ' '
      end do

      text1(1:8) = 'FARFIELD'
      call place_integer_in_string(1, text1, 17)
      call place_integer_in_string(4, text1, 21)
      call place_integer_in_string(1, text1, 28)
      call place_integer_in_string(imax, text1, 33)
      call place_integer_in_string(1, text1, 40)
      call place_integer_in_string(kmax, text1, 45)
      write(iunit,'(A)'), trim(text1)

    end if topology_2d

  end if dimensions

end subroutine write_bc_file

!=============================================================================80
!
! Replaces text in a string with a (positive) integer
! Placement is determined by index of last character to be replaced
!
!=============================================================================80
subroutine place_integer_in_string(val, string, rindex)

  integer, intent(in) :: val, rindex
  character(*), intent(inout) :: string

  integer :: intlen, lindex
  character(20) :: text

! Determine length of integer

  intlen = ceiling(log10(dble(val)+0.1))

! Determine first index to replace

  lindex = rindex - intlen + 1

! Write integer as a string

  write(text,*) val
  text = adjustl(text)

! Place in string

  string(lindex:rindex) = trim(text)

end subroutine place_integer_in_string

!=============================================================================80
!
! Replaces text in a string with a substring
! Placement is determined by index of last character to be replaced
!
!=============================================================================80
subroutine place_substring_in_string(substring, string, rindex)

  integer, intent(in) :: rindex
  character(*), intent(in) :: substring
  character(*), intent(inout) :: string

  integer :: sublen, lindex

! Determine length of substring

  sublen = len(substring)

! Determine first index to replace

  lindex = rindex - sublen + 1

! Place in string

  string(lindex:rindex) = trim(substring)

end subroutine place_substring_in_string

end module util
