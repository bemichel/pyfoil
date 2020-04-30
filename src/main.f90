module main
  Use vardef, only : options_type

implicit none

type mytype
  double precision :: val
  double precision, dimension(:), allocatable :: consvar
end type mytype

contains

subroutine mysubroutine(a, b, tt)
  implicit none
  type(mytype) :: tt
  double precision, intent(in) :: a
  double precision, dimension(:), intent(inout) :: b

  print *,'Running mysubroutine'
  b = a*2
  tt%val = a*3

end subroutine mysubroutine

subroutine allouemonarraycheri(size, tt)
  implicit none
  type(mytype) :: tt
  integer, intent(in) :: size
  integer :: i
  print *,'Running allouemonarraycheri', size
  if (allocated(tt%consvar))then
    print *,'Already allocated'
  end if
  allocate(tt%consvar(size))
  do i=1, size
    tt%consvar(i) = dble(i)
  end do

end subroutine allouemonarraycheri

!=============================================================================80
!
! Driver subroutine to execute user commands
!
!=============================================================================80
subroutine compute_grid(whichgrid, surf, options)
  Use vardef,       only : airfoil_surface_type, options_type,      &
                           srf_grid_type, grid_stats_type
  Use edge_grid,    only : transform_airfoil
  Use surface_grid, only : create_grid, copy_edges, compute_quality_stats
  Use memory,       only : grid_allocation, grid_deallocation,      &
                           qstats_allocation, qstats_deallocation
  Use util,         only : write_srf_grid, write_srf_grid_tecplot,  &
                           write_quality_stats, write_bc_file

  character(4), intent(in)                  :: whichgrid
  type(airfoil_surface_type), intent(inout) :: surf
  type(options_type), intent(inout)         :: options

  type(airfoil_surface_type) :: newsurf
  type(srf_grid_type)        :: grid
  type(grid_stats_type)      :: qstats
  integer iunit

  write(*,*) 'Generate grid for airfoil'
  write(*,*)

! Translate and scale buffer airfoil
  call transform_airfoil(surf%x, surf%y)

  ! print *,"compute_grid: surf%x = ",surf%x
  ! print *,"compute_grid: surf%y = ",surf%y
  ! print *,"compute_grid: surf%npoints = ",surf%npoints

  if (whichgrid == 'SMTH') then
    write(*,*) '  SMTH  Smoothed airfoil using current surface grid options'
    write(*,*) '        (recommended)'

!   Allocate new airfoil
    ! print *,"compute_grid: surf%tegap = ",surf%tegap
    ! print *,"compute_grid: options%nsrfdefault = ",options%nsrfdefault
    options%nsrf = options%nsrfdefault
    ! print *,"compute_grid: options%nsrf = ",options%nsrf
    if (surf%tegap) then
      newsurf%tegap = .true.
      newsurf%npoints = options%nsrf - options%nte - 1
    else
      newsurf%tegap = .false.
      newsurf%npoints = options%nsrf
    end if
    ! print *,"compute_grid: newsurf%tegap   = ",newsurf%tegap
    ! print *,"compute_grid: newsurf%npoints = ",newsurf%npoints
    allocate(newsurf%x(newsurf%npoints))
    allocate(newsurf%y(newsurf%npoints))
    newsurf%x = 0.d0
    newsurf%y = 0.d0
    ! print *,"compute_grid: newsurf%x = ",newsurf%x
    ! print *,"compute_grid: newsurf%y = ",newsurf%y

!   Use XFoil pangen routine

    call pangen(newsurf%x, newsurf%y, newsurf%npoints, surf%x, surf%y,   &
                surf%npoints, 1.d0, 0.50d0, 0.2d0, 1.d0, 1.d0, 1.d0,     &
                1.d0)
    ! print *,"compute_grid: after pangen, newsurf%x = ",newsurf%x
    ! print *,"compute_grid: after pangen, newsurf%y = ",newsurf%y

    call transform_airfoil(newsurf%x, newsurf%y)

!   Set number of points in i direction

    if (options%topology == 'OGRD') then
      options%imax = options%nsrf
    else
      options%imax = options%nsrf + options%nwake*2
    end if

! Allocate grid

    grid%imax = options%imax
    grid%jmax = options%jmax
    call grid_allocation(grid)

!   Create surface grid

    call create_grid(newsurf, options, grid, .true.)

!   Deallocate new airfoil

    deallocate(newsurf%x)
    deallocate(newsurf%y)

  else
    write(*,*) '  BUFF  Buffer airfoil defined directly by loaded geometry'
    write(*,*) '        (nsrf will be set to number of points in buffer airfoil)'

!   Reset nsrf if user has changed it; must match airfoil geometry

    if (surf%tegap) then
      options%nsrf = surf%npoints + options%nte + 1
      write(*,*)
      write(*,*) "Error: trailing edge gap detected. Buffer airfoil "    &
                 //"must be closed"
      write(*,*) "to use BUFF option."
      stop
    else
      options%nsrf = surf%npoints
    end if

!   Set number of points in i direction

    if (options%topology == 'OGRD') then
      options%imax = options%nsrf
    else
      options%imax = options%nsrf + options%nwake*2
    end if

! Allocate grid

    grid%imax = options%imax
    grid%jmax = options%jmax
    call grid_allocation(grid)

!   Create surface grid

    call create_grid(surf, options, grid, .false.)

  end if

! Copy edges for O- or C-grid

  call copy_edges(grid, options%topology)

! Compute grid quality information

  call qstats_allocation(qstats, grid%imax, grid%jmax)
  call compute_quality_stats(grid, qstats)

! Write out grid
  iunit = 12
  open(iunit, file=trim(options%project_name)//'.dat', status='replace')
  write(*,*)
  write(*,*) 'Writing grid to file '//trim(options%project_name)//'.dat ...'
  call write_srf_grid_tecplot(iunit, grid, options%griddim, options%nplanes,           &
                              options%plane_delta)
  close(iunit)

! Write out grid

  iunit = 12
  open(iunit, file=trim(options%project_name)//'.p3d', status='replace')
  write(*,*)
  write(*,*) 'Writing grid to file '//trim(options%project_name)//'.p3d ...'
  call write_srf_grid(iunit, grid, options%griddim, options%nplanes,           &
                      options%plane_delta)
  close(iunit)

! Write grid quality information to file

  iunit = 13
  open(iunit, file=trim(options%project_name)//'_stats.p3d', status='replace')
  write(*,*)
  write(*,*) 'Writing grid quality information to file '//                     &
             trim(options%project_name)//'_stats.p3d ...'
  call write_quality_stats(iunit, qstats, options%griddim, options%nplanes)
  close(iunit)

! Deallocate grid

  call grid_deallocation(grid)
  call qstats_deallocation(qstats)

! Write boundary conditions file (.nmf format)

  iunit = 14
  open(iunit, file=trim(options%project_name)//'.nmf', status='replace')
  write(*,*)
  write(*,*) 'Writing boundary conditions file '//                             &
             trim(options%project_name)//'.nmf ...'
  call write_bc_file(iunit, options)
  close(iunit)


end subroutine compute_grid

end module main
