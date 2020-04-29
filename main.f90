module main
  Use vardef, only : options_type

implicit none

type mytype
  double precision :: val
  double precision, dimension(:), allocatable :: consvar
end type mytype

contains

subroutine set_options(n, project_name, options)
  implicit none
  integer(4), intent(in)   :: n
  character(n), intent(in) :: project_name
  type(options_type)       :: options

  options%project_name = project_name
end subroutine set_options

subroutine print_options(options)
  implicit none
  type(options_type) :: options

  print *,'Running print_options'
  print *,'options%project_name = ',options%project_name
  print *,'options%imax = ',options%imax
  print *,'options%jmax = ',options%jmax

end subroutine print_options

subroutine mysubroutine(a, b, tt)
  implicit none
  type(mytype) :: tt
  double precision, intent(in) :: a
  double precision, intent(inout) :: b

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

end module main
