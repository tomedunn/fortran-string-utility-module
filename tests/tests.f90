program main
  use string_utility_module
  implicit none
  ! local variables:
  character(:), allocatable :: str
  logical, allocatable :: results(:)

  write(*,'("subroutine str_convert_to_lowercase:")')
  call TEST_str_convert_to_lowercase(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)

  write(*,'("subroutine str_convert_to_uppercase:")')
  call TEST_str_convert_to_uppercase(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)

  write(*,'("function str_is_integer:")')
  call TEST_str_is_integer(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)

  write(*,'("function str_is_real:")')
  call TEST_str_is_real(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)

contains

  subroutine TEST_str_convert_to_lowercase( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: nTests = 1
    character(len=*), parameter :: str_lo = 'abcdefghijklmnopqrstuvwxyz'
    character(len=*), parameter :: str_up = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    ! local variables:
    character(:), allocatable :: str

    allocate(results(nTests))

    str = str_up
    call str_convert_to_lowercase(str)
    results(1) = str == str_lo
  end subroutine TEST_str_convert_to_lowercase

  subroutine TEST_str_convert_to_uppercase( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: nTests = 1
    character(len=*), parameter :: str_lo = 'abcdefghijklmnopqrstuvwxyz'
    character(len=*), parameter :: str_up = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    ! local variables:
    character(:), allocatable :: str

    allocate(results(nTests))

    str = str_lo
    call str_convert_to_uppercase(str)
    results(1) = str == str_up
  end subroutine TEST_str_convert_to_uppercase

  subroutine TEST_str_is_integer( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: nTests = 4

    allocate(results(nTests))

    results(1) = str_is_integer(' 1     ')
    results(2) = str_is_integer(' +1    ')
    results(3) = str_is_integer(' +1e0  ')
    results(4) = str_is_integer(' +1e+0 ')
  end subroutine TEST_str_is_integer

  subroutine TEST_str_is_real( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: nTests = 8

    allocate(results(nTests))

    results(1) = str_is_real(' 1       ')
    results(2) = str_is_real(' +1      ')
    results(3) = str_is_real(' +1.     ')
    results(4) = str_is_real(' +1.0    ')
    results(5) = str_is_real(' +1.d0   ')
    results(6) = str_is_real(' +1.0d0  ')
    results(7) = str_is_real(' +1.0d+0 ')
    results(8) = str_is_real(' +1.0d+0 ')
  end subroutine TEST_str_is_real
end program main
