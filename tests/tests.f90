!===============================================================================
program main
!===============================================================================
  use string_utility_module
  implicit none
  ! local variables:
  character(:), allocatable :: str
  logical, allocatable :: results(:)


  write(*,'("subroutine STR_CONVERT_TO_LOWERCASE:")')
  call TEST_str_convert_to_lowercase(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)


  write(*,'("subroutine STR_CONVERT_TO_UPPERCASE:")')
  call TEST_str_convert_to_uppercase(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)


  write(*,'("function STR_IS_INTEGER:")')
  call TEST_str_is_integer(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)


  write(*,'("function STR_IS_NAME:")')
  call TEST_str_is_name(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)


  write(*,'("function STR_IS_NUMBER:")')
  call TEST_str_is_number(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)


  write(*,'("function STR_IS_REAL:")')
  call TEST_str_is_real(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)


  write(*,'("function STR_LOC_NEXT_REAL:")')
  call TEST_str_loc_next_real(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)


  write(*,'("subroutine STR_PARSE_NEXT_DELIMITER:")')
  call TEST_str_parse_next_delimiter(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)


  write(*,'("subroutine STR_PARSE_NEXT_SPACE:")')
  call TEST_str_parse_next_space(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)


  write(*,'("subroutine STR_PARSE_NEXT_TOKEN:")')
  call TEST_str_parse_next_token(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)

contains

!===============================================================================
! TEST_str_convert_to_lowercase:
!
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
!===============================================================================

!===============================================================================
! TEST_str_convert_to_uppercase:
!
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
!===============================================================================

!===============================================================================
! TEST_str_is_integer:
!
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
!===============================================================================

!===============================================================================
! TEST_str_is_name:
!
  subroutine TEST_str_is_name( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: nTests = 12

    allocate(results(nTests))

    results(01) = str_is_name(' n        ')
    results(02) = str_is_name(' name     ')
    results(03) = str_is_name(' name_    ')
    results(04) = str_is_name(' name_1   ')
    results(05) = str_is_name(' name_one ')
    results(06) = str_is_name(' name1    ')
    results(07) = str_is_name(' name1_   ')

    results(08) = .not.str_is_name(' _      ')
    results(09) = .not.str_is_name(' _name  ')
    results(10) = .not.str_is_name(' 1      ')
    results(11) = .not.str_is_name(' 1name  ')
    results(12) = .not.str_is_name(' name 1 ')
  end subroutine TEST_str_is_name
!===============================================================================

!===============================================================================
! TEST_str_is_number:
!
  subroutine TEST_str_is_number( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: nTests = 12

    allocate(results(nTests))

    results(01) = str_is_number(' 1       ')
    results(02) = str_is_number(' +1      ')
    results(03) = str_is_number(' +1e0    ')
    results(04) = str_is_number(' +1e+0   ')
    results(05) = str_is_number(' +1.     ')
    results(06) = str_is_number(' +1.0    ')
    results(07) = str_is_number(' +1.d0   ')
    results(08) = str_is_number(' +1.0d0  ')
    results(09) = str_is_number(' +1.0d+0 ')
    results(10) = str_is_number(' +1.0d+0 ')

    results(11) = .not.str_is_number(' -       ')
    results(12) = .not.str_is_number(' .d+00   ')
  end subroutine TEST_str_is_number
!===============================================================================

!===============================================================================
! TEST_str_is_real:
!
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
!===============================================================================

!===============================================================================
! TEST_str_loc_next_real:
!
  subroutine TEST_str_loc_next_real( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: nTests = 1

    allocate(results(nTests))

    results(1) = 6 == str_loc_next_real('word;1.0;')
  end subroutine TEST_str_loc_next_real
!===============================================================================

!===============================================================================
! TEST_str_parse_next_delimiter:
!
  subroutine TEST_str_parse_next_delimiter( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: nTests = 2
    ! local variables:
    integer :: i
    character(:), allocatable :: str
    character(:), allocatable :: delimiter

    allocate(results(nTests))

    i = 1
    str = 'word space  double-space'
    call str_parse_next_delimiter(str, i, delimiter)
    results(1) = delimiter == ' ' .and. i == 6

    call str_parse_next_delimiter(str, i, delimiter)
    results(2) = delimiter == '  ' .and. i == 13
  end subroutine TEST_str_parse_next_delimiter
!===============================================================================

!===============================================================================
! TEST_str_parse_next_space:
!
  subroutine TEST_str_parse_next_space( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: nTests = 2
    ! local variables:
    integer :: i
    character(:), allocatable :: str
    character(:), allocatable :: space

    allocate(results(nTests))

    i = 1
    str = 'word space  double-space'
    call str_parse_next_space(str, i, space)
    results(1) = space == ' ' .and. i == 6

    call str_parse_next_space(str, i, space)
    results(2) = space == '  ' .and. i == 13
  end subroutine TEST_str_parse_next_space
!===============================================================================

!===============================================================================
! TEST_str_parse_next_token:
!
  subroutine TEST_str_parse_next_token( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: nTests = 3
    ! local variables:
    integer :: i
    character(:), allocatable :: str
    character(:), allocatable :: token

    allocate(results(nTests))

    i = 1
    str = 'word; 100, w10'
    call str_parse_next_token(str, i, token)
    results(1) = token == 'word' .and. i == 5

    call str_parse_next_token(str, i, token)
    results(2) = token == '100' .and. i == 10

    call str_parse_next_token(str, i, token)
    results(3) = token == 'w10' .and. i == 15
  end subroutine TEST_str_parse_next_token
!===============================================================================
end program main
!===============================================================================
