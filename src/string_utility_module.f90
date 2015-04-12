!===============================================================================
module string_utility_module
!===============================================================================
  use, intrinsic :: iso_fortran_env
  implicit none

  private

  ! public procedures:
  public :: convert_to_lower_case
  public :: convert_to_upper_case
  public :: is_integer
  public :: is_real
  public :: lower_case
  public :: upper_case

  ! parameters:
  integer, parameter :: CK = selected_char_kind('DEFAULT')
  !integer, parameter :: LK = logical_kinds(min(3,size(logical_kinds)))
  character(kind=CK,len=*), parameter :: space = ' '
  character(kind=CK,len=*), parameter :: tab   = achar(9)


contains

!===============================================================================
! convert_to_lower_case:
!
!   Converts upper case characters in the given character string to lower case.
!
  pure subroutine convert_to_lower_case( str )
    character(kind=CK,len=*), intent(inout) :: str
    ! local variables:
    integer :: i, ic

    do i = 1, len(str)
      ic = ichar(str(i:i))
      if (64 < ic .and. ic < 91) str(i:i) = char(ic + 32)
    end do
  end subroutine convert_to_lower_case
!===============================================================================

!===============================================================================
! convert_to_upper_case:
!
!   Takes character array and coverts all characters a-z to upper-case.
!   Conversion is done in place.
!
  pure subroutine convert_to_upper_case( str )
    character(kind=CK,len=*), intent(inout) :: str
    ! local variables:
    integer :: i, ic

    do i = 1, len(str)
      ic = ichar(str(i:i))
      if (96 < ic .and. ic < 123) str(i:i) = char(ic - 32)
    end do
  end subroutine convert_to_upper_case
!===============================================================================


!===============================================================================
! lower_case:
!
!   Returns given string with all characters A-Z converted to lower case.
!
  pure function lower_case( str ) result( lstr )
    character(kind=CK,len=*), intent(in) :: str
    character(:), allocatable :: lstr
    lstr = str
    call convert_to_lower_case(lstr)
  end function lower_case
!===============================================================================


!===============================================================================
! upper_case:
!
!   Returns given string with all characters a-z converted to upper case.
!
  pure function upper_case( str ) result( ustr )
    character(kind=CK,len=*), intent(in) :: str
    character(:), allocatable :: ustr
    ustr = str
    call convert_to_upper_case(ustr)
  end function upper_case
!===============================================================================

!===============================================================================
! is_integer:
!
!   Returns .TRUE. if the given string contains an integer value. Returns
!   .FALSE. otherwise.
!
!       ans   |F |F      |T  |F    |F      |T    |T|
!       stage |0 |1      |2  |3    |4      |5    |6|
!       regex |   [\+\-]? \d* ([eE] [\+\-]? \d+)?  |
!
  function is_integer( str ) result( ans )
    character(kind=CK,len=*), intent(in) :: str
    logical :: ans
    !logical(LK) :: ans
    ! local variables:
    integer :: i
    logical :: not_integer
    integer :: stage

    ! determine number type and length
    stage = 0
    not_integer = .false.
    do i = 1, len(str)
      select case(str(i:i))
      case (space,tab)
        ! white space
        !not_integer = .not.allow_white_spaces
        select case (stage)
        case (0,6)
          continue
        case (2,5)
          stage = 6
        case default
          not_integer = .true.
        end select
      case ('-')
        ! minus sign
        select case(stage)
        case(0)
          stage = 1
        case default
          not_integer = .true.
        end select
      case ('+')
        ! plus sign
        select case(stage)
        case(0)
          stage = 1
        case(3)
          stage = 4
        case default
          not_integer = .true.
        end select
      case ('0':'9')
        ! digit
        select case(stage)
        case(0:1)
          stage = 2
        case(3:4)
          stage = 5
        case default
          continue
        end select
      case ('e','E')
        ! exponent
        select case(stage)
        case(2)
          stage = 3
        case default
          not_integer = .true.
        end select
      case default
        not_integer = .true.
      end select

      if (not_integer) exit
    end do

    ! determine if integer
    if (not_integer) then
      ans = .false.
    else
      select case (stage)
      case (2,5,6)
        ans = .true.
      case default
        ans = .false.
      end select
    end if
  end function is_integer
!===============================================================================

!===============================================================================
! is_real:
!
!   Returns .TRUE. if the given string contains a real value. Returns .FALSE.
!   otherwise.
!       ans   |F |F      |T  |T   |T  |F      |F      |T     |T|
!       stage |0 |1      |2  |3   |4  |5      |6      |7     |8|
!       regex |   [\+\-]? \d* (\.? \d* ([deDE] [\+\-]? \d+)?)  |
!
  pure function is_real( str ) result( ans )
    character(*), intent(in) :: str
    logical :: ans
    ! local variables:
    integer :: i
    logical :: has_leading_digit
    logical :: not_real
    integer :: stage

    ! determine number type and length
    stage = 0
    not_real = .false.
    has_leading_digit = .false.
    do i = 1, len(str)
      select case(str(i:i))
      case (space,tab)
        ! white space
        !not_real = .not.allow_white_spaces
        select case (stage)
        case (0,8)
          continue
        case (2:4,7)
          stage = 8
        case default
          not_real = .true.
        end select
      case ('+','-')
        select case(stage)
        case(0)
          stage = 1
        case(5)
          stage = 6
        case default
          not_real = .true.
        end select
      case ('0':'9')
        select case(stage)
        case(0:1)
          stage = 2
          has_leading_digit = .true.
        case(3)
          stage = 4
        case(5:6)
          stage = 7
        case default
          continue
        end select
      case ('.')
        select case(stage)
        case(0:2)
          stage = 3
        case default
          not_real = .true.
        end select
      case ('e','E','d','D')
        select case(stage)
        case(2:4)
          stage = 5
        case default
          not_real = .true.
        end select
      case default
        not_real = .true.
      end select

      if (not_real) exit
    end do

    ! determine if real
    if (not_real) then
      ans = .false.
    else
      select case (stage)
      case (2,4,7,8)
        ans = .true.
      case (3)
        ans = has_leading_digit
      case default
        ans = .false.
      end select
    end if
  end function is_real
!===============================================================================
end module string_utility_module
!===============================================================================
