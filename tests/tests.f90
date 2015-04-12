program main
  use string_utility_module
  implicit none
  ! local parameters:
  character(len=*), parameter :: str_lo = 'abcdefghijklmnopqrstuvwxyz'
  character(len=*), parameter :: str_up = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  ! local variables:
  character(:), allocatable :: str

  str = str_lo
  write(*,'("Original string:")')
  write(*,'(A)') str
  write(*,'(A)') ''

  call convert_to_upper_case(str)
  write(*,'("Uppercase string:")')
  write(*,'(A)') str
  write(*,'(A)') ''

  call convert_to_lower_case(str)
  write(*,'("Lowercase string:")')
  write(*,'(A)') str
  write(*,'(A)') ''

  ! integer tests
  str = ' '
  write(*,'("String: ",A)') str
  if (is_integer(str)) then
    write(*,'("is a valid integer.")')
  else
    write(*,'("is NOT a valid integer.")')
  end if
  write(*,'(A)') ''

  str = '1'
  write(*,'("String: ",A)') str
  if (is_integer(str)) then
    write(*,'("is a valid integer.")')
  else
    write(*,'("is NOT a valid integer.")')
  end if
  write(*,'(A)') ''

  str = '+1'
  write(*,'("String: ",A)') str
  if (is_integer(str)) then
    write(*,'("is a valid integer.")')
  else
    write(*,'("is NOT a valid integer.")')
  end if
  write(*,'(A)') ''

  str = '1e'
  write(*,'("String: ",A)') str
  if (is_integer(str)) then
    write(*,'("is a valid integer.")')
  else
    write(*,'("is NOT a valid integer.")')
  end if
  write(*,'(A)') ''

  str = '1e1'
  write(*,'("String: ",A)') str
  if (is_integer(str)) then
    write(*,'("is a valid integer.")')
  else
    write(*,'("is NOT a valid integer.")')
  end if
  write(*,'(A)') ''

  str = '1e+1'
  write(*,'("String: ",A)') str
  if (is_integer(str)) then
    write(*,'("is a valid integer.")')
  else
    write(*,'("is NOT a valid integer.")')
  end if
  write(*,'(A)') ''

  str = ' 1e+1 '
  write(*,'("String: ",A)') str
  if (is_integer(str)) then
    write(*,'("is a valid integer.")')
  else
    write(*,'("is NOT a valid integer.")')
  end if
  write(*,'(A)') ''

  ! real tests
  str = ' '
  write(*,'("String: ",A)') str
  if (is_real(str)) then
    write(*,'("is a valid real.")')
  else
    write(*,'("is NOT a valid real.")')
  end if
  write(*,'(A)') ''

  str = '1'
  write(*,'("String: ",A)') str
  if (is_real(str)) then
    write(*,'("is a valid real.")')
  else
    write(*,'("is NOT a valid real.")')
  end if
  write(*,'(A)') ''

  str = '+1'
  write(*,'("String: ",A)') str
  if (is_real(str)) then
    write(*,'("is a valid real.")')
  else
    write(*,'("is NOT a valid real.")')
  end if
  write(*,'(A)') ''

  str = '+1.'
  write(*,'("String: ",A)') str
  if (is_real(str)) then
    write(*,'("is a valid real.")')
  else
    write(*,'("is NOT a valid real.")')
  end if
  write(*,'(A)') ''

  str = '.'
  write(*,'("String: ",A)') str
  if (is_real(str)) then
    write(*,'("is a valid real.")')
  else
    write(*,'("is NOT a valid real.")')
  end if
  write(*,'(A)') ''

  str = '1d'
  write(*,'("String: ",A)') str
  if (is_real(str)) then
    write(*,'("is a valid real.")')
  else
    write(*,'("is NOT a valid real.")')
  end if
  write(*,'(A)') ''

  str = '1d1'
  write(*,'("String: ",A)') str
  if (is_real(str)) then
    write(*,'("is a valid real.")')
  else
    write(*,'("is NOT a valid real.")')
  end if
  write(*,'(A)') ''

  str = '1d+1'
  write(*,'("String: ",A)') str
  if (is_real(str)) then
    write(*,'("is a valid real.")')
  else
    write(*,'("is NOT a valid real.")')
  end if
  write(*,'(A)') ''

  str = ' 1d+1 '
  write(*,'("String: ",A)') str
  if (is_real(str)) then
    write(*,'("is a valid real.")')
  else
    write(*,'("is NOT a valid real.")')
  end if
  write(*,'(A)') ''

contains

  subroutine TEST_convert_to_lower_case( results )
    logical, intent(inout) :: results(:)
    ! local parameters:
    character(len=*), parameter :: str_lo = 'abcdefghijklmnopqrstuvwxyz'
    character(len=*), parameter :: str_up = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    ! local variables:
    character(:), allocatable :: str

    str = str_lo
    call convert_to_upper_case(str)
    results(1) = all(str == str_up)
  end subroutine TEST_convert_to_lower_case

  subroutine TEST_convert_to_upper_case( results )
    logical, intent(inout) :: results(:)
    ! local parameters:
    character(len=*), parameter :: str_lo = 'abcdefghijklmnopqrstuvwxyz'
    character(len=*), parameter :: str_up = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    ! local variables:
    character(:), allocatable :: str

    str = str_up
    call convert_to_upper_case(str)
    results(1) = all(str == str_lo)
  end subroutine TEST_convert_to_upper_case

  subroutine TEST_is_integer( results )
    logical, intent(inout) :: results(:)

    results(1) = is_integer(' 1'       )
    results(2) = is_integer(' +1'      )
    results(3) = is_integer(' +1e0'    )
    results(4) = is_integer(' +1e+0'   )
  end subroutine TEST_is_integer

  subroutine TEST_is_real( results )
    logical, intent(inout) :: results(:)

    results(1) = is_real(' 1'       )
    results(2) = is_real(' +1'      )
    results(3) = is_real(' +1.'     )
    results(4) = is_real(' +1.0'    )
    results(5) = is_real(' +1.d0'   )
    results(6) = is_real(' +1.0d0'  )
    results(7) = is_real(' +1.0d+0' )
    results(8) = is_real(' +1.0d+0 ')
  end subroutine TEST_is_real
end program main
