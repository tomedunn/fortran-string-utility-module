# Define tests source files
set ( SU_TESTS_SOURCE
  ${SU_TESTS_SOURCE_DIR}/tests.f90
  ${SU_SOURCE}
)

# Define tests executable
add_executable ( ${SU_TESTS_EXE} ${SU_TESTS_SOURCE} )
