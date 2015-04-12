# Define tests source files
set ( SU_TESTS_SRC
  ${SU_TESTS_DIR}/tests.f90
  ${SU_SRC}
)

# Define tests executable
add_executable ( ${SU_TESTS_EXE} ${SU_TESTS_SRC} )
