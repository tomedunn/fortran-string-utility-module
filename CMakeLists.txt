cmake_minimum_required ( VERSION 2.8.8 FATAL_ERROR )

project( fortran-string-utility-module )

# Set CMake language to Fortran
enable_language ( Fortran )

# Define object binary directory
set ( BIN_DIR ${CMAKE_BINARY_DIR}/bin )
set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib )

# place executables in the binary root directory
set ( EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR} )

# Define lib as the module directory
set ( CMAKE_Fortran_MODULE_DIRECTORY ${LIB_DIR} )

# Define string utility module source directory
set ( SU_SOURCE_DIR ${CMAKE_SOURCE_DIR}/src )
set ( SU_SOURCE ${SU_SOURCE_DIR}/string_utility_module.f90 )

#set ( BUILD_SU_TESTS TRUE )
#if ( BUILD_SU_TESTS )
  # Set the name for string utility tests executable
  set ( SU_TESTS_EXE su_tests )

  # Define tests directory
  set ( SU_TESTS_SOURCE_DIR ${CMAKE_SOURCE_DIR}/tests )
  add_subdirectory ( ${SU_TESTS_SOURCE_DIR} ${BIN} )
#endif
