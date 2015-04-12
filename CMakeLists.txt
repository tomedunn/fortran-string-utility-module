cmake_minimum_required ( VERSION 2.8.8 FATAL_ERROR )

# Set CMake language to Fortran
enable_language ( Fortran )



# Define object binary directory
set ( BIN ${CMAKE_BINARY_DIR}/bin )

# Define lib as the module directory
set ( LIB ${CMAKE_BINARY_DIR}/lib )
set ( CMAKE_Fortran_MODULE_DIRECTORY ${LIB} )

# Define source directory
set ( SU_DIR ${CMAKE_SOURCE_DIR}/src )
set ( SU_SRC ${SU_DIR}/string_utility_module.f90 )
#add_subdirectory ( ${SU_DIR} ${LIB} )

#set ( BUILD_SU_TESTS TRUE )
#if ( BUILD_SU_TESTS )
  # Set the name for string utility tests executable
  set ( SU_TESTS_EXE su_tests )

  # Define tests directory
  set ( SU_TESTS_DIR ${CMAKE_SOURCE_DIR}/tests )
  add_subdirectory ( ${SU_TESTS_DIR} ${BIN} )
#endif
