# Set minimum to guarantee Fortran submodule suport
cmake_minimum_required(VERSION 3.7)

# Project name and programming languanges
project(ftensor Fortran)

# Paths that CMake will search for cmake
# module files that end with .cmake
set(CMAKE_MODULE_PATH
    ${CMAKE_SOURCE_DIR}/cmake
    ${CMAKE_SOURCE_DIR}/cmake/math
    /usr/local/share/cmake/Modules
    )

# Set options with standard values

option(ENABLE_64BIT_INTEGERS  "Enable 64-bit integers"                         ON)
option(ENABLE_AUTO_BLAS       "Enable CMake to autodetect BLAS"                ON)
option(ENABLE_AUTO_LAPACK     "Enable CMake to autodetect LAPACK"              ON)
option(ENABLE_OMP             "Enable OpenMP parallelization"                  ON)
option(ENABLE_THREADED_MKL    "Enable OpenMP parallelization in MKL"           ON)
option(ENABLE_RUNTIME_CHECKS  "Enable all runtime checks (for debug purposes)" OFF)
option(ENABLE_PFUNIT          "Enable unit tests with pFUnit"                  OFF)


set (Fortran_sources
     ${CMAKE_SOURCE_DIR}/src/kinds.F90
     ${CMAKE_SOURCE_DIR}/src/tensor_class.F90)

include(Fortran_flags)

if(NOT ENABLE_64BIT_INTEGERS)
   add_definitions(-DHAS_32BIT_INTEGERS)
   message(STATUS "32bit integers enabled")
endif()

# Math detection
set(BLAS_LANG "Fortran")
set(LAPACK_LANG "Fortran")
set(MKL_COMPILER_BINDINGS ${CMAKE_Fortran_COMPILER_ID})
include(ConfigMath)

# Set lapack and blas libraries
if(LAPACK_FOUND AND BLAS_FOUND)
    set(lapackblas_libraries ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES})
endif()

# Make library
add_library(ftensor_library ${Fortran_sources})

# Supress ranlib no symbols warning for modules with no subroutines
if(APPLE)
    SET(CMAKE_C_ARCHIVE_CREATE   "<CMAKE_AR> Scr <TARGET> <LINK_FLAGS> <OBJECTS>")
    SET(CMAKE_CXX_ARCHIVE_CREATE "<CMAKE_AR> Scr <TARGET> <LINK_FLAGS> <OBJECTS>")
    SET(CMAKE_C_ARCHIVE_FINISH   "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")
    SET(CMAKE_CXX_ARCHIVE_FINISH "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")
endif(APPLE)

# Link library to Lapack, Libint, and PCMSolver 
target_link_libraries(ftensor_library ${lapackblas_libraries})

target_include_directories(ftensor_library PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

add_executable(ftensor ${CMAKE_SOURCE_DIR}/src/ftensor_program.F90)

target_link_libraries(ftensor ${lapackblas_libraries} ftensor_library ${EXTRA_LINKER_FLAGS})

set_target_properties(ftensor PROPERTIES LINKER_LANGUAGE Fortran)

if(ENABLE_RUNTIME_CHECKS)
    message(STATUS "Runtime checks enabled")
endif()

if(ENABLE_PFUNIT)

  # pFUnit - Fortran unit testing package 

  cmake_minimum_required(VERSION 3.12) # Due to GFTL (used by pFUnit), 
                                       # which sets <prefix>_ROOT CMake variables, 
                                       # assuming >= 3.12 policy (CMP0074)

  find_package(PFUNIT)

  if(PFUNIT_FOUND)
    enable_testing()  
    include(UnitTests)
    message(STATUS "Unit testing requested, and pFUnit successfully located!")
  else()
    message(FATAL_ERROR "Unit testing requested, but could not find pFUnit!")
  endif()

endif()
