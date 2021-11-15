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

option(ENABLE_64BIT_INTEGERS  "Enable 64-bit integers"                                   ON)
option(ENABLE_AUTO_BLAS       "Enable CMake to autodetect BLAS"                          ON)
option(ENABLE_AUTO_LAPACK     "Enable CMake to autodetect LAPACK"                        ON)
option(ENABLE_OMP             "Enable OpenMP parallelization"                            ON)
option(ENABLE_THREADED_MKL    "Enable OpenMP parallelization in MKL"                     ON)
option(ENABLE_RUNTIME_CHECKS  "Enable all runtime checks (for debug purposes)"           OFF)

set (Fortran_sources
     src/eT_program.F90)

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

add_executable(ftensor ${CMAKE_SOURCE_DIR}/src/ftensor_program.F90)

target_link_libraries(ftensor ${lapackblas_libraries} ${EXTRA_LINKER_FLAGS})

set_target_properties(ftensor PROPERTIES LINKER_LANGUAGE Fortran)

if(ENABLE_RUNTIME_CHECKS)
    message(STATUS "Runtime checks enabled")
endif()
