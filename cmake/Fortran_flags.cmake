#
#
#   eT - a coupled cluster program
#   Copyright (C) 2016-2021 the authors of eT
#
#   eT is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   eT is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program. If not, see <https://www.gnu.org/licenses/>.
#
#
#   Fortran flags for ifort and gfortran
# 
#   1. Intel ifort
# 
if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
# 
#   Set standard flags 
# 
    set(CMAKE_Fortran_FLAGS "-fpp -O3 -warn all -xHost -no-wrap-margin -stand f18")
#
#   Turn off xHost spam by disabling remark #10382
#   and truncated source warning (#5194) and warning for external Lapack/BLAS
#   routines (#8889)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -diag-disable=10382,5194,8889")
#
#   Enable 64 bit flag if requested (default)
# 
    if(ENABLE_64BIT_INTEGERS)
        set(CMAKE_Fortran_FLAGS
            "${CMAKE_Fortran_FLAGS} -i8"
            )
    else()
        set(CMAKE_Fortran_FLAGS
            "${CMAKE_Fortran_FLAGS} -i4"
            )        
    endif()
# 
#   Enable openmp if requested (default) 
# 
    if(ENABLE_OMP)
        set(CMAKE_Fortran_FLAGS
            "${CMAKE_Fortran_FLAGS} -qopenmp -parallel"
            )
    endif()
#
#   Enable runtime checks if requested (not default)
# 
    if(ENABLE_RUNTIME_CHECKS)
        set(CMAKE_Fortran_FLAGS
            "${CMAKE_Fortran_FLAGS} -check bounds" # need to add more & test this 
            )
    endif()
#
endif()
# 
#   2. GNU gfortran
# 
if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
# 
#   Set standard flags 
# 
    set(CMAKE_Fortran_FLAGS "-std=f2018 -O3 -Wall -march=native -cpp")
# 
#   Testing processor 32-bit or 64-bit
# 
    if(${CMAKE_HOST_SYSTEM_PROCESSOR} MATCHES "i386")
        set(CMAKE_Fortran_FLAGS
            "${CMAKE_Fortran_FLAGS} -m32"
            )
    endif()
# 
    if(${CMAKE_HOST_SYSTEM_PROCESSOR} MATCHES "x86_64")
        set(CMAKE_Fortran_FLAGS
            "${CMAKE_Fortran_FLAGS} -m64"
            )
    endif()
# 
#   Enable 64 bit flag if requested (default)
# 
    if(ENABLE_64BIT_INTEGERS)
        set(CMAKE_Fortran_FLAGS
            "${CMAKE_Fortran_FLAGS} -fdefault-integer-8"
            )
    endif()
# 
#   Enable openmp if requested (default) 
# 
    if(ENABLE_OMP)
        set(CMAKE_Fortran_FLAGS
            "${CMAKE_Fortran_FLAGS} -fopenmp"
            )
    endif()
# 
#   Enable runtime checks if requested (not default)
# 
    if(ENABLE_RUNTIME_CHECKS)
        set(CMAKE_Fortran_FLAGS
            "${CMAKE_Fortran_FLAGS} -fcheck=all"    
            )
    endif()
# 
endif()

if(DEFINED EXTRA_Fortran_FLAGS)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${EXTRA_Fortran_FLAGS}")
endif()

