module kinds
!
   use, intrinsic :: iso_fortran_env, only: real32, real64, int32, int64
!
   implicit none
!
   integer, parameter :: sp  = real32
   integer, parameter :: dp  = real64
   integer, parameter :: i64 = int64
   integer, parameter :: i32 = int32
!
end module kinds
