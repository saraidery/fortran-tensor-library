module tensor_class
!
   use kinds
!
   type :: tensor
!
      integer :: rank
!
      integer, dimension(:), allocatable, private :: dimensions
!
      real(dp), dimension(:), allocatable, private :: array
!
   end type tensor
!
   interface tensor
!
      procedure :: new_tensor
!
   end interface tensor
!
contains
!
   function new_tensor(dimensions) result(this)
!
      implicit none
!
      type(tensor) :: this
!
      integer, dimension(:) :: dimensions
!
      this%rank = size(dimensions)
!
      allocate(this%dimensions(this%rank))
!
      this%dimensions = dimensions
!
      print*, "I am a tensor with rank", this%rank, "and dimensions", this%dimensions
!
   end function new_tensor
!
end module tensor_class
