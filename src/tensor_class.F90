module tensor_class
!!
!!    Tensor class
!!
!!    Class for arbitrary-rank tensors. Stores the tensor and handles
!!    its indices and total dimensionality.
!!
   use kinds
!
   type :: tensor
!
      integer, private :: rank
!
      integer, private :: n_elements
!
      integer, dimension(:), allocatable, private :: dimensions
!
      real(dp), dimension(:), allocatable, private :: array
!
   contains
!
      procedure, public :: initialize
      procedure, private :: get_n_elements
!
      final :: destructor
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
!
   function new_tensor(dimensions) result(this)
!!
!!    New tensor
!!
      implicit none
!
      type(tensor) :: this
!
      integer, dimension(:), intent(in) :: dimensions
!
      integer :: k
!
      this%rank = size(dimensions)
!
      allocate(this%dimensions(this%rank))
!
      this%dimensions = dimensions
!
      this%n_elements = 1
!
      do k = 1, this%rank
!
         this%n_elements = this%n_elements * this%dimensions(k)
!
      enddo
!
      print*, "Created tensor object with:"
!
      print*, ""
      print*, "Rank: ", this%rank
      print*, "Dimensions: ", this%dimensions
      print*, "Number of elements: ", this%n_elements
!
   end function new_tensor
!
!
   subroutine initialize(this)
!!
!!    Initialize
!!
      implicit none
!
      class(tensor), intent(inout) :: this
!
      allocate(this%array(this%n_elements))
!
   end subroutine initialize
!
!
   subroutine destructor(this)
!!
!!    Destructor
!!
      implicit none
!
      type(tensor), intent(inout) :: this
!
      if (allocated(this%array)) deallocate(this%array)
!
   end subroutine destructor
!
!
   function get_n_elements(this, first, last) result(n_elements)
!!
!!    Get n elements
!!
      implicit none
!
      class(tensor), intent(in) :: this
      integer, intent(in) :: first, last
!
      integer :: n_elements
      integer :: i
!
      n_elements = 1
!
      do i = first, last
!
         n_elements = n_elements * this%dimensions(i)
!
      end do
!
   end function get_n_elements
!
!
end module tensor_class
