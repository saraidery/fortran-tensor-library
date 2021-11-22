module tensor_class
!!
!!    Tensor class
!!
!!    Class for arbitrary-rank tensors. Stores the tensor and handles
!!    its indices and total dimensionality.
!!
   use parameters
!
   type :: tensor
!
      integer :: rank
!
      integer, private :: n_elements
!
      integer, dimension(:), allocatable, private :: dimensions
!
      real(dp), dimension(:), allocatable :: array
!
   contains
!
      procedure, public :: initialize
      procedure, public :: get_n_elements
      procedure, public :: set_array
!
      final :: destructor
!
      procedure, public :: sort
!
      procedure, public :: copy
!
      procedure, public :: set_to_zero
!
      generic, public :: get_pointer & 
                      => get_pointer_1, &
                         get_pointer_2, &
                         get_pointer_3, & 
                         get_pointer_4
!
      procedure, private :: get_pointer_1 
      procedure, private :: get_pointer_2 
      procedure, private :: get_pointer_3 
      procedure, private :: get_pointer_4
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
   subroutine copy(this, that, scale)
!
      implicit none 
!
      class(tensor), intent(in) :: this 
!
      class(tensor), intent(inout) :: that 
!
      real(dp), optional, intent(in) :: scale 
!
      call dcopy(this%n_elements, this%array, 1, that%array, 1)
!
      if (present(scale)) call dscal(this%n_elements, scale, that%array, 1)
!
   end subroutine copy
!
!
   subroutine set_to_zero(this)
!
      use parameters
!
      implicit none 
!
      class(tensor), intent(inout) :: this 
!
      integer :: k 
!
!$omp parallel do private(k)
      do k = 1, this%n_elements
!
         this%array(k) = zero 
!
      enddo
!$omp end parallel do
!
   end subroutine set_to_zero 
!
!
   subroutine sort(this, that, from, to)
!!
!!    Sort
!!
      use string_class, only: string
      use sorter_class, only: sorter
!
      implicit none 
!
      class(tensor), intent(in) :: this 
!
      type(tensor), intent(inout) :: that 
!
      character(len=*), intent(in) :: from, to 
!
      class(sorter), allocatable :: my_sorter
!
      my_sorter = sorter(this%dimensions, string(from), string(to))
!
      call my_sorter%sort(this%array, that%array)
!
   end subroutine sort
!
!
   subroutine get_pointer_1(this, array_pointer)
!
      implicit none 
!
      class(tensor), intent(in), target :: this 
!
      real(dp), dimension(:), pointer :: array_pointer
!
      array_pointer(1 : this%dimensions(1)) => this%array(1 : this%n_elements)
!
   end subroutine get_pointer_1
!
!
   subroutine get_pointer_2(this, array_pointer)
!
      implicit none 
!
      class(tensor), intent(in), target :: this 
!
      real(dp), dimension(:,:), pointer :: array_pointer
!
      array_pointer(1 : this%dimensions(1), &
                    1 : this%dimensions(2)) => this%array(1 : this%n_elements)
!
   end subroutine get_pointer_2
!
!
   subroutine get_pointer_3(this, array_pointer)
!
      implicit none 
!
      class(tensor), intent(in), target :: this 
!
      real(dp), dimension(:,:,:), pointer :: array_pointer
!
      array_pointer(1 : this%dimensions(1), &
                    1 : this%dimensions(2), &
                    1 : this%dimensions(3)) => this%array(1 : this%n_elements)
!
   end subroutine get_pointer_3
!
   subroutine get_pointer_4(this, array_pointer)
!
      implicit none 
!
      class(tensor), intent(in), target :: this 
!
      real(dp), dimension(:,:,:,:), pointer :: array_pointer
!
      array_pointer(1 : this%dimensions(1), &
                    1 : this%dimensions(2), &
                    1 : this%dimensions(3), &
                    1 : this%dimensions(4)) => this%array(1 : this%n_elements)
!
   end subroutine get_pointer_4
!
!
   subroutine set_array(this, array)
!!
!!    Set array
!!
      implicit none
!
      class(tensor), intent(inout) :: this
      real(dp), dimension(this%n_elements), intent(in) :: array
!
      call dcopy(this%n_elements, array, 1, this%array, 1)
!
   end subroutine set_array
!
!
end module tensor_class
