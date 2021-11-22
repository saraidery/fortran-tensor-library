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
!
      final :: destructor
!
      procedure, public :: sort2 
      procedure, public :: get_2array_pointer 
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
   subroutine sort2(this, from, to)
!!
!!    Sort2
!!
      use string_class, only: string 
!
      implicit none 
!
      class(tensor), intent(inout) :: this 
!
      character(len=*), intent(in) :: from, to 
!
      type(string), allocatable :: from_string, to_string
!
      integer, dimension(:), allocatable :: permutation 
!
      integer, dimension(:), allocatable :: old_dimensions, new_dimensions 
      integer, dimension(:), allocatable :: old_indices, new_indices
!
      real(dp), dimension(:), allocatable :: new_array 
!
      integer :: new_p, new_q, new_pq, old_pq
!
      from_string = string(from)
      to_string   = string(to)
!
      allocate(old_dimensions(this%rank))
      allocate(new_dimensions(this%rank))
!
      allocate(old_indices(this%rank))
      allocate(new_indices(this%rank))
!
      permutation = to_string%get_permutation_indices(from_string)
!
      old_dimensions = this%dimensions 
!
      new_dimensions = [this%dimensions(permutation(1)), & 
                        this%dimensions(permutation(2))] 
!
      allocate(new_array(this%n_elements))
!
      do new_q = 1, new_dimensions(2) 
         do new_p = 1, new_dimensions(1)
!
            new_indices = [new_p, new_q] 
            old_indices = [new_indices(permutation(1)), new_indices(permutation(2))] 
!
            new_pq = new_dimensions(1)*(new_indices(2) - 1) + new_indices(1) 
            old_pq = old_dimensions(1)*(old_indices(2) - 1) + old_indices(1) 
!
            new_array(new_pq) = this%array(old_pq)
!          
         enddo
      enddo 
!
      this%array      = new_array
      this%dimensions = new_dimensions
!
   end subroutine sort2
!
!
   subroutine get_2array_pointer(this, array_pointer)
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
   end subroutine get_2array_pointer
!
!
end module tensor_class
