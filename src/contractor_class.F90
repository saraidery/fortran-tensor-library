module contractor_class
!
   use parameters
   use tensor_class, only: tensor
   use string_class, only: string
!
   type :: contractor
!
      integer :: M, N, K_A, K_B
      integer :: leading_dimension_A, leading_dimension_B
!
      character(len=1) :: transpose_A, transpose_B
!
   contains
!
      procedure, public  :: contract
!
      procedure :: determine_contraction
      procedure, private :: get_information_from_A
      procedure, private :: get_information_from_B
      procedure, private :: reset
!
   end type contractor
!
   interface contractor
!
      procedure :: new_contractor
!
   end interface contractor
!
contains
!
!
   function new_contractor() result(this)
!!
!!    New contractor
!!
      implicit none
!
      type(contractor) :: this
!
      this%M = 0
      this%N = 0
      this%K_A = 0
      this%K_B = 0
      this%leading_dimension_A = 0
      this%leading_dimension_B = 0
!
      this%transpose_A = ""
      this%transpose_B = ""
!
   end function new_contractor
!
!
   subroutine contract(this, A, indices_A, B, indices_B, C, alpha, beta)
!!
!!    Contract
!!
      implicit none
!
      class(contractor) :: this
!
      type(tensor), intent(in)    :: A, B
      type(tensor), intent(inout) :: C
!
      type(string), intent(in) :: indices_A, indices_B
!
      real(dp), intent(in), optional :: alpha, beta
!
      type(string) :: string_A, string_B
!
      real(dp) :: alpha_, beta_
!
      string_A = string(indices_A)
      string_B = string(indices_B)
!
      alpha_ = one
      if (present(alpha)) alpha_ = alpha
!
      beta_  = zero
      if (present(beta)) beta_ = beta
!
      call this%determine_contraction(A, string_A, B, string_B)
!
      call dgemm(this%transpose_A,         &
                 this%transpose_B,         &
                 this%M,                   &
                 this%N,                   &
                 this%K_A,                 &
                 alpha_,                   &
                 A%array,                  &
                 this%leading_dimension_A, &
                 B%array,                  &
                 this%leading_dimension_B, &
                 beta_,                    &
                 C%array,                  &
                 this%M)
!
      call this%reset
!
   end subroutine contract
!
!
   subroutine determine_contraction(this, A, string_A, B, string_b)
!!
!!    Determine contraction
!!
      implicit none
!
      class(contractor), intent(inout) :: this
!
      class(tensor), intent(in) :: A, B
      class(string), intent(in) :: string_A, string_B
!
      integer :: first_index_A, first_index_B, length, counter
!
      call string_a%find_longest_common_substring(string_B, first_index_A, &
                                                  first_index_B, length, counter)
!
!
      if (counter == 0) then
         error stop "Did not find common substring: Cannot determine contraction"
      end if
!
      if (counter > 1) then
         error stop "Found more than one common substring: Cannot determine contraction"
      end if
!
!
      call this%get_information_from_A(A, first_index_A, length)
!
      call this%get_information_from_B(B, first_index_B, length)
!
!
      if (this%K_A .ne. this%K_B) then
!
         print '(a,i0,a,i0,a)', "Supposed to contract ", this%K_A, &
                              " elements of A with ", this%K_B, " elements of B."
         error stop "Different number of elements to contract over"
!
      end if
!
   end subroutine determine_contraction
!
!
   subroutine get_information_from_A(this, A, first_index, n_indices)
!!
!!    Get information from A
!!
!!    Get information about dimensions and transposition from the
!!    frist array in the matrix-matrix multiplication
!!
      implicit none
!
      class(contractor), intent(inout) :: this
!
      type(tensor), intent(in) :: A
!
      integer, intent(in)   :: first_index, n_indices
!
      if (first_index == 1) then
!
         this%transpose_A = 'T'
!
         this%K_A = A%get_n_elements(1, n_indices)
         this%leading_dimension_A = this%K_A
!
         this%M = A%get_n_elements(n_indices + 1, A%rank)
!
      else if (first_index + n_indices - 1 == A%rank) then
!
         this%transpose_A = 'N'
!
         this%M = A%get_n_elements(1, first_index - 1)
         this%leading_dimension_A = this%M
!
         this%K_A = A%get_n_elements(first_index, A%rank)
!
      else
!
         error stop "Could not determine information from array B"
!
      end if
!
   end subroutine get_information_from_A
!
!
   subroutine get_information_from_B(this, B, first_index, n_indices)
!!
!!    Get information from B
!!
!!    Get information about dimensions and transposition from the
!!    second array in the matrix-matrix multiplication
!!
      implicit none
!
      class(contractor), intent(inout) :: this
!
      type(tensor), intent(in) :: B
!
      integer, intent(in) :: first_index, n_indices
!
      if (first_index == 1) then
!
         this%transpose_B = 'N'
!
         this%K_B = B%get_n_elements(1, n_indices)
!
         this%leading_dimension_B = this%K_B
!
         this%N = B%get_n_elements(n_indices + 1, B%rank)
!
      else if (first_index + n_indices - 1 == B%rank) then
!
         this%transpose_B = 'T'
!
         this%N = B%get_n_elements(1, first_index - 1)
!
         this%leading_dimension_B = this%N
!
         this%K_B = B%get_n_elements(first_index, B%rank)
!
      else
!
         error stop "Could not determine information from array B"
!
      end if
!
   end subroutine get_information_from_B
!
!
   subroutine reset(this)
!!
!!    Reset contractor
!!
      implicit none
!
      class(contractor), intent(out) :: this
!
      this%M = 0
      this%N = 0
      this%K_A = 0
      this%K_B = 0
      this%leading_dimension_A = 0
      this%leading_dimension_B = 0
!
      this%transpose_A = ""
      this%transpose_B = ""
!
   end subroutine reset
!
!
end module contractor_class
