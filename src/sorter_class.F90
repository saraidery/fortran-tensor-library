module sorter_class
!
   use parameters
!
   type :: sorter
!
      integer, private :: rank 
!
      integer, dimension(:), allocatable :: a_dimensions, b_dimensions
!
      integer, dimension(:), allocatable :: a_permutation, b_permutation  
!
      integer, private :: array_lengths
!
   contains
!
      procedure, public :: sort 
!
      procedure, private :: sort2
      procedure, private :: sort3
      procedure, private :: sort4
!
   end type sorter 
!
   interface sorter 
!
      procedure :: new_sorter 
!
   end interface sorter 
!
contains 
!
   function new_sorter(dimensions, from, to) result(this)
!
      use string_class, only: string 
!
      implicit none 
!
      integer, dimension(:), intent(in) :: dimensions 
!
      class(string), intent(in) :: from, to 
!
      type(sorter) :: this 
!
      integer :: k 
!
      this%rank = size(dimensions)
!
      allocate(this%a_dimensions(this%rank))
      allocate(this%b_dimensions(this%rank))
!
      this%a_dimensions = dimensions 
!
      this%a_permutation = from%get_permutation_indices(to)
      this%b_permutation = to%get_permutation_indices(from)
!
      do k = 1, this%rank 
!
         this%b_dimensions(k) = this%a_dimensions(this%b_permutation(k))
!
      enddo
!
      this%array_lengths = 1 
!
      do k = 1, this%rank 
!
         this%array_lengths = this%array_lengths * this%a_dimensions(k)
!
      enddo 
!
   end function new_sorter
!
!
   subroutine sort(this, a, b)
!
      class(sorter), intent(in) :: this 
!
      real(dp), dimension(this%array_lengths), intent(in)  :: a  
      real(dp), dimension(this%array_lengths), intent(out) :: b  
!
      select case (this%rank)
!
         case (2)
            call this%sort2(a, b)

         case (3)
            call this%sort3(a, b)

         case (4)
            call this%sort4(a, b)

         case default
            error stop "No sorting routine for rank"
!
      end select 
!
   end subroutine sort 
!
!
   subroutine sort2(this, a, b)
!
      implicit none 
!
      class(sorter), intent(in) :: this 
!
      real(dp), dimension(this%array_lengths), intent(in)  :: a  
      real(dp), dimension(this%array_lengths), intent(out) :: b        
!
      integer :: p, q, b_pq, a_pq
!
      integer, dimension(2) :: a_ind, b_ind 
!
!$omp parallel do private(q, p, a_ind, b_ind, b_pq, a_pq)
      do q = 1, this%b_dimensions(2) 
         do p = 1, this%b_dimensions(1)
!
            b_ind = [p, q] 
!
            a_ind = [b_ind(this%a_permutation(1)), &
                     b_ind(this%a_permutation(2))] 
!
            b_pq = this%b_dimensions(1)*(b_ind(2) - 1) + b_ind(1) 
!
            a_pq = this%a_dimensions(1)*(a_ind(2) - 1) + a_ind(1) 
!
            b(b_pq) = a(a_pq)
!          
         enddo
      enddo 
!$omp end parallel do
!
   end subroutine sort2
!
!
   subroutine sort3(this, a, b)
!
      implicit none 
!
      class(sorter), intent(in) :: this 
!
      real(dp), dimension(this%array_lengths), intent(in)  :: a  
      real(dp), dimension(this%array_lengths), intent(out) :: b        
!
      integer :: p, q, r, b_pqr, a_pqr
!
      integer, dimension(3) :: a_ind, b_ind 
!
!$omp parallel do private(p, q, r, a_ind, b_ind, b_pqr, a_pqr)
      do r = 1, this%b_dimensions(3)
         do q = 1, this%b_dimensions(2) 
            do p = 1, this%b_dimensions(1)
!
               b_ind = [p, q, r] 
!
               a_ind = [b_ind(this%a_permutation(1)), &
                        b_ind(this%a_permutation(2)), &
                        b_ind(this%a_permutation(3))] 
!
               b_pqr = this%b_dimensions(1)*&
                      (this%b_dimensions(2)*(b_ind(3) - 1) + b_ind(2) - 1) &
                     + b_ind(1) 
!
               a_pqr = this%a_dimensions(1)*&
                      (this%a_dimensions(2)*(a_ind(3) - 1) + a_ind(2) - 1) &
                     + a_ind(1) 
!
               b(b_pqr) = a(a_pqr)
!          
            enddo
         enddo 
      enddo
!$omp end parallel do
!
   end subroutine sort3 
!
!
   subroutine sort4(this, a, b)
!
      implicit none 
!
      class(sorter), intent(in) :: this 
!
      real(dp), dimension(this%array_lengths), intent(in)  :: a  
      real(dp), dimension(this%array_lengths), intent(out) :: b        
!
      integer :: p, q, r, s, b_pqrs, a_pqrs
!
      integer, dimension(4) :: a_ind, b_ind 
!
!$omp parallel do private(p, q, r, s, a_ind, b_ind, b_pqrs, a_pqrs)
      do s = 1, this%b_dimensions(4)
         do r = 1, this%b_dimensions(3)
            do q = 1, this%b_dimensions(2) 
               do p = 1, this%b_dimensions(1)
!
                  b_ind = [p, q, r, s] 
!
                  a_ind = [b_ind(this%a_permutation(1)), &
                           b_ind(this%a_permutation(2)), &
                           b_ind(this%a_permutation(3)), &
                           b_ind(this%a_permutation(4))] 
!
                  b_pqrs = this%b_dimensions(1)*&
                          (this%b_dimensions(2)*&
                          (this%b_dimensions(3)*(b_ind(4) - 1) + b_ind(3) - 1) &
                         + b_ind(2) - 1) &
                         + b_ind(1) 
!
                  a_pqrs = this%a_dimensions(1)*&
                          (this%a_dimensions(2)*&
                          (this%a_dimensions(3)*(a_ind(4) - 1) + a_ind(3) - 1) &
                         + a_ind(2) - 1) &
                         + a_ind(1) 
!
                  b(b_pqrs) = a(a_pqrs)
!          
               enddo
            enddo
         enddo 
      enddo
!$omp end parallel do
!
   end subroutine sort4
!
!
end module sorter_class