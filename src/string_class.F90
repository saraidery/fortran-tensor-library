module string_class
!!
!!    String class
!!
   use parameters
!
   type :: string
!
      character(len=:), allocatable :: string
!
      integer :: length
!
   contains
!
      procedure, public :: get_length
      procedure, public :: print_
!
   end type string
!
   interface string
!
      procedure :: new_string
!
   end interface string
!
contains
!
!
   function new_string(characters) result(this)
!!
!!    New string
!!
      implicit none
!
      type(string) :: this
!
      character(len=*), intent(in) :: characters
!
      this%string = characters
!
      this%length = len(characters)
!
   end function new_string
!
!
   subroutine print_(this)
!!
!!    Print
!!
      implicit none
!
      class(string), intent(in) :: this
!
      print '(a)', this%string
!
   end subroutine print_
!
!
   pure function get_length(this) result(length)
!!
!!    Get length
!!
      implicit none
!
      class(string), intent(in) :: this
!
      integer :: length
!
      length = this%length
!
   end function get_length
!
!
end module string_class
