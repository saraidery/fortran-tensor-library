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
      procedure, public :: find_substring
!
   end type string
!
   interface string
!
      procedure :: new_string
      procedure :: copy_string
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
   function copy_string(that) result(this)
!!
!!    Copy string
!!
      implicit none
!
      type(string), intent(in) :: that
      type(string) :: this
!
      this%string = that%string
      this%length = that%length
!
   end function copy_string
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
   subroutine find_substring(this, substring, start_index, found)
!!
!!    Find substring
!!
!!    Adapted from string_utilities.F90 by Sarai D. Folkestad
!!    (https://gitlab.com/eT-program/eT/-/blob/0814712a2c4cd4e931361aaaa267ee7cea877f40/src/tools/string_utilities.F90)
!!    licensed under GPLv3.
!!
!!    Returns index of the first matching character in the string
!!
      implicit none
!
      class(string), intent(in) :: this
      character(len=*), intent(in)  :: substring
!
      integer, intent(out) :: start_index
      logical, intent(out) :: found
!
      integer :: substring_length, cursor
!
      substring_length = len(trim(substring))
!
      start_index = 1
      cursor = start_index + substring_length - 1
!
      found = .false.
!
      do while (cursor .le. len(this%string))
!
         if (this%string(start_index : cursor) == trim(substring)) then
!
            found = .true.
            exit
!
         else
!
            start_index = start_index + 1
            cursor = cursor + 1
!
         endif
      enddo
!
      if (.not. found) start_index = 0
!
   end subroutine find_substring
!
!
end module string_class
