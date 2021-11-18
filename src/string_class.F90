module string_class
!!
!!    String class
!!
   use parameters
!
   type :: string
!
      character(len=:), allocatable :: chars
!
      integer :: length
!
   contains
!
      procedure, public :: get_length
      procedure, public :: print_
      procedure, public :: find_substring
      procedure, public :: convert_to_lowercase
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
      this%chars = characters
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
      this%chars = that%chars
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
      print '(a)', this%chars
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
      class(string), intent(in) :: substring
!
      integer, intent(out) :: start_index
      logical, intent(out) :: found
!
      integer :: substring_length, cursor
!
      substring_length = substring%get_length()
!
      start_index = 1
      cursor = start_index + substring_length - 1
!
      found = .false.
!
      do while (cursor .le. len(this%chars))
!
         if (this%chars(start_index : cursor) == substring%chars) then
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
   subroutine convert_to_lowercase(this)
!!
!!    Convert to lowercase
!!
!!    Taken from string_utilities.F90 by Eirik F. Kjønstad
!!    (https://gitlab.com/eT-program/eT/-/blob/0814712a2c4cd4e931361aaaa267ee7cea877f40/src/tools/string_utilities.F90)
!!
!!    Assumes ASCII table for representing characters as integers,
!!    where the lowercase letter is +32 relative to the uppercase letters.
!!
!!    Note: uppercase (65-90) and lowercase (97-122).
!!
      implicit none
!
      class(string) :: this
!
      integer :: element, current_character
!
      do element = 1, this%get_length()
!
!        Represent character as integer
         current_character = ichar(this%chars(element : element))
!
!        Convert if character is in the range of uppercase characters
!
         if (current_character >= 65 .and. current_character <= 90) then ! Between A and Z
!
            current_character = current_character + 32
!
         endif
!
!        Replace the character by the (possibly) lowercased letter
!
         this%chars(element : element) = char(current_character)
!
      enddo
!
   end subroutine convert_to_lowercase
!
!
end module string_class
