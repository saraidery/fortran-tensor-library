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
      procedure :: get_length
      procedure :: print_
      procedure :: find_substring
      procedure :: convert_to_lowercase
      procedure :: find_longest_common_substring
      procedure :: get_permutation_indices
      procedure :: get_chars 
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
   subroutine find_longest_common_substring(string_a, string_b, index_a, index_b, &
                                            max_length, counter)
!!
!!    Find longest common substring
!!
!!    Finds longest common substring in string_a and string_b
!!    and returns the index of the first common element and the length
!!
!!    Optional: returns number of equally long substrings
!!
      implicit none
!
      class(string), intent(in) :: string_a, string_b
!
      integer, intent(out) :: index_a, index_b, max_length
      integer, intent(out), optional :: counter
!
      integer, dimension(:,:), allocatable :: substring_lengths
!
      integer :: i, j, len_a, len_b, counter_
!
      index_a = 0
      index_b = 0
      max_length = 0
!
      len_a = string_a%get_length()
      len_b = string_b%get_length()
!
      allocate(substring_lengths(len_a, len_b))
      substring_lengths = 0
!
      counter_ = 0
!
      do j = 1, len_b
         do i = 1, len_a
!
            if (string_a%chars(i:i) == string_b%chars(j:j)) then
!
               if (i == 1 .or. j == 1) then
!
                  substring_lengths(i, j) = 1
!
               else
!
!                 If the previous character matched
                  substring_lengths(i, j) = substring_lengths(i-1, j-1) + 1
!
               end if
!
               if (max_length < substring_lengths(i, j)) then
!
                  max_length = substring_lengths(i, j)
                  counter_ = 1
                  index_a = i - max_length + 1
                  index_b = j - max_length + 1
!
               else if (max_length == substring_lengths(i, j)) then
!
                  counter_ = counter_ + 1
!
               end if
!
            else
!
               substring_lengths(i, j) = 0
!
            end if
!
         end do
      end do
!
      if (present(counter)) counter = counter_
      deallocate(substring_lengths)
!
   end subroutine find_longest_common_substring
!
!
   function get_chars(this) result(chars)
!
      implicit none 
!
      class(string), intent(in) :: this 
!
      character(this%length), allocatable :: chars 
!
      chars = this%chars 
!
   end function get_chars
!
!
   function get_permutation_indices(this, that) result(indices)
!!
!!    Get permutation indices
!!
      implicit none 
!
      class(string), intent(in) :: this, that 
!
      integer, dimension(this%length) :: indices 
!
      character(len=:), allocatable :: that_chars
!
      logical :: found 
!
      integer :: k
!
      that_chars = that%get_chars()
!
      do k = 1, this%length
!
         call that%find_substring(string(this%chars(k:k)), indices(k), found)
!
         if (.not. found) &
            error stop "Assumed permutation string is not a permutation string!"
!
      enddo
!
   end function get_permutation_indices
!
!
end module string_class
