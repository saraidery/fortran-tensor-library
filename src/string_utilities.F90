module string_utilities
!
contains
!
!
   subroutine is_substring_in_string(string, substring, start, found)
!!
!!    Substring in string
!!
!!    Adapted from string_utilities.F90 by Sarai D. Folkestad
!!    (https://gitlab.com/eT-program/eT/-/blob/0814712a2c4cd4e931361aaaa267ee7cea877f40/src/tools/string_utilities.F90)
!!    licensed under GPLv3.
!!
      implicit none
!
      character(len=*), intent(in) :: string
      character(len=*), intent(in) :: substring
!
      integer, intent(out) :: start
      logical, intent(out) :: found
!
      integer :: substring_length, cursor
!
      substring_length = len(trim(substring))
!
      start = 1
      cursor = start + substring_length - 1
!
      found = .false.
!
      do while (cursor .lt. len(string))
!
         if (string(start : cursor) == trim(substring)) then
!
            found = .true.
            exit
!
         else
!
            start = start + 1
            cursor = cursor + 1
!
         endif
      enddo
!
   end subroutine is_substring_in_string
!
!
   subroutine find_longest_substring_from_start(string_a, string_b, index_b, length, found)
!!
!!    Find longest substring from start
!!    Written by Alexander C. Paul
!!
!!    Finds length and starting point of the longest substring containing
!!    element 1 of string A that is also contained in string B
!!
      implicit none
!
      character(len=*), intent(in) :: string_a, string_b
!
      integer, intent(out) :: index_b, length
!
      integer :: a
      logical :: found
!
      found = .false.
      index_b = 0
      length  = 0
!
      do a = 1, len(string_a)
!
         call is_substring_in_string(string_b, string_a(1:a), index_b, found)
!
         if (found) then
            length = a
         end if
!
      end do
!
   end subroutine find_longest_substring_from_start
!
!
   subroutine find_longest_substring_from_end(string_a, string_b, index_b, length, found)
!!
!!    Find longest substring from end
!!    Written by Alexander C. Paul
!!
!!    Finds length and starting point of the longest substring containing
!!    last element of string A that is also contained in string B
!!
      implicit none
!
      character(len=*), intent(in) :: string_a, string_b
!
      integer, intent(out) :: index_b, length
!
      integer :: a, len_a
      logical :: found
!
      found = .false.
      index_b = 0
      length  = 0
!
      len_a = len(string_a)
!
      do a = len_a, 1, -1
!
         call is_substring_in_string(string_b, string_a(a:len_a), index_b, found)
!
         if (found) then
            length = len_a - a + 1
         end if
!
      end do
!
   end subroutine find_longest_substring_from_end
!
!
end module string_utilities
