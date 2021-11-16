program ftensor
!
   use tensor_class, only: tensor
!
   type(tensor) :: my_tensor
!
   my_tensor = tensor([5, 100, 2, 6])
!
end program ftensor
