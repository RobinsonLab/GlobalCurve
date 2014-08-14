function derivative(lamp) result(d_lamp)
! take derivative of data using central difference method
implicit none
real,dimension(:),intent(in) :: lamp
real,dimension(size(lamp)) :: d_lamp

! Local Var's
integer :: len,i

! Begin
len=size(lamp)
forall (i=2:len-1)
	d_lamp(i)=(lamp(i+1)-lamp(i-1))/2. ! central difference 
end forall
! @ boundaries use foreward and backward difference derivatives
d_lamp(1)=lamp(2)-lamp(1)
d_lamp(len)=lamp(len)-lamp(len-1)

end function derivative
