function shift(lamp,d_lamp,q_shift)
! function shifts data by a given q_shift relative to the observer
! + qshift shifts the data to the right.
	use h_utils
	implicit none
	real,dimension(:),intent(in) :: lamp
	real,dimension(:),intent(in) :: d_lamp ! dim=size(lamp)
	real,intent(in) :: q_shift
	real,dimension(size(lamp)) :: shift

! Local Var's
integer :: len,i,num
real :: delta

!Begin
len=size(lamp)
call round(q_shift,num,delta)

if (num>0) then
	shift(num+1:len)=lamp(1:len-num)+d_lamp(1:len-num)*(-delta)
	shift(1:num)=0.
else
	num=-num
	shift(1:len-num)=lamp(num+1:len)+d_lamp(num+1:len)*(-delta)
	shift(len-num+1:len)=0.
endif

end function shift