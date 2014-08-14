subroutine round(qs,num,delta)
implicit none
real,intent(in) :: qs
integer,intent(out) :: num
real, intent(out) :: delta

! qs must be posive
	num=floor(qs)
	delta=qs-num
	if (delta>0.5) then
		delta=delta-1
		num=num+1
	endif
end subroutine
