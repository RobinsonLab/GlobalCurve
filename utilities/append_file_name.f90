function append_string(str1,str2,str3) result(res)
 	implicit none
	character(*), intent(in) :: str1
	character(*), intent(in) :: str2
	character(len(str1)),intent(out) :: str3
	integer :: res
! new file extension length must not exceed old one
!	character(len(str1)-3) :: root

		write(str3,'a') str1,str2
		res=0
		return

!	print *,str3 
end function append_string