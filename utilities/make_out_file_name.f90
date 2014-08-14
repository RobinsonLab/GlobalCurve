function make_out_file_name(str1,str2,str3) result(res)
 	implicit none
	character(*), intent(in) :: str1
	character(*), intent(in) :: str2
	character(len(str1)),intent(out) :: str3
	integer :: res
! new file extension length must not exceed old one
!	character(len(str1)-3) :: root
	integer :: pos


	pos = index(str1,'.')
	if (pos > 0) then 
!		root = str1(1:pos)
		write(str3,'(a<pos>,a)') str1,str2
		res=0
		return
	else
		res=1
		return ! couldn't find a file extension in str1
	end if

!	print *,str3 
end function make_out_file_name

