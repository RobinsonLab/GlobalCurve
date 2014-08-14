function frac_sat(current_model,current_data) result(bind)
! bind represents the unscaled fractional saturation of the protein
! x_pts and K should be expressed as -log()
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: bind

! Global params
	! h_vars,intent(in)
	!	type(param_struct),dimension(:),intent(in) :: param_list

! Local Parameters
	real :: f_min,f_max
	real,dimension(current_data%len) :: x
	real,dimension(size(current_model%model%param(1)%param_basis)) :: Ka
	real,dimension(0:size(current_model%model%param(1)%param_basis)) :: term
	real :: Q
	integer :: ind,len,num_K
	integer :: i,j
! Begin
! numchan=expt_data%len
	num_K=size(current_model%model%param(1)%param_basis)
	len=current_data%len
	Ka=param_list(current_model%model%param(1)%param_basis)%val
	ind=floor(param_list(current_model%model%param(2)%param_basis(1))%val+0.1)
	f_min=param_list(current_model%model%param(3)%param_basis(1))%val
	f_max=param_list(current_model%model%param(3)%param_basis(2))%val

	x=10**(-current_data%x_pts) ! x is [Ca^2+]; x_pts are pCa

	term(0)=1.
	do i=1,len
		term(1)=Ka(1)*x(i)
		do j=2,num_K ! i is the time counter
			term(j)=term(j-1)*Ka(j)
		enddo !j
		Q=sum(term)
		bind(i)=f_min + (f_max-f_min)*term(ind-1)/Q
	enddo !i


return
end function frac_sat
