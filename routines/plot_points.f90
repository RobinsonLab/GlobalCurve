function plot_points(x_points,y_points,x_lims,y_lims,title, &
	axes_label,symbol,device,add_to_plot,color,llog) result(deviceID)
	implicit none 
	real,dimension(:),target,intent(in) :: x_points
	real,dimension(:),target,intent(in) :: y_points
	real,dimension(2),optional,target,intent(in) :: x_lims
	real,dimension(2),optional,target,intent(inout) :: y_lims
	character(*),optional,intent(in) :: title
	character(*),dimension(2),optional,intent(in) :: axes_label
	integer,optional,intent(in) :: symbol
	character(*),optional,intent(in) :: device
	logical,optional, intent(in) :: add_to_plot
	integer,optional,intent(in) :: color
	logical,optional, intent(in) :: llog
	integer :: deviceID

! Local Params	

	character,parameter :: Dtitle = ''
 	character(1),dimension(2),parameter :: Daxes_label = (/'x','y'/)
 	integer,parameter :: Dsymbol = 9
 	character(3),parameter :: Ddevice = '/w9'
	integer :: Dcolor = 1

	real,dimension(:),pointer :: Wx_lims
	real,dimension(:),pointer :: Wy_lims
	real,dimension(:),pointer :: Wy_points
	character(80) :: Wtitle
	character(80),dimension(2) :: Waxes_label
	integer :: Wsymbol
	character(80) :: Wdevice
	logical :: Wadd_to_plot
	integer :: Wcolor
	integer :: plot_type
	integer :: len,i
! points = {{x_1,x_2,...},{y_1,y_2,...}}^T
! lims = {{x_min,x_max},{y_min,y_max}}

! Begin

      INTEGER PGOPEN

	
!	write(*,*)'plot points called'
	
	if (present(llog)) then
		if (llog) then
			allocate(Wy_points(size(y_points)))
			plot_type = 20
			Wy_points=0.
! a where construct would be more appropriate but i can't get it to work
			forall(i=1:size(y_points),y_points(i) > 0.)
				Wy_points(i)=LOG10(y_points(i))
			end forall
!			elsewhere
!				Wy_points=0.

!			endwhere
		else
			plot_type = 1
			Wy_points=>y_points
		endif
	else
		plot_type = 1
		Wy_points=>y_points
	endif


 	if (present(x_lims)) then
		Wx_lims => x_lims
	else
		allocate(Wx_lims(2))
		Wx_lims = (/ minval(x_points), maxval(x_points) /)
	end if

 	if (present(y_lims)) then
		if (present(llog)) then
			if (llog) then
				allocate(Wy_lims(2))
				if (y_lims(1) < 1.) y_lims(1)=1.
				Wy_lims = log10(y_lims)
			else
				Wy_lims => y_lims 
			endif
		else
			Wy_lims => y_lims 
		endif
	else	
		allocate(Wy_lims(2))
		Wy_lims = (/ minval(Wy_points), maxval(Wy_points) /)
	end if
	if (present(title)) then
		Wtitle = title
	else
		Wtitle = Dtitle
	end if
	if (present(axes_label)) then
		Waxes_label = axes_label
	else
		Waxes_label = Daxes_label
	end if
	if (present(symbol)) then
		Wsymbol = symbol
	else
		Wsymbol = Dsymbol
	end if
	if (present(device)) then
		Wdevice = device
	else
		Wdevice = Ddevice
	end if
	if (present(color)) then
		Wcolor = color
	else
		Wcolor = Dcolor
	end if
	
	


! Call PGENV to specify the range of the axes and to draw a box, and
! PGLAB to label it. 

	Wadd_to_plot = .false.
	if ( present(add_to_plot) ) then
		if (add_to_plot) then
			Wadd_to_plot=.true.
		else
			Wadd_to_plot=.false.
		end if
	end if
	if ( Wadd_to_plot ) then
		deviceID = -1
	else
		deviceID = PGOPEN(Wdevice)
		IF (deviceID .LE. 0) STOP 'Failure opening graphics window'
		CALL PGENV(Wx_lims(1),Wx_lims(2),Wy_lims(1),Wy_lims(2),0,plot_type)
		CALL PGLAB(Waxes_label(1), Waxes_label(2), Wtitle)
	end if
! change color for points
	call pgsci(Wcolor)

    CALL PGPT(size(x_points),x_points,Wy_points,Wsymbol)
 
      
!	  CALL PGCLOS
	deallocate(Wy_points)
	return      
END function

