!RIMETTERE module myequations
!RIMETTERE
!RIMETTERE     use opendiff
!RIMETTERE
!RIMETTERE     type, extends(equation) :: burgers_equation_fd_1d
!RIMETTERE         contains
!RIMETTERE             procedure :: forcing => forcing_burgers
!RIMETTERE     endtype burgers_equation_fd_1d
!RIMETTERE
!RIMETTERE contains
!RIMETTERE
!RIMETTERE     function forcing_burgers(this, inp, t) result(opr)
!RIMETTERE         class(burgers_equation_fd_1d) :: this
!RIMETTERE         type(firstderive_operator) :: der
!RIMETTERE         type(field_fd_1d) :: inp
!RIMETTERE         type(field_fd_1d) :: opr
!RIMETTERE         real :: t
!RIMETTERE
!RIMETTERE         opr = der%derive(inp)
!RIMETTERE
!RIMETTERE     end function forcing_burgers
!RIMETTERE
!RIMETTERE end module myequations

program burgers

    use opendiff
!RIMETTERE    use myequations

    integer :: it
    integer :: er

    class(field), allocatable :: u1
    class(field), allocatable :: u2
    class(field), allocatable :: u3
    class(mesh), allocatable :: m1

    allocate(field_fd_1d :: u1)
    allocate(field_fd_1d :: u2)
    allocate(field_fd_1d :: u3)
    allocate(mesh_fd_1d :: m1)

!RIMETTERE    type(burgers_equation_fd_1d) :: burg_equ

    !type(euler_integrator) :: integ
!RIMETTERE    character(128) :: integrator_type = 'euler'
!RIMETTERE    class(integrator), allocatable :: integ

!RIMETTERE    if(integrator_type == 'euler') allocate(euler_integrator :: integ)
!RIMETTERE    if(integrator_type == 'lsrk')  allocate(lsrk_integrator  :: integ)
!RIMETTERE    integ%dt = 0.1

    call m1%init(error=er)

    call u1%init(m1, error=er)
    call u2%init(m1, error=er)
!    er = u3%init(m1)

    u3 = u1 + u2

    call m1%output(error=er)

    call u1%output("1ciao.dat", error=er)
    call u2%output("2ciao.dat", error=er)
    call u3%output("3ciao.dat", error=er)

!RIMETTERE    er = u1%output(filename="inizio.dat")
!RIMETTERE
!RIMETTERE    do it = t%itmin,1 !t%itmax
!RIMETTERE        print*,'it: ',it
!RIMETTERE        er = integ%integrate(inp=u1, equ=burg_equ, t=it*integ%dt)
!RIMETTERE    enddo
!RIMETTERE
!RIMETTERE    er = u1%output(filename="fine.dat")

end program burgers
