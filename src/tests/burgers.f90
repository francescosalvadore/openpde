module myequations

    use opendiff

    type, extends(equation) :: burgers_equation_fd_1d
        contains
            procedure :: forcing => forcing_burgers
    endtype burgers_equation_fd_1d

contains

    function forcing_burgers(this, inp, t) result(opr)
        class(burgers_equation_fd_1d) :: this
        class(field), target :: inp
        real :: t
        class(field), allocatable :: opr
        type(spatialop_fd_1d_der_c) :: der1d

        allocate(opr, source=inp)

        opr = der1d%operate(inp)

    end function forcing_burgers

end module myequations

program burgers

    use opendiff
    use myequations

    integer :: it
    integer :: er

    class(mesh), allocatable :: m1
    class(field), allocatable :: u1
    class(field), allocatable :: u2
    class(field), allocatable :: u3
    class(spatialop), allocatable :: der1d
    class(integrator), allocatable :: integ

    integer :: itmin=0, itmax=10

    type(burgers_equation_fd_1d) :: burg_equ

    ! These should be done reading from JSON input files and returning right
    ! pointers following factory pattern or similar
    allocate(mesh_fd_1d :: m1)    
    allocate(field_fd_1d :: u1)    
    allocate(field_fd_1d :: u2)    
    allocate(spatialop_fd_1d_der_c :: der1d)
    allocate(euler_integrator :: integ)

    allocate(u3, source=u1)    

    integ%dt = 0.1
 
    er = m1%init()

    er = u1%init(m1)
    er = u2%init(m1)

    !u3 = u1 + u1 * u2
    u3 = der1d%operate(u1)

!RIMETTERE    er = u1%output(filename="inizio.dat")
!RIMETTERE
    do it = itmin, itmax
        print*,'it: ',it
        er = integ%integrate(inp=u1, equ=burg_equ, t=it*integ%dt)
    enddo
!RIMETTERE
!RIMETTERE    er = u1%output(filename="fine.dat")

    er = m1%output()
    er = u1%output("1ciao.dat")
    er = u2%output("2ciao.dat")
    er = u3%output("3ciao.dat")
 
end program burgers
