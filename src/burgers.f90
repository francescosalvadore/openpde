module myequations
    !< Fake Burgers equation.

    use opendiff

    implicit none
    private
    public :: burgers_equation

    type, extends(equation) :: burgers_equation
        !< Fake Burgers equations.
        !<
        !< @note The reason the next is a pointer is just to make it a pointee
        !< when pointed inside forcing_burgers function.
        class(spatial_operator_der1), pointer :: der1 !< First derivative.
        contains
            procedure :: init => init_burgers        !< Initialize equation.
            procedure :: forcing => forcing_burgers  !< Forcing equation.
    endtype burgers_equation

contains
    function forcing_burgers(this, inp, t) result(opr)
        !< Return the field after forcing the equation.
        class(burgers_equation), intent(in)         :: this     !< The equation.
        class(field),            intent(in), target :: inp      !< Input field.
        real(R_P),               intent(in)         :: t        !< Time.
        class(field), allocatable                   :: opr      !< Field computed.
        class(spatial_operator_der1), pointer       :: der1_cur !< Dummy pointer for spatial operator.
        allocate(opr, source=inp)
!USELESS        associate(d1 => this%der1)
!USELESS            select type(d1)
!USELESS                type is(spatialop_fd_1d_der1_c)
!USELESS                    der1_cur => this%der1
!USELESS                class default
!USELESS                   STOP 'Error passing field to add'
!USELESS            endselect
!USELESS        endassociate
        der1_cur => this%der1
        opr = der1_cur%operate(inp)
!OK TOO        opr = this%der1%operate(inp)
    end function forcing_burgers

    function init_burgers(this) result(error)
        !< Initialize equation.
        class(burgers_equation), intent(inout) :: this  !< The equation.
        integer(I4P)                           :: error !< Error status.
        ! The next is to be read by JSON
        allocate(spatial_operator_der1_fd_1d :: this%der1)
        error = 0
    end function init_burgers

end module myequations

program burgers
    !< Testing program of fake Burgers equation.

    use opendiff
    use myequations

    class(mesh), allocatable       :: m1       !< Mesh.
    class(field), allocatable      :: u1       !< Field 1.
    class(field), allocatable      :: u2       !< Field 2.
    class(field), allocatable      :: u3       !< Field 3.
    class(integrator), allocatable :: integ    !< Integrator.
    type(burgers_equation)         :: burg_equ !< Burgers equation.
    integer(I_P)                   :: itmin=0  !< Fist time step.
    integer(I_P)                   :: itmax=10 !< Last time step.
    integer(I_P)                   :: it       !< Time step counter.
    integer(I_P)                   :: er       !< Error status.
    !DER IN MAIN class(spatialop), allocatable :: der1d

    ! These should be done reading from JSON input files and returning right
    ! pointers following factory pattern or similar
    allocate(mesh_fd_1d :: m1)
    allocate(field_fd_1d :: u1)
    allocate(field_fd_1d :: u2)
    !DER IN MAIN allocate(spatialop_fd_1d_der1_c :: der1d)
    allocate(integrator_euler :: integ)

    allocate(u3, source=u1)

    integ%dt = 0.1_R_P

    call m1%init(error=er)

    call u1%init(m1, error=er)
    call u2%init(m1, error=er)

    er = burg_equ%init()

    u3 = u1 + u1 * u2
    !DER IN MAIN u3 = der1d%operate(u1)

!RIMETTERE    er = u1%output(filename="inizio.dat")
!RIMETTERE
    do it = itmin, itmax
        print*,'it: ',it
        er = integ%integrate(inp=u1, equ=burg_equ, t=it*integ%dt)
    enddo
!RIMETTERE
!RIMETTERE    er = u1%output(filename="fine.dat")

    call m1%output(error=er)
    call u1%output("1ciao.dat", error=er)
    call u2%output("2ciao.dat", error=er)
    call u3%output("3ciao.dat", error=er)

end program burgers
