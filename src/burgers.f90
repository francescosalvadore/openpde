module myequations

    use opendiff
    use opendiff_kinds

    implicit none
    private
    public :: burgers_equation

    type, extends(equation) :: burgers_equation
        ! The reason the next is a pointer is just to make it a pointee
        ! when pointed inside forcing_burgers function
        class(spatial_operator_der1), pointer :: der1
        contains
            procedure :: init => init_burgers
            procedure :: forcing => forcing_burgers
    endtype burgers_equation

contains

    function init_burgers(this) result(res)
        class(burgers_equation) :: this
        integer :: res

        ! The next is to be read by JSON
        allocate(spatial_operator_der1_fd_1d :: this%der1)

        res = 0
    end function init_burgers

    function forcing_burgers(this, inp, t) result(opr)
        class(burgers_equation) :: this
        class(field), target :: inp
        real(R8P) :: t
        class(field), allocatable :: opr
        class(spatial_operator_der1), pointer :: der1_cur

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
    !DER IN MAIN class(spatialop), allocatable :: der1d
    class(integrator), allocatable :: integ

    integer :: itmin=0, itmax=10

    type(burgers_equation) :: burg_equ

    ! These should be done reading from JSON input files and returning right
    ! pointers following factory pattern or similar
    allocate(mesh_fd_1d :: m1)
    allocate(field_fd_1d :: u1)
    allocate(field_fd_1d :: u2)
    !DER IN MAIN allocate(spatialop_fd_1d_der1_c :: der1d)
    allocate(integrator_euler :: integ)

    allocate(u3, source=u1)

    integ%dt = 0.1

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
