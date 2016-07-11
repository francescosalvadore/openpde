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
            procedure :: bc => bc_burgers            !< Equation boundary conditions imposition.
            procedure :: forcing => forcing_burgers  !< Forcing equation.
            procedure :: init => init_burgers        !< Initialize equation.
    endtype burgers_equation

contains
    subroutine bc_burgers(this, inp, t)
        !< Equation boundary conditions imposition.
        class(burgers_equation), intent(in)            :: this     !< The equation.
        class(field),            intent(inout), target :: inp      !< Field.
        real(R_P),               intent(in)            :: t        !< Time.
        class(field_fd_1d), pointer                    :: inp_cur  !< Pointer to input field.
        class(mesh_fd_1d), pointer                     :: mesh_cur !< Pointer to input mehs.
        integer(I_P)                                   :: ng       !< Number of ghost cells.
        integer(I_P)                                   :: n        !< Counter.
        integer(I_P)                                   :: i        !< Counter.

        select type(inp)
            type is(field_fd_1d)
                inp_cur => inp
            class default
               STOP 'Error passing field to bc'
        end select

        associate(mm => inp%m)
            select type(mm)
                type is(mesh_fd_1d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate

        n  = mesh_cur%n
        ng = mesh_cur%ng
        do i=1-ng,0
            inp_cur%val(i) = inp_cur%val(i+n)
        enddo
        do i=n+1,n+ng
            inp_cur%val(i) = inp_cur%val(i-n)
        enddo

    end subroutine bc_burgers

    function init_burgers(this) result(error)
        !< Initialize equation.
        class(burgers_equation), intent(inout) :: this  !< The equation.
        integer(I_P)                           :: error !< Error status.
        ! The next is to be read by JSON
        allocate(spatial_operator_der1_fd_1d :: this%der1)
        error = 0
    end function init_burgers

    function forcing_burgers(this, inp, t) result(opr)
        !< Return the field after forcing the equation.
        class(burgers_equation), intent(in)         :: this     !< The equation.
        class(field),            intent(in), target :: inp      !< Input field.
        real(R_P),               intent(in)         :: t        !< Time.
        class(field), allocatable                   :: opr      !< Field computed.
        class(spatial_operator_der1), pointer       :: der1_cur !< Dummy pointer for spatial operator.
        allocate(opr, source=inp)

        der1_cur => this%der1
        opr = der1_cur%operate(inp)
!OK TOO        opr = this%der1%operate(inp)
    end function forcing_burgers
end module myequations

program burgers
    !< Testing program of fake Burgers equation.

    use opendiff
    use myequations

    class(mesh), allocatable       :: m1       !< Mesh.
    class(field), allocatable      :: u1       !< Field 1.
    !TEST class(field), allocatable      :: u2       !< Field 2.
    !TEST class(field), allocatable      :: u3       !< Field 3.
    class(integrator), allocatable :: integ    !< Integrator.
    type(burgers_equation)         :: burg_equ !< Burgers equation.
    integer(I_P)                   :: itmin=0  !< Fist time step.
    integer(I_P)                   :: itmax=1000 !< Last time step.
    integer(I_P)                   :: it       !< Time step counter.
    integer(I_P)                   :: er       !< Error status.
    !TEST class(spatialop), allocatable :: der1d
    character(16)                  :: output_name !< Output file name.

    ! These should be done reading from JSON input files and returning right
    ! pointers following factory pattern or similar
    allocate(mesh_fd_1d :: m1)
    allocate(field_fd_1d :: u1)
    !TEST allocate(field_fd_1d :: u2)
    !TEST allocate(spatialop_fd_1d_der1_c :: der1d)
    allocate(integrator_euler :: integ)

    !TEST allocate(u3, source=u1)

    integ%dt = 0.001_R_P

    call m1%init(error=er)

    call u1%init(m1, error=er)

    er = burg_equ%init()

    !TEST call u2%init(m1, error=er)
    !TEST u3 = u1 + u1 * u2
    !TEST u3 = der1d%operate(u1)

    output_name = "out_XXXXXXXX.dat"
    write(output_name(5:12),"(I8.8)") itmin
    call u1%output(output_name, error=er)
    do it = itmin, itmax
        er = integ%integrate(inp=u1, equ=burg_equ, t=it*integ%dt)
        if(mod(it,10)==0) then
            print*,'it: ',it
            write(output_name(5:12),"(I8.8)") it
            call u1%output(output_name, error=er)
        endif
    enddo

    call m1%output(error=er)

end program burgers
