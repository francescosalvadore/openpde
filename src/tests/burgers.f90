!< Openpde test program: Burgers equation.
module burgers_equation_m
    !< (Fake) Burgers equation definition.

    use openpde

    implicit none
    private
    public :: burgers_equation

    type, extends(equation) :: burgers_equation
        !< (Fake) Burgers equations.
        !<
        !< @note The reason the next is a pointer is just to make it a pointee when pointed inside forcing_burgers function.
        class(spatial_operator_d1), pointer :: d1 !< First derivative.
        class(spatial_operator_d2), pointer :: d2 !< Second derivative.
        contains
            ! deferred public methods
            procedure :: bc => bc_burgers            !< Equation boundary conditions.
            procedure :: forcing => forcing_burgers  !< Forcing equation.
            procedure :: init => init_burgers        !< Initialize equation.
    endtype burgers_equation

contains
    ! deferred public methods
    subroutine bc_burgers(this, inp, t)
        !< Equation boundary conditions imposition.
        class(burgers_equation), intent(in)            :: this     !< The equation.
        class(field),            intent(inout), target :: inp      !< Field.
        real(R_P),               intent(in)            :: t        !< Time.
        class(field_FD_1D), pointer                    :: inp_cur  !< Pointer to input field.
        class(mesh_FD_1D), pointer                     :: mesh_cur !< Pointer to input mehs.
        integer(I_P)                                   :: i        !< Counter.

        call associate_field_FD_1D(field_input=inp,                     &
                                   calling_procedure='bc_burgers(inp)', &
                                   field_pointer=inp_cur)
        call associate_mesh_FD_1D(mesh_input=inp%m,                      &
                                  calling_procedure='bc_burgers(inp%m)', &
                                  mesh_pointer=mesh_cur)
        do i=1-mesh_cur%ng,0
            inp_cur%val(i) = inp_cur%val(i+mesh_cur%n)
        enddo
        do i=mesh_cur%n+1, mesh_cur%n+mesh_cur%ng
            inp_cur%val(i) = inp_cur%val(i-mesh_cur%n)
        enddo
    end subroutine bc_burgers

    function init_burgers(this) result(error)
        !< Initialize equation.
        class(burgers_equation), intent(inout) :: this  !< The equation.
        integer(I_P)                           :: error !< Error status.

        ! The next is to be read by JSON
        allocate(spatial_operator_d1_FD_1D :: this%d1)
        allocate(spatial_operator_d2_FD_1D :: this%d2)
        error = 0
    end function init_burgers

    function forcing_burgers(this, inp, t) result(opr)
        !< Return the field after forcing the equation.
        class(burgers_equation), intent(in)         :: this   !< The equation.
        class(field),            intent(in), target :: inp    !< Input field.
        real(R_P),               intent(in)         :: t      !< Time.
        class(field), allocatable                   :: opr    !< Field computed.
        class(spatial_operator_d1), pointer         :: d1_cur !< Dummy pointer for spatial operator.
        class(spatial_operator_d2), pointer         :: d2_cur !< Dummy pointer for spatial operator.
        class(field), allocatable                   :: opr1   !< Field computed.
        class(field), allocatable                   :: opr2   !< Field computed.

        allocate(opr, source=inp)
        allocate(opr1, source=inp)
        allocate(opr2, source=inp)
        d1_cur => this%d1
        d2_cur => this%d2
        opr1 = d1_cur%operate(inp)
        opr2 = d2_cur%operate(inp)
        opr = opr1 + 0.1_R_P * opr2
    end function forcing_burgers
end module burgers_equation_m

program burgers
    !< Openpde test program: Burgers equation.

    use openpde
    use burgers_equation_m

    class(mesh), allocatable       :: mesh_       !< Mesh.
    class(field), allocatable      :: u1          !< Field 1.
    class(integrator), allocatable :: integrator_ !< Integrator.
    type(burgers_equation)         :: equ_burgers !< Burgers equation.
    integer(I_P)                   :: itmin=0     !< Fist time step.
    integer(I_P)                   :: itmax=1000  !< Last time step.
    integer(I_P)                   :: it          !< Time step counter.
    integer(I_P)                   :: er          !< Error status.
    character(16)                  :: output_name !< Output file name.

    ! These should be done reading from JSON input files and returning right pointers following factory pattern or similar
    allocate(mesh_FD_1D :: mesh_)
    allocate(field_FD_1D :: u1)
    allocate(integrator_euler_explicit :: integrator_)
    call mesh_%init(error=er)
    call u1%init(field_mesh=mesh_, error=er)
    er = equ_burgers%init()
    integrator_%dt = 0.001_R_P
    output_name = "out_XXXXXXXX.dat"
    write(output_name(5:12),"(I8.8)") itmin
    call u1%output(output_name, error=er)
    do it = itmin, itmax
        er = integrator_%integrate(inp=u1, equ=equ_burgers, t=it*integrator_%dt)
        if(mod(it,10)==0) then
            print*,'it: ',it
            write(output_name(5:12),"(I8.8)") it
            call u1%output(output_name, error=er)
        endif
    enddo
    call mesh_%output(error=er)
end program burgers
