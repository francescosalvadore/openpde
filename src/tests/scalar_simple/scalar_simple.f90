!< Openpde test: scalar simple equation.
module scalar_simple_equation_m
    !< Scalar simple equation definition.
    !< The PDE solved is
    !<
    !< $$ \frac{\partial u}{\partial t} + a \frac{\partial u}{\partial x} + b \frac{\partial^2 u}{\partial x^2} = 0 $$
    !< where `a` and `b` are two constant scalars.
    !<
    !< This is a one dimensional, unsteady, linear PDE. It is solved adopting a finite difference approximations of
    !< first and second derivatives computed on a uniform mesh.
    !<
    !< The spatial operators are approximated as:
    !<
    !< $$ \frac{\partial u}{\partial x}|_i = \frac{u_{i+1}-u_i}{h} \quad i=1,N $$
    !< $$ \frac{\partial^2 u}{\partial x^2}|_i = \frac{u_{i+1}-2u_i+u_{i-1}}{h^2} \quad i=1,N $$
    !< where `N` is the number of discrete points and `h=L/N` is the (uniform) grid resolution, `L` is the domain length.
    !<
    !< The explicit Euler's method is used for advancing on time.

    use openpde

    implicit none
    private
    public :: scalar_simple_equation

    type, extends(equation) :: scalar_simple_equation
        !< (Fake) scalar_simple equations.
        !<
        !< @note The reason the `du` and `ddu` are pointers is just to make them a pointee when pointed
        !< inside [[equation:forcing]] function.
        real(R_P)                           :: a=0._R_P !< `a` equation coefficient.
        real(R_P)                           :: b=0._R_P !< `b` equation coefficient.
        class(spatial_operator_d1), pointer :: du       !< First derivative.
        class(spatial_operator_d2), pointer :: ddu      !< Second derivative.
        contains
            ! deferred public methods
            procedure, pass(this) :: bc      !< Equation boundary conditions.
            procedure, pass(this) :: forcing !< Forcing equation.
            procedure, pass(this) :: init    !< Initialize equation.
    endtype scalar_simple_equation
contains
    ! deferred public methods
    subroutine bc(this, inp, t)
        !< Equation boundary conditions imposition.
        class(scalar_simple_equation), intent(in)            :: this     !< The equation.
        class(field),                  intent(inout), target :: inp      !< Field.
        real(R_P),                     intent(in)            :: t        !< Time.
        class(field_FD_1D), pointer                          :: inp_cur  !< Pointer to input field.
        class(mesh_FD_1D), pointer                           :: mesh_cur !< Pointer to input mehs.
        integer(I_P)                                         :: i        !< Counter.

        call associate_field_FD_1D(field_input=inp, calling_procedure='bc(inp)', field_pointer=inp_cur)
        call associate_mesh_FD_1D(mesh_input=inp%m, calling_procedure='bc(inp%m)', mesh_pointer=mesh_cur)
        do i=1-mesh_cur%ng,0
            inp_cur%val(i) = inp_cur%val(i+mesh_cur%n)
        enddo
        do i=mesh_cur%n+1, mesh_cur%n+mesh_cur%ng
            inp_cur%val(i) = inp_cur%val(i-mesh_cur%n)
        enddo
    end subroutine bc

    function init(this) result(error)
        !< Initialize equation.
        class(scalar_simple_equation), intent(inout) :: this  !< The equation.
        integer(I_P)                                 :: error !< Error status.

        allocate(spatial_operator_d1_FD_1D :: this%du)
        allocate(spatial_operator_d2_FD_1D :: this%ddu)
        error = 0
    end function init

    function forcing(this, inp, t) result(opr)
        !< Return the field after forcing the equation.
        class(scalar_simple_equation), intent(in)         :: this    !< The equation.
        class(field),                  intent(in), target :: inp     !< Input field.
        real(R_P),                     intent(in)         :: t       !< Time.
        class(field), allocatable                         :: opr     !< Field computed after forcing the residual function.
        class(spatial_operator_d1), pointer               :: du_opr  !< Dummy pointer of the first derivative operator.
        class(spatial_operator_d2), pointer               :: ddu_opr !< Dummy pointer of the second derivative operator.
        class(field), allocatable                         :: du      !< Dummy storage of the first derivative operator result.
        class(field), allocatable                         :: ddu     !< Dummy storage of the second derivative operator result.

        allocate(opr, source=inp)
        allocate(du, source=inp)
        allocate(ddu, source=inp)
        du_opr => this%du
        ddu_opr => this%ddu
        du = du_opr%operate(inp)
        ddu = ddu_opr%operate(inp)
        opr = (-this%a) * du - this%b * ddu
    end function forcing
end module scalar_simple_equation_m

program scalar_simple
    !< Openpde test program: scalar_simple equation.

    use openpde
    use scalar_simple_equation_m

    class(mesh),       allocatable :: mesh_       !< The mesh.
    class(field),      allocatable :: u           !< The field.
    class(integrator), allocatable :: integrator_ !< The integrator.
    class(equation),   allocatable :: equation_   !< The equation.
    integer(I_P)                   :: itmin=0     !< Fist time step.
    integer(I_P)                   :: itmax=1000  !< Last time step.
    integer(I_P)                   :: it          !< Time step counter.
    integer(I_P)                   :: er          !< Error status.
    character(16)                  :: output_name !< Output file name.
    logical                        :: json_found  !< Flag inquiring the presence of json input file.

    allocate(mesh_FD_1D :: mesh_)
    allocate(field_FD_1D :: u)
    allocate(scalar_simple_equation :: equation_)
    allocate(integrator_euler_explicit :: integrator_)
    inquire(file='scalar_simple.json', exist=json_found)
    if (json_found) then
        call mesh_%init(filename='scalar_simple.json')
        call u%init(field_mesh=mesh_)
        er = equation_%init()
        integrator_%dt = 0.001_R_P
    else
        call mesh_%init(error=er)
        call u%init(field_mesh=mesh_, error=er)
        er = equation_%init()
        integrator_%dt = 0.001_R_P
    endif

    output_name = "out_XXXXXXXX.dat"
    write(output_name(5:12),"(I8.8)") itmin
    call u%output(output_name, error=er)
    do it = itmin, itmax
        er = integrator_%integrate(inp=u, equ=equation_, t=it*integrator_%dt)
        if(mod(it,10)==0) then
            print*,'it: ',it
            write(output_name(5:12),"(I8.8)") it
            call u%output(output_name, error=er)
        endif
    enddo
    call mesh_%output(error=er)
end program scalar_simple
