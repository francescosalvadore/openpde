!< Openpde test: scalar simple equation for Finite Volume 1D methods.
module scalar_simple_equation_FV_1D_m
    !< Scalar simple equation definition for Finite Volume 1D methods.
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

    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use json_module
    use openpde

    implicit none
    private
    public :: scalar_simple_equation

    type, extends(equation) :: scalar_simple_equation
        !< Scalar simple equations for Finite Volume 1D methods.
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
            ! public methods
            generic :: load => load_from_json !< Load equation definition from file.
            ! private methods
            procedure, pass(this), private :: load_from_json !< Load equation definition from jSON file.
    endtype scalar_simple_equation
contains
    ! deferred public methods
    subroutine bc(this, inp, t)
        !< Equation boundary conditions imposition.
        class(scalar_simple_equation), intent(in)            :: this     !< The equation.
        class(field),                  intent(inout), target :: inp      !< Field.
        real(R_P),                     intent(in)            :: t        !< Time.
        class(field_FV_1D), pointer                          :: inp_cur  !< Pointer to input field.
        class(mesh_FV_1D), pointer                           :: mesh_cur !< Pointer to input mehs.
        integer(I_P)                                         :: b        !< Counter.
        integer(I_P)                                         :: i        !< Counter.

        inp_cur => associate_field_FV_1D(field_input=inp, emsg='calling procedure scalar_simple_eqaution%bc')
        mesh_cur => associate_mesh_FV_1D(mesh_input=inp%m, emsg='calling procedure scalar_simple_eqaution%bc')
        do b=1, mesh_cur%nb
            if (b==1) then
                do i=1-mesh_cur%blocks(b)%ng,0
                    inp_cur%blocks(b)%val(i) = inp_cur%blocks(mesh_cur%nb)%val(i+mesh_cur%blocks(mesh_cur%nb)%n)
                enddo
                do i=mesh_cur%blocks(b)%n+1, mesh_cur%blocks(b)%n+mesh_cur%blocks(b)%ng
                    inp_cur%blocks(b)%val(i) = inp_cur%blocks(b+1)%val(i-mesh_cur%blocks(b+1)%n)
                enddo
            elseif (b==mesh_cur%nb) then
                do i=1-mesh_cur%blocks(b)%ng,0
                    inp_cur%blocks(b)%val(i) = inp_cur%blocks(b-1)%val(i+mesh_cur%blocks(b-1)%n)
                enddo
                do i=mesh_cur%blocks(b)%n+1, mesh_cur%blocks(b)%n+mesh_cur%blocks(b)%ng
                    inp_cur%blocks(b)%val(i) = inp_cur%blocks(1)%val(i-mesh_cur%blocks(1)%n)
                enddo
            else
                do i=1-mesh_cur%blocks(b)%ng,0
                    inp_cur%blocks(b)%val(i) = inp_cur%blocks(b-1)%val(i+mesh_cur%blocks(b-1)%n)
                enddo
                do i=mesh_cur%blocks(b)%n+1, mesh_cur%blocks(b)%n+mesh_cur%blocks(b)%ng
                    inp_cur%blocks(b)%val(i) = inp_cur%blocks(b+1)%val(i-mesh_cur%blocks(b+1)%n)
                enddo
            endif
        end do
    end subroutine bc

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

    subroutine init(this, description, filename, error)
        !< Initialize equation.
        class(scalar_simple_equation), intent(inout)         :: this        !< The equation.
        character(*),                  intent(in),  optional :: description !< Equation description.
        character(*),                  intent(in),  optional :: filename    !< Initialization file name.
        integer(I_P),                  intent(out), optional :: error       !< Error status.

        if (present(filename)) then
            call this%load(filename=filename, error=error)
        else
            this%a = -1.0_R_P
            this%b = -0.1_R_P
            allocate(spatial_operator_d1_FV_1D :: this%du)
            allocate(spatial_operator_d2_FV_1D :: this%ddu)
            if (present(error)) error = 0
        endif
    end subroutine init

    ! private methods
    subroutine load_from_json(this, filename, error)
        !< Load mesh definition from JSON file.
        class(scalar_simple_equation), intent(inout)         :: this      !< The equation.
        character(*),                  intent(in)            :: filename  !< File name of JSON file.
        integer(I_P),                  intent(out), optional :: error     !< Error status.
        character(len=:), allocatable                        :: mesh_type !< Mesh type.
        type(json_file)                                      :: json      !< JSON file handler.
        logical                                              :: found     !< Flag inquiring the result json parsing.

        call json%initialize()
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        call json%load_file(filename=filename)
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        call json%get('mesh.type', mesh_type, found)
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        if (.not.found) then
            write(stderr, "(A)")' error: mesh definition of "'//filename//'" incomplete!'
            write(stderr, "(A)")'   "type" missing'
            stop
        endif
        if (mesh_type=="finite volume 1D") then
            allocate(spatial_operator_d1_FV_1D :: this%du)
            allocate(spatial_operator_d2_FV_1D :: this%ddu)
            call json%get('equation.a', this%a, found)
            if (json%failed()) then
                call json%print_error_message(stderr) ; stop
            end if
            if (.not.found) then
                write(stderr, "(A)")' error: equation definition of "'//filename//'" incomplete!'
                write(stderr, "(A)")'   "a" missing'
                stop
            endif
            call json%get('equation.b', this%b, found)
            if (json%failed()) then
                call json%print_error_message(stderr) ; stop
            end if
            if (.not.found) then
                write(stderr, "(A)")' error: equation definition of "'//filename//'" incomplete!'
                write(stderr, "(A)")'   "b" missing'
                stop
            endif
        else
            write(stderr, "(A)")' error: mesh definition of "'//filename//'" is not "finite volume 1D"!'
            stop
        endif
    endsubroutine load_from_json
end module scalar_simple_equation_FV_1D_m

program scalar_simple_FV_1D
    !< Openpde test: scalar simple equation for Finite Volume 1D methods.

    use openpde
    use scalar_simple_equation_FV_1D_m

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

    allocate(mesh_FV_1D :: mesh_)
    allocate(field_FV_1D :: u)
    allocate(scalar_simple_equation :: equation_)
    allocate(integrator_euler_explicit :: integrator_)
    inquire(file='scalar_simple_FV_1D.json', exist=json_found)
    if (json_found) then
        call mesh_%init(filename='scalar_simple_FV_1D.json')
        call u%init(field_mesh=mesh_)
        call equation_%init(filename='scalar_simple_FV_1D.json')
        call integrator_%init(filename='scalar_simple_FV_1D.json')
    else
        call mesh_%init
        call u%init(field_mesh=mesh_)
        call equation_%init
        call integrator_%init
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
end program scalar_simple_FV_1D
