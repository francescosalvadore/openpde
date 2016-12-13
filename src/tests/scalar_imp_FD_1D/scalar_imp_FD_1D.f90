!< Openpde test: scalar simple equation for Finite Difference 1D methods.
module scalar_imp_equation_FD_1D_m
    !< Scalar simple equation definition for Finite Difference 1D methods.
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
    public :: scalar_imp_equation_adv

    type, extends(equation_adv) :: scalar_imp_equation_adv
        !< Scalar simple equations for Finite Difference 1D methods.
        !<
        !< @note The reason the `du_opr` and `ddu_opr` are pointers is just to make them a pointee when pointed
        !< inside [[equation:forcing]] function.
        real(R_P)                           :: a=0._R_P     !< `a` equation coefficient.
        real(R_P)                           :: b=0._R_P     !< `b` equation coefficient.
        class(spatial_operator_d1), pointer :: du_opr       !< First derivative.
        class(spatial_operator_d2), pointer :: ddu_opr      !< Second derivative.
        class(field), allocatable           :: du           !< Dummy storage of the first derivative operator result.
        class(field), allocatable           :: ddu          !< Dummy storage of the second derivative operator result.
        class(field), allocatable           :: dumg(:,:)           !< Dummy storage of the first derivative operator result.
        class(field), allocatable           :: ddumg(:,:)          !< Dummy storage of the second derivative operator result.

        class(f2m_d2), pointer :: ddui_opr                   !< First derivative implicit.
        class(f2m_d1), pointer :: dui_opr                   !< First derivative implicit.
        class(matrix), allocatable :: dui
        class(matrix), allocatable :: ddui
        contains
            ! deferred public methods
            procedure, pass(this) :: bc_e    !< Equation boundary conditions.
            procedure, pass(this) :: bc_i    !< Equation boundary conditions.
            procedure, pass(this) :: resid_e !< Forcing equation explicit.
            procedure, pass(this) :: resid_i !< Forcing equation implicit.
            procedure, pass(this) :: init    !< Initialize equation.
            ! public methods
            generic :: load => load_from_json !< Load equation definition from file.
            ! private methods
            procedure, pass(this), private :: load_from_json !< Load equation definition from jSON file.
    endtype scalar_imp_equation_adv
contains
    ! deferred public methods
    subroutine init(this, n_equ, field_mesh, inp, description, filename, error)
        !< Initialize equation.
        class(scalar_imp_equation_adv), intent(inout)         :: this        !< The equation.
        integer(I_P),                   intent(in)            :: n_equ       !< Number of equations
        class(mesh),                    intent(in), target    :: field_mesh  !< Mesh of the field.
        class(field),                   intent(inout), target    :: inp(:)      !< Input field.
        character(*),                   intent(in),  optional :: description !< Equation description.
        character(*),                   intent(in),  optional :: filename    !< Initialization file name.
        integer(I_P),                   intent(out), optional :: error       !< Error status.
        integer(I_P)                                          :: ie          !< Equation index
        integer(I_P)                                          :: n           !< Counter.
        class(mesh_FD_1D), pointer                            :: mesh_cur    !< Mesh pointer.

        mesh_cur => associate_mesh_FD_1D(mesh_input=field_mesh)
        n = mesh_cur%n

        this%n_equ = n_equ
        this%n_size = n

        ! explicit section
        this%enable_explicit = .true.
        allocate(field_FD_1D :: this%resvar_e(n_equ))
        do ie=1,n_equ
            call this%resvar_e(ie)%init(field_mesh=field_mesh)
        enddo

        allocate(this%du, mold=inp(1))
        allocate(this%ddu, mold=inp(1))
        call this%du%init(field_mesh=field_mesh)
        call this%ddu%init(field_mesh=field_mesh)

        if (present(filename)) then
            call this%load(filename=filename, error=error)
        else
            this%a = -1.0_R_P
            this%b = -0.1_R_P
            allocate(spatial_operator_d1_FD_1D :: this%du_opr)
            allocate(spatial_operator_d2_FD_1D :: this%ddu_opr)
            if (present(error)) error = 0
        endif

        ! implicit section
        this%enable_implicit = .false.
        allocate(linsolver_gmlapack :: this%solver)
        call this%solver%init(n) !TODO should be generalized on 50

        allocate(f2m_d1_FD_1D  :: this%dui_opr)
        allocate(this%dui_opr%mat, mold=this%solver%mat) !TODO should be put in init()

        allocate(f2m_d2_FD_1D  :: this%ddui_opr)
        allocate(this%ddui_opr%mat, mold=this%solver%mat) !TODO should be put in init()

        allocate(this%dui, mold=this%solver%mat)
        allocate(this%ddui, mold=this%solver%mat)

        allocate(this%resvar_i, mold=this%solver%mat)
        call this%resvar_i%init(n)

        allocate(f2v_FD_1D :: this%f2v_opr)
        allocate(this%f2v_opr%vec, mold=this%solver%vec) ! TODO should be put in init()

        allocate(v2f_FD_1D :: this%v2f_opr)
        this%v2f_opr%mesh => inp(1)%m
        this%v2f_opr%n_equ = n_equ

        ! multigrid section
        allocate(multigrid_FD_1D :: this%mg)
        call this%mg%init(inp=inp(1), levels_number=2)
        allocate(this%dumg(size(inp, dim=1), this%mg%levels_number), mold=inp(1))
        call this%mg%create_subgrids_field(inp=inp, subgrids=this%dumg)
        allocate(this%ddumg(size(inp, dim=1), this%mg%levels_number), mold=inp(1))
        call this%mg%create_subgrids_field(inp=inp, subgrids=this%ddumg)

    end subroutine init

    subroutine bc_e(this, inp, t)
        !< Equation boundary or fixed conditions imposition.
        class(scalar_imp_equation_adv), intent(in)            :: this     !< The equation.
        class(field),                   intent(inout), target :: inp(:)   !< Field.
        real(R_P),                      intent(in)            :: t        !< Time.
        class(field_FD_1D), pointer                           :: inp_cur  !< Pointer to input field.
        class(mesh_FD_1D), pointer                            :: mesh_cur !< Pointer to input mehs.
        integer(I_P)                                          :: i        !< Counter.
        integer(I_P)                                          :: ie       !< Counter.

        do ie=1,size(inp)
            inp_cur => associate_field_FD_1D(field_input=inp(ie), emsg='calling procedure scalar_simple_eqaution%bc')
            mesh_cur => associate_mesh_FD_1D(mesh_input=inp(ie)%m, emsg='calling procedure scalar_simple_eqaution%bc')
            do i=1-mesh_cur%ng,0
                inp_cur%val(i) = 0._R_P ! inp_cur%val(i+mesh_cur%n)
            enddo
            do i=mesh_cur%n+1, mesh_cur%n+mesh_cur%ng
                inp_cur%val(i) = 0._R_P ! inp_cur%val(i-mesh_cur%n)
            enddo
        enddo
    end subroutine bc_e

    subroutine bc_i(this, matA, vecB, t)
        !< Equation boundary conditions imposition.
        class(scalar_imp_equation_adv), intent(in)            :: this     !< The equation.
        class(matrix),                  intent(inout), target :: matA     !< Input field.
        class(vector),                  intent(inout), target :: vecB     !< Input field.
        real(R_P),                      intent(in)            :: t        !< Time.
        class(field_FD_1D), pointer                           :: inp_cur  !< Pointer to input field.
        class(mesh_FD_1D), pointer                            :: mesh_cur !< Pointer to input mehs.
        integer(I_P)                                          :: i        !< Counter.
        integer(I_P)                                          :: ie       !< Counter.
        integer(I_P)                                          :: n        !< Counter.
        integer(I_P)                                          :: j        !< Counter.

        n = matA%n
        !matA_cur => associate_matrix_simple(matrix_input=matA)
        !vecB_cur => associate_vector_simple(vector_input=vecB)
        call vecB%set(1,0._R_P)
        call vecB%set(vecB%n,0._R_P)
        do i=1,n,n-1
            do j=1,n
                call matA%set(1, j, 0._R_P)
            enddo
        enddo
        call matA%set(1, 1, 1._R_P)
        call matA%set(n, n, 1._R_P)
        call matA%output("matAbis.dat")
        call vecB%output("vecBbis.dat")
    end subroutine bc_i

    subroutine resid_e(this, inp, t)
        !< Return the field after forcing the equation.
        class(scalar_imp_equation_adv), intent(inout)      :: this    !< The equation.
        class(field),                   intent(in), target :: inp(:)  !< Input field.
        real(R_P),                      intent(in)         :: t       !< Time.
        class(spatial_operator_d1), pointer                :: du_opr  !< Dummy pointer of the first derivative operator.
        class(spatial_operator_d2), pointer                :: ddu_opr !< Dummy pointer of the second derivative operator.

        du_opr   => this%du_opr
        ddu_opr  => this%ddu_opr
        this%du  =  du_opr%operate(inp(1))
        this%ddu =  ddu_opr%operate(inp(1))

        this%resvar_e(1) = (-this%a) * this%du - this%b * this%ddu

    end subroutine resid_e

    subroutine resid_emg(this, inp, t, output, i_mg)
        !< Return the field after forcing the equation.
        class(scalar_imp_equation_adv), intent(inout)         :: this      !< The equation.
        class(field),                   intent(inout), target :: inp(:)    !< Input field.
        real(R_P),                      intent(in)            :: t         !< Time.
        class(field),                   intent(inout), target :: output(:) !< Input field.
        integer(I_P),                   intent(in)            :: i_mg      !< Time.
        class(spatial_operator_d1), pointer                   :: du_opr    !< Dummy pointer of the first derivative operator.
        class(spatial_operator_d2), pointer                   :: ddu_opr   !< Dummy pointer of the second derivative operator.

        ! call this%bc_emg(inp, t) ! cazzo
        call this%bc_e(inp, t)

        du_opr           => this%du_opr
        ddu_opr          => this%ddu_opr
        this%dumg(1, i_mg)  =  du_opr%operate(inp(1))
        this%ddumg(1, i_mg) =  ddu_opr%operate(inp(1))

        output(1) = (-this%a) * this%dumg(1, i_mg) - this%b * this%ddumg(1, i_mg)

    end subroutine resid_emg

    subroutine resid_i(this, inp, t)
        !< Return the matrix of residuals.
        class(scalar_imp_equation_adv), intent(inout)      :: this     !< The equation.
        class(field),                   intent(in), target :: inp(:)   !< Input field.
        real(R_P),                      intent(in)         :: t        !< Time.
        class(f2m_d1), pointer                             :: dui_opr  !< Dummy pointer of the first derivative operator.
        class(f2m_d2), pointer                             :: ddui_opr !< Dummy pointer of the first derivative operator.

        ddui_opr => this%ddui_opr
        this%ddui = ddui_opr%operate(inp) !RIMETTERE ,1,1)

        dui_opr => this%dui_opr
        this%dui = dui_opr%operate(inp) !RIMETTERE ,1,1)

        this%resvar_i = (-this%a) * this%dui - this%b * this%ddui

    end subroutine resid_i

    ! private methods
    subroutine load_from_json(this, filename, error)
        !< Load mesh definition from JSON file.
        class(scalar_imp_equation_adv), intent(inout)         :: this      !< The equation.
        character(*),                   intent(in)            :: filename  !< File name of JSON file.
        integer(I_P),                   intent(out), optional :: error     !< Error status.
        character(len=:), allocatable                         :: mesh_type !< Mesh type.
        type(json_file)                                       :: json      !< JSON file handler.
        logical                                               :: found     !< Flag inquiring the result json parsing.

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
        if (mesh_type=="finite difference 1D") then
            allocate(spatial_operator_d1_FD_1D :: this%du_opr)
            allocate(spatial_operator_d2_FD_1D :: this%ddu_opr)
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
            write(stderr, "(A)")' error: mesh definition of "'//filename//'" is not "finite difference 1D"!'
            stop
        endif
    endsubroutine load_from_json
end module scalar_imp_equation_FD_1D_m

program scalar_imp_FD_1D
    !< Openpde test: scalar simple equation for Finite Difference 1D methods.

    use openpde
    use scalar_imp_equation_FD_1D_m

    class(mesh), allocatable                :: mesh_       !< The mesh.
    class(field), allocatable, dimension(:) :: u           !< The field.
    class(integrator_adv), allocatable      :: integrator_ !< The integrator.
    class(equation_adv), allocatable        :: equation_   !< The equation.
    integer(I_P)                            :: itmin=0     !< Fist time step.
    integer(I_P)                            :: itmax=1000  !< Last time step.
    integer(I_P)                            :: n_equ=1     !< Number of equations
    integer(I_P)                            :: it          !< Time step counter.
    integer(I_P)                            :: ie          !< Number of fields counter
    integer(I_P)                            :: er          !< Error status.
    character(16)                           :: output_name !< Output file name.
    logical                                 :: json_found  !< Flag inquiring the presence of json input file.

    allocate(mesh_FD_1D :: mesh_)
    allocate(field_FD_1D :: u(n_equ))
    allocate(scalar_imp_equation_adv :: equation_)
    !EXPLICIT allocate(integrator_adv_euler_explicit :: integrator_)
    allocate(integrator_adv_euler_implicit :: integrator_)
    inquire(file='scalar_imp_FD_1D.json', exist=json_found)
    if (json_found) then
        call mesh_%init(filename='scalar_imp_FD_1D.json')
        do ie = 1,n_equ
            call u(ie)%init(field_mesh=mesh_)
        enddo
        call equation_%init(filename='scalar_imp_FD_1D.json',n_equ=1, field_mesh=mesh_,inp=u)
        call integrator_%init(filename='scalar_imp_FD_1D.json', equ=equation_)
    else
        do ie = 1,n_equ
            call u(ie)%init(field_mesh=mesh_)
        enddo
        call equation_%init(n_equ=1, field_mesh=mesh_,inp=u)
        call integrator_%init(equ=equation_)
    endif

    output_name = "out_XXXXXXXX.dat"
    write(output_name(5:12),"(I8.8)") itmin
    do ie = 1,n_equ
        call u(ie)%output(output_name, error=er)
    enddo
    do it = itmin, itmax
        er = integrator_%integrate(inp=u, equ=equation_, t=it*integrator_%dt)
        if(mod(it,10)==0) then
            print*,'it: ',it
            write(output_name(5:12),"(I8.8)") it
            do ie = 1,n_equ
                call u(ie)%output(output_name, error=er)
            enddo
        endif
    enddo
    call mesh_%output(error=er)
end program scalar_imp_FD_1D
