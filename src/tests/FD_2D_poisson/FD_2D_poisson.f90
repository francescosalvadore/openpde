!< Openpde test: poisson FD 2D
module fd_2d_poisson_m
    !< Openpde test: poisson FD 2D

    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use json_module
    use openpde

    implicit none
    private
    public :: fd_2d_poisson_equation_adv

    type, extends(equation_adv) :: fd_2d_poisson_equation_adv
        !< Scalar simple equations for Finite Difference 1D methods.
        !<
        !< @note The reason the `ddu_opr` are pointers is just to make them a pointee when pointed
        !< inside [[equation:forcing]] function.
        real(R_P)                           :: a=0._R_P     !< `a` equation coefficient.
        real(R_P)                           :: b=0._R_P     !< `b` equation coefficient.
        class(spatial_operator_d2), pointer :: ddu_opr      !< Second derivative.
        class(field), allocatable           :: ddux          !< Dummy storage of the second derivative operator result.
        class(field), allocatable           :: dduy          !< Dummy storage of the second derivative operator result.

        class(f2m_d2), pointer :: ddui_opr                   !< First derivative implicit.
        class(matrix), allocatable :: dduix
        class(matrix), allocatable :: dduiy
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
    endtype fd_2d_poisson_equation_adv
contains
    ! deferred public methods
    subroutine init(this, n_equ, field_mesh, inp, description, filename, error)
        !< Initialize equation.
        class(fd_2d_poisson_equation_adv), intent(inout)        :: this        !< The equation.
        integer(I_P),   intent(in)            :: n_equ       !< Number of equations
        class(mesh),    intent(in), target    :: field_mesh  !< Mesh of the field.
        class(field),   intent(in), target, dimension(:) :: inp     !< Input field.
        character(*),   intent(in),  optional :: description !< Equation description.
        character(*),   intent(in),  optional :: filename    !< Initialization file name.
        integer(I_P),   intent(out), optional :: error       !< Error status.
        integer(I_P)                          :: ie          !< Equation index
        integer(I_P)                          :: nx,  ny, n
        class(mesh_FD_2D), pointer :: mesh_cur

        this%n_equ = n_equ

        mesh_cur => associate_mesh_FD_2D(mesh_input=field_mesh)
        nx = mesh_cur%nx
        ny = mesh_cur%ny
        n = nx * ny
        this%n_size = n

        this%m => field_mesh

        ! explicit section
        this%enable_explicit = .true.
        allocate(field_FD_2D :: this%resvar_e(n_equ))
        do ie=1,n_equ
            call this%resvar_e(ie)%init(field_mesh=field_mesh)
        enddo

        allocate(this%ddux, mold=inp(1))
        allocate(this%dduy, mold=inp(1))
        call this%ddux%init(field_mesh=field_mesh)
        call this%dduy%init(field_mesh=field_mesh)

        if (present(filename)) then
            call this%load(filename=filename, error=error)
        else
            this%a = -1.0_R_P
            this%b = -0.1_R_P
            allocate(spatial_operator_d2_FD_2D :: this%ddu_opr)
            if (present(error)) error = 0
        endif

        ! implicit section     
        this%enable_implicit = .false.
        allocate(linsolver_gmlapack :: this%solver)
        call this%solver%init(n) !TODO should be generalized on 50

        allocate(f2m_d2_FD_2D  :: this%ddui_opr)
        allocate(this%ddui_opr%mat, mold=this%solver%mat) !TODO should be put in init()

        allocate(this%dduix, mold=this%solver%mat)
        allocate(this%dduiy, mold=this%solver%mat)

        allocate(this%resvar_i, mold=this%solver%mat)
        call this%resvar_i%init(n) 

        allocate(f2v_FD_2D :: this%f2v_opr)
        allocate(this%f2v_opr%vec, mold=this%solver%vec) ! TODO should be put in init()

        allocate(v2f_FD_2D :: this%v2f_opr)

        this%v2f_opr%mesh => inp(1)%m
        this%v2f_opr%n_equ = n_equ

    end subroutine init

    subroutine bc_e(this, inp, t)
        !< Equation boundary or fixed conditions imposition.
        class(fd_2d_poisson_equation_adv), intent(in)            :: this    !< The equation.
        class(field),                  intent(inout), target, dimension(:) :: inp      !< Field.
        real(R_P),                     intent(in)            :: t        !< Time.
        class(field_FD_2D), pointer                          :: inp_cur  !< Pointer to input field.
        class(mesh_FD_2D), pointer                           :: mesh_cur !< Pointer to input mehs.
        integer(I_P)                                         :: i, j     !< Counter.
        integer(I_P)                                         :: ie       !< Counter.

        do ie=1,size(inp)
            inp_cur => associate_field_FD_2D(field_input=inp(ie), emsg='calling procedure scalar_simple_eqaution%bc')
            mesh_cur => associate_mesh_FD_2D(mesh_input=inp(ie)%m, emsg='calling procedure scalar_simple_eqaution%bc')
            do j=1-mesh_cur%ngy,mesh_cur%ny+mesh_cur%ngy
            do i=1-mesh_cur%ngx,0
                inp_cur%val(i,j) = 0._R_P
            enddo
            enddo
            do j=1-mesh_cur%ngy,mesh_cur%ny+mesh_cur%ngy
            do i=mesh_cur%nx+1, mesh_cur%nx+mesh_cur%ngx
                inp_cur%val(i,j) = 0._R_P 
            enddo
            enddo
            do j=1-mesh_cur%ngy,0
            do i=1-mesh_cur%ngx,mesh_cur%nx+mesh_cur%ngx
                inp_cur%val(i,j) = 0._R_P 
            enddo
            enddo
            do j=mesh_cur%ny+1, mesh_cur%ny+mesh_cur%ngy
            do i=1-mesh_cur%ngx,mesh_cur%nx+mesh_cur%ngx
                inp_cur%val(i,j) = 0._R_P 
            enddo
            enddo
        enddo
    end subroutine bc_e

    subroutine bc_i(this, matA, vecB, t)
        !< Equation boundary conditions imposition.
        class(fd_2d_poisson_equation_adv), intent(in)            :: this     !< The equation.
        class(matrix),    intent(inout), target :: matA  !< Input field.
        class(vector),    intent(inout), target :: vecB  !< Input field.
        real(R_P),                     intent(in)            :: t        !< Time.
        class(field_FD_2D), pointer                          :: inp_cur  !< Pointer to input field.
        class(mesh_FD_2D), pointer                           :: mesh_cur !< Pointer to input mehs.
        integer(I_P)                                         :: i        !< Counter.
        integer(I_P)                                         :: ie        !< Counter.
        !class(vector_simple), pointer  :: vecB_cur
        !class(matrix_simple), pointer  :: matA_cur
        integer(I_P) :: nx, ny, j, i_vec, n, k

        mesh_cur => associate_mesh_FD_2D(mesh_input=this%m)

        !n = matA%n
        !matA_cur => associate_matrix_simple(matrix_input=matA)
        !vecB_cur => associate_vector_simple(vector_input=vecB)
        !nx = 50; ny= 40; n=nx*ny
        nx = mesh_cur%nx 
        ny = mesh_cur%ny 
        n  = nx*ny
        do j=1,ny,ny-1
        do i=1,nx
            i_vec = (j-1)*nx+i
            call vecB%set(i_vec,0._R_P)
            do k=1,n
                call matA%set(i_vec, k, 0._R_P)
            enddo
            call matA%set(i_vec, i_vec, 1._R_P)
        enddo
        enddo
        do j=1,ny
        do i=1,nx,nx-1
            i_vec = (j-1)*nx+i
            call vecB%set(i_vec,0._R_P)
            do k=1,n
                call matA%set(i_vec, k, 0._R_P)
            enddo
            call matA%set(i_vec, i_vec, 1._R_P)
        enddo
        enddo
        !call matA%output("matAbis.dat")
        !call vecB%output("vecBbis.dat")
    end subroutine bc_i

    subroutine resid_e(this, inp, t)
        !< Return the field of residuals.
        class(fd_2d_poisson_equation_adv), intent(inout) :: this  !< The equation.
        class(field), intent(in), target, dimension(:)   :: inp   !< Input field.
        real(R_P), intent(in)                            :: t     !< Time.

        associate(dd => this%ddu_opr, T => inp(1), R => this%resvar_e(1), ddux => this%ddux, dduy => this%dduy, &
                  x => 1, y => 2, a => this%a, b => this%b)
            ddux = dd%operate(T, x) 
            dduy = dd%operate(T, y)
            R = a * ddux + b * dduy
            !IT SHOULD BE R = a * dd%operate(T, x) + b *dd%operate(T, y)
        end associate

    end subroutine resid_e

    subroutine resid_i(this, inp, t)
        !< Return the matrix of residuals.
        class(fd_2d_poisson_equation_adv), intent(inout) :: this  !< The equation.
        class(field), intent(in), target, dimension(:)   :: inp   !< Input field.
        real(R_P), intent(in)                            :: t     !< Time.

        associate(ddi => this%ddui_opr, R => this%resvar_i, dduix => this%dduix, dduiy => this%dduiy, &
                  Teq => 1, T => 1, x => 1, y => 2, a => this%a , b => this%b)
            dduix = ddi%operate(inp, Teq, T, x)
            dduiy = ddi%operate(inp, Teq, T, y)
            R = a * dduix + b * dduiy
            !IT SHOULD BE R = a * ddi%operate(inp, Teq, T, x) + b * ddi%operate(inp, Teq, T, y)
        end associate

    end subroutine resid_i

    ! private methods
    subroutine load_from_json(this, filename, error)
        !< Load mesh definition from JSON file.
        class(fd_2d_poisson_equation_adv), intent(inout)        :: this      !< The equation.
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
        if (mesh_type=="finite difference 2D") then
            allocate(spatial_operator_d2_FD_2D :: this%ddu_opr)
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
end module fd_2d_poisson_m

program fd_2d_poisson
    !< Openpde test: scalar simple equation for Finite Difference 1D methods.

    use openpde
    use fd_2d_poisson_m

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

    allocate(mesh_FD_2D :: mesh_)
    allocate(field_FD_2D :: u(n_equ))
    allocate(fd_2d_poisson_equation_adv :: equation_)
    !EXPLICIT allocate(integrator_adv_euler_explicit :: integrator_)
    allocate(integrator_adv_euler_implicit :: integrator_)
    inquire(file='FD_2D_poisson.json', exist=json_found)
    if (json_found) then
        call mesh_%init(filename='FD_2D_poisson.json')
        do ie = 1,n_equ
            call u(ie)%init(field_mesh=mesh_)
        enddo
        call equation_%init(filename='FD_2D_poisson.json',n_equ=1, field_mesh=mesh_,inp=u)
        call integrator_%init(filename='FD_2D_poisson.json', equ=equation_)
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
end program fd_2d_poisson
