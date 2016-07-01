module opendiff
!------------------------------------------------------
    type, abstract :: mesh
        character(128) :: description
        contains
            procedure(abstract_meshinit) , deferred :: init 
            procedure(abstract_meshoutput) , deferred :: output 
    endtype mesh
    type, extends(mesh) :: mesh_fd_1d
        integer :: n  ! number of points
        integer :: ng ! number of ghost points
        integer :: s  ! number of replicas for steps/stages
        real    :: h  ! cell size
        contains
            procedure :: init => mesh_fd_1d_init
            procedure :: output => mesh_fd_1d_output
    endtype mesh_fd_1d
!------------------------------------------------------
    type, abstract :: field
        character(128) :: description
        class(mesh), pointer :: m  ! pointer to the mesh of the field
        contains
            procedure(abstract_fieldinit)                , deferred :: init 
            procedure(abstract_fieldoutput)              , deferred :: output
            procedure(abstract_fieldadd)                 , deferred :: add
            procedure(abstract_fieldassign)              , deferred :: assign
            !procedure(abstract_fieldsub)                 , deferred :: sub
            !RIMETTERE procedure(abstract_fieldmul)                 , deferred :: mul
            !RIMETTERE procedure(abstract_fieldmulreal)             , deferred :: mulreal
            !RIMETTERE procedure(abstract_fieldrealmul) , pass(rhs) , deferred :: realmul
            !RIMETTERE !procedure :: assig       => assig_mesh_fd_1d_scal
            !RIMETTERE !procedure :: assigreal   => assigreal_mesh_fd_1d_scal
            generic, public :: operator(+)  => add                             
            !RIMETTERE generic, public :: operator(*)  => mul, realmul, mulreal
            generic, public :: assignment(=) => assign
    endtype field
    type, extends(field) :: field_fd_1d
        real, allocatable, dimension(:) :: val
        contains
            procedure :: init    => field_fd_1d_init
            procedure :: output  => field_fd_1d_output
            procedure :: add     => field_fd_1d_add
            procedure :: assign  => field_fd_1d_assign
            !RIMETTERE procedure :: sub     => field_fd_1d_sub
            !RIMETTERE procedure :: mul     => field_fd_1d_mul
            !RIMETTERE procedure :: mulreal => field_fd_1d_mulreal
            !RIMETTERE procedure :: realmul => field_fd_1d_realmul
    endtype field_fd_1d
!------------------------------------------------------
!RIMETTERE     type, abstract :: integrator
!RIMETTERE         character(128) :: description
!RIMETTERE         real :: dt
!RIMETTERE         contains
!RIMETTERE             procedure(abstract_integrate), deferred :: integrate
!RIMETTERE     endtype integrator
!RIMETTERE     type, extends(integrator) :: euler_integrator
!RIMETTERE         contains
!RIMETTERE             procedure :: integrate => euler_integrate
!RIMETTERE     endtype euler_integrator
!RIMETTERE !    type, extends(integrator) :: lsrk_integrator
!RIMETTERE !        integer :: stages = 2
!RIMETTERE !        contains
!RIMETTERE !            procedure :: integrate => lsrk_integrate
!RIMETTERE !    endtype lsrk_integrator
!------------------------------------------------------
!RIMETTERE    type, abstract :: spatialop
!RIMETTERE        character(128) :: description
!RIMETTERE        contains
!RIMETTERE            procedure(abstract_operate), deferred :: operate
!RIMETTERE    endtype spatialop
!RIMETTERE    type, extends(spatialop) :: firstderive_operator
!RIMETTERE        contains
!RIMETTERE            procedure :: operate => firstderive_operate
!RIMETTERE    endtype firstderive_operator
!RIMETTERE!------------------------------------------------------
!RIMETTERE    type, abstract :: equation
!RIMETTERE        character(128) :: description
!RIMETTERE        contains
!RIMETTERE            procedure(abstract_forcing), deferred :: forcing
!RIMETTERE    endtype equation
!RIMETTERE    ! the concrete types are implemented at application level (by the user)
!RIMETTERE    ! predefined examples might be provided as well
!------------------------------------------------------
    abstract interface
        function abstract_meshinit(this) result(res)
            import :: mesh
            class(mesh) :: this
            integer :: res
        end function abstract_meshinit
    endinterface

    abstract interface
        function abstract_meshoutput(this) result(res)
            import :: mesh
            class(mesh) :: this
            integer :: res
        end function abstract_meshoutput
    endinterface

    abstract interface
        function abstract_fieldinit(this, fieldmesh) result(res)
            import :: field, mesh
            class(field) :: this
            class(mesh), target :: fieldmesh
            integer :: res
        end function abstract_fieldinit
    endinterface

    abstract interface
        function abstract_fieldoutput(this, filename) result(res)
            import :: field
            class(field) :: this
            character(len=*) :: filename
            integer :: res
        end function abstract_fieldoutput
    endinterface

    abstract interface
        function abstract_fieldadd(lhs, rhs) result(opr)
            import :: field
            class(field), intent(in) :: lhs
            class(field), target, intent(in) :: rhs
            class(field), allocatable :: opr
        end function abstract_fieldadd
    endinterface

    abstract interface
        subroutine abstract_fieldassign(lhs,rhs) 
            import :: field
            class(field), intent(inout) :: lhs
            class(field), target, intent(in) :: rhs
        end subroutine abstract_fieldassign
    endinterface

!RIMETTERE    abstract interface
!RIMETTERE        function abstract_integrate(this, inp, equ, t) result(res)
!RIMETTERE            import :: integrator, field, equation
!RIMETTERE            class(integrator) :: this
!RIMETTERE            class(field)      :: inp 
!RIMETTERE            class(equation)   :: equ
!RIMETTERE            integer :: res
!RIMETTERE            real :: t
!RIMETTERE        end function abstract_integrate
!RIMETTERE    endinterface
!RIMETTERE
!RIMETTERE    abstract interface
!RIMETTERE        function abstract_forcing(this, inp, t) result(opr)
!RIMETTERE            import :: equation, field
!RIMETTERE            class(equation)   :: this
!RIMETTERE            class(field)      :: inp
!RIMETTERE            class(field), allocatable :: opr
!RIMETTERE            real :: t
!RIMETTERE        end function abstract_forcing
!RIMETTERE    endinterface
!RIMETTERE
!RIMETTERE    abstract interface
!RIMETTERE        function abstract_operate(this, inp) result(opr)
!RIMETTERE            import :: spatialop, field
!RIMETTERE            class(spatialop)  :: this
!RIMETTERE            class(field)       :: inp
!RIMETTERE            class(field), allocatable       :: opr
!RIMETTERE        end function abstract_operate
!RIMETTERE    endinterface
!------------------------------------------------------
contains

    function mesh_fd_1d_init(this) result(res)
        class(mesh_fd_1d) :: this
        integer :: res
        this%n  = 50
        this%ng = 2
        this%s  = 1
        this%h  = 0.1
        res = 0
    end function mesh_fd_1d_init

    function mesh_fd_1d_output(this) result(res)
        class(mesh_fd_1d) :: this
        integer :: res
        print*,"n: ",this%n
        print*,"ng: ",this%ng
        print*,"s: ",this%s 
        print*,"h: ",this%h
        res = 0
    end function mesh_fd_1d_output

    function field_fd_1d_init(this, fieldmesh) result(res)
        class(field_fd_1d) :: this
        class(mesh), target :: fieldmesh
        class(mesh_fd_1d), pointer :: fieldmesh_cur
        integer :: res
        integer :: n
        select type(fieldmesh)
            type is(mesh_fd_1d)
                fieldmesh_cur => fieldmesh
            class default
               STOP 'Error passing mesh'
        end select
        this%m => fieldmesh_cur
        n = fieldmesh_cur%n
        allocate(this%val(1:n))
        call random_number(this%val)
        res = 0
    end function field_fd_1d_init

    function field_fd_1d_output(this, filename) result(res)
        class(field_fd_1d) :: this
        character(len=*) :: filename
        integer :: res
        open(unit=11,file=filename)
        write(11,*) this%val(:)
        close(11)
        res = 0
    end function field_fd_1d_output

    function field_fd_1d_add(lhs, rhs) result(opr)
        class(mesh_fd_1d), pointer :: mesh_cur
        class(field_fd_1d), intent(in) :: lhs
        class(field), target, intent(in) :: rhs
        class(field_fd_1d), pointer :: rhs_cur
        class(field), allocatable, target :: opr
        class(field_fd_1d), pointer :: opr_cur

        select type(rhs)
            type is(field_fd_1d)
                rhs_cur => rhs
            class default
               STOP 'Error passing field to add'
        end select

        allocate(field_fd_1d :: opr)
        select type(opr)
            type is(field_fd_1d)
                opr_cur => opr
            class default
               STOP 'Error passing field to add'
        end select

        associate(mm => lhs%m)
            select type(mm)
                type is(mesh_fd_1d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate

        allocate(opr_cur%val(1:mesh_cur%n))
        opr_cur%m => lhs%m
        opr_cur%val = lhs%val + rhs_cur%val

    end function field_fd_1d_add

    subroutine field_fd_1d_assign(lhs, rhs) 
        class(field_fd_1d), intent(inout) :: lhs
        class(field), target, intent(in) :: rhs
        class(field_fd_1d), pointer :: rhs_cur
        class(mesh_fd_1d), pointer :: mesh_cur

        select type(rhs)
            type is(field_fd_1d)
                rhs_cur => rhs
            class default
               STOP 'Error passing field to assign'
        end select

        associate(mm => lhs%m)
            select type(mm)
                type is(mesh_fd_1d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate

        if(allocated(lhs%val)) deallocate(lhs%val)
        allocate(lhs%val(1:mesh_cur%n))
        lhs%m   => rhs_cur%m
        lhs%val = rhs_cur%val

    end subroutine field_fd_1d_assign

!RIMETTERE    function field_fd_1d_mul(lhs, rhs) result(opr)
!RIMETTERE        class(field_fd_1d) :: lhs, rhs
!RIMETTERE        type(field_fd_1d) :: opr
!RIMETTERE        opr%m => lhs%m ! sync mesh
!RIMETTERE        opr%val = lhs%val * rhs%val
!RIMETTERE    end function field_fd_1d_mul
!RIMETTERE
!RIMETTERE    function field_fd_1d_mulreal(lhs, rhs) result(opr)
!RIMETTERE        class(field_fd_1d) :: lhs
!RIMETTERE        real :: rhs
!RIMETTERE        type(field_fd_1d) :: opr
!RIMETTERE        opr%m => lhs%m ! sync mesh
!RIMETTERE        opr%val = lhs%val * rhs
!RIMETTERE    end function field_fd_1d_mulreal
!RIMETTERE
!RIMETTERE    function field_fd_1d_realmul(lhs, rhs) result(opr)
!RIMETTERE        real :: lhs
!RIMETTERE        class(field_fd_1d) :: rhs
!RIMETTERE        type(field_fd_1d) :: opr
!RIMETTERE        opr%m => lhs%m ! sync mesh
!RIMETTERE        opr%val = rhs * lhs%val
!RIMETTERE    end function field_fd_1d_realmul
!RIMETTERE
!RIMETTERE    function euler_integrate(this, inp, equ, t) result(res)
!RIMETTERE        class(euler_integrator) :: this
!RIMETTERE        class(field) :: inp
!RIMETTERE        class(equation) :: equ
!RIMETTERE        integer :: res
!RIMETTERE        real :: t
!RIMETTERE        inp = inp + (equ%forcing(inp=inp, t=t) * this%dt)
!RIMETTERE        res = 0
!RIMETTERE    end function euler_integrate
!RIMETTERE
!RIMETTERE    !function lsrk_integrate(this, inp, equ, t) result(res)
!RIMETTERE    !    class(lsrk_integrator) :: this
!RIMETTERE    !    class(field)  :: inp
!RIMETTERE    !    class(equation) :: equ
!RIMETTERE    !    integer, parameter :: registers=2
!RIMETTERE    !    type(mesh_fd_1d_scal)  :: stage(1:registers)
!RIMETTERE    !    type(error) :: res
!RIMETTERE    !    real :: t        
!RIMETTERE    !    integer :: s 
!RIMETTERE!   !    computing stages
!RIMETTERE    !    stage(1) = inp
!RIMETTERE    !    stage(2) = inp*0.
!RIMETTERE    !    do s=1, this%stages
!RIMETTERE!   !        stage(2) = stage(2) * self%A(s) + stage(1)%t(t=t + self%C(s) * Dt) * Dt
!RIMETTERE    !        stage(2) = stage(2) * 2. + equ%forcing(inp=stage(1),t=t + 3. * this%dt) * this%dt
!RIMETTERE    !        stage(1) = stage(1) + stage(2) * 4. !self%B(s)
!RIMETTERE    !    enddo
!RIMETTERE    !    inp = stage(1)
!RIMETTERE    !    res%val = 0
!RIMETTERE    !end function lsrk_integrate
!RIMETTERE
!RIMETTERE    function firstderive_operate(this, inp) result(res)
!RIMETTERE        class(firstderive_operator) :: this
!RIMETTERE        class(field) :: inp
!RIMETTERE        class(field), allocatable :: res
!RIMETTERE        allocate(field_fd_1d :: res%field(inp%n))
!RIMETTERE        res%n = inp%n
!RIMETTERE        res%h = inp%h
!RIMETTERE        do i=2,inp%n-1
!RIMETTERE            res%field(i) = (inp%field(i+1) - inp%field(i-1))/(2.*inp%h)
!RIMETTERE        enddo
!RIMETTERE        res%field(1)     = (inp%field(2) - inp%field(inp%n))/(2.*inp%h)
!RIMETTERE        res%field(inp%n) = (inp%field(1) - inp%field(inp%n-1))/(2.*inp%h)
!RIMETTERE    end function firstderive_operate

end module opendiff




!    type, extends(mesh) :: mesh_fd_1d_scal
!        integer :: n
!        integer :: s ! should include replicas for steps/stages
!        real :: h
!        real, allocatable :: field(:) ! (:,:)
!        contains
!            procedure :: init               => init_mesh_fd_1d_scal
!            procedure :: output             => output_mesh_fd_1d_scal
!            procedure :: add                => add_mesh_fd_1d_scal
!            !procedure :: sub               => sub_mesh_fd_1d_scal
!            procedure :: mul                => mul_mesh_fd_1d_scal
!            procedure :: mulreal            => mulreal_mesh_fd_1d_scal
!            procedure, pass(rhs) :: realmul => realmul_mesh_fd_1d_scal
!            !procedure :: assig       => assig_mesh_fd_1d_scal
!            !procedure :: assigreal   => assigreal_mesh_fd_1d_scal
!            generic, public :: operator(+)  => add                             
!            generic, public :: operator(*)  => mul, realmul, mulreal
!    endtype mesh_fd_1d_scal
!
!
!    type :: firstorder_derx
!        real :: weights
!        contains
!            procedure :: derive => firstorder_derive
!    endtype firstorder_derx
!
!    type :: r_scalar
!        real :: val
!        contains
!            procedure :: init => init_r_scalar
!    endtype r_scalar
!
!    type :: time
!        integer :: itmin, itmax
!        contains
!            procedure :: init => init_time
!    endtype time
!
!    type :: error
!        integer :: val
!    endtype error
!
!    type :: message
!        real :: val
!        contains
!            procedure :: output => output_message
!    endtype message
!
!    interface
!        function abstract_integrate(this, inp, equ, t) result(res)
!            import :: integrator, mesh_fd_1d_scal, error, equation
!            class(integrator) :: this
!            type(mesh_fd_1d_scal) :: inp !, rhs
!            class(equation) :: equ
!            type(error) :: res
!            real :: t
!        end function abstract_integrate
!    endinterface
!
!    interface
!        function abstract_forcing(this, inp, t) result(opr)
!            import :: equation, mesh_fd_1d_scal
!            class(equation) :: this
!            type(mesh_fd_1d_scal) :: inp
!            type(mesh_fd_1d_scal) :: opr
!            real :: t
!        end function abstract_forcing
!    endinterface
!
!    interface
!        function abstract_operate(this, inp) result(opr)
!            import :: equation, mesh_fd_1d_scal
!            class(equation) :: this
!            type(mesh_fd_1d_scal) :: inp
!            type(mesh_fd_1d_scal) :: opr
!            real :: t
!        end function abstract_forcing
!    endinterface
!
!contains
!
!    function firstorder_derive(this, inp) result(res)
!        class(firstorder_derx) :: this
!        type(mesh_fd_1d_scal) :: inp
!        type(mesh_fd_1d_scal) :: res
!        allocate(res%field(inp%n))
!        res%n = inp%n
!        res%h = inp%h
!        do i=2,inp%n-1
!            res%field(i) = (inp%field(i+1) - inp%field(i-1))/(2.*inp%h)
!        enddo
!        res%field(1)     = (inp%field(2) - inp%field(inp%n))/(2.*inp%h)
!        res%field(inp%n) = (inp%field(1) - inp%field(inp%n-1))/(2.*inp%h)
!    end function firstorder_derive
!
!    function euler_integrate(this, inp, equ, t) result(res)
!        class(euler_integrator) :: this
!        type(mesh_fd_1d_scal) :: inp !, rhs
!        class(equation) :: equ
!        type(error) :: res
!        real :: t
!        inp = inp + (equ%forcing(inp=inp, t=t) * this%dt)
!        res%val = 0
!    end function euler_integrate
!
!    function lsrk_integrate(this, inp, equ, t) result(res)
!        class(lsrk_integrator) :: this
!        type(mesh_fd_1d_scal)  :: inp ! , rhs
!        class(equation) :: equ
!        integer, parameter :: registers=2
!        type(mesh_fd_1d_scal)  :: stage(1:registers)
!        type(error) :: res
!        real :: t        
!        integer :: s 
!!       computing stages
!        stage(1) = inp
!        stage(2) = inp*0.
!        do s=1, this%stages
!!           stage(2) = stage(2) * self%A(s) + stage(1)%t(t=t + self%C(s) * Dt) * Dt
!            stage(2) = stage(2) * 2. + equ%forcing(inp=stage(1),t=t + 3. * this%dt) * this%dt
!            stage(1) = stage(1) + stage(2) * 4. !self%B(s)
!        enddo
!        inp = stage(1)
!        res%val = 0
!     end function lsrk_integrate
!
!    function init_mesh_fd_1d_scal(this) result(res)
!        class(mesh_fd_1d_scal) :: this
!        type(error) :: res
!        integer :: i
!        this%n = 50
!        this%h = 0.1
!        allocate(this%field(this%n))
!        do i=1,this%n
!            this%field(i) = sqrt(real(i))
!        enddo
!        res%val = 0
!    end function init_mesh_fd_1d_scal
!
!    function add_mesh_fd_1d_scal(lhs, rhs) result(opr)
!        class(mesh_fd_1d_scal), intent(in) :: lhs, rhs
!        type(mesh_fd_1d_scal) :: opr
!        opr = lhs
!        opr%field = lhs%field + rhs%field
!    end function add_mesh_fd_1d_scal
!
!    function mul_mesh_fd_1d_scal(lhs, rhs) result(opr)
!        class(mesh_fd_1d_scal), intent(in) :: lhs, rhs
!        type(mesh_fd_1d_scal) :: opr
!        opr = lhs
!        opr%field = lhs%field * rhs%field
!    end function mul_mesh_fd_1d_scal
!
!    function realmul_mesh_fd_1d_scal(lhs, rhs) result(opr)
!        real, intent(in) :: lhs
!        class(mesh_fd_1d_scal), intent(in) :: rhs
!        type(mesh_fd_1d_scal) :: opr
!        opr = rhs
!        opr%field = lhs * rhs%field
!    end function realmul_mesh_fd_1d_scal
!
!    function mulreal_mesh_fd_1d_scal(rhs, lhs) result(opr)
!        real, intent(in) :: lhs
!        class(mesh_fd_1d_scal), intent(in) :: rhs
!        type(mesh_fd_1d_scal) :: opr
!        opr = rhs
!        opr%field = lhs * rhs%field
!    end function mulreal_mesh_fd_1d_scal
!
!    function init_r_scalar(this) result(res)
!        class(r_scalar) :: this
!        type(error) :: res
!        this%val = 0.1
!        res%val = 0
!    end function init_r_scalar
!
!    function init_time(this) result(res)
!        class(time) :: this
!        type(error) :: res
!        this%itmin = 0
!        this%itmax = 10
!        res%val = 0
!    end function init_time
!
!    function output_message(this) result(res)
!        class(message) :: this
!        type(error) :: res
!        print*,"message: ",this%val
!        res%val = 0
!    end function output_message
!
!    function output_mesh_fd_1d_scal(this, filename) result(res)
!        class(mesh_fd_1d_scal) :: this
!        character(len=*) :: filename
!        type(error) :: res
!        open(unit=11,file=filename)
!        write(11,*) this%field(:)
!        close(11)
!        res%val = 0
!    end function output_mesh_fd_1d_scal
!
!end module opendiff
