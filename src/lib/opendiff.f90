!< opendiff: Open Fortran Library for PDE solving (OpenFoam done right).
module opendiff
    !< opendiff: Open Fortran Library for PDE solving (OpenFoam done right).
    use opendiff_kinds

    implicit none
    private
    public :: mesh
    public :: mesh_fd_1d
    public :: field
    public :: field_fd_1d

    type, abstract :: mesh
        !< Abstract class for *mesh* handling.
        character(128) :: description !< Mesh description.
        contains
            procedure(abstract_meshinit),    deferred :: init   !< Initilize mesh.
            procedure(abstract_meshoutput) , deferred :: output !< Output mesh data.
    endtype mesh
    type, extends(mesh) :: mesh_fd_1d
        !< Finite difference 1D class for *mesh* handling.
        integer(I4P) :: n  !< number of points.
        integer(I4P) :: ng !< number of ghost points.
        integer(I4P) :: s  !< number of replicas for steps/stages.
        real(R8P)    :: h  !< cell size.
        contains
            procedure :: init => mesh_fd_1d_init     !< Initilize mesh.
            procedure :: output => mesh_fd_1d_output !< Output mesh data.
    endtype mesh_fd_1d

    type, abstract :: field
        !< Abstract class for *field* handling.
        character(128)       :: description !< Field description.
        class(mesh), pointer :: m           !< Pointer to the mesh of the field.
        contains
            procedure(abstract_fieldinit)                , deferred :: init   !< Initilize field.
            procedure(abstract_fieldoutput)              , deferred :: output !< Output field data.
            procedure(abstract_fieldadd)                 , deferred :: add    !< Add fields.
            procedure(abstract_fieldassign)              , deferred :: assign !< Assign fields.
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
        !< Finite difference 1D class for *field* handling.
        real(R8P), allocatable, dimension(:) :: val !< Field value.
        contains
            procedure :: init    => field_fd_1d_init   !< Initilize field.
            procedure :: output  => field_fd_1d_output !< Output field data.
            procedure :: add     => field_fd_1d_add    !< Add fields.
            procedure :: assign  => field_fd_1d_assign !< Assign fields.
            !RIMETTERE procedure :: sub     => field_fd_1d_sub
            !RIMETTERE procedure :: mul     => field_fd_1d_mul
            !RIMETTERE procedure :: mulreal => field_fd_1d_mulreal
            !RIMETTERE procedure :: realmul => field_fd_1d_realmul
    endtype field_fd_1d
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
    abstract interface
        function abstract_meshinit(this) result(res)
            import :: I4P, mesh
            class(mesh), intent(inout) :: this
            integer(I4P)               :: res
        end function abstract_meshinit
    endinterface

    abstract interface
        function abstract_meshoutput(this) result(res)
            import :: I4P, mesh
            class(mesh), intent(in) :: this
            integer(I4P)            :: res
        end function abstract_meshoutput
    endinterface

    abstract interface
        function abstract_fieldinit(this, fieldmesh) result(res)
            import :: field, I4P, mesh
            class(field), intent(inout)      :: this
            class(mesh),  intent(in), target :: fieldmesh
            integer(I4P)                     :: res
        end function abstract_fieldinit
    endinterface

    abstract interface
        function abstract_fieldoutput(this, filename) result(res)
            import :: field, I4P
            class(field),     intent(in) :: this
            character(len=*), intent(in) :: filename
            integer(I4P)                 :: res
        end function abstract_fieldoutput
    endinterface

    abstract interface
        function abstract_fieldadd(lhs, rhs) result(opr)
            import :: field
            class(field), intent(in)         :: lhs
            class(field), intent(in), target :: rhs
            class(field), allocatable        :: opr
        end function abstract_fieldadd
    endinterface

    abstract interface
        subroutine abstract_fieldassign(lhs,rhs)
            import :: field
            class(field), intent(inout)      :: lhs
            class(field), intent(in), target :: rhs
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
        !< Initialize finite difference 1D mesh.
        class(mesh_fd_1d), intent(inout) :: this !< The mesh.
        integer(I4P)                     :: res  !< Result (error code?).
        this%n  = 50
        this%ng = 2
        this%s  = 1
        this%h  = 0.1_R8P
        res = 0
    end function mesh_fd_1d_init

    function mesh_fd_1d_output(this) result(res)
        !< Output mesh data.
        class(mesh_fd_1d), intent(in) :: this !< The mesh.
        integer(I4P)                  :: res  !< Result (error code?).
        print*,"n: ", this%n
        print*,"ng: ", this%ng
        print*,"s: ", this%s
        print*,"h: ", this%h
        res = 0
    end function mesh_fd_1d_output

    function field_fd_1d_init(this, fieldmesh) result(res)
        !< Initialize finite difference 1D field.
        class(field_fd_1d), intent(inout)      :: this          !< The field.
        class(mesh),        intent(in), target :: fieldmesh     !< Mesh of the field.
        integer(I4P)                           :: res           !< Result (error code?).
        class(mesh_fd_1d), pointer             :: fieldmesh_cur !< Dummy pointer for mesh.
        integer(I4P)                           :: n             !< Counter.
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
        !< Output field data.
        class(field_fd_1d), intent(in) :: this     !< The field.
        character(len=*),   intent(in) :: filename !< Output file name.
        integer(I4P)                   :: res      !< Result (error code?).
        open(unit=11,file=filename)
        write(11,*) this%val(:)
        close(11)
        res = 0
    end function field_fd_1d_output

    function field_fd_1d_add(lhs, rhs) result(opr)
        !< Add fields.
        class(field_fd_1d), intent(in)         :: lhs      !< Left hand side.
        class(field),       intent(in), target :: rhs      !< Left hand side.
        class(field), allocatable, target      :: opr      !< Operator result.
        class(field_fd_1d), pointer            :: rhs_cur  !< Dummy pointer for rhs.
        class(field_fd_1d), pointer            :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_fd_1d),  pointer            :: mesh_cur !< Dummy pointer for mesh.
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
        !< Assign fields.
        class(field_fd_1d), intent(inout)      :: lhs      !< Left hand side.
        class(field),       intent(in), target :: rhs      !< Right hand side.
        class(field_fd_1d), pointer            :: rhs_cur  !< Dummy pointer for rhs.
        class(mesh_fd_1d),  pointer            :: mesh_cur !< Dummy pointer for mesh.
        select type(rhs)
            type is(field_fd_1d)
                rhs_cur => rhs
            class default
               STOP 'Error passing field to assign'
        end select
        associate(mm => rhs%m)
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
