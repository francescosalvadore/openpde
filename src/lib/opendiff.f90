!< opendiff: Open Fortran Library for PDE solving.
module opendiff
    !< opendiff: Open Fortran Library for PDE solving.
    use opendiff_kinds

    implicit none
    private
    public :: mesh
    public :: mesh_fd_1d
    public :: field
    public :: field_fd_1d
    public :: spatialop
    public :: spatialop_fd_1d_der_c
    public :: integrator
    public :: euler_integrator
    public :: equation

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
        real(R4P)    :: h  !< cell size.
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
            procedure(abstract_fieldass)                 , deferred :: ass    !< Assign fields.
            procedure(abstract_fieldsub)                 , deferred :: sub
            procedure(abstract_fieldmul)                 , deferred :: mul
            procedure(abstract_fieldmulreal)             , deferred :: mulreal
            procedure(abstract_fieldrealmul) , pass(rhs) , deferred :: realmul
            !RIMETTERE !procedure :: assig       => assig_mesh_fd_1d_scal
            !RIMETTERE !procedure :: assigreal   => assigreal_mesh_fd_1d_scal
            generic, public :: operator(+)  => add
            generic, public :: operator(-)  => sub
            generic, public :: operator(*)  => mul, realmul, mulreal
            generic, public :: assignment(=) => ass
    endtype field
    type, extends(field) :: field_fd_1d
        !< Finite difference 1D class for *field* handling.
        real(R4P), allocatable, dimension(:) :: val !< Field value.
        contains
            procedure :: init    => field_fd_1d_init   !< Initilize field.
            procedure :: output  => field_fd_1d_output !< Output field data.
            procedure :: add     => field_fd_1d_add    !< Add fields.
            procedure :: ass     => field_fd_1d_assign !< Assign fields.
            procedure :: sub     => field_fd_1d_sub
            procedure :: mul     => field_fd_1d_mul
            procedure :: mulreal => field_fd_1d_mulreal
            procedure, pass(rhs) :: realmul => field_fd_1d_realmul
    endtype field_fd_1d

    type, abstract :: integrator
        character(128) :: description
        real :: dt
        contains
            procedure(abstract_integrate), deferred :: integrate
    endtype integrator
    type, extends(integrator) :: euler_integrator
        contains
            procedure :: integrate => euler_integrate
    endtype euler_integrator
!RIMETTERE !    type, extends(integrator) :: lsrk_integrator
!RIMETTERE !        integer :: stages = 2
!RIMETTERE !        contains
!RIMETTERE !            procedure :: integrate => lsrk_integrate
!RIMETTERE !    endtype lsrk_integrator
!------------------------------------------------------
    type, abstract :: spatialop
        character(128) :: description
        contains
            procedure(abstract_operate), deferred :: operate
    endtype spatialop
    type, extends(spatialop) :: spatialop_fd_1d_der_c
        contains
            procedure :: operate => spatialop_fd_1d_der_c_operate
    endtype spatialop_fd_1d_der_c
!------------------------------------------------------
    type, abstract :: equation
        character(128) :: description
        contains
            procedure(abstract_forcing), deferred :: forcing
    endtype equation
    ! the concrete types are implemented at application level (by the user)
    ! predefined examples might be provided as well
!------------------------------------------------------

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
        function abstract_fieldsub(lhs, rhs) result(opr)
            import :: field
            class(field), intent(in)         :: lhs
            class(field), intent(in), target :: rhs
            class(field), allocatable        :: opr
        end function abstract_fieldsub
    endinterface

    abstract interface
        function abstract_fieldmul(lhs, rhs) result(opr)
            import :: field
            class(field), intent(in)         :: lhs
            class(field), intent(in), target :: rhs
            class(field), allocatable        :: opr
        end function abstract_fieldmul
    endinterface

    abstract interface
        function abstract_fieldrealmul(k, rhs) result(opr)
            import :: field
            real, intent(in) :: k
            class(field), intent(in) :: rhs
            class(field), allocatable        :: opr
        end function abstract_fieldrealmul
    endinterface

    abstract interface
        function abstract_fieldmulreal(lhs, k) result(opr)
            import :: field
            class(field), intent(in)         :: lhs
            real, intent(in) :: k
            class(field), allocatable        :: opr
        end function abstract_fieldmulreal
    endinterface

    abstract interface
        subroutine abstract_fieldass(lhs,rhs)
            import :: field
            class(field), intent(inout)      :: lhs
            class(field), intent(in), target :: rhs
        end subroutine abstract_fieldass
    endinterface

    abstract interface
        function abstract_integrate(this, inp, equ, t) result(res)
            import :: integrator, field, equation
            class(integrator) :: this
            class(field), target      :: inp
            class(equation), target   :: equ
            integer :: res
            real :: t
        end function abstract_integrate
    endinterface

    abstract interface
        function abstract_forcing(this, inp, t) result(opr)
            import :: equation, field
            class(equation)   :: this
            class(field), target      :: inp
            class(field), allocatable :: opr
            real :: t
        end function abstract_forcing
    endinterface

    abstract interface
        function abstract_operate(this, inp) result(opr)
            import :: spatialop, field
            class(spatialop)  :: this
            class(field), target       :: inp
            class(field), allocatable       :: opr
        end function abstract_operate
    endinterface
!------------------------------------------------------
contains
    function mesh_fd_1d_init(this) result(res)
        !< Initialize finite difference 1D mesh.
        class(mesh_fd_1d), intent(inout) :: this !< The mesh.
        integer(I4P)                     :: res  !< Result (error code?).
        this%n  = 50
        this%ng = 2
        this%s  = 1
        this%h  = 0.1_R4P
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

    function field_fd_1d_sub(lhs, rhs) result(opr)
        !< Sub fields.
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
        opr_cur%val = lhs%val - rhs_cur%val
    end function field_fd_1d_sub

    function field_fd_1d_mul(lhs, rhs) result(opr)
        !< Mul fields.
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
        opr_cur%val = lhs%val * rhs_cur%val
    end function field_fd_1d_mul

    function field_fd_1d_realmul(k, rhs) result(opr)
        !< Realmul fields.
        real, intent(in) :: k
        class(field_fd_1d),       intent(in) :: rhs      !< Left hand side.
        class(field), allocatable, target      :: opr      !< Operator result.
        class(field_fd_1d), pointer            :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_fd_1d),  pointer            :: mesh_cur !< Dummy pointer for mesh.
        allocate(field_fd_1d :: opr)
        select type(opr)
            type is(field_fd_1d)
                opr_cur => opr
            class default
               STOP 'Error passing field to add'
        end select
        associate(mm => rhs%m)
            select type(mm)
                type is(mesh_fd_1d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate
        allocate(opr_cur%val(1:mesh_cur%n))
        opr_cur%m => rhs%m
        opr_cur%val = k * rhs%val
    end function field_fd_1d_realmul

    function field_fd_1d_mulreal(lhs, k) result(opr)
        !< Mulreal fields.
        class(field_fd_1d), intent(in) :: lhs      !< Left hand side.
        real, intent(in) :: k
        class(field), allocatable, target      :: opr      !< Operator result.
        class(field_fd_1d), pointer            :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_fd_1d),  pointer            :: mesh_cur !< Dummy pointer for mesh.
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
        opr_cur%val = lhs%val * k
    end function field_fd_1d_mulreal

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

    function euler_integrate(this, inp, equ, t) result(res)
        class(euler_integrator) :: this
        class(field), target :: inp
        class(field), allocatable :: for
        class(equation), target :: equ
        integer :: res
        real :: t
        select type(inp)
            type is(field_fd_1d)
                print *,"t, dt, inp: ",t, this%dt, inp%val
        end select
        allocate(for, source=inp)
        ! the temporary variable for seems to be needed by intel compiler
        ! otherwise there is an internal compiler error or seg fault
        ! especially multiplying by this%dt. Why....?
        for = equ%forcing(inp=inp, t=t) 
        inp = inp + this%dt * for
        res = 0
    end function euler_integrate

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

    function spatialop_fd_1d_der_c_operate(this, inp) result(opr)
        class(spatialop_fd_1d_der_c) :: this
        class(field), target :: inp
        class(field_fd_1d), pointer :: inp_cur
        class(field), allocatable, target :: opr
        class(field_fd_1d), pointer            :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_fd_1d),  pointer            :: mesh_cur !< Dummy pointer for mesh.
        real :: h
        integer :: i, n
        allocate(field_fd_1d :: opr)
        select type(opr)
            type is(field_fd_1d)
                opr_cur => opr
            class default
               STOP 'Error passing field to add'
        end select
        select type(inp)
            type is(field_fd_1d)
                inp_cur => inp
            class default
               STOP 'Error passing field to spatial operate'
        end select
        associate(mm => inp%m)
            select type(mm)
                type is(mesh_fd_1d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate
        h = mesh_cur%h
        n = mesh_cur%n
        allocate(opr_cur%val(1:n))
        opr_cur%m => mesh_cur
        do i=2,n-1
            opr_cur%val(i) = (inp_cur%val(i+1) - inp_cur%val(i-1))/(2.*h)
        enddo
        opr_cur%val(1) = (inp_cur%val(2) - inp_cur%val(n))/(2.*h)
        opr_cur%val(n) = (inp_cur%val(1) - inp_cur%val(n-1))/(2.*h)
    end function spatialop_fd_1d_der_c_operate
end module opendiff
