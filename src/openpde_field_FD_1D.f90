!< Concrete class of field for Finite Difference 1D methods.
module openpde_field_FD_1D
    !< Concrete class of field for Finite Difference 1D methods.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use openpde_field_abstract
    use openpde_kinds
    use openpde_mesh_abstract
    use openpde_mesh_FD_1D

    implicit none
    private
    public :: associate_field_FD_1D, field_FD_1D

    type, extends(field) :: field_FD_1D
        !< Concrete class of field for Finite Difference 1D methods.
        real(R_P), allocatable, dimension(:) :: val !< Field value.
        contains
            ! deferred public methods
            procedure, pass(this) :: associate_mesh !< Associate field to a mesh.
            procedure, pass(this) :: init           !< Initilize field.
            procedure, pass(this) :: output         !< Output field data.
            ! deferred private methods
            procedure, pass(lhs),   private :: add          !< Add fields.
            procedure, pass(lhs),   private :: assign_field !< Assign fields.
            procedure, pass(lhs),   private :: mul          !< Multiply fields.
            procedure, pass(lhs),   private :: mulreal      !< Multiply field for real.
            procedure, pass(rhs),   private :: realmul      !< Multiply real for field.
            procedure, pass(lhs),   private :: sub          !< Subtract fields.
            procedure, pass(lhs),   private :: div          !< Subtract fields.
            ! public methods
            procedure, pass(this) :: set !< Set field.

            !procedure, pass(rhs) :: newrealmul
            !public :: operator(**) => newrealmul
    endtype field_FD_1D

    interface associate_field_FD_1D
       module procedure associate_field_FD_1D_scalar, &
                        associate_field_FD_1D_rank1,  &
                        associate_field_FD_1D_rank2
    end interface
contains
    ! public, non TBP
    function associate_field_FD_1D_scalar(field_input, emsg) result(field_pointer)
        !< Check the type of the field passed as input and return a Finite Difference 1D field pointer associated to field.
        class(field),       intent(in), target   :: field_input   !< Input field.
        character(*),       intent(in), optional :: emsg          !< Auxiliary error message.
        class(field_FD_1D), pointer              :: field_pointer !< Finite Difference 1D field pointer.

        select type(field_input)
            type is(field_FD_1D)
                field_pointer => field_input
            class default
               write(stderr, '(A)')'error: cast field to field_FD_1D'
               if (present(emsg)) write(stderr, '(A)') emsg
               stop
        end select
    end function associate_field_FD_1D_scalar

    function associate_field_FD_1D_rank1(field_input, emsg) result(field_pointer)
        !< Check the type of the field passed as input and return a Finite Difference 1D field pointer associated to field.
        class(field),       intent(in), target   :: field_input(:)   !< Input field.
        character(*),       intent(in), optional :: emsg             !< Auxiliary error message.
        class(field_FD_1D), pointer              :: field_pointer(:) !< Finite Difference 1D field pointer.

        select type(field_input)
            type is(field_FD_1D)
                field_pointer => field_input
            class default
               write(stderr, '(A)')'error: cast field to field_FD_1D'
               if (present(emsg)) write(stderr, '(A)') emsg
               stop
        end select
    end function associate_field_FD_1D_rank1

    function associate_field_FD_1D_rank2(field_input, emsg) result(field_pointer)
        !< Check the type of the field passed as input and return a Finite Difference 1D field pointer associated to field.
        class(field),       intent(in), target   :: field_input(:,:)   !< Input field.
        character(*),       intent(in), optional :: emsg               !< Auxiliary error message.
        class(field_FD_1D), pointer              :: field_pointer(:,:) !< Finite Difference 1D field pointer.

        select type(field_input)
            type is(field_FD_1D)
                field_pointer => field_input
            class default
               write(stderr, '(A)')'error: cast field to field_FD_1D'
               if (present(emsg)) write(stderr, '(A)') emsg
               stop
        end select
    end function associate_field_FD_1D_rank2

    ! deferred public methods
    subroutine associate_mesh(this, field_mesh, error)
        !< Associate field to a mesh.
        class(field_FD_1D), intent(inout)         :: this       !< The field.
        class(mesh),        intent(in), target    :: field_mesh !< Mesh of the field.
        integer(I_P),       intent(out), optional :: error      !< Error status.
        class(mesh_FD_1D), pointer                :: mesh_cur   !< Dummy pointer for mesh.

        mesh_cur => associate_mesh_FD_1D(mesh_input=field_mesh, emsg='calling procedure field_FD_1D%associate_mesh')
        this%m => mesh_cur
        if (allocated(this%val)) deallocate(this%val) ; allocate(this%val(1-mesh_cur%ng:mesh_cur%n+mesh_cur%ng))
        if (present(error)) error = 0
    end subroutine associate_mesh

    subroutine init(this, field_mesh, description, error)
        !< Initialize finite difference 1D field.
        class(field_FD_1D), intent(inout)         :: this        !< The field.
        class(mesh),        intent(in), target    :: field_mesh  !< Mesh of the field.
        character(*),       intent(in),  optional :: description !< Mesh description
        integer(I_P),       intent(out), optional :: error       !< Error status.
        class(mesh_FD_1D),  pointer               :: mesh_cur    !< Dummy pointer for mesh.
        integer(I_P)                              :: i           !< Counter.

        call this%free
        call this%associate_mesh(field_mesh=field_mesh, error=error)
        if (present(description)) this%description = description
        mesh_cur => associate_mesh_FD_1D(mesh_input=field_mesh, emsg='calling procedure field_FD_1D%init')

        this%val(:) = 0._R_P

!        do i = 1, mesh_cur%n
        do i = 1-mesh_cur%ng, mesh_cur%n+mesh_cur%ng
            this%val(i) = sin(i*2._R_P*acos(-1._R_P)/mesh_cur%n)
        enddo

        if (present(error)) error = 0
    end subroutine init

    subroutine output(this, filename, error)
        !< Output field data.
        class(field_FD_1D), intent(in)            :: this     !< The field.
        character(len=*),   intent(in)            :: filename !< Output file name.
        integer(I_P),       intent(out), optional :: error    !< Error status.
        integer(I_P)                              :: imin     !< Lower extent.
        integer(I_P)                              :: imax     !< Upper extent.
        integer(I_P)                              :: i        !< Counter.

        imin = lbound(this%val, dim=1)
        imax = ubound(this%val, dim=1)
        open(unit=11, file=filename)
        do i=imin, imax
            write(11, *) this%val(i)
        enddo
        close(11)
        if (present(error)) error = 0
    end subroutine output

    ! deferred private methods
    function add(lhs, rhs) result(opr)
        !< Add fields.
        class(field_FD_1D), intent(in)         :: lhs     !< Left hand side.
        class(field),       intent(in), target :: rhs     !< Left hand side.
        class(field), allocatable, target      :: opr     !< Operator result.
        class(field_FD_1D), pointer            :: rhs_cur !< Dummy pointer for rhs.
        class(field_FD_1D), pointer            :: opr_cur !< Dummy pointer for operator result.

        rhs_cur => associate_field_FD_1D(field_input=rhs, emsg='calling procedure field_FD_1D%add')
        allocate(field_FD_1D :: opr)
        opr_cur => associate_field_FD_1D(field_input=opr, emsg='calling procedure field_FD_1D%add')
        call opr_cur%associate_mesh(field_mesh=lhs%m)
        opr_cur%val = lhs%val + rhs_cur%val
    end function add

    subroutine assign_field(lhs, rhs)
        !< Assign fields.
        class(field_FD_1D), intent(inout)      :: lhs     !< Left hand side.
        class(field),       intent(in), target :: rhs     !< Right hand side.
        class(field_FD_1D), pointer            :: rhs_cur !< Dummy pointer for rhs.

        rhs_cur => associate_field_FD_1D(field_input=rhs, emsg='calling procedure field_FD_1D%assign')
        call lhs%associate_mesh(field_mesh=rhs_cur%m)
        lhs%val = rhs_cur%val
    end subroutine assign_field

    function mul(lhs, rhs) result(opr)
        !< Multiply fields.
        class(field_FD_1D), intent(in)         :: lhs     !< Left hand side.
        class(field),       intent(in), target :: rhs     !< Left hand side.
        class(field), allocatable, target      :: opr     !< Operator result.
        class(field_FD_1D), pointer            :: rhs_cur !< Dummy pointer for rhs.
        class(field_FD_1D), pointer            :: opr_cur !< Dummy pointer for operator result.

        rhs_cur => associate_field_FD_1D(field_input=rhs, emsg='calling procedure field_FD_1D%mul')
        allocate(field_FD_1D :: opr)
        opr_cur => associate_field_FD_1D(field_input=opr, emsg='calling procedure field_FD_1D%mul')
        call opr_cur%associate_mesh(field_mesh=lhs%m)
        opr_cur%val = lhs%val * rhs_cur%val
    end function mul

    function mulreal(lhs, rhs) result(opr)
        !< Multiply field for real.
        class(field_FD_1D), intent(in)    :: lhs     !< Left hand side.
        real(R_P),          intent(in)    :: rhs     !< Right hand side.
        class(field), allocatable, target :: opr     !< Operator result.
        class(field_FD_1D), pointer       :: opr_cur !< Dummy pointer for operator result.

        allocate(field_FD_1D :: opr)
        opr_cur => associate_field_FD_1D(field_input=opr, emsg='calling procedure field_FD_1D%mulreal')
        call opr_cur%associate_mesh(field_mesh=lhs%m)
        opr_cur%val = lhs%val * rhs
    end function mulreal

    function realmul(lhs, rhs) result(opr)
        !< Multiply real for field.
        real(R_P),          intent(in)    :: lhs     !< Left hand side.
        class(field_FD_1D), intent(in)    :: rhs     !< Right hand side.
        class(field), allocatable, target :: opr     !< Operator result.
        class(field_FD_1D), pointer       :: opr_cur !< Dummy pointer for operator result.

        allocate(field_FD_1D :: opr)
        opr_cur => associate_field_FD_1D(field_input=opr, emsg='calling procedure field_FD_1D%realmul')
        call opr_cur%associate_mesh(field_mesh=rhs%m)
        opr_cur%val = lhs * rhs%val
    end function realmul

    function newrealmul(lhs, rhs) result(opr)
        !< Multiply real for field.
        real(R_P),          intent(in)    :: lhs     !< Left hand side.
        class(field_FD_1D), intent(in)    :: rhs     !< Right hand side.
        class(field), allocatable, target :: opr     !< Operator result.
        class(field_FD_1D), pointer       :: opr_cur !< Dummy pointer for operator result.

        allocate(field_FD_1D :: opr)
        opr_cur => associate_field_FD_1D(field_input=opr, emsg='calling procedure field_FD_1D%realmul')
        call opr_cur%associate_mesh(field_mesh=rhs%m)
        opr_cur%val = lhs * rhs%val
    end function newrealmul

    function sub(lhs, rhs) result(opr)
        !< Subtract fields.
        class(field_FD_1D), intent(in)         :: lhs     !< Left hand side.
        class(field),       intent(in), target :: rhs     !< Left hand side.
        class(field), allocatable, target      :: opr     !< Operator result.
        class(field_FD_1D), pointer            :: rhs_cur !< Dummy pointer for rhs.
        class(field_FD_1D), pointer            :: opr_cur !< Dummy pointer for operator result.

        rhs_cur => associate_field_FD_1D(field_input=rhs, emsg='calling procedure field_FD_1D%sub')
        allocate(field_FD_1D :: opr)
        opr_cur => associate_field_FD_1D(field_input=opr, emsg='calling procedure field_FD_1D%sub')
        call opr_cur%associate_mesh(field_mesh=lhs%m)
        opr_cur%val = lhs%val - rhs_cur%val
    end function sub

    function div(lhs, rhs) result(opr)
        !< Subtract fields.
        class(field_FD_1D), intent(in)         :: lhs     !< Left hand side.
        class(field),       intent(in), target :: rhs     !< Left hand side.
        class(field), allocatable, target      :: opr     !< Operator result.
        class(field_FD_1D), pointer            :: rhs_cur !< Dummy pointer for rhs.
        class(field_FD_1D), pointer            :: opr_cur !< Dummy pointer for operator result.

        rhs_cur => associate_field_FD_1D(field_input=rhs, emsg='calling procedure field_FD_1D%sub')
        allocate(field_FD_1D :: opr)
        opr_cur => associate_field_FD_1D(field_input=opr, emsg='calling procedure field_FD_1D%sub')
        call opr_cur%associate_mesh(field_mesh=lhs%m)
        opr_cur%val = lhs%val/rhs_cur%val
    end function div

    ! public methods
     subroutine set(this, field_mesh, description, val, error)
        !< Set field.
        class(field_FD_1D), intent(inout)                 :: this        !< The field.
        class(mesh),        intent(in),  optional, target :: field_mesh  !< Mesh of the field.
        character(*),       intent(in),  optional         :: description !< Mesh description
        real(R_P),          intent(in),  optional         :: val(1:)     !< Field value.
        integer(I_P),       intent(out), optional         :: error       !< Error status.

        if (present(field_mesh)) call this%associate_mesh(field_mesh=field_mesh, error=error)
        if (present(description)) this%description = description
        if (present(val)) this%val(1:) = val ! TO BE FIXED SINCE THERE ARE GHOST NODES
        if (present(error)) error = 0
    end subroutine set

    ! public overridden methods
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(field_FD_1D), intent(inout) :: this !< The field.

        ! call this%field%free
        if (allocated(this%val)) deallocate(this%val)
    end subroutine free
end module openpde_field_FD_1D
