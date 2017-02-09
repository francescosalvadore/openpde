!< Concrete class of field for Finite Volume 1D methods.
module openpde_field_FV_1D
    !< Concrete class of field for Finite Volume 1D methods.
    !<
    !< This field is associated to a meh based on a uniform, Cartesian cell-centered discretization of the domain,
    !< that is a multiblock mesh.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use openpde_field_abstract
    use openpde_field_block_FV_1D
    use openpde_kinds
    use openpde_mesh_abstract
    use openpde_mesh_FV_1D
    use openpde_mesh_block_FV_1D

    implicit none
    private
    public :: associate_field_FV_1D, field_FV_1D

    type, extends(field) :: field_FV_1D
        !< Concrete class of field for Finite Volume 1D methods.
        integer(I_P)                                       :: nb     !< Number of blocks.
        type(field_block_FV_1D), allocatable, dimension(:) :: blocks !< The blocks.
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
    endtype field_FV_1D
contains
    ! public, non TBP
    function associate_field_FV_1D(field_input, emsg) result(field_pointer)
        !< Check the type of the field passed as input and return a Finite Difference 1D field pointer associated to field.
        class(field),       intent(in), target   :: field_input   !< Input field.
        character(*),       intent(in), optional :: emsg          !< Auxiliary error message.
        class(field_FV_1D), pointer              :: field_pointer !< Finite Difference 1D field pointer.

        select type(field_input)
            type is(field_FV_1D)
                field_pointer => field_input
            class default
               write(stderr, '(A)')'error: cast field to field_FV_1D'
               if (present(emsg)) write(stderr, '(A)') emsg
               stop
        end select
      end function associate_field_FV_1D

    ! deferred public methods
    subroutine associate_mesh(this, field_mesh, error)
        !< Associate field to a mesh.
        class(field_FV_1D), intent(inout)         :: this       !< The field.
        class(mesh),        intent(in), target    :: field_mesh !< Mesh of the field.
        integer(I_P),       intent(out), optional :: error      !< Error status.
        class(mesh_FV_1D), pointer                :: mesh_cur   !< Dummy pointer for mesh.
        integer(I_P)                              :: b          !< Counter.

        mesh_cur => associate_mesh_FV_1D(mesh_input=field_mesh, emsg='calling procedure field_FV_1D%associate_mesh')
        this%m => mesh_cur
        this%nb = mesh_cur%nb
        if (allocated(this%blocks)) deallocate(this%blocks) ; allocate(this%blocks(1:this%nb))
        do b=1, this%nb
            call this%blocks(b)%alloc(mesh_block=mesh_cur%blocks(b), error=error)
        end do
        if (present(error)) error = 0
    end subroutine associate_mesh

    subroutine init(this, field_mesh, description, error)
        !< Initialize finite difference 1D field.
        class(field_FV_1D), intent(inout)         :: this        !< The field.
        class(mesh),        intent(in), target    :: field_mesh  !< Mesh of the field.
        character(*),       intent(in),  optional :: description !< Mesh description
        integer(I_P),       intent(out), optional :: error       !< Error status.
        class(mesh_FV_1D),  pointer               :: mesh_cur    !< Dummy pointer for mesh.
        integer(I_P)                              :: b           !< Counter.

        call this%free
        call this%associate_mesh(field_mesh=field_mesh, error=error)
        if (present(description)) this%description = description
        mesh_cur => associate_mesh_FV_1D(mesh_input=field_mesh, emsg='calling procedure field_FV_1D%init')
        do b=1, this%nb
            call this%blocks(b)%init(mesh_field=mesh_cur, b=b, error=error)
        end do
        if (present(error)) error = 0
    end subroutine init

    subroutine output(this, filename, error)
        !< Output field data.
        class(field_FV_1D), intent(in)            :: this     !< The field.
        character(len=*),   intent(in)            :: filename !< Output file name.
        integer(I_P),       intent(out), optional :: error    !< Error status.
        class(mesh_FV_1D),  pointer               :: mesh_cur !< Dummy pointer for mesh.
        integer(I_P)                              :: b        !< Counter.

        mesh_cur => associate_mesh_FV_1D(mesh_input=this%m, emsg='calling procedure field_FV_1D%output')
        open(unit=11, file=filename)
        do b=1, mesh_cur%nb
            call this%blocks(b)%output(unit=11, mesh_block=mesh_cur%blocks(b), error=error)
        enddo
        close(11)
        if (present(error)) error = 0
    end subroutine output

    ! deferred private methods
    function add(lhs, rhs) result(opr)
        !< Add fields.
        class(field_FV_1D), intent(in)         :: lhs     !< Left hand side.
        class(field),       intent(in), target :: rhs     !< Left hand side.
        class(field), allocatable, target      :: opr     !< Operator result.
        class(field_FV_1D), pointer            :: rhs_cur !< Dummy pointer for rhs.
        class(field_FV_1D), pointer            :: opr_cur !< Dummy pointer for operator result.

        rhs_cur => associate_field_FV_1D(field_input=rhs, emsg='calling procedure field_FV_1D%add')
        allocate(field_FV_1D :: opr)
        opr_cur => associate_field_FV_1D(field_input=opr, emsg='calling procedure field_FV_1D%add')
        call opr_cur%associate_mesh(field_mesh=lhs%m)
        opr_cur%blocks = lhs%blocks + rhs_cur%blocks
    end function add

    subroutine assign_field(lhs, rhs)
        !< Assign fields.
        class(field_FV_1D), intent(inout)      :: lhs     !< Left hand side.
        class(field),       intent(in), target :: rhs     !< Right hand side.
        class(field_FV_1D), pointer            :: rhs_cur !< Dummy pointer for rhs.

        rhs_cur => associate_field_FV_1D(field_input=rhs, emsg='calling procedure field_FV_1D%assign')
        call lhs%associate_mesh(field_mesh=rhs_cur%m)
        lhs%blocks = rhs_cur%blocks
    end subroutine assign_field

    function mul(lhs, rhs) result(opr)
        !< Multiply fields.
        class(field_FV_1D), intent(in)         :: lhs     !< Left hand side.
        class(field),       intent(in), target :: rhs     !< Left hand side.
        class(field), allocatable, target      :: opr     !< Operator result.
        class(field_FV_1D), pointer            :: rhs_cur !< Dummy pointer for rhs.
        class(field_FV_1D), pointer            :: opr_cur !< Dummy pointer for operator result.

        rhs_cur => associate_field_FV_1D(field_input=rhs, emsg='calling procedure field_FV_1D%mul')
        allocate(field_FV_1D :: opr)
        opr_cur => associate_field_FV_1D(field_input=opr, emsg='calling procedure field_FV_1D%mul')
        call opr_cur%associate_mesh(field_mesh=lhs%m)
        opr_cur%blocks = lhs%blocks * rhs_cur%blocks
    end function mul

    function mulreal(lhs, rhs) result(opr)
        !< Multiply field for real.
        class(field_FV_1D), intent(in)    :: lhs     !< Left hand side.
        real(R_P),          intent(in)    :: rhs     !< Right hand side.
        class(field), allocatable, target :: opr     !< Operator result.
        class(field_FV_1D), pointer       :: opr_cur !< Dummy pointer for operator result.

        allocate(field_FV_1D :: opr)
        opr_cur => associate_field_FV_1D(field_input=opr, emsg='calling procedure field_FV_1D%mulreal')
        call opr_cur%associate_mesh(field_mesh=lhs%m)
        opr_cur%blocks = lhs%blocks * rhs
    end function mulreal

    function realmul(lhs, rhs) result(opr)
        !< Multiply real for field.
        real(R_P),          intent(in)    :: lhs     !< Left hand side.
        class(field_FV_1D), intent(in)    :: rhs     !< Right hand side.
        class(field), allocatable, target :: opr     !< Operator result.
        class(field_FV_1D), pointer       :: opr_cur !< Dummy pointer for operator result.

        allocate(field_FV_1D :: opr)
        opr_cur => associate_field_FV_1D(field_input=opr, emsg='calling procedure field_FV_1D%realmul')
        call opr_cur%associate_mesh(field_mesh=rhs%m)
        opr_cur%blocks = lhs * rhs%blocks
    end function realmul

    function sub(lhs, rhs) result(opr)
        !< Subtract fields.
        class(field_FV_1D), intent(in)         :: lhs     !< Left hand side.
        class(field),       intent(in), target :: rhs     !< Left hand side.
        class(field), allocatable, target      :: opr     !< Operator result.
        class(field_FV_1D), pointer            :: rhs_cur !< Dummy pointer for rhs.
        class(field_FV_1D), pointer            :: opr_cur !< Dummy pointer for operator result.

        rhs_cur => associate_field_FV_1D(field_input=rhs, emsg='calling procedure field_FV_1D%sub')
        allocate(field_FV_1D :: opr)
        opr_cur => associate_field_FV_1D(field_input=opr, emsg='calling procedure field_FV_1D%sub')
        call opr_cur%associate_mesh(field_mesh=lhs%m)
        opr_cur%blocks = lhs%blocks - rhs_cur%blocks
    end function sub

    function div(lhs, rhs) result(opr)
        !< Subtract fields.
        class(field_FV_1D), intent(in)         :: lhs     !< Left hand side.
        class(field),       intent(in), target :: rhs     !< Left hand side.
        class(field), allocatable, target      :: opr     !< Operator result.
        class(field_FV_1D), pointer            :: rhs_cur !< Dummy pointer for rhs.
        class(field_FV_1D), pointer            :: opr_cur !< Dummy pointer for operator result.

        rhs_cur => associate_field_FV_1D(field_input=rhs, emsg='calling procedure field_FV_1D%sub')
        allocate(field_FV_1D :: opr)
        opr_cur => associate_field_FV_1D(field_input=opr, emsg='calling procedure field_FV_1D%sub')
        call opr_cur%associate_mesh(field_mesh=lhs%m)
        opr_cur%blocks = lhs%blocks / rhs_cur%blocks
    end function div

    ! public overridden methods
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(field_FV_1D), intent(inout) :: this !< The field.

        ! call this%field%free
        if (allocated(this%blocks)) deallocate(this%blocks)
    end subroutine free
end module openpde_field_FV_1D
