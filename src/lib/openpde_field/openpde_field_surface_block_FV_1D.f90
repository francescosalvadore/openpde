!< Concrete class of field surface block for Finite Volume 1D methods.
module openpde_field_surface_block_FV_1D
    !< Concrete class of field surface block for Finite Volume 1D methods.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use openpde_kinds
    use openpde_field_block_FV_1D
    use openpde_mesh_FV_1D
    use openpde_mesh_block_FV_1D

    implicit none
    private
    public :: field_surface_block_FV_1D

    type :: field_surface_block_FV_1D
        !< Concrete class of field surface block for Finite Volume 1D methods.
        real(R_P), allocatable, dimension(:) :: val !< Block value.
        contains
            ! public methods
            procedure, pass(this) :: alloc          !< Allocate block.
            procedure, pass(this) :: compute_fluxes !< Compute fluxes of field through surfaces.
            procedure, pass(this) :: free           !< Free dynamic memory.
            procedure, pass(this) :: init           !< Initilize block.
            procedure, pass(this) :: output         !< Output block data.
            ! public operators
            generic, public :: operator(+) => add                   !< Operator `+` overloading.
            generic, public :: operator(*) => mul, realmul, mulreal !< Operator `*` overloading.
            generic, public :: operator(-) => sub                   !< Operator `-` overloading.
            generic, public :: assignment(=) => assign_block        !< Assignment overloading.
            ! private methods
            procedure, pass(lhs), private :: add          !< Add blocks.
            procedure, pass(lhs), private :: assign_block !< Assign blocks.
            procedure, pass(lhs), private :: mul          !< Multiply blocks.
            procedure, pass(lhs), private :: mulreal      !< Multiply block for real.
            procedure, pass(rhs), private :: realmul      !< Multiply real for block.
            procedure, pass(lhs), private :: sub          !< Subtract blocks.
    endtype field_surface_block_FV_1D
contains
    ! public methods
    subroutine alloc(this, mesh_block, error)
        !< Allocate block.
        class(field_surface_block_FV_1D), intent(inout)         :: this       !< The block.
        type(mesh_block_FV_1D),           intent(in)            :: mesh_block !< Mesh of the block.
        integer(I_P),                     intent(out), optional :: error      !< Error status.

        call this%free
        allocate(this%val(0:mesh_block%n))
        if (present(error)) error = 0
    end subroutine alloc

    subroutine compute_fluxes(this, field_cell, mesh_block, error)
        !< Compute fluxes of field through surfaces.
        class(field_surface_block_FV_1D), intent(inout)         :: this       !< The block.
        type(field_block_FV_1D),          intent(in)            :: field_cell !< Field at cells center.
        type(mesh_block_FV_1D),           intent(in)            :: mesh_block !< Mesh of the block.
        integer(I_P),                     intent(out), optional :: error      !< Error status.
        integer(I_P)                                            :: i          !< Counter.

        do i=0, mesh_block%n
            this%val(i) = field_cell%val(i)
        enddo
    end subroutine compute_fluxes

    elemental subroutine free(this)
        !< Free dynamic memory.
        class(field_surface_block_FV_1D), intent(inout) :: this !< The field.

        if (allocated(this%val)) deallocate(this%val)
    end subroutine free

    subroutine init(this, mesh_block, error)
        !< Initialize block.
        class(field_surface_block_FV_1D), intent(inout)         :: this       !< The block.
        type(mesh_block_FV_1D),           intent(in)            :: mesh_block !< Mesh of the block.
        integer(I_P),                     intent(out), optional :: error      !< Error status.

        call this%alloc(mesh_block=mesh_block, error=error)
        this%val = 0._R_P
        if (present(error)) error = 0
    end subroutine init

    subroutine output(this, unit, mesh_block, error)
        !< Output block data.
        class(field_surface_block_FV_1D), intent(in)            :: this       !< The block.
        integer(I_P),                     intent(in)            :: unit       !< Unit file.
        type(mesh_block_FV_1D),           intent(in)            :: mesh_block !< Mesh of the block.
        integer(I_P),                     intent(out), optional :: error      !< Error status.
        integer(I_P)                                            :: i          !< Counter.

        do i=0, mesh_block%n
            write(unit, *) this%val(i)
        enddo
        if (present(error)) error = 0
    end subroutine output

    ! private methods
    elemental function add(lhs, rhs) result(opr)
        !< Add blocks.
        class(field_surface_block_FV_1D), intent(in) :: lhs !< Left hand side.
        type(field_surface_block_FV_1D),  intent(in) :: rhs !< Left hand side.
        type(field_surface_block_FV_1D)              :: opr !< Operator result.

        opr%val = lhs%val + rhs%val
    end function add

    elemental subroutine assign_block(lhs, rhs)
        !< Assign blocks.
        class(field_surface_block_FV_1D), intent(inout) :: lhs !< Left hand side.
        type(field_surface_block_FV_1D),  intent(in)    :: rhs !< Left hand side.

        lhs%val = rhs%val
    end subroutine assign_block

    elemental function mul(lhs, rhs) result(opr)
        !< Multiply blocks.
        class(field_surface_block_FV_1D), intent(in) :: lhs !< Left hand side.
        type(field_surface_block_FV_1D),  intent(in) :: rhs !< Left hand side.
        type(field_surface_block_FV_1D)              :: opr !< Operator result.

        opr%val = lhs%val * rhs%val
    end function mul

    elemental function mulreal(lhs, rhs) result(opr)
        !< Multiply field for real.
        class(field_surface_block_FV_1D), intent(in) :: lhs !< Left hand side.
        real(R_P),                        intent(in) :: rhs !< Right hand side.
        type(field_surface_block_FV_1D)              :: opr !< Operator result.

        opr%val = lhs%val * rhs
    end function mulreal

    elemental function realmul(lhs, rhs) result(opr)
        !< Multiply real for field.
        real(R_P),                        intent(in) :: lhs !< Left hand side.
        class(field_surface_block_FV_1D), intent(in) :: rhs !< Right hand side.
        type(field_surface_block_FV_1D)              :: opr !< Operator result.

        opr%val = lhs * rhs%val
    end function realmul

    elemental function sub(lhs, rhs) result(opr)
        !< Subtract blocks.
        class(field_surface_block_FV_1D), intent(in) :: lhs !< Left hand side.
        type(field_surface_block_FV_1D),  intent(in) :: rhs !< Left hand side.
        type(field_surface_block_FV_1D)              :: opr !< Operator result.

        opr%val = lhs%val - rhs%val
    end function sub
end module openpde_field_surface_block_FV_1D
