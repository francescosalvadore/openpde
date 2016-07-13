!< Abstract class of field.
module openpde_field_abstract
    !< Abstract class of field.
    use openpde_mesh_abstract
    use openpde_kinds

    implicit none
    private
    public :: field

    type, abstract :: field
        !< Abstract class of field.
        character(len=:), allocatable :: description !< Field description.
        class(mesh), pointer          :: m => null() !< Pointer to the mesh of the field.
        contains
            ! deferred public methods
            procedure(abstract_associate_mesh), pass(this), deferred :: associate_mesh !< Associate mesh.
            procedure(abstract_init),           pass(this), deferred :: init           !< Initilize field.
            procedure(abstract_output),         pass(this), deferred :: output         !< Output field data.
            ! deferred private methods
            procedure(abstract_simmetric_operator), pass(lhs),  private, deferred :: add            !< Add fields.
            procedure(abstract_assign),             pass(lhs),  private, deferred :: assign_field   !< Assign fields.
            procedure(abstract_simmetric_operator), pass(lhs),  private, deferred :: mul            !< Multiply fields.
            procedure(abstract_field_op_real),      pass(lhs),  private, deferred :: mulreal        !< Multiply field for real.
            procedure(abstract_real_op_field),      pass(rhs),  private, deferred :: realmul        !< Multiply real for field.
            procedure(abstract_simmetric_operator), pass(lhs),  private, deferred :: sub            !< Subtract fields.
            ! public operators
            generic, public :: operator(+) => add                   !< Operator `+` overloading.
            generic, public :: operator(*) => mul, realmul, mulreal !< Operator `*` overloading.
            generic, public :: operator(-) => sub                   !< Operator `-` overloading.
            generic, public :: assignment(=) => assign_field        !< Assignment overloading.
            ! public methods
            procedure, pass(this) :: free !< Free dynamic memory.
    endtype field

    ! deferred public methods interfaces
    abstract interface
        !< Associate a mesh to field.
        subroutine abstract_associate_mesh(this, field_mesh, error)
            !< Associate a mesh to field.
            import :: field, I_P, mesh
            class(field), intent(inout)         :: this       !< The field.
            class(mesh),  intent(in), target    :: field_mesh !< The mesh.
            integer(I_P), intent(out), optional :: error      !< Error status.
        end subroutine abstract_associate_mesh
    endinterface

    abstract interface
        !< Initialize the field.
        subroutine abstract_init(this, field_mesh, description, error)
            !< Initialize the field.
            import :: field, I_P, mesh
            class(field), intent(inout)         :: this        !< The field.
            class(mesh),  intent(in), target    :: field_mesh  !< The mesh.
            character(*), intent(in),  optional :: description !< Description of the field.
            integer(I_P), intent(out), optional :: error       !< Error status.
        end subroutine abstract_init
    endinterface

    abstract interface
        !< Output the field.
        subroutine abstract_output(this, filename, error)
            !< Output the field.
            import :: field, I_P
            class(field),     intent(in)            :: this     !< The field.
            character(len=*), intent(in)            :: filename !< Output fiel name.
            integer(I_P),     intent(out), optional :: error    !< Error status.
        end subroutine abstract_output
    endinterface

    ! deferred private methods interfaces
    abstract interface
        !< Symmetric operator field.op.field.
        function abstract_simmetric_operator(lhs, rhs) result(opr)
            !< Symmetric operator field.op.field.
            import :: field
            class(field), intent(in)         :: lhs !< Left hand side.
            class(field), intent(in), target :: rhs !< Right hand side.
            class(field), allocatable        :: opr !< Operator result.
        end function abstract_simmetric_operator
    endinterface

    abstract interface
        !< Non symmetric operator field.op.real.
        function abstract_field_op_real(lhs, rhs) result(opr)
            !< Non symmetric operator field.op.real.
            import :: field, R_P
            class(field), intent(in)  :: lhs !< Left hand side.
            real(R_P),    intent(in)  :: rhs !< Right hand side.
            class(field), allocatable :: opr !< Operator result.
        end function abstract_field_op_real
    endinterface

    abstract interface
        !< Non symmetric operator real.op.field.
        function abstract_real_op_field(lhs, rhs) result(opr)
            !< Non symmetric operator real.op.field.
            import :: field, R_P
            real(R_P),    intent(in)  :: lhs !< Left hand side.
            class(field), intent(in)  :: rhs !< Right hand side.
            class(field), allocatable :: opr !< Operator result.
        end function abstract_real_op_field
    endinterface

    abstract interface
        !< Assignment overloading.
        subroutine abstract_assign(lhs, rhs)
            !< Assignment overloading.
            import :: field
            class(field), intent(inout)      :: lhs !< Left hand side.
            class(field), intent(in), target :: rhs !< Right hand side.
        end subroutine abstract_assign
    endinterface
contains
    ! public methods
    elemental subroutine free(this)
        !< Free dynamic memory.
        !<
        !< @todo Discuss about the *free* of mesh pointer.
        class(field), intent(inout) :: this !< The field.
        if (allocated(this%description)) deallocate(this%description)
        ! if (associated(this%m)) then
        !   call this%m%free
        !   deallocate(this%m)
        ! endif
        ! this%m => null()
    end subroutine free
end module openpde_field_abstract
