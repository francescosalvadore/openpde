!< Abstract class of field.
module opendiff_adt_field
    !< Abstract class of field.
    use opendiff_adt_mesh
    use opendiff_kinds

    implicit none
    private
    public :: field

    type, abstract :: field
        !< Abstract class for *field* handling.
        character(len=:), allocatable :: description !< Field description.
        class(mesh), pointer          :: m => null() !< Pointer to the mesh of the field.
        contains
            ! deferred methods
            procedure(abstract_simmetric_operator),       private, deferred :: add            !< Add fields.
            procedure(abstract_assign),                   private, deferred :: assign_field   !< Assign fields.
            procedure(abstract_associate_mesh),           private, deferred :: associate_mesh !< Associate field to a mesh.
            procedure(abstract_init),                              deferred :: init           !< Initilize field.
            procedure(abstract_simmetric_operator),       private, deferred :: mul            !< Multiply fields.
            procedure(abstract_field_op_real),            private, deferred :: mulreal        !< Multiply field for real.
            procedure(abstract_output),                            deferred :: output         !< Output field data.
            procedure(abstract_simmetric_operator),       private, deferred :: sub            !< Subtract fields.
            procedure(abstract_real_op_field), pass(rhs), private, deferred :: realmul        !< Multiply real for field.
            !RIMETTERE !procedure :: assigreal   => assigreal_mesh_fd_1d_scal
            ! public methods
            procedure :: free !< Free dynamic memory.
            ! operators
            generic, public :: operator(+) => add                   !< Operator `+` overloading.
            generic, public :: operator(*) => mul, realmul, mulreal !< Operator `*` overloading.
            generic, public :: operator(-) => sub                   !< Operator `-` overloading.
            generic, public :: assignment(=) => assign_field        !< Assignment overloading.
    endtype field

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

    abstract interface
        !< Associate a mesh to field.
        subroutine abstract_associate_mesh(this, fieldmesh, error)
            !< Associate a mesh to field.
            import :: field, I_P, mesh
            class(field), intent(inout)         :: this      !< The field.
            class(mesh),  intent(in), target    :: fieldmesh !< The mesh.
            integer(I_P), intent(out), optional :: error     !< Error status.
        end subroutine abstract_associate_mesh
    endinterface

    abstract interface
        !< Initialize the field.
        subroutine abstract_init(this, fieldmesh, description, error)
            !< Initialize the field.
            import :: field, I_P, mesh
            class(field), intent(inout)         :: this        !< The field.
            class(mesh),  intent(in), target    :: fieldmesh   !< The mesh.
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
contains
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(field), intent(inout) :: this !< The mesh.
        if (allocated(this%description)) deallocate(this%description)
        ! if (associated(this%m)) deallocate(this%m) ; this%m => null()
    end subroutine free
end module opendiff_adt_field
