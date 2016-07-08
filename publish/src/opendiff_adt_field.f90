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
        function abstract_simmetric_operator(lhs, rhs) result(opr)
            import :: field
            class(field), intent(in)         :: lhs
            class(field), intent(in), target :: rhs
            class(field), allocatable        :: opr
        end function abstract_simmetric_operator
    endinterface

    abstract interface
        function abstract_field_op_real(lhs, rhs) result(opr)
            import :: field, R8P
            class(field), intent(in)  :: lhs
            real(R8P),    intent(in)  :: rhs
            class(field), allocatable :: opr
        end function abstract_field_op_real
    endinterface

    abstract interface
        function abstract_real_op_field(lhs, rhs) result(opr)
            import :: field, R8P
            real(R8P),    intent(in)  :: lhs
            class(field), intent(in)  :: rhs
            class(field), allocatable :: opr
        end function abstract_real_op_field
    endinterface

    abstract interface
        subroutine abstract_assign(lhs, rhs)
            import :: field
            class(field), intent(inout)      :: lhs
            class(field), intent(in), target :: rhs
        end subroutine abstract_assign
    endinterface

    abstract interface
        subroutine abstract_associate_mesh(this, fieldmesh, error)
            import :: field, I4P, mesh
            class(field), intent(inout)         :: this
            class(mesh),  intent(in), target    :: fieldmesh
            integer(I4P), intent(out), optional :: error
        end subroutine abstract_associate_mesh
    endinterface

    abstract interface
        subroutine abstract_init(this, fieldmesh, description, error)
            import :: field, I4P, mesh
            class(field), intent(inout)         :: this
            class(mesh),  intent(in), target    :: fieldmesh
            character(*), intent(in),  optional :: description
            integer(I4P), intent(out), optional :: error
        end subroutine abstract_init
    endinterface

    abstract interface
        subroutine abstract_output(this, filename, error)
            import :: field, I4P
            class(field),     intent(in)            :: this
            character(len=*), intent(in)            :: filename
            integer(I4P),     intent(out), optional :: error
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