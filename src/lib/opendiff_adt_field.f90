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
            procedure(abstract_fieldadd),            private, deferred :: add            !< Add fields.
            procedure(abstract_fieldassign),         private, deferred :: assign_field   !< Assign fields.
            procedure(abstract_fieldassociate_mesh), private, deferred :: associate_mesh !< Associate field to a mesh.
            procedure(abstract_fieldinit),                    deferred :: init           !< Initilize field.
            procedure(abstract_fieldoutput),                  deferred :: output         !< Output field data.
            ! public methods
            procedure :: free !< Free dynamic memory.
            ! operators
            generic, public :: operator(+)  => add           !< Operator `+` overloading.
            generic, public :: assignment(=) => assign_field !< Assignment overloading.
    endtype field

    abstract interface
        function abstract_fieldadd(lhs, rhs) result(opr)
            import :: field
            class(field), intent(in)         :: lhs
            class(field), intent(in), target :: rhs
            class(field), allocatable        :: opr
        end function abstract_fieldadd
    endinterface

    abstract interface
        subroutine abstract_fieldassign(lhs, rhs)
            import :: field
            class(field), intent(inout)      :: lhs
            class(field), intent(in), target :: rhs
        end subroutine abstract_fieldassign
    endinterface

    abstract interface
        subroutine abstract_fieldassociate_mesh(this, fieldmesh, error)
            import :: field, I4P, mesh
            class(field), intent(inout)         :: this
            class(mesh),  intent(in), target    :: fieldmesh
            integer(I4P), intent(out), optional :: error
        end subroutine abstract_fieldassociate_mesh
    endinterface

    abstract interface
        subroutine abstract_fieldinit(this, fieldmesh, description, error)
            import :: field, I4P, mesh
            class(field), intent(inout)         :: this
            class(mesh),  intent(in), target    :: fieldmesh
            character(*), intent(in),  optional :: description
            integer(I4P), intent(out), optional :: error
        end subroutine abstract_fieldinit
    endinterface

    abstract interface
        subroutine abstract_fieldoutput(this, filename, error)
            import :: field, I4P
            class(field),     intent(in)            :: this
            character(len=*), intent(in)            :: filename
            integer(I4P),     intent(out), optional :: error
        end subroutine abstract_fieldoutput
    endinterface
contains
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(field), intent(inout) :: this !< The mesh.
        if (allocated(this%description)) deallocate(this%description)
        ! if (associated(this%m)) deallocate(this%m) ; this%m => null()
    end subroutine free
end module opendiff_adt_field
