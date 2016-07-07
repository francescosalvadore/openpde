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
        character(128)       :: description !< Field description.
        class(mesh), pointer :: m           !< Pointer to the mesh of the field.
        contains
            procedure(abstract_fieldinit),   deferred :: init   !< Initilize field.
            procedure(abstract_fieldoutput), deferred :: output !< Output field data.
            procedure(abstract_fieldadd),    deferred :: add    !< Add fields.
            procedure(abstract_fieldassign), deferred :: assign !< Assign fields.
            generic, public :: operator(+)  => add
            generic, public :: assignment(=) => assign
    endtype field

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
        subroutine abstract_fieldassign(lhs, rhs)
            import :: field
            class(field), intent(inout)      :: lhs
            class(field), intent(in), target :: rhs
        end subroutine abstract_fieldassign
    endinterface
end module opendiff_adt_field
