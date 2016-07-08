!< Abstract class of spatial operator.
module opendiff_adt_spatial_operator
    !< Abstract class of spatial operator.
    use opendiff_adt_field

    implicit none
    private
    public :: spatial_operator

    type, abstract :: spatial_operator
        character(len=:), allocatable :: description !< Operator description.
        contains
            procedure(abstract_operate), deferred :: operate !< Operator operation.
    endtype spatial_operator

    abstract interface
        function abstract_operate(this, inp) result(opr)
            import :: spatial_operator, field
            class(spatial_operator)   :: this
            class(field), target      :: inp
            class(field), allocatable :: opr
        end function abstract_operate
    endinterface
end module opendiff_adt_spatial_operator
