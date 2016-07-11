!< Abstract class of spatial operator.
module opendiff_adt_spatial_operator
    !< Abstract class of spatial operator.
    use opendiff_adt_field

    implicit none
    private
    public :: spatial_operator

    type, abstract :: spatial_operator
        !< Abstract class for *spatial operator* handling.
        character(len=:), allocatable :: description !< Operator description.
        contains
            procedure(abstract_operate), deferred :: operate !< Operator operation.
    endtype spatial_operator

    abstract interface
        !< Operator operation.
        function abstract_operate(this, inp) result(opr)
            !< Operator operation.
            import :: spatial_operator, field
            class(spatial_operator), intent(in)         :: this !< The operator.
            class(field),            intent(in), target :: inp  !< Input field.
            class(field), allocatable                   :: opr  !< Field resulting after the operator application.
        end function abstract_operate
    endinterface
end module opendiff_adt_spatial_operator
