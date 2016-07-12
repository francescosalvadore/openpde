!< Abstract class of spatial operator.
module opendiff_adt_spatial_operator
    !< Abstract class of spatial operator.
    use opendiff_kinds
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
        function abstract_operate(this, inp, dir) result(opr)
            !< Operator operation.
            import :: spatial_operator, field, I_P
            class(spatial_operator), intent(in)         :: this !< The operator.
            class(field),            intent(in), target :: inp  !< Input field.
            class(field), allocatable, target           :: opr  !< Field resulting after the operator application.
            integer(I_P), optional                      :: dir
        end function abstract_operate
    endinterface
end module opendiff_adt_spatial_operator
