!< Abstract class of spatial operator.
module openpde_spatial_operator_abstract
    !< Abstract class of spatial operator.
    use openpde_field_abstract
    use openpde_kinds

    implicit none
    private
    public :: spatial_operator

    type, abstract :: spatial_operator
        !< Abstract class of spatial operator.
        character(len=:), allocatable :: description !< Operator description.
        contains
            ! deferred public methods
            procedure(abstract_operate), pass(this), deferred :: operate !< Operator function.
    endtype spatial_operator

    abstract interface
        !< Operator operation.
        function abstract_operate(this, inp, dir) result(opr)
            !< Operator function.
            import :: spatial_operator, field, I_P
            class(spatial_operator), intent(in)           :: this !< The operator.
            class(field),            intent(in), target   :: inp  !< Input field.
            integer(I_P),            intent(in), optional :: dir  !< Direction of operation.
            class(field), allocatable, target             :: opr  !< Field resulting after the operator application.
        end function abstract_operate
    endinterface
end module openpde_spatial_operator_abstract
