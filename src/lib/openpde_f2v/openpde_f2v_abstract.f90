!< Abstract class of f2v (field to vector).
module openpde_f2v_abstract
    !< Abstract class of f2v (field to vector).
    use openpde_field_abstract
    use openpde_vector_abstract
    use openpde_kinds

    implicit none
    private
    public :: f2v

    type, abstract :: f2v
        !< Abstract class of f2v.
        class(vector), allocatable :: vec            !< Vector used only to decide the type
        contains
            ! deferred public methods
            procedure(abstract_operate), pass(this), deferred :: operate !< Operator function.
    endtype f2v

    abstract interface
        !< Operator operation.
        function abstract_operate(this, fie) result(vec)
            !< Operator function.
            import :: I_P, f2v, field, vector
            class(f2v), intent(in)              :: this !< The operator.
            class(field),  intent(in), target, dimension(:)   :: fie  !< Input field.
            class(vector), allocatable          :: vec  !< Resulting vector.
        end function abstract_operate
    endinterface
end module openpde_f2v_abstract
