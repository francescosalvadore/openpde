!< Abstract class of f2m (field to matrix) operator.
module openpde_f2m_abstract
    !< Abstract class of f2m (field to matrix) operator.
    use openpde_field_abstract
    use openpde_matrix_abstract
    use openpde_kinds

    implicit none
    private
    public :: f2m

    type, abstract :: f2m
        !< Abstract class of f2m operator.
        character(len=:), allocatable :: description !< Operator description.
        class(matrix), allocatable :: mat            !< Matrix used only to decide the type
        contains
            ! deferred public methods
            procedure(abstract_operate), deferred :: operate !< Operator function.
    endtype f2m

    abstract interface
        !< Operator operation.
        function abstract_operate(this, inp) result(opr)
            !< Operator function.
            import :: I_P, f2m, field, matrix
            class(f2m), intent(in)         :: this !< The operator.
            class(field),            intent(in), target   :: inp  !< Input field.
            class(matrix), allocatable, target            :: opr  !< Matrix representing the operator as f(u)=A*u
        end function abstract_operate
    endinterface
end module openpde_f2m_abstract