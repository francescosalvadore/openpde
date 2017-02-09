!< Abstract class of v2f (vector to field).
module openpde_v2f_abstract
    !< Abstract class of v2f (vector to field).
    use openpde_field_abstract
    use openpde_mesh_abstract
    use openpde_vector_abstract
    use openpde_kinds

    implicit none
    private
    public :: v2f

    type, abstract :: v2f
        !< Abstract class of v2f.
        class(mesh), pointer :: mesh
        integer(I_P) :: n_equ
        contains
            ! deferred public methods
            procedure(abstract_operate), pass(this), deferred :: operate !< Operator function.
    endtype v2f

    abstract interface
        !< Operator operation.
        subroutine abstract_operate(this, vec, fie)
            !< Operator function.
            import :: I_P, v2f, field, vector
            class(v2f), intent(in)                   :: this !< The operator.
            class(vector), intent(in)                :: vec  !< Input vector.
            class(field), intent(inout), dimension(:)  :: fie  !< Resulting field.
        end subroutine abstract_operate
    endinterface
end module openpde_v2f_abstract
