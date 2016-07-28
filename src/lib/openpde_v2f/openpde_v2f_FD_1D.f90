!< Concrete class of v2f (vector to field) for FD 1D
module openpde_v2f_FD_1D
    !< Concrete class of v2f (vector to field) for FD 1D
    use openpde_field_abstract
    use openpde_v2f_abstract
    use openpde_field_FD_1D
    use openpde_kinds
    use openpde_mesh_FD_1D
    use openpde_vector_abstract

    implicit none
    private
    public :: v2f_FD_1D

    type, extends(v2f) :: v2f_FD_1D
        !< Concrete class of v2f (vector to field) for FD 1D
        contains
            procedure :: operate !< Operator operation.
    endtype v2f_FD_1D
contains
    function operate(this, vec) result(fie)
        !< Operator operation.
        class(v2f_FD_1D), intent(in)        :: this      !< The operator.
        class(vector), intent(in)           :: vec       !< Input vector.
        class(field), allocatable           :: fie       !< Returned field.
        class(field_FD_1D), pointer         :: fie_cur   !< Dummy pointer for input field.
        class(mesh_FD_1D),  pointer         :: mesh_cur  !< Dummy pointer for mesh.
        integer(I_P)                        :: i         !< Counter.
        integer(I_P)                        :: n         !< Number of points.
        !integer(I_P)                        :: ng        !< Number of points.

        n = vec%n

        allocate(field_FD_1D :: fie)
        call fie%init(field_mesh=this%mesh)

        !mesh_cur => associate_mesh_FD_1D(mesh_input=this%mesh)
        !ng = mesh_cur%ng

        fie_cur => associate_field_FD_1D(field_input=fie)

        ! No concrete features of vector are used so dynamic casting is not needed
        do i=1, n
            fie_cur%val(i) = vec%get(i)
        enddo

    end function operate
end module openpde_v2f_FD_1D
