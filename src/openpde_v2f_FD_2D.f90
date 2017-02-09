!< Concrete class of v2f (vector to field) for FD 1D
module openpde_v2f_FD_2D
    !< Concrete class of v2f (vector to field) for FD 1D
    use openpde_field_abstract
    use openpde_v2f_abstract
    use openpde_field_FD_2D
    use openpde_kinds
    use openpde_mesh_FD_2D
    use openpde_vector_abstract

    implicit none
    private
    public :: v2f_FD_2D

    type, extends(v2f) :: v2f_FD_2D
        !< Concrete class of v2f (vector to field) for FD 1D
        contains
            procedure :: operate !< Operator operation.
    endtype v2f_FD_2D
contains
    subroutine operate(this, vec, fie)
        !< Operator operation.
        class(v2f_FD_2D), intent(in)        :: this      !< The operator.
        class(vector), intent(in)           :: vec       !< Input vector.
        class(field), intent(inout), dimension(:) :: fie       !< Returned field.
        class(field_FD_2D), pointer         :: fie_cur   !< Dummy pointer for input field.
        class(mesh_FD_2D),  pointer         :: mesh_cur  !< Dummy pointer for mesh.
        integer(I_P)                        :: i         !< Counter.
        integer(I_P)                        :: j         !< Counter.
        integer(I_P)                        :: i_equ         !< Counter.
        integer(I_P)                        :: i_vec         !< Counter.
        integer(I_P)                        :: n, nx, ny         !< Number of points.
        integer(I_P)                        :: n_equ         !< Number of points.

        n_equ = this%n_equ

        mesh_cur => associate_mesh_FD_2D(mesh_input=this%mesh)
        nx = mesh_cur%nx
        ny = mesh_cur%ny
        n = nx*ny

        do i_equ = 1, n_equ
            call fie(i_equ)%init(field_mesh=this%mesh)

            fie_cur => associate_field_FD_2D(field_input=fie(i_equ))

            ! No concrete features of vector are used so dynamic casting is not needed
            do j=1, ny
            do i=1, nx
                i_vec = n*(n_equ-1) + (j-1)*nx + i
                fie_cur%val(i,j) = vec%get(i_vec)
            enddo
            enddo
        enddo

    end subroutine operate
end module openpde_v2f_FD_2D
