!< Concrete class of f2v (field to vector) for FD 1D
module openpde_f2v_FD_1D
    !< Concrete class of f2v (field to vector) for FD 1D
    use openpde_field_abstract
    use openpde_f2v_abstract
    use openpde_field_FD_1D
    use openpde_kinds
    use openpde_mesh_FD_1D
    use openpde_vector_abstract

    implicit none
    private
    public :: f2v_FD_1D

    type, extends(f2v) :: f2v_FD_1D
        !< Concrete class of f2v (field to vector) for FD 1D
        contains
            procedure :: operate !< Operator operation.
    endtype f2v_FD_1D
contains
    function operate(this, fie) result(vec)
        !< Operator operation.
        class(f2v_FD_1D), intent(in)       :: this     !< The operator.
        class(field), intent(in), target   :: fie      !< Input field.
        class(vector), allocatable         :: vec      !< Returned vector.
        class(field_FD_1D), pointer        :: fie_cur  !< Dummy pointer for input field.
        class(mesh_FD_1D),  pointer        :: mesh_cur !< Dummy pointer for mesh.
        integer(I_P)                       :: i        !< Counter.
        integer(I_P)                       :: n        !< Number of points.

        fie_cur => associate_field_FD_1D(field_input=fie, emsg='casting error')

        mesh_cur => associate_mesh_FD_1D(mesh_input=fie%m, emsg='mesh')
        n =  mesh_cur%n

        allocate(vec, mold=this%vec)
        call vec%init(n)

        ! No concrete features of vector are used so dynamic casting is not needed
        do i=1, n
        !    print*,"i, fie_cur%val(i) :",i, fie_cur%val(i)
            call vec%set(i, fie_cur%val(i))
        enddo
    end function operate
end module openpde_f2v_FD_1D
