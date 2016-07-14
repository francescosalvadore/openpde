!< Concrete class of spatial operator of first derivative for Finite Difference 2D methods.
module openpde_spatial_operator_d1_FD_2D
    !< Concrete class of spatial operator of first derivative for Finite Difference 2D methods.
    use openpde_field_abstract
    use openpde_spatial_operator_d1_abstract
    use openpde_field_FD_2D
    use openpde_kinds
    use openpde_mesh_FD_2D

    implicit none
    private
    public :: spatial_operator_d1_FD_2D

    type, extends(spatial_operator_d1) :: spatial_operator_d1_FD_2D
        !< Concrete class of spatial operator of first derivative for Finite Difference 2D methods.
        contains
            ! deferred public methods
            procedure :: operate !< Operator operation.
    endtype spatial_operator_d1_FD_2D
contains
    function operate(this, inp, dir) result(opr)
        !< Operator operation.
        class(spatial_operator_d1_FD_2D), intent(in)           :: this     !< The operator.
        class(field),                     intent(in), target   :: inp      !< Input field.
        integer(I_P),                     intent(in), optional :: dir      !< Direction of operation.
        class(field), allocatable, target                      :: opr      !< Field resulting after the operator application.
        class(field_FD_2D), pointer                            :: inp_cur  !< Dummy pointer for input field.
        class(field_FD_2D), pointer                            :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_FD_2D),  pointer                            :: mesh_cur !< Dummy pointer for mesh.
        integer(I_P)                                           :: dir_     !< Direction of operation.
        integer(I_P)                                           :: i        !< Counter.
        integer(I_P)                                           :: j        !< Counter.

        allocate(field_FD_2D :: opr)
        call associate_field_FD_2D(field_input=opr,                                         &
                                   calling_procedure='operate_spatial_operator_FD_2D(opr)', &
                                   field_pointer=opr_cur)
        call associate_field_FD_2D(field_input=inp,                                         &
                                   calling_procedure='operate_spatial_operator_FD_2D(inp)', &
                                   field_pointer=inp_cur)
        call associate_mesh_FD_2D(mesh_input=inp%m,                                          &
                                  calling_procedure='operate_spatial_operator_FD_2D(inp%m)', &
                                  mesh_pointer=mesh_cur)
        call opr_cur%associate_mesh(field_mesh=inp%m)
        dir_= 1 ; if (present(dir)) dir_ = dir
        if (dir_==1) then
            do i=1, mesh_cur%nx
                opr_cur%val(i, :) = (inp_cur%val(i+1, :) - inp_cur%val(i, :))/(mesh_cur%hx)
            enddo
        else
            do j=1, mesh_cur%ny
                opr_cur%val(:, j) = (inp_cur%val(:, j+1) - inp_cur%val(:, j))/(mesh_cur%hy)
            enddo
        endif
    end function operate
end module openpde_spatial_operator_d1_FD_2D

