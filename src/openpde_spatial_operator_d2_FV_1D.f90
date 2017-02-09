!< Concrete class of spatial operator of second derivative for Finite Volume 1D methods.
module openpde_spatial_operator_d2_FV_1D
    !< Concrete class of spatial operator of second derivative for Finite Volume 1D methods.
    use openpde_field_abstract
    use openpde_spatial_operator_d2_abstract
    use openpde_field_FV_1D
    use openpde_kinds
    use openpde_mesh_FV_1D

    implicit none
    private
    public :: spatial_operator_d2_FV_1D

    type, extends(spatial_operator_d2) :: spatial_operator_d2_FV_1D
        !< Concrete class of spatial operator of second derivative for Finite Volume 1D methods.
        contains
            procedure :: operate !< Operator operation.
    endtype spatial_operator_d2_FV_1D
contains
    function operate(this, inp, dir) result(opr)
        !< Operator operation.
        class(spatial_operator_d2_FV_1D), intent(in)           :: this     !< The operator.
        class(field),                     intent(in), target   :: inp      !< Input field.
        integer(I_P),                     intent(in), optional :: dir      !< Direction of operation.
        class(field), allocatable, target                      :: opr      !< Field resulting after the operator application.
        class(field_FV_1D), pointer                            :: inp_cur  !< Dummy pointer for input field.
        class(field_FV_1D), pointer                            :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_FV_1D),  pointer                            :: mesh_cur !< Dummy pointer for mesh.
        integer(I_P)                                           :: b        !< Counter.
        integer(I_P)                                           :: i        !< Counter.

        allocate(field_FV_1D :: opr)
        opr_cur => associate_field_FV_1D(field_input=opr, emsg='calling procedure spatial_operator_d2_FV_1D%operate')
        inp_cur => associate_field_FV_1D(field_input=inp, emsg='calling procedure spatial_operator_d2_FV_1D%operate')
        mesh_cur => associate_mesh_FV_1D(mesh_input=inp%m, emsg='calling procedure spatial_operator_d2_FV_1D%operate')
        call opr_cur%associate_mesh(field_mesh=inp%m)
        do b=1, mesh_cur%nb
            do i=1, mesh_cur%blocks(b)%n
                opr_cur%blocks(b)%val(i) = (inp_cur%blocks(b)%val(i+1) - &
                                            2._R_P*inp_cur%blocks(b)%val(i) + &
                                            inp_cur%blocks(b)%val(i-1))/(mesh_cur%blocks(b)%h**2)
            enddo
        enddo
    end function operate
end module openpde_spatial_operator_d2_FV_1D
