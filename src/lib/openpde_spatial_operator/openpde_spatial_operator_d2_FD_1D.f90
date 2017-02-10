!< Concrete class of spatial operator of second derivative for Finite Difference 1D methods.
module openpde_spatial_operator_d2_FD_1D
    !< Concrete class of spatial operator of second derivative for Finite Difference 1D methods.
    use openpde_field_abstract
    use openpde_spatial_operator_d2_abstract
    use openpde_field_FD_1D
    use openpde_kinds
    use openpde_mesh_FD_1D

    implicit none
    private
    public :: spatial_operator_d2_FD_1D

    type, extends(spatial_operator_d2) :: spatial_operator_d2_FD_1D
        !< Concrete class of spatial operator of second derivative for Finite Difference 1D methods.
        contains
            procedure :: operate !< Operator operation.
    endtype spatial_operator_d2_FD_1D
contains
    function operate(this, inp, dir) result(opr)
        !< Operator operation.
        class(spatial_operator_d2_FD_1D), intent(in)           :: this     !< The operator.
        class(field),                     intent(in), target   :: inp      !< Input field.
        integer(I_P),                     intent(in), optional :: dir      !< Direction of operation.
        class(field), allocatable, target                      :: opr      !< Field resulting after the operator application.
        class(field_FD_1D), pointer                            :: inp_cur  !< Dummy pointer for input field.
        class(field_FD_1D), pointer                            :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_FD_1D),  pointer                            :: mesh_cur !< Dummy pointer for mesh.
        integer(I_P)                                           :: i        !< Counter.

        allocate(field_FD_1D :: opr)
        opr_cur => associate_field_FD_1D(field_input=opr, emsg='calling procedure spatial_operator_d2_FD_1D%operate')
        inp_cur => associate_field_FD_1D(field_input=inp, emsg='calling procedure spatial_operator_d2_FD_1D%operate')
        mesh_cur => associate_mesh_FD_1D(mesh_input=inp%m, emsg='calling procedure spatial_operator_d2_FD_1D%operate')
        call opr_cur%associate_mesh(field_mesh=inp%m)
        do i=1, mesh_cur%n
            !opr_cur%val(i) = (inp_cur%val(i+1) - 2._R_P*inp_cur%val(i) + inp_cur%val(i-1))/(mesh_cur%h**2)
            opr_cur%val(i) = (-1./12.*inp_cur%val(i+2) +4./3.*inp_cur%val(i+1) &
                              -5./2.*inp_cur%val(i) +4./3.*inp_cur%val(i-1) -1./12.*inp_cur%val(i-2))/(mesh_cur%h**2)
        enddo
    end function operate
end module openpde_spatial_operator_d2_FD_1D
