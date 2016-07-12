!< Concrete class of spatial operator for 1D derivative for Finite Difference 1D.
module opendiff_spatial_operator_der2_fd_2d
    !< Concrete class of spatial operator for 1D derivative for Finite Difference 1D.
    use opendiff_adt_field
    use opendiff_adt_spatial_operator_der2
    use opendiff_field_fd_2d
    use opendiff_kinds
    use opendiff_mesh_fd_2d

    implicit none
    private
    public :: spatial_operator_der2_fd_2d

    type, extends(spatial_operator_der2) :: spatial_operator_der2_fd_2d
        !< Concrete class of spatial operator for first derivative for Finite Difference 1D.
        contains
            procedure :: operate !< Operator operation.
    endtype spatial_operator_der2_fd_2d
contains
    function operate(this, inp, dir) result(opr)
        !< Operator operation.
        class(spatial_operator_der2_fd_2d), intent(in)         :: this     !< The operator.
        class(field),                       intent(in), target :: inp      !< Input field.
        class(field), allocatable, target                      :: opr      !< Field resulting after the operator application.
        class(field_fd_2d), pointer                            :: inp_cur  !< Dummy pointer for input field.
        class(field_fd_2d), pointer                            :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_fd_2d),  pointer                            :: mesh_cur !< Dummy pointer for mesh.
        real(R_P)                                              :: hx, hy        !< Space step.
        integer(I_P)                                           :: i, j        !< Counter.
        integer(I_P)                                           :: nx, ny        !< Counter.
        integer(I_P)                                           :: ngx, ngy       !< Number of ghost cells.
        integer(I_P), optional                                 :: dir
        integer(I_P)                                           :: ddir
        if(.not.(present(dir))) ddir = 1
        allocate(field_fd_2d :: opr)
        select type(opr)
            type is(field_fd_2d)
                opr_cur => opr
            class default
               STOP 'Error passing field to add'
        end select
        select type(inp)
            type is(field_fd_2d)
                inp_cur => inp
            class default
               STOP 'Error passing field to spatial operate'
        end select
        associate(mm => inp%m)
            select type(mm)
                type is(mesh_fd_2d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate
        hx = mesh_cur%hx
        hy = mesh_cur%hy
        nx = mesh_cur%nx
        ny = mesh_cur%ny
        ngx = mesh_cur%ngx
        ngy = mesh_cur%ngy
        allocate(opr_cur%val(1-ngx:nx+ngx, 1-ngy:ny+ngy))
        opr_cur%m => mesh_cur
        do i=1,nx
            !opr_cur%val(i) = (inp_cur%val(i+1) - inp_cur%val(i-1))/(2.*h)
            opr_cur%val(i,j) = (inp_cur%val(i+1,j) - inp_cur%val(i,j))/(hx)
        enddo
        !opr_cur%val(1) = (inp_cur%val(2) - inp_cur%val(n))/(2.*h)
        !opr_cur%val(n) = (inp_cur%val(1) - inp_cur%val(n-1))/(2.*h)
    end function operate
end module opendiff_spatial_operator_der2_fd_2d
