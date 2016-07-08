!< Concrete class of spatial operator for 1D derivative for Finite Difference 1D.
module opendiff_spatial_operator_der1_fd_1d
    !< Concrete class of spatial operator for 1D derivative for Finite Difference 1D.
    use opendiff_adt_field
    use opendiff_adt_spatial_operator_der1
    use opendiff_field_fd_1d
    use opendiff_kinds
    use opendiff_mesh_fd_1d

    implicit none
    private
    public :: spatial_operator_der1_fd_1d

    type, extends(spatial_operator_der1) :: spatial_operator_der1_fd_1d
        !< Concrete class of spatial operator for 1D derivative for Finite Difference 1D.
        contains
            procedure :: operate !< Operator operation.
    endtype spatial_operator_der1_fd_1d
contains
    function operate(this, inp) result(opr)
        class(spatial_operator_der1_fd_1d) :: this
        class(field), target               :: inp
        class(field_fd_1d), pointer        :: inp_cur
        class(field), allocatable, target  :: opr
        class(field_fd_1d), pointer        :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_fd_1d),  pointer        :: mesh_cur !< Dummy pointer for mesh.
        real(R8P)                          :: h
        integer                            :: i, n
        allocate(field_fd_1d :: opr)
        select type(opr)
            type is(field_fd_1d)
                opr_cur => opr
            class default
               STOP 'Error passing field to add'
        end select
        select type(inp)
            type is(field_fd_1d)
                inp_cur => inp
            class default
               STOP 'Error passing field to spatial operate'
        end select
        associate(mm => inp%m)
            select type(mm)
                type is(mesh_fd_1d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate
        h = mesh_cur%h
        n = mesh_cur%n
        allocate(opr_cur%val(1:n))
        opr_cur%m => mesh_cur
        do i=2,n-1
            opr_cur%val(i) = (inp_cur%val(i+1) - inp_cur%val(i-1))/(2.*h)
        enddo
        opr_cur%val(1) = (inp_cur%val(2) - inp_cur%val(n))/(2.*h)
        opr_cur%val(n) = (inp_cur%val(1) - inp_cur%val(n-1))/(2.*h)
    end function operate
end module opendiff_spatial_operator_der1_fd_1d