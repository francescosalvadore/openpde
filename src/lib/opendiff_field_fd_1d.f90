!< Concrete class of field for Finite Difference 1D.
module opendiff_field_fd_1d
    !< Concrete class of field for Finite Difference 1D.
    use opendiff_adt_field
    use opendiff_adt_mesh
    use opendiff_kinds
    use opendiff_mesh_fd_1d

    implicit none
    private
    public :: field_fd_1d

    type, extends(field) :: field_fd_1d
        !< Finite difference 1D class for *field* handling.
        real(R8P), allocatable, dimension(:) :: val !< Field value.
        contains
            procedure :: init    => field_fd_1d_init   !< Initilize field.
            procedure :: output  => field_fd_1d_output !< Output field data.
            procedure :: add     => field_fd_1d_add    !< Add fields.
            procedure :: assign  => field_fd_1d_assign !< Assign fields.
    endtype field_fd_1d
contains
    function field_fd_1d_init(this, fieldmesh) result(res)
        !< Initialize finite difference 1D field.
        class(field_fd_1d), intent(inout)      :: this          !< The field.
        class(mesh),        intent(in), target :: fieldmesh     !< Mesh of the field.
        integer(I4P)                           :: res           !< Result (error code?).
        class(mesh_fd_1d), pointer             :: fieldmesh_cur !< Dummy pointer for mesh.
        integer(I4P)                           :: n             !< Counter.
        select type(fieldmesh)
            type is(mesh_fd_1d)
                fieldmesh_cur => fieldmesh
            class default
               STOP 'Error passing mesh'
        end select
        this%m => fieldmesh_cur
        n = fieldmesh_cur%n
        allocate(this%val(1:n))
        call random_number(this%val)
        res = 0
    end function field_fd_1d_init

    function field_fd_1d_output(this, filename) result(res)
        !< Output field data.
        class(field_fd_1d), intent(in) :: this     !< The field.
        character(len=*),   intent(in) :: filename !< Output file name.
        integer(I4P)                   :: res      !< Result (error code?).
        open(unit=11,file=filename)
        write(11,*) this%val(:)
        close(11)
        res = 0
    end function field_fd_1d_output

    function field_fd_1d_add(lhs, rhs) result(opr)
        !< Add fields.
        class(field_fd_1d), intent(in)         :: lhs      !< Left hand side.
        class(field),       intent(in), target :: rhs      !< Left hand side.
        class(field), allocatable, target      :: opr      !< Operator result.
        class(field_fd_1d), pointer            :: rhs_cur  !< Dummy pointer for rhs.
        class(field_fd_1d), pointer            :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_fd_1d),  pointer            :: mesh_cur !< Dummy pointer for mesh.
        select type(rhs)
            type is(field_fd_1d)
                rhs_cur => rhs
            class default
               STOP 'Error passing field to add'
        end select
        allocate(field_fd_1d :: opr)
        select type(opr)
            type is(field_fd_1d)
                opr_cur => opr
            class default
               STOP 'Error passing field to add'
        end select
        associate(mm => lhs%m)
            select type(mm)
                type is(mesh_fd_1d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate
        allocate(opr_cur%val(1:mesh_cur%n))
        opr_cur%m => lhs%m
        opr_cur%val = lhs%val + rhs_cur%val
    end function field_fd_1d_add

    subroutine field_fd_1d_assign(lhs, rhs)
        !< Assign fields.
        class(field_fd_1d), intent(inout)      :: lhs      !< Left hand side.
        class(field),       intent(in), target :: rhs      !< Right hand side.
        class(field_fd_1d), pointer            :: rhs_cur  !< Dummy pointer for rhs.
        class(mesh_fd_1d),  pointer            :: mesh_cur !< Dummy pointer for mesh.
        select type(rhs)
            type is(field_fd_1d)
                rhs_cur => rhs
            class default
               STOP 'Error passing field to assign'
        end select
        associate(mm => rhs%m)
            select type(mm)
                type is(mesh_fd_1d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate
        if(allocated(lhs%val)) deallocate(lhs%val)
        allocate(lhs%val(1:mesh_cur%n))
        lhs%m   => rhs_cur%m
        lhs%val = rhs_cur%val
    end subroutine field_fd_1d_assign
end module opendiff_field_fd_1d
