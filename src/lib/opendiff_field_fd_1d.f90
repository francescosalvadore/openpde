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
            ! deferred methods
            procedure, private :: add            !< Add fields.
            procedure, private :: assign_field   !< Assign fields.
            procedure, private :: associate_mesh !< Associate field to a mesh.
            procedure          :: init           !< Initilize field.
            procedure          :: output         !< Output field data.
            ! public methods
            procedure :: set !< Set field.
            ! operators
    endtype field_fd_1d
contains
    function add(lhs, rhs) result(opr)
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
    end function add

    subroutine assign_field(lhs, rhs)
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
    end subroutine assign_field

    subroutine associate_mesh(this, fieldmesh, error)
        !< Associate field to a mesh.
        class(field_fd_1d), intent(inout)         :: this          !< The field.
        class(mesh),        intent(in), target    :: fieldmesh     !< Mesh of the field.
        integer(I4P),       intent(out), optional :: error         !< Error status.
        class(mesh_fd_1d), pointer                :: fieldmesh_cur !< Dummy pointer for mesh.
        select type(fieldmesh)
            type is(mesh_fd_1d)
                fieldmesh_cur => fieldmesh
            class default
               STOP 'Error passing mesh'
        end select
        this%m => fieldmesh_cur
        if (allocated(this%val)) deallocate(this%val) ; allocate(this%val(1:fieldmesh_cur%n))
        if (present(error)) error = 0
    end subroutine associate_mesh

    elemental subroutine free(this)
        !< Free dynamic memory.
        class(field_fd_1d), intent(inout) :: this !< The mesh.
        if (allocated(this%description)) deallocate(this%description)
        if (allocated(this%val)) deallocate(this%val)
        ! if (associated(this%m)) deallocate(this%m) ; this%m => null()
    end subroutine free

    subroutine init(this, fieldmesh, description, error)
        !< Initialize finite difference 1D field.
        class(field_fd_1d), intent(inout)         :: this          !< The field.
        class(mesh),        intent(in), target    :: fieldmesh     !< Mesh of the field.
        character(*),       intent(in),  optional :: description   !< Mesh description
        integer(I4P),       intent(out), optional :: error         !< Error status.
        call this%free
        call this%associate_mesh(fieldmesh=fieldmesh, error=error)
        if (present(description)) this%description = description
        call random_number(this%val)
        if (present(error)) error = 0
    end subroutine init

    subroutine output(this, filename, error)
        !< Output field data.
        class(field_fd_1d), intent(in)            :: this     !< The field.
        character(len=*),   intent(in)            :: filename !< Output file name.
        integer(I4P),       intent(out), optional :: error    !< Error status.
        open(unit=11,file=filename)
        write(11,*) this%val(:)
        close(11)
        if (present(error)) error = 0
    end subroutine output

     subroutine set(this, fieldmesh, description, val, error)
        !< Set mesh.
        class(field_fd_1d), intent(inout)                 :: this        !< The field.
        class(mesh),        intent(in),  optional, target :: fieldmesh   !< Mesh of the field.
        character(*),       intent(in),  optional         :: description !< Mesh description
        real(R8P),          intent(in),  optional         :: val(1:)     !< Field value.
        integer(I4P),       intent(out), optional         :: error       !< Error status.
        if (present(fieldmesh)) call this%associate_mesh(fieldmesh=fieldmesh, error=error)
        if (present(description)) this%description = description
        if (present(val)) this%val = val
        if (present(error)) error = 0
    end subroutine set
end module opendiff_field_fd_1d
