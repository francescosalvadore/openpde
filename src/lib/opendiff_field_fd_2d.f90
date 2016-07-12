!< Concrete class of field for Finite Difference 1D.
module opendiff_field_fd_2d
    !< Concrete class of field for Finite Difference 1D.
    use opendiff_adt_field
    use opendiff_adt_mesh
    use opendiff_kinds
    use opendiff_mesh_fd_2d

    implicit none
    private
    public :: field_fd_2d

    type, extends(field) :: field_fd_2d
        !< Finite difference 1D class for *field* handling.
        real(R_P), allocatable, dimension(:,:) :: val !< Field value.
        contains
            ! deferred methods
            procedure,            private :: add            !< Add fields.
            procedure,            private :: assign_field   !< Assign fields.
            procedure,            private :: associate_mesh !< Associate field to a mesh.
            procedure                     :: init           !< Initilize field.
            procedure                     :: output         !< Output field data.
            procedure,            private :: sub            !< Subtract fields.
            procedure,            private :: mul            !< Multiply fields.
            procedure,            private :: mulreal        !< Multiply field for real.
            procedure, pass(rhs), private :: realmul        !< Multiply real for field.
            ! public methods
            procedure :: set !< Set field.
            ! operators
    endtype field_fd_2d
contains
    function add(lhs, rhs) result(opr)
        !< Add fields.
        class(field_fd_2d), intent(in)         :: lhs      !< Left hand side.
        class(field),       intent(in), target :: rhs      !< Left hand side.
        class(field), allocatable, target      :: opr      !< Operator result.
        class(field_fd_2d), pointer            :: rhs_cur  !< Dummy pointer for rhs.
        class(field_fd_2d), pointer            :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_fd_2d),  pointer            :: mesh_cur !< Dummy pointer for mesh.
        integer                                :: nx, ny, ngx, ngy

        select type(rhs)
            type is(field_fd_2d)
                rhs_cur => rhs
            class default
               STOP 'Error passing field to add'
        end select
        allocate(field_fd_2d :: opr)
        select type(opr)
            type is(field_fd_2d)
                opr_cur => opr
            class default
               STOP 'Error passing field to add'
        end select
        associate(mm => lhs%m)
            select type(mm)
                type is(mesh_fd_2d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate

        nx = mesh_cur%nx
        ny = mesh_cur%ny
        ngx = mesh_cur%ngx
        ngy = mesh_cur%ngy
        allocate(opr_cur%val(1-ngx:nx+ngx, 1-ngy: ny+ngy))
        opr_cur%m => lhs%m
        opr_cur%val = lhs%val + rhs_cur%val
    end function add

    subroutine assign_field(lhs, rhs)
        !< Assign fields.
        class(field_fd_2d), intent(inout)      :: lhs      !< Left hand side.
        class(field),       intent(in), target :: rhs      !< Right hand side.
        class(field_fd_2d), pointer            :: rhs_cur  !< Dummy pointer for rhs.
        class(mesh_fd_2d),  pointer            :: mesh_cur !< Dummy pointer for mesh.
        integer                                :: nx, ny, ngx, ngy
        select type(rhs)
            type is(field_fd_2d)
                rhs_cur => rhs
            class default
               STOP 'Error passing field to assign'
        end select
        associate(mm => rhs%m)
            select type(mm)
                type is(mesh_fd_2d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate
        if(allocated(lhs%val)) deallocate(lhs%val)
        nx = mesh_cur%nx
        ny = mesh_cur%ny
        ngx = mesh_cur%ngx
        ngy = mesh_cur%ngy
        allocate(lhs%val(1-ngx:nx+ngx,1-ngy:ny+ngy))
        lhs%m   => rhs_cur%m
        lhs%val = rhs_cur%val
    end subroutine assign_field

    subroutine associate_mesh(this, fieldmesh, error)
        !< Associate field to a mesh.
        class(field_fd_2d), intent(inout)         :: this          !< The field.
        class(mesh),        intent(in), target    :: fieldmesh     !< Mesh of the field.
        integer(I_P),       intent(out), optional :: error         !< Error status.
        class(mesh_fd_2d), pointer                :: fieldmesh_cur !< Dummy pointer for mesh.
        integer                                   :: nx, ny, ngx, ngy
        select type(fieldmesh)
            type is(mesh_fd_2d)
                fieldmesh_cur => fieldmesh
            class default
               STOP 'Error passing mesh'
        end select
        this%m => fieldmesh_cur
        nx = fieldmesh_cur%nx
        ny = fieldmesh_cur%ny
        ngx = fieldmesh_cur%ngx
        ngy = fieldmesh_cur%ngy
        if (allocated(this%val)) deallocate(this%val) ; allocate(this%val(1-ngx:nx+ngx, 1-ngy:ny+ngy))
        if (present(error)) error = 0
    end subroutine associate_mesh

    elemental subroutine free(this)
        !< Free dynamic memory.
        class(field_fd_2d), intent(inout) :: this !< The mesh.
        if (allocated(this%description)) deallocate(this%description)
        if (allocated(this%val)) deallocate(this%val)
        ! if (associated(this%m)) deallocate(this%m) ; this%m => null()
    end subroutine free

    subroutine init(this, fieldmesh, description, error)
        !< Initialize finite difference 1D field.
        class(field_fd_2d), intent(inout)         :: this          !< The field.
        class(mesh),        intent(in), target    :: fieldmesh     !< Mesh of the field.
        character(*),       intent(in),  optional :: description   !< Mesh description
        integer(I4P)                              :: i, j, nx, ny, ngx, ngy
        class(mesh_fd_2d),  pointer               :: mesh_cur !< Dummy pointer for mesh.
        integer(I_P),       intent(out), optional :: error         !< Error status.
        call this%free
        call this%associate_mesh(fieldmesh=fieldmesh, error=error)
        if (present(description)) this%description = description

!        call random_number(this%val)

        select type(fieldmesh)
            type is(mesh_fd_2d)
                mesh_cur => fieldmesh
            class default
                STOP 'Error setting mesh'
        end select
        nx = mesh_cur%nx
        ny = mesh_cur%ny
        ngx = mesh_cur%ngx
        ngy = mesh_cur%ngy
        print *,"number of points of mesh in init n,ng: ",nx,ny, ngx,ngy
        do j = 1-ngy,ny+ngy
            do i = 1-ngx,nx+ngx
                this%val(i,j) = sin(i*2.*acos(-1.)/nx)*sin(j*2.*acos(-1.)/ny)
            enddo
        enddo

        if (present(error)) error = 0
    end subroutine init

    function mul(lhs, rhs) result(opr)
        !< Multiply fields.
        class(field_fd_2d), intent(in)         :: lhs      !< Left hand side.
        class(field),       intent(in), target :: rhs      !< Left hand side.
        class(field), allocatable, target      :: opr      !< Operator result.
        class(field_fd_2d), pointer            :: rhs_cur  !< Dummy pointer for rhs.
        class(field_fd_2d), pointer            :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_fd_2d),  pointer            :: mesh_cur !< Dummy pointer for mesh.
        integer                                :: nx, ngx, ny, ngy
        select type(rhs)
            type is(field_fd_2d)
                rhs_cur => rhs
            class default
               STOP 'Error passing field to add'
        end select
        allocate(field_fd_2d :: opr)
        select type(opr)
            type is(field_fd_2d)
                opr_cur => opr
            class default
               STOP 'Error passing field to add'
        end select
        associate(mm => lhs%m)
            select type(mm)
                type is(mesh_fd_2d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate
        nx = mesh_cur%nx
        ny = mesh_cur%ny
        ngx = mesh_cur%ngx
        ngy = mesh_cur%ngy
        allocate(opr_cur%val(1-ngx:nx+ngx, 1-ngy:ny+ngy))
        opr_cur%m => lhs%m
        opr_cur%val = lhs%val * rhs_cur%val
    end function mul

    function mulreal(lhs, rhs) result(opr)
        !< Multiply field for real.
        class(field_fd_2d), intent(in)    :: lhs      !< Left hand side.
        real(R_P),          intent(in)    :: rhs      !< Right hand side.
        class(field), allocatable, target :: opr      !< Operator result.
        class(field_fd_2d), pointer       :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_fd_2d),  pointer       :: mesh_cur !< Dummy pointer for mesh.
        integer                           :: nx, ny, ngx, ngy

        allocate(field_fd_2d :: opr)
        select type(opr)
            type is(field_fd_2d)
                opr_cur => opr
            class default
               STOP 'Error passing field to add'
        end select
        associate(mm => lhs%m)
            select type(mm)
                type is(mesh_fd_2d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate
        nx = mesh_cur%nx
        ny = mesh_cur%ny
        ngx = mesh_cur%ngx
        ngy = mesh_cur%ngy
        allocate(opr_cur%val(1-ngx:nx+ngx, 1-ngy: ny+ngy))
        opr_cur%m => lhs%m
        opr_cur%val = lhs%val * rhs
    end function mulreal

    subroutine output(this, filename, error)
        !< Output field data.
        class(field_fd_2d), intent(in)            :: this     !< The field.
        character(len=*),   intent(in)            :: filename !< Output file name.
        integer(I4P)                              :: imin,imax,i, j, jmin, jmax
        integer(I_P),       intent(out), optional :: error    !< Error status.
        imin = lbound(this%val,1)
        imax = ubound(this%val,1)
        jmin = lbound(this%val,2)
        jmax = ubound(this%val,2)
        open(unit=11,file=filename)
        do j=jmin,jmax
        do i=imin,imax
            write(11,*) this%val(i,j)
        enddo
        enddo
        close(11)
        if (present(error)) error = 0
    end subroutine output

    function realmul(lhs, rhs) result(opr)
        !< Multiply real for field.
        real(R_P),          intent(in)    :: lhs      !< Left hand side.
        class(field_fd_2d), intent(in)    :: rhs      !< Right hand side.
        class(field), allocatable, target :: opr      !< Operator result.
        class(field_fd_2d), pointer       :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_fd_2d),  pointer       :: mesh_cur !< Dummy pointer for mesh.
        integer                           :: nx, ny, ngx, ngy

        allocate(field_fd_2d :: opr)
        select type(opr)
            type is(field_fd_2d)
                opr_cur => opr
            class default
               STOP 'Error passing field to add'
        end select
        associate(mm => rhs%m)
            select type(mm)
                type is(mesh_fd_2d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate
        nx = mesh_cur%nx
        ny = mesh_cur%ny
        ngx = mesh_cur%ngx
        ngy = mesh_cur%ngy
        allocate(opr_cur%val(1-ngx:nx+ngx, 1-ngy: ny+ngy))
        opr_cur%m => rhs%m
        opr_cur%val = lhs * rhs%val
    end function realmul

     subroutine set(this, fieldmesh, description, val, error)
        !< Set mesh.
        class(field_fd_2d), intent(inout)                 :: this        !< The field.
        class(mesh),        intent(in),  optional, target :: fieldmesh   !< Mesh of the field.
        character(*),       intent(in),  optional         :: description !< Mesh description
        real(R_P),          intent(in),  optional         :: val(1:,1:)     !< Field value.
        integer(I_P),       intent(out), optional         :: error       !< Error status.
        if (present(fieldmesh)) call this%associate_mesh(fieldmesh=fieldmesh, error=error)
        if (present(description)) this%description = description
        if (present(val)) this%val = val ! TO BE FIXED SINCE THERE ARE GHOST NODES
        if (present(error)) error = 0
    end subroutine set

    function sub(lhs, rhs) result(opr)
        !< Subtract fields.
        class(field_fd_2d), intent(in)         :: lhs      !< Left hand side.
        class(field),       intent(in), target :: rhs      !< Left hand side.
        class(field), allocatable, target      :: opr      !< Operator result.
        class(field_fd_2d), pointer            :: rhs_cur  !< Dummy pointer for rhs.
        class(field_fd_2d), pointer            :: opr_cur  !< Dummy pointer for operator result.
        class(mesh_fd_2d),  pointer            :: mesh_cur !< Dummy pointer for mesh.
        integer                                :: nx, ny, ngx, ngy

        select type(rhs)
            type is(field_fd_2d)
                rhs_cur => rhs
            class default
               STOP 'Error passing field to add'
        end select
        allocate(field_fd_2d :: opr)
        select type(opr)
            type is(field_fd_2d)
                opr_cur => opr
            class default
               STOP 'Error passing field to add'
        end select
        associate(mm => lhs%m)
            select type(mm)
                type is(mesh_fd_2d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate
        nx = mesh_cur%nx
        ny = mesh_cur%ny
        ngx = mesh_cur%ngx
        ngy = mesh_cur%ngy
        allocate(opr_cur%val(1-ngx:nx+ngx, 1-ngy:ny+ngy))
        opr_cur%m => lhs%m
        opr_cur%val = lhs%val - rhs_cur%val
    end function sub
end module opendiff_field_fd_2d
