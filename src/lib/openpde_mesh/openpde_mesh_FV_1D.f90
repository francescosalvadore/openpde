!< Concrete class of mesh for Finite Volume 1D methods.
module openpde_mesh_FV_1D
    !< Concrete class of mesh for Finite Volume 1D methods.
    !<
    !< This mesh is based on a uniform, Cartesian cell-centered discretization of the domain.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use json_module
    use openpde_mesh_abstract
    use openpde_mesh_block_FV_1D
    use openpde_kinds
    use stringifor
    use vtk_fortran

    implicit none
    private
    public :: associate_mesh_FV_1D, mesh_FV_1D

    type, extends(mesh) :: mesh_FV_1D
        !< Concrete class of mesh for Finite Volume 1D methods.
        integer(I_P)                        :: nb        !< Number of blocks.
        type(mesh_block_FV_1D), allocatable :: blocks(:) !< The blocks.
        contains
            ! deferred public methods
            procedure, pass(this) :: init   !< Initilize mesh.
            procedure, pass(this) :: output !< Output data.
            ! public methods
            generic               :: load => load_from_json !< Load mesh definition from file.
            procedure, pass(this) :: set                    !< Set mesh.
            ! private methods
            procedure, pass(this), private :: load_from_json !< Load mesh definition from jSON file.
    endtype mesh_FV_1D
contains
    ! public, non TBP
    function associate_mesh_FV_1D(mesh_input, emsg) result(mesh_pointer)
        !< Check the type of the mesh passed as input and return a Finite Difference 1D mesh pointer associated to mesh.
        class(mesh),       intent(in), target   :: mesh_input    !< Input mesh.
        character(*),      intent(in), optional :: emsg          !< Auxiliary error message.
        class(mesh_FV_1D), pointer              :: mesh_pointer  !< Finite Difference 1D mesh pointer.

        select type(mesh_input)
            type is(mesh_FV_1D)
                mesh_pointer => mesh_input
            class default
               write(stderr, '(A)')'error: cast mesh to mesh_FV_1D'
               if (present(emsg)) write(stderr, '(A)') emsg
               stop
        end select
      end function associate_mesh_FV_1D

    ! deferred public methods
    subroutine init(this, description, filename, error)
        !< Initialize mesh.
        class(mesh_FV_1D), intent(inout)         :: this        !< The mesh.
        character(*),      intent(in),  optional :: description !< Mesh description.
        character(*),      intent(in),  optional :: filename    !< Initialization file name.
        integer(I_P),      intent(out), optional :: error       !< Error status.

        call this%free
        if (present(description)) this%description = description
        if (present(filename)) then
            call this%load(filename=filename, error=error)
        else
            this%nb = 1
            allocate(this%blocks(1))
            call this%blocks(1)%set(n=50, ng=2, h=0.05_R_P, error=error)
        endif
        call this%output(error=error)
    end subroutine init

    subroutine output(this, error)
        !< Output mesh.
        class(mesh_FV_1D), intent(in)            :: this  !< The mesh.
        integer(I_P),      intent(out), optional :: error !< Error status.
        integer(I_P)                             :: b     !< Counter.

        if (allocated(this%description)) print "(A)", this%description
        if (allocated(this%blocks)) then
            print*, 'nb: ', this%nb
            do b=lbound(this%blocks, dim=1), ubound(this%blocks, dim=1)
                print*, 'block ', b
                call this%blocks(b)%output(error=error)
            end do
        end if
    end subroutine output

    ! public methods
    pure subroutine set(this, description, nb, n, ng, h, error)
        !< Set mesh.
        class(mesh_FV_1D), intent(inout)         :: this        !< The mesh.
        character(*),      intent(in),  optional :: description !< Mesh description
        integer(I_P),      intent(in),  optional :: nb          !< Number of blocks.
        integer(I_P),      intent(in),  optional :: n(1:)       !< Number of points for each block.
        integer(I_P),      intent(in),  optional :: ng(1:)      !< Number of ghost points for each block.
        real(R_P),         intent(in),  optional :: h(1:)       !< Cell size for each block.
        integer(I_P),      intent(out), optional :: error       !< Error status.
        integer(I_P)                             :: b           !< Counter.

        if (present(description)) this%description = description
        if (present(nb)) then
            this%nb = nb
            if (allocated(this%blocks)) then
                if (nb/=size(this%blocks, dim=1)) then
                    deallocate(this%blocks)
                    allocate(this%blocks(1:nb))
                endif
            endif
        endif
        if (allocated(this%blocks)) then
            do b=lbound(this%blocks, dim=1), ubound(this%blocks, dim=1)
                call this%blocks(b)%set(n=n(b), ng=ng(b), h=h(b), error=error)
            end do
        end if
    end subroutine set

    ! private methods
    subroutine load_from_json(this, filename, error)
        !< Load mesh definition from JSON file.
        class(mesh_FV_1D), intent(inout)         :: this         !< The mesh.
        character(*),      intent(in)            :: filename     !< File name of JSON file.
        integer(I_P),      intent(out), optional :: error        !< Error status.
        character(len=:), allocatable            :: mesh_type    !< Mesh type.
        type(string)                             :: block_number !< Block number ID, e.g. 'block5', 'block33'.
        type(json_file)                          :: json         !< JSON file handler.
        logical                                  :: found        !< Flag inquiring the result json parsing.
        integer(I_P)                             :: b            !< Counter.

        call json%initialize()
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        call json%load_file(filename=filename)
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        call json%get('mesh.type', mesh_type, found)
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        if (.not.found) then
            write(stderr, "(A)")' error: mesh definition of "'//filename//'" incomplete!'
            write(stderr, "(A)")'   "type" missing'
            stop
        endif
        if (mesh_type=="finite volume 1D") then
            call json%get('mesh.nb', this%nb, found)
            if (json%failed()) then
                call json%print_error_message(stderr) ; stop
            end if
            if (.not.found) then
                write(stderr, "(A)")' error: mesh definition of "'//filename//'" incomplete!'
                write(stderr, "(A)")'   "n" missing'
                stop
            endif
            if (allocated(this%blocks)) deallocate(this%blocks) ; allocate(this%blocks(1:this%nb))
            do b=lbound(this%blocks, dim=1), ubound(this%blocks, dim=1)
                block_number = b
                block_number = block_number%replace(old='+', new='')
                call this%blocks(b)%load(json=json, block_number='block'//block_number, error=error)
            end do
        else
            write(stderr, "(A)")' error: mesh definition of "'//filename//'" is not "finite volume 1D"!'
            stop
        endif
    endsubroutine load_from_json

    ! overridden public methods
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(mesh_FV_1D), intent(inout) :: this !< The mesh.
        if (allocated(this%description)) deallocate(this%description)
        if (allocated(this%blocks)) deallocate(this%blocks)
    end subroutine free
end module openpde_mesh_FV_1D
