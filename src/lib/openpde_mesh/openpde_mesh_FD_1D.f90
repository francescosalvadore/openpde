!< Concrete class of mesh for Finite Difference 1D methods.
module openpde_mesh_FD_1D
    !< Concrete class of mesh for Finite Difference 1D methods.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use json_module
    use openpde_mesh_abstract
    use openpde_kinds
    use vtk_fortran

    implicit none
    private
    public :: associate_mesh_FD_1D, mesh_FD_1D

    type, extends(mesh) :: mesh_FD_1D
        !< Concrete class of mesh for Finite Difference 1D methods.
        integer(I_P) :: n=0      !< Number of points.
        integer(I_P) :: ng=0     !< Number of ghost points.
        integer(I_P) :: s=0      !< Number of replicas for steps/stages.
        real(R_P)    :: h=0._R_P !< Cell size.
        contains
            ! deferred public methods
            procedure, pass(this) :: init   !< Initilize mesh.
            procedure, pass(this) :: output !< Output data.
            ! public methods
            generic               :: load => load_from_json !< Load mesh definition from file.
            procedure, pass(this) :: set                    !< Set mesh.
            ! private methods
            procedure, pass(this) :: load_from_json !< Load mesh definition from jSON file.
    endtype mesh_FD_1D
contains
    ! public, non TBP
    subroutine associate_mesh_FD_1D(mesh_input, calling_procedure, mesh_pointer)
        !< Check the type of the mesh passed as input and return a Finite Difference 1D mesh pointer associated to mesh.
        class(mesh),       intent(in), target     :: mesh_input        !< Input mesh.
        character(*),      intent(in)             :: calling_procedure !< Name of the calling procedure.
        class(mesh_FD_1D), intent(inout), pointer :: mesh_pointer      !< Finite Difference 1D mesh pointer.

        select type(mesh_input)
            type is(mesh_FD_1D)
                mesh_pointer => mesh_input
            class default
               write(stderr, '(A)')' error: wrong mesh class'
               write(stderr, '(A)')' Calling procedure "'//calling_procedure//'"'
               stop
        end select
      end subroutine associate_mesh_FD_1D

    ! deferred public methods
    subroutine init(this, description, filename, error)
        !< Initialize mesh.
        class(mesh_FD_1D), intent(inout)         :: this        !< The mesh.
        character(*),      intent(in),  optional :: description !< Mesh description.
        character(*),      intent(in),  optional :: filename    !< Initialization file name.
        integer(I_P),      intent(out), optional :: error       !< Error status.

        call this%free
        if (present(description)) this%description = description
        if (present(filename)) then
            call this%load(filename=filename, error=error)
        else
            this%n = 50
            this%ng = 2
            this%s = 1
            this%h = 0.05_R8P
            if (present(error)) error = 0
        endif
    end subroutine init

    subroutine output(this, error)
        !< Output mesh.
        class(mesh_FD_1D), intent(in)            :: this  !< The mesh.
        integer(I_P),      intent(out), optional :: error !< Error status.

        if (allocated(this%description)) print "(A)", this%description
        print*, "n: ", this%n
        print*, "ng: ", this%ng
        print*, "s: ", this%s
        print*, "h: ", this%h
        if (present(error)) error = 0
    end subroutine output

    ! public methods
    pure subroutine set(this, description, n, ng, s, h, error)
        !< Set mesh.
        class(mesh_FD_1D), intent(inout)         :: this        !< The mesh.
        character(*),      intent(in),  optional :: description !< Mesh description
        integer(I_P),      intent(in),  optional :: n           !< Number of points.
        integer(I_P),      intent(in),  optional :: ng          !< Number of ghost points.
        integer(I_P),      intent(in),  optional :: s           !< Number of replicas for steps/stages.
        real(R_P),         intent(in),  optional :: h           !< Cell size.
        integer(I_P),      intent(out), optional :: error       !< Error status.

        if (present(description)) this%description = description
        if (present(n)) this%n = n
        if (present(ng)) this%ng = ng
        if (present(s)) this%s = s
        if (present(h)) this%h = h
        if (present(error)) error = 0
    end subroutine set

    ! private methods
    subroutine load_from_json(this, filename, error)
        !< Load mesh definition from JSON file.
        class(mesh_FD_1D), intent(inout)         :: this      !< The mesh.
        character(*),      intent(in)            :: filename  !< File name of JSON file.
        integer(I_P),      intent(out), optional :: error     !< Error status.
        character(len=:), allocatable            :: mesh_type !< Mesh type.
        type(json_file)                          :: json      !< JSON file handler.
        logical                                  :: found     !< Flag inquiring the result json parsing.

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
        if (mesh_type=="finite difference 1D") then
            call json%get('mesh.n', this%n, found)
            if (json%failed()) then
                call json%print_error_message(stderr) ; stop
            end if
            if (.not.found) then
                write(stderr, "(A)")' error: mesh definition of "'//filename//'" incomplete!'
                write(stderr, "(A)")'   "n" missing'
                stop
            endif
            call json%get('mesh.ng', this%ng, found)
            if (json%failed()) then
                call json%print_error_message(stderr) ; stop
            end if
            if (.not.found) then
                write(stderr, "(A)")' error: mesh definition of "'//filename//'" incomplete!'
                write(stderr, "(A)")'   "ng" missing'
                stop
            endif
            call json%get('mesh.h', this%h, found)
            if (json%failed()) then
                call json%print_error_message(stderr) ; stop
            end if
            if (.not.found) then
                write(stderr, "(A)")' error: mesh definition of "'//filename//'" incomplete!'
                write(stderr, "(A)")'   "h" missing'
                stop
            endif
        else
            write(stderr, "(A)")' error: mesh definition of "'//filename//'" is not "finite difference 1D"!'
            stop
        endif
    endsubroutine load_from_json
end module openpde_mesh_FD_1D
