!< Concrete class of mesh block for Finite Volume 1D methods.
module openpde_mesh_block_FV_1D
    !< Concrete class of mesh block for Finite Volume 1D methods.
    !<
    !< This mesh block is based on a uniform, Cartesian cell-centered discretization of the domain.
    !< The nodes-cell numeration adopts the following convention:
    !<
    !< ```
    !<                  0     1     2          n    n+1
    !< +-----+-----+... +-----+-----+-----+... +-----+-----+... +-----+
    !< |     |     |    |     |     |     |    |     +     +    +     +
    !< |-ng  |-ng+1|    |  0  |  1  |  2  |    |  n  + n+1 +    + n+ng+
    !< |     |     |    |     |     |     |    |     +     +    +     +
    !< +-----+-----+... +-----+-----+-----+... +-----+-----+... +-----+
    !<
    !< ```
    !< where
    !<
    !<+`n` is the **number of cells**
    !<+`ng` is the **number of ghost cells**
    !<
    !< The cell size is **uniform** and equals to `h`.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use json_module
    use openpde_kinds

    implicit none
    private
    public :: mesh_block_FV_1D

    type :: mesh_block_FV_1D
        !< Concrete class of mesh block for Finite Volume 1D methods.
        integer(I_P) :: n=0      !< Number of nodes.
        integer(I_P) :: ng=0     !< Number of ghost cells.
        real(R_P)    :: h=0._R_P !< Cell size.
        contains
            ! public methods
            procedure, pass(this) :: output                 !< Output data.
            generic               :: load => load_from_json !< Load block definition from file.
            procedure, pass(this) :: set                    !< Set block.
            ! private methods
            procedure, pass(this), private :: load_from_json !< Load block definition from jSON file.
    endtype mesh_block_FV_1D
contains
    ! public methods
    subroutine output(this, error)
        !< Output block.
        class(mesh_block_FV_1D), intent(in)            :: this  !< The block.
        integer(I_P),            intent(out), optional :: error !< Error status.

        print*, "n: ", this%n
        print*, "ng: ", this%ng
        print*, "h: ", this%h
        if (present(error)) error = 0
    end subroutine output

    pure subroutine set(this, n, ng, h, error)
        !< Set block.
        class(mesh_block_FV_1D), intent(inout)         :: this        !< The block.
        integer(I_P),            intent(in),  optional :: n           !< Number of cells.
        integer(I_P),            intent(in),  optional :: ng          !< Number of ghost cells.
        real(R_P),               intent(in),  optional :: h           !< Cell size.
        integer(I_P),            intent(out), optional :: error       !< Error status.

        if (present(n)) this%n = n
        if (present(ng)) this%ng = ng
        if (present(h)) this%h = h
        if (present(error)) error = 0
    end subroutine set

    ! private methods
    subroutine load_from_json(this, json, block_number, error)
        !< Load block definition from JSON file.
        class(mesh_block_FV_1D), intent(inout)         :: this         !< The block.
        type(json_file),         intent(inout)         :: json         !< JSON file handler.
        character(*),            intent(in)            :: block_number !< Block number ID, e.g. 'block5', 'block33'.
        integer(I_P),            intent(out), optional :: error        !< Error status.
        logical                                        :: found        !< Flag inquiring the result json parsing.

        call json%get('mesh.'//block_number//'.n', this%n, found)
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        if (.not.found) then
            write(stderr, "(A)")' error: mesh definition incomplete!'
            write(stderr, "(A)")'   "mesh.'//block_number//'.n" missing'
            stop
        endif
        call json%get('mesh.'//block_number//'.ng', this%ng, found)
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        if (.not.found) then
            write(stderr, "(A)")' error: mesh definition incomplete!'
            write(stderr, "(A)")'   "mesh.'//block_number//'ng" missing'
            stop
        endif
        call json%get('mesh.'//block_number//'.h', this%h, found)
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        if (.not.found) then
            write(stderr, "(A)")' error: mesh definition incomplete!'
            write(stderr, "(A)")'   "mesh.'//block_number//'h" missing'
            stop
        endif
        if (present(error)) error = 0
    endsubroutine load_from_json
end module openpde_mesh_block_FV_1D
