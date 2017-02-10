!< Openpde test: scalar simple equation for Finite Difference 1D methods.
module flame_1d_FD_1D_m
    !< Scalar simple equation definition for Finite Difference 1D methods.
    !< The PDE solved is
    !<
    !< $$ \frac{\partial u}{\partial t} + a \frac{\partial u}{\partial x} + b \frac{\partial^2 u}{\partial x^2} = 0 $$
    !< where `a` and `b` are two constant scalars.
    !<
    !< This is a one dimensional, unsteady, linear PDE. It is solved adopting a finite difference approximations of
    !< first and second derivatives computed on a uniform mesh.
    !<
    !< The spatial operators are approximated as:
    !<
    !< $$ \frac{\partial u}{\partial x}|_i = \frac{u_{i+1}-u_i}{h} \quad i=1,N $$
    !< $$ \frac{\partial^2 u}{\partial x^2}|_i = \frac{u_{i+1}-2u_i+u_{i-1}}{h^2} \quad i=1,N $$
    !< where `N` is the number of discrete points and `h=L/N` is the (uniform) grid resolution, `L` is the domain length.
    !<
    !< The explicit Euler's method is used for advancing on time.

    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use json_module
    use openpde

    implicit none
    private
    public :: flame_1d_equation_adv

    type, extends(equation_adv) :: flame_1d_equation_adv
        !< Scalar simple equations for Finite Difference 1D methods.
        !<
        !< @note The reason the `du_opr` and `ddu_opr` are pointers is just to make them a pointee when pointed
        !< inside [[equation:forcing]] function.
        real(R_P)                           :: a=0._R_P     !< `a` equation coefficient.
        real(R_P)                           :: b=0._R_P     !< `b` equation coefficient.
        class(spatial_operator_d1), pointer :: du_opr       !< First derivative.
        class(spatial_operator_d2), pointer :: ddu_opr      !< Second derivative.
        class(field), allocatable           :: du           !< Dummy storage of the first derivative operator result.
        class(field), allocatable           :: ddu          !< Dummy storage of the second derivative operator result.

        class(f2m_d2), pointer :: ddui_opr                   !< First derivative implicit.
        class(f2m_d1), pointer :: dui_opr                   !< First derivative implicit.
        class(matrix), allocatable :: dui
        class(matrix), allocatable :: ddui

        class(field), allocatable :: tem, uuu, yya, wwx
        class(field), allocatable :: te1, te2, te3, te4, te5, te6
        real(R_P) :: pra, gam, lew, dha, dam, zel, pre, xfn, llx, xan, xfo, vfr
 
        contains
            ! deferred public methods
            procedure, pass(this) :: bc_e    !< Equation boundary conditions.
            procedure, pass(this) :: bc_i    !< Equation boundary conditions.
            procedure, pass(this) :: resid_e !< Forcing equation explicit.
            procedure, pass(this) :: resid_i !< Forcing equation implicit.
            procedure, pass(this) :: init    !< Initialize equation.
            ! public methods
            generic :: load => load_from_json !< Load equation definition from file.
            ! private methods
            procedure, pass(this), private :: load_from_json !< Load equation definition from jSON file.
    endtype flame_1d_equation_adv
contains
    ! deferred public methods
    subroutine init(this, n_equ, field_mesh, inp, description, filename, error)
        !< Initialize equation.
        class(flame_1d_equation_adv), intent(inout)       :: this        !< The equation.
        integer(I_P), intent(in)                          :: n_equ       !< Number of equations
        class(mesh),  intent(in), target                  :: field_mesh  !< Mesh of the field.
        class(field), intent(inout), target, dimension(:) :: inp         !< Input field.
        character(*), intent(in),  optional               :: description !< Equation description.
        character(*), intent(in),  optional               :: filename    !< Initialization file name.
        integer(I_P), intent(out), optional               :: error       !< Error status.
        integer(I_P)                                      :: ie          !< Equation index
        integer(I_P)                                      :: n
        integer(I_P)                                      :: ng
        integer(I_P)                                      :: i
        real(R_P)                                         :: pos_x
        class(mesh_FD_1D), pointer                        :: mesh_cur
        class(field_FD_1D), pointer                       :: tem_cur
        class(field_FD_1D), pointer                       :: yya_cur
        class(field_FD_1D), pointer                       :: te1_cur
        class(field_FD_1D), pointer                       :: te2_cur
        class(field_FD_1D), pointer                       :: rhu_cur
        class(field_FD_1D), pointer                       :: rho_cur

        mesh_cur => associate_mesh_FD_1D(mesh_input=field_mesh)
        n = mesh_cur%n
        ng = mesh_cur%ng

        this%n_equ = n_equ
        this%n_size = n

        allocate(field_FD_1D :: this%tem)
        allocate(field_FD_1D :: this%uuu)
        allocate(field_FD_1D :: this%yya)
        allocate(field_FD_1D :: this%wwx)
        allocate(field_FD_1D :: this%te1)
        allocate(field_FD_1D :: this%te2)
        allocate(field_FD_1D :: this%te3)
        allocate(field_FD_1D :: this%te4)
        allocate(field_FD_1D :: this%te5)
        allocate(field_FD_1D :: this%te6)

        associate( rho => inp(1), rhu => inp(2), rhe => inp(3), rya => inp(4), &
            tem => this%tem, uuu => this%uuu, yya => this%yya, wwx => this%wwx, &
            te1 => this%te1, te2 => this%te2, te3 => this%te3, te4 => this%te4, te5 => this%te5, te6 => this%te6, &
            pra => this%pra, gam => this%gam, lew => this%lew, dha => this%dha, dam => this%dam, zel => this%zel, &
            pre => this%pre, xfn => this%xfn, xan => this%xan, xfo => this%xfo, vfr => this%vfr, llx => this%llx)

            call tem%init(field_mesh=field_mesh)
            call uuu%init(field_mesh=field_mesh)
            call yya%init(field_mesh=field_mesh)
            call wwx%init(field_mesh=field_mesh)
            call te1%init(field_mesh=field_mesh)
            call te2%init(field_mesh=field_mesh)
            call te3%init(field_mesh=field_mesh)
            call te4%init(field_mesh=field_mesh)
            call te5%init(field_mesh=field_mesh)
            call te6%init(field_mesh=field_mesh)

            this%pra = 0.74_R_P
            this%gam = 1.32
            this%lew = 1._R_P
            this%dha = 10000._R_P
            this%dam = 80000._R_P
            this%zel = 1060._R_P
            this%pre = 10000._R_P
            this%xfn = 4.8_R_P
            this%xfo = this%xfn
            this%xan = this%xfn
            this%vfr = 0._R_P
            this%llx = 12._R_P

            tem_cur => associate_field_FD_1D(field_input=tem, emsg='tem_cur')
            yya_cur => associate_field_FD_1D(field_input=yya, emsg='yya_cur')
            te1_cur => associate_field_FD_1D(field_input=te1, emsg='te1_cur')
            te2_cur => associate_field_FD_1D(field_input=te2, emsg='te2_cur')
            rhu_cur => associate_field_FD_1D(field_input=rhu, emsg='rhu_cur')
            rho_cur => associate_field_FD_1D(field_input=rho, emsg='rho_cur')

            do i=1-ng,n+ng
                pos_x = (i-1)*12._R_P/256
                tem_cur%val(i) = 86.3 + 253.7 * (-atan(-500.)+atan(50.*(pos_x-0.4*llx)))
                te1_cur%val(i) = 1._R_P/tem_cur%val(i)
                te2_cur%val(i) = tem_cur%val(i)-1.0_R_P
                yya_cur%val(i) = 0.054861 *(1._R_P-0.5_R_P*(2._R_P/acos(-1._R_P)*atan(pos_x-0.4_R_P*llx)+1._R_P))
            enddo

            rho = gam/(gam-1._R_P)*pre*te1_cur
            rhe = rho*(1._R_P/gam*te2_cur+yya*dha)
            rya = rho*yya

            !do i=1-ng,n+ng
            !    rhu_cur%val(i) = 0.026*rho_cur%val(1)
            !enddo

            ! CORRECT IN PRINCIPLE
            !rho = gam/(gam-1._R_P)*pre/tem
            !rhu = 0._R_P
            !rhe = rho*(1._R_P/gam*(tem-1._R_P)+yya*dha+0.5_R_P*(rhu/rho)**2)
            !rya = rho*yya

        endassociate

        this%enable_explicit = .true.
        this%enable_implicit = .false.

        ! explicit section
        allocate(field_FD_1D :: this%resvar_e(n_equ))
        do ie=1,n_equ
            call this%resvar_e(ie)%init(field_mesh=field_mesh)
        enddo

        allocate(this%du, mold=inp(1))
        allocate(this%ddu, mold=inp(1))
        call this%du%init(field_mesh=field_mesh)
        call this%ddu%init(field_mesh=field_mesh)

        if (present(filename)) then
            call this%load(filename=filename, error=error)
        else
            this%a = -1.0_R_P
            this%b = -0.1_R_P
            allocate(spatial_operator_d1_FD_1D :: this%du_opr)
            allocate(spatial_operator_d2_FD_1D :: this%ddu_opr)
            if (present(error)) error = 0
        endif

        ! implicit section     
        allocate(linsolver_gmlapack :: this%solver)
        call this%solver%init(n) !TODO should be generalized on 50

        allocate(f2m_d1_FD_1D  :: this%dui_opr)
        allocate(this%dui_opr%mat, mold=this%solver%mat) !TODO should be put in init()

        allocate(f2m_d2_FD_1D  :: this%ddui_opr)
        allocate(this%ddui_opr%mat, mold=this%solver%mat) !TODO should be put in init()

        allocate(this%dui, mold=this%solver%mat)
        allocate(this%ddui, mold=this%solver%mat)

        allocate(this%resvar_i, mold=this%solver%mat)
        call this%resvar_i%init(n) 

        allocate(f2v_FD_1D :: this%f2v_opr)
        allocate(this%f2v_opr%vec, mold=this%solver%vec) ! TODO should be put in init()

        allocate(v2f_FD_1D :: this%v2f_opr)
        this%v2f_opr%mesh => inp(1)%m
        this%v2f_opr%n_equ = n_equ

    end subroutine init

    subroutine bc_e(this, inp, t)
        !< Equation boundary or fixed conditions imposition.
        class(flame_1d_equation_adv), intent(in)            :: this     !< The equation.
        class(field),                  intent(inout), target, dimension(:) :: inp      !< Field.
        real(R_P),                     intent(in)            :: t        !< Time.
        class(field_FD_1D), pointer                          :: inp_cur  !< Pointer to input field.
        class(mesh_FD_1D), pointer                           :: mesh_cur !< Pointer to input mehs.
        integer(I_P)                                         :: i        !< Counter.
        integer(I_P)                                         :: ie        !< Counter.

        do ie=1,size(inp)
            inp_cur => associate_field_FD_1D(field_input=inp(ie), emsg='bc_e inp_cur')
            mesh_cur => associate_mesh_FD_1D(mesh_input=inp(ie)%m, emsg='bc_e mesh_cur')
            do i=1-mesh_cur%ng,0
                inp_cur%val(i) = inp_cur%val(1)
            enddo
            do i=mesh_cur%n+1, mesh_cur%n+mesh_cur%ng
                inp_cur%val(i) = inp_cur%val(mesh_cur%n)
            enddo
        enddo
    end subroutine bc_e

    subroutine bc_i(this, matA, vecB, t)
        !< Equation boundary conditions imposition.
        class(flame_1d_equation_adv), intent(in)            :: this     !< The equation.
        class(matrix),    intent(inout), target :: matA  !< Input field.
        class(vector),    intent(inout), target :: vecB  !< Input field.
        real(R_P),                     intent(in)            :: t        !< Time.
        class(field_FD_1D), pointer                          :: inp_cur  !< Pointer to input field.
        class(mesh_FD_1D), pointer                           :: mesh_cur !< Pointer to input mehs.
        integer(I_P)                                         :: i        !< Counter.
        integer(I_P)                                         :: ie        !< Counter.
        !class(vector_simple), pointer  :: vecB_cur
        !class(matrix_simple), pointer  :: matA_cur
        integer(I_P) :: n, j 

        n = matA%n
        !matA_cur => associate_matrix_simple(matrix_input=matA)
        !vecB_cur => associate_vector_simple(vector_input=vecB)
        call vecB%set(1,0._R_P)
        call vecB%set(vecB%n,0._R_P)
        do i=1,n,n-1
            do j=1,n
                call matA%set(1, j, 0._R_P)
            enddo
        enddo
        call matA%set(1, 1, 1._R_P)
        call matA%set(n, n, 1._R_P)
        call matA%output("matAbis.dat")
        call vecB%output("vecBbis.dat")
    end subroutine bc_i

    subroutine resid_e(this, inp, t)
        !< Return the field after forcing the equation.
        class(flame_1d_equation_adv), intent(inout)         :: this    !< The equation.
        class(field),                  intent(in), target, dimension(:) :: inp     !< Input field.
        real(R_P),                     intent(in)         :: t       !< Time.

        class(field_FD_1D), pointer                          :: tem_cur  !< Pointer to input field.
        class(field_FD_1D), pointer                          :: wwx_cur  !< Pointer to input field.
        class(field_FD_1D), pointer                          :: rya_cur  !< Pointer to input field.
        class(field_FD_1D), pointer                          :: rho_cur  !< Pointer to input field.
        class(field_FD_1D), pointer                          :: rhu_cur  !< Pointer to input field.
        class(field_FD_1D), pointer                          :: rhe_cur  !< Pointer to input field.
        class(field_FD_1D), pointer                          :: r1_cur   !< Pointer to input field.
        class(field_FD_1D), pointer                          :: r2_cur   !< Pointer to input field.
        class(field_FD_1D), pointer                          :: r3_cur   !< Pointer to input field.
        class(field_FD_1D), pointer                          :: r4_cur   !< Pointer to input field.
        class(field_FD_1D), pointer                          :: uuu_cur  !< Pointer to input field.
        class(field_FD_1D), pointer                          :: yya_cur  !< Pointer to input field.
        real(R_P)                                            :: csp, dya, duu, dee, dth, dr1, dr2
        class(mesh_FD_1D), pointer                           :: mesh_cur
        integer(I_P)                                         :: n, ng

        mesh_cur => associate_mesh_FD_1D(mesh_input=inp(1)%m, emsg='resid_e mesh_cur')
        n        = mesh_cur%n
        ng       = mesh_cur%ng

        associate(ddx => this%du_opr, dsx => this%ddu_opr, &
            rho => inp(1), rhu => inp(2), rhe => inp(3), rya => inp(4), &
            te1 => this%te1, te2 => this%te2, te3 => this%te3, te4 => this%te4, te5 => this%te5, te6 => this%te6, &
            uuu => this%uuu, gam => this%gam, tem => this%tem, pra => this%pra, dam => this%dam, &
            yya => this%yya, dha => this%dha, wwx => this%wwx, lew => this%lew, zel => this%zel, &
            r1 => this%resvar_e(1), r2 => this%resvar_e(2), r3 => this%resvar_e(3), r4 => this%resvar_e(4))

                tem_cur => associate_field_FD_1D(field_input=tem, emsg='tem_cur')
                wwx_cur => associate_field_FD_1D(field_input=wwx, emsg='wwx_cur')
                rya_cur => associate_field_FD_1D(field_input=rya, emsg='rya_cur')
                rho_cur => associate_field_FD_1D(field_input=rho, emsg='rho_cur')
                rhu_cur => associate_field_FD_1D(field_input=rhu, emsg='rhu_cur')
                rhe_cur => associate_field_FD_1D(field_input=rhe, emsg='rhe_cur')
                rya_cur => associate_field_FD_1D(field_input=rya, emsg='rya_cur')
                r1_cur  => associate_field_FD_1D(field_input=r1,  emsg='r1_cur ')
                r2_cur  => associate_field_FD_1D(field_input=r2,  emsg='r2_cur ')
                r3_cur  => associate_field_FD_1D(field_input=r3,  emsg='r3_cur ')
                r4_cur  => associate_field_FD_1D(field_input=r4,  emsg='r4_cur ')
                uuu_cur => associate_field_FD_1D(field_input=uuu, emsg='uuu_cur')
                yya_cur => associate_field_FD_1D(field_input=yya, emsg='yya_cur')
 
                uuu = rhu/rho
                te1 = rhe-rya*dha
                te1 = te1/rho
                te1 = te1-0.5_R_P*uuu*uuu
                tem = gam*te1
                tem_cur%val(:) = tem_cur%val(:) + 1.0_R_P
                yya = rya/rho
                wwx_cur%val(:) = dam*rya_cur%val(:)*exp(-zel/tem_cur%val(:)*gam/(gam-1._R_P))

                !tem = gam*((rhe-rya*dha)/rho-0.5_R_P*uuu*uuu) + 1.0_R_P
                !wwx = dam*rya*exp(-zel/tem*gam/(gam-1._R_P))

                !r1 = -ddx%operate(rhu)
                !r2 = -0.5*((ddx%operate(rhu*uuu))+(uuu*ddx%operate(rhu))+ (rhu*ddx%operate(uuu))) - &
                !     ddx%operate((gam-1.)/gam*rho*tem)+dsx%operate(4./3.*pra*uuu)
                !r3 = -0.5*(ddx%operate(rhe*uuu)+rhe/rho*ddx%operate(rhu)+rhu*ddx%operate(rhe/rho))- &
                !     ddx%operate((gam-1.)/gam*rho*tem*uuu)+dsx%operate(tem+1./lew*(yya*dha+yyb*dhb))
                !r4 = -0.5*(ddx%operate(rya*uuu)+yya*ddx%operate(rhu)+rhu*ddx%operate(yya))+dsx%operate(1./lew*yya)-wwx

                te1 = ddx%operate(rhu*uuu)
                te2 = ddx%operate(rhu)
                te3 = ddx%operate(uuu)
                te4 = ddx%operate((gam-1.)/gam*rho*tem)
                te5 = dsx%operate(4./3.*pra*uuu)
                te6 = te1+uuu*te2+rhu*te3

                r1 = (-1._R_P)* te2
                r2 = (-0.5_R_P)*te6-te4+te5

                te1 = ddx%operate(rhe*uuu)
                !te2 keeping the value
                te3 = ddx%operate(rhe/rho)
                te4 = ddx%operate((gam-1._R_P)/gam*rho*tem*uuu)
                te5 = dsx%operate(tem+1._R_P/lew*yya*dha)
                te6 = te1+rhe/rho*te2+rhu*te3

                r3 = (-0.5_R_P)*te6-te4+te5

                te1 = ddx%operate(rya*uuu)
                !te2 keeping the value
                te3 = ddx%operate(rya/rho)
                te5 = dsx%operate(1._R_P/lew*yya)
                te6 = te1+rya/rho*te2+rhu*te3

                r4 = (-0.5_R_P)*te6+te5-wwx

                !RIMETTERE ! left boundary inlet radiative conditions
                !RIMETTERE csp = sqrt((gam-1._R_P)*tem_cur%val(1))
                !RIMETTERE dya = r4_cur%val(1)/rho_cur%val(1)-rya_cur%val(1)/(rho_cur%val(1)*rho_cur%val(1))*r1_cur%val(1)
                !RIMETTERE duu = r2_cur%val(1)/rho_cur%val(1)-rhu_cur%val(1)/(rho_cur%val(1)*rho_cur%val(1))*r1_cur%val(1)
                !RIMETTERE dee = r3_cur%val(1)/rho_cur%val(1)-rhe_cur%val(1)/(rho_cur%val(1)*rho_cur%val(1))*r1_cur%val(1)
                !RIMETTERE dth = gam*(dee-uuu_cur%val(1)*duu-dha*dya)
                !RIMETTERE dr1 = duu-(gam-1._R_P)/(gam*csp)*(dth+tem_cur%val(1)/rho_cur%val(1)*r1_cur%val(1))
                !RIMETTERE duu = 0._R_P
                !RIMETTERE r1_cur%val(1) = rho_cur%val(1)*csp/(((gam- 1._R_P)/gam)*tem_cur%val(1))*(duu-dr1)
                !RIMETTERE dee = uuu_cur%val(1)*duu
                !RIMETTERE r2_cur%val(1) = rho_cur%val(1)*duu+uuu_cur%val(1)*r1_cur%val(1)
                !RIMETTERE r3_cur%val(1) = rhe_cur%val(1)/rho_cur%val(1)*r1_cur%val(1)+rho_cur%val(1)*dee
                !RIMETTERE r4_cur%val(1) = yya_cur%val(1)*r1_cur%val(1)
                !RIMETTERE !'1' : {'lhs':'uuu','equation':'rhu/rho'},
                !RIMETTERE !'2' : {'lhs':'yya','equation':'rya/rho'},
                !RIMETTERE !'3' : {'lhs':'tem','equation':'gam*((rhe-rya*dha)/rho-0.5*(uuu**2)+1.)'},
                !RIMETTERE !'4' : {'lhs':'csp','equation':'sqrt((gam-1.)*tem)'},
                !RIMETTERE !'5' : {'lhs':'dya','equation':'rhs_rya/rho-rya /rho**2*rhs_rho'},
                !RIMETTERE !'6' : {'lhs':'duu','equation':'rhs_rhu/rho-rhu /rho**2*rhs_rho'},
                !RIMETTERE !'7' : {'lhs':'dee','equation':'rhs_rhe/rho-rhe /rho**2*rhs_rho'},
                !RIMETTERE !'8' : {'lhs':'dth','equation':'gam*(dee-uuu*duu- dha*dya)'},
                !RIMETTERE !'9' : {'lhs':'dr1','equation':'duu-(gam-1.)/(gam*csp)*(dth+ tem/rho*rhs_rho)'},
                !RIMETTERE !'10' : {'lhs':'duu','equation':'0.'},
                !RIMETTERE !'11' : {'lhs':'rhs_rho','equation':'rho*csp/(((gam- 1.)/gam)*tem)*(duu-dr1)'},
                !RIMETTERE !'12' : {'lhs':'dee','equation':'uuu*duu'},
                !RIMETTERE !'13' : {'lhs':'rhs_rhu','equation':'rho*duu+uuu*rhs_rho'},
                !RIMETTERE !'14' : {'lhs':'rhs_rhe','equation':'rhe/rho *rhs_rho+rho*dee'},
                !RIMETTERE !'15' : {'lhs':'rhs_rya','equation':'yya*rhs_rho'},

                !RIMETTERE ! right boundary non-reflecting radiative conditions
                !RIMETTERE csp = sqrt((gam-1._R_P)*tem_cur%val(n))
                !RIMETTERE dya = r4_cur%val(n)/rho_cur%val(n)-rya_cur%val(n)/(rho_cur%val(n)*rho_cur%val(n))*r1_cur%val(n)
                !RIMETTERE duu = r2_cur%val(n)/rho_cur%val(n)-rhu_cur%val(n)/(rho_cur%val(n)*rho_cur%val(n))*r1_cur%val(n)
                !RIMETTERE dee = r3_cur%val(n)/rho_cur%val(n)-rhe_cur%val(n)/(rho_cur%val(n)*rho_cur%val(n))*r1_cur%val(n)
                !RIMETTERE dth = gam*(dee-uuu_cur%val(n)*duu-dha*dya)
                !RIMETTERE dr2 = duu+(gam-1._R_P)/(gam*csp)*(dth+tem_cur%val(n)/rho_cur%val(n)*r1_cur%val(n))
                !RIMETTERE r1_cur%val(n) = gam*rho_cur%val(n)*csp*dr2/(2._R_P*tem_cur%val(n)*(gam-1._R_P))-&
                !RIMETTERE                 rho_cur%val(n)/tem_cur%val(n)*dth
                !RIMETTERE r2_cur%val(n) = 0.5_R_P*rho_cur%val(n)*dr2+uuu_cur%val(n)*r1_cur%val(n)
                !RIMETTERE dee = 1._R_P/gam*dth+dya*dha+uuu_cur%val(n)*0.5_R_P*dr2
                !RIMETTERE r3_cur%val(n) = rhe_cur%val(n)/rho_cur%val(n)*r1_cur%val(n)+rho_cur%val(n)*dee
                !RIMETTERE r4_cur%val(n) = rya_cur%val(n)/rho_cur%val(n)*r1_cur%val(n)+rho_cur%val(n)*dya
                !RIMETTERE !'1' : {'lhs':'uuu','equation':'rhu/rho' },
                !RIMETTERE !'2' : {'lhs':'tem','equation':'gam*((rhe-rya*dha-ryb*dhb)/rho-0.5*uuu**2)+1.'},
                !RIMETTERE !'3' : {'lhs':'csp','equation':'sqrt((gam-1.)*tem)'},
                !RIMETTERE !'4' : {'lhs':'dya','equation':'rhs_rya/rho-rya/rho**2*rhs_rho'},
                !RIMETTERE !'5' : {'lhs':'dyb','equation':'rhs_ryb/rho-ryb/rho**2*rhs_rho'},
                !RIMETTERE !'6' : {'lhs':'duu','equation':'rhs_rhu/rho-rhu/rho**2*rhs_rho'},
                !RIMETTERE !'7' : {'lhs':'dee','equation':'rhs_rhe/rho-rhe/rho**2*rhs_rho'},
                !RIMETTERE !'8' : {'lhs':'dth','equation':'gam*(dee-uuu*duu-dha*dya-dhb*dyb)'},
                !RIMETTERE !'9' : {'lhs':'dr2','equation':'duu+(gam-1.)/(gam*csp)*(dth+tem/rho*rhs_rho)'},
                !RIMETTERE !'10' : {'lhs':'rhs_rho','equation':'gam*rho*csp*dr2/(2.*tem*(gam-1.))-rho/tem*dth'},
                !RIMETTERE !'11' : {'lhs':'rhs_rhu','equation':'0.5*rho*dr2+uuu*rhs_rho'},
                !RIMETTERE !'12' : {'lhs':'dee','equation':'1./gam*dth+dya*dha+dyb*dhb+uuu*0.5*dr2'},
                !RIMETTERE !'13' : {'lhs':'rhs_rhe','equation':'rhe/rho*rhs_rho+rho*dee'},
                !RIMETTERE !'14' : {'lhs':'rhs_rya','equation':'rya/rho*rhs_rho+rho*dya'},
                !RIMETTERE !'15' : {'lhs':'rhs_ryb','equation':'ryb/rho*rhs_rho+rho*dyb'},

        endassociate

!'1' : {'equation':'-#[ddx]{rhu}'},
!'2' : {'equation':'-0.5*(#[ddx]{rhu*uuu}+uuu*#[ddx]{rhu}+rhu*#[ddx]{uuu})-#[ddx]{(gam-1.)/gam*rho*tem}+#[dsx]{4./3.*pra*uuu}'},
!#'2': {'equation':'-#[ddx]{rho*uuu**2+(gam-1.)/gam*rho*tem}+#[dsx]{4./3.*pra*uuu}'},
!'3' : {'equation':'-0.5*(#[ddx]{rhe*uuu}+rhe/rho*#[ddx]{rhu}+rhu*#[ddx]{rhe/rho})-#[ddx]{(gam-1.)/gam*rho*tem*uuu}+#[dsx]{tem+1./lew*(yya*dha+yyb*dhb)}'},
!#'3': {'equation':'-#[ddx]{(rhe+(gam-1.)/gam*rho*tem)*uuu}+#[dsx]{tem+1./lew*(yya*dha+yyb*dhb)}'},
!'4' : {'equation':'-0.5*(#[ddx]{rya*uuu}+yya*#[ddx]{rhu}+rhu*#[ddx]{yya})+#[dsx]{1./lew*yya}-wwx'}

    end subroutine resid_e

    subroutine resid_i(this, inp, t)
        !< Return the matrix of residuals.
        class(flame_1d_equation_adv), intent(inout) :: this   !< The equation.
        class(field),  intent(in), target, dimension(:) :: inp  !< Input field.
        real(R_P),  intent(in)                       :: t       !< Time.
        class(f2m_d1), pointer          :: dui_opr             !< Dummy pointer of the first derivative operator.
        class(f2m_d2), pointer          :: ddui_opr             !< Dummy pointer of the first derivative operator.

        ddui_opr => this%ddui_opr
        this%ddui = ddui_opr%operate(inp) !RIMETTERE ,1,1)

        dui_opr => this%dui_opr
        this%dui = dui_opr%operate(inp) !RIMETTERE ,1,1)

        this%resvar_i = (-this%a) * this%dui - this%b * this%ddui

    end subroutine resid_i

    ! private methods
    subroutine load_from_json(this, filename, error)
        !< Load mesh definition from JSON file.
        class(flame_1d_equation_adv), intent(inout)        :: this      !< The equation.
        character(*),                  intent(in)            :: filename  !< File name of JSON file.
        integer(I_P),                  intent(out), optional :: error     !< Error status.
        character(len=:), allocatable                        :: mesh_type !< Mesh type.
        type(json_file)                                      :: json      !< JSON file handler.
        logical                                              :: found     !< Flag inquiring the result json parsing.

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
            allocate(spatial_operator_d1_FD_1D :: this%du_opr)
            allocate(spatial_operator_d2_FD_1D :: this%ddu_opr)
            call json%get('equation.a', this%a, found)
            if (json%failed()) then
                call json%print_error_message(stderr) ; stop
            end if
            if (.not.found) then
                write(stderr, "(A)")' error: equation definition of "'//filename//'" incomplete!'
                write(stderr, "(A)")'   "a" missing'
                stop
            endif
            call json%get('equation.b', this%b, found)
            if (json%failed()) then
                call json%print_error_message(stderr) ; stop
            end if
            if (.not.found) then
                write(stderr, "(A)")' error: equation definition of "'//filename//'" incomplete!'
                write(stderr, "(A)")'   "b" missing'
                stop
            endif
        else
            write(stderr, "(A)")' error: mesh definition of "'//filename//'" is not "finite difference 1D"!'
            stop
        endif
    endsubroutine load_from_json

    character(len=8) function int2str(k)
    !   "Convert an integer to string."
        integer, intent(in) :: k
        write (int2str, "(I8.8)") k
        int2str = adjustl(int2str)
    end function int2str
end module flame_1d_FD_1D_m


program flame_1d_FD_1D
    !< Openpde test: scalar simple equation for Finite Difference 1D methods.

    use openpde
    use flame_1d_FD_1D_m

    class(mesh), allocatable                :: mesh_         !< The mesh.
    class(field), allocatable, dimension(:) :: u             !< The field.
    class(integrator_adv), allocatable      :: integrator_   !< The integrator.
    class(equation_adv), allocatable        :: equation_     !< The equation.
    integer(I_P)                            :: itmin=0       !< Fist time step.
    integer(I_P)                            :: itmax=100000  !< Last time step.
    integer(I_P)                            :: itout=1000  !< Last time step.
    integer(I_P)                            :: n_equ=4       !< Number of equations
    integer(I_P)                            :: it            !< Time step counter.
    integer(I_P)                            :: ie            !< Number of fields counter
    integer(I_P)                            :: er            !< Error status.
    character(16)                           :: output_name   !< Output file name.
    logical                                 :: json_found    !< Flag inquiring the presence of json input file.
    character(len=2)                        :: temp          !< Temporary string
    class(field_FD_1D), pointer             :: inp_cur(:)    !< Field input pointer.

    allocate(mesh_FD_1D :: mesh_)
    allocate(field_FD_1D :: u(n_equ))
    allocate(flame_1d_equation_adv :: equation_)
    !EXPLICIT allocate(integrator_adv_euler_explicit :: integrator_)
    !allocate(integrator_adv_euler_implicit :: integrator_)
    allocate(integrator_adv_rk_implicit :: integrator_)
    inquire(file='flame_1d_FD_1D.json', exist=json_found)
    if (json_found) then
        print*,"json found"
        call mesh_%init(filename='flame_1d_FD_1D.json')
        do ie = 1,n_equ
            call u(ie)%init(field_mesh=mesh_)
        enddo
        call equation_%init(filename='flame_1d_FD_1D.json',n_equ=n_equ, field_mesh=mesh_,inp=u)
        call integrator_%init(filename='flame_1d_FD_1D.json', equ=equation_)
    else
        print*,"json not found"
        call mesh_%init()
        do ie = 1,n_equ
            call u(ie)%init(field_mesh=mesh_)
        enddo
        call equation_%init(n_equ=n_equ, field_mesh=mesh_,inp=u)
        call integrator_%init(equ=equation_)
    endif

    output_name = "out_XXXXXXXX.dat"
    write(output_name(5:12),"(I8.8)") itmin
    do ie = 1,n_equ
        write(temp,"(I2.2)") ie
        call u(ie)%output(output_name//"_equ_"//temp, error=er)
    enddo
    !STOP
    do it = itmin, itmax
        er = integrator_%integrate(inp=u, equ=equation_, t=it*integrator_%dt)
        if(mod(it,itout)==0) then
            print*,'it: ',it
            write(output_name(5:12),"(I8.8)") it
            do ie = 1,n_equ
                write(temp,"(I2.2)") ie
                call u(ie)%output(output_name//"_equ_"//temp, error=er)
            enddo
        endif
    enddo
    call mesh_%output(error=er)
end program flame_1d_FD_1D
