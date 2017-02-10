subroutine dgetrf(M, N, A, LDA, IPIV, INFO)
integer :: M
integer :: N
integer :: LDA
double precision, dimension( LDA, * ) :: A
integer, dimension( * ) :: IPIV
integer :: INFO 
end subroutine dgetrf

subroutine dgetrs(M, N, A, LDA, IPIV, INFO)
integer :: M
integer :: N
integer :: LDA
double precision, dimension( LDA, * ) :: A
integer, dimension( * ) :: IPIV
integer :: INFO 
end subroutine dgetrs
