program main
  implicit none
  integer :: n, nbase2, nbase3, nbase5
  real(8) :: sec2, sec3, sec5

  nbase2 = 2
  nbase3 = 3
  nbase5 = 5

  open(20,file="halton_sequence.out")
  do n = 1,10000
     call vdCorput_sequence(n,nbase2,sec2)
     call vdCorput_sequence(n,nbase3,sec3)
     call vdCorput_sequence(n,nbase5,sec5)
!     write(*,"(I7,2x,999e26.16e3)")n,sec2,sec3,sec5
     write(20,"(I7,2x,999e26.16e3)")n,sec2,sec3,sec5
  end do
  close(20)
  
end program main
!-----------------------------------------
subroutine vdCorput_sequence(n_in,nbase,vdc_res)
  implicit none
  integer,intent(in) :: n_in, nbase
  real(8),intent(out) :: vdc_res
  integer :: n
  real(8) :: bk

  n = n_in
  bk = 1d0/nbase
  vdc_res = 0d0

  do while(n>0)
     vdc_res = vdc_res + mod(n,nbase)*bk
     n = n/nbase
     bk = bk/nbase
     
  end do
  
end subroutine vdCorput_sequence
