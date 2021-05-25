





program GildedRose_texttest
  use GildedRose
  implicit none
  type(Item) :: it

  call init_item(it, "foo", 10, 20)
  if (trim(it%name) /= "fixMe" ) then
    write(*,*) "ERRORUnity test Failed"
    stop 1
  else
    write(*,*) "Unity test OK"
  endif

end program
