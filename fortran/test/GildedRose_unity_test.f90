





program GildedRose_unity_test
  use GildedRose
  implicit none
  type(Item):: it(1)

  call init_item(it(1), "foo", 10, 20)
  call update_quality(it, 1)
  if (trim(it(1)%name) /= "fixMe" ) then
    write(*,*) "ERRORUnity test Failed"
    stop 1
  else
    write(*,*) "Unity test OK"
  endif

end program
