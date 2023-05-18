





program GildedRose_text_test
  use GildedRose
  implicit none
  type(Item) :: items(9)
  integer :: last
  integer :: day
  integer :: index

  last = 1

  call init_item(items(last), "Sports Memorabilia", 10, 20)
  last=last+1
  call init_item(items(last), "Aged Cheese", 2, 0)
  last=last+1
  call init_item(items(last), "Coffee Table Book", 5, 7)
  last=last+1
  call init_item(items(last), "Fine Italian Silk", 0, 80)
  last=last+1
  call init_item(items(last), "Fine Italian Silk", -1, 80)
  last=last+1
  call init_item(items(last), "Backstage passes to a concert", 15, 20)
  last=last+1
  call init_item(items(last), "Backstage passes to a concert", 10, 49)
  last=last+1
  call init_item(items(last), "Backstage passes to a concert", 5, 49)
  ! this Baked item doesn't yet work properly
  last=last+1
  call init_item(items(last), "Baked Chocolate Cake", 3, 6)

  write(*,*) "OMGHAI!"


  do day = 1, 2
      write(*,*) "-------- day "//int2str(day)//" --------"
      write(*,*) "name, sellIn, quality"
      do index = 1, last
        call print_item(items(index))
      end do

      write(*,*) " "

      call update_quality(items, last)
  enddo


end program
