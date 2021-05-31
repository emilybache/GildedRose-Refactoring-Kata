module GildedRose
    implicit none
    private
    save

    public init_item
    public print_item
    public update_quality
    public int2str

    integer, parameter, public :: MAX_STR_LEN = 80

    type, public :: Item
        character(len=MAX_STR_LEN) :: name
        integer :: sellIn
        integer :: quality
    end type

contains

    subroutine init_item(it, name, sellIn, quality)
       type(Item) :: it
       character(len=*) :: name 
       integer :: sellIn 
       integer :: quality 

       it%name = trim(name)
       it%sellIn = sellIn
       it%quality = quality

    end  

    function int2str( int_val )
      implicit none
      integer :: int_val
      character( int(log10(real(max(abs(int_val),1)))) + 1 + &
          (1-sign(1,int_val))/2 ) :: int2str
      character(64) :: all_chars      
      write(all_chars,*) int_val
      int2str = trim( adjustl( all_chars ) )
    end function
      
    subroutine print_item(it)
      type(Item) :: it 
      write(*,*) trim(it%name), ', ', int2str(it%sellIn), ', ', int2str(it%quality)
    end subroutine 
      
    subroutine update_quality(items, size)
        type(Item), dimension(:) :: items
        integer :: size 

        integer :: i 

        do i = 1, size
            if (items(i)%name /= "Aged Brie" .and. items(i)%name /= "Backstage passes to a TAFKAL80ETC concert" ) then

                if (items(i)%quality > 0) then

                    if (items(i)%name /= "Sulfuras, Hand of Ragnaros" ) then

                        items(i)%quality = items(i)%quality - 1
                    endif

                endif

            else

                if (items(i)%quality < 50) then

                    items(i)%quality = items(i)%quality + 1
    
                    if ( items(i)%name =="Backstage passes to a TAFKAL80ETC concert") then

                        if (items(i)%sellIn < 11) then

                            if (items(i)%quality < 50) then

                                items(i)%quality = items(i)%quality + 1

                            endif

                        endif

                        if (items(i)%sellIn < 6) then

                            if (items(i)%quality < 50) then

                                items(i)%quality = items(i)%quality + 1

                            endif

                        endif

                    endif

                endif

            endif
            if (items(i)%name /= "Sulfuras, Hand of Ragnaros") then

                items(i)%sellIn = items(i)%sellIn - 1

            endif
    
            if (items(i)%sellIn < 0) then

                if (items(i)%name /=  "Aged Brie" ) then

                    if (items(i)%name /=  "Backstage passes to a TAFKAL80ETC concert" ) then

                        if (items(i)%quality > 0 ) then

                            if (items(i)%name /= "Sulfuras, Hand of Ragnaros" )then

                                items(i)%quality = items(i)%quality - 1

                            endif

                        endif
                        
                    else

                        items(i)%quality = items(i)%quality - items(i)%quality

                    end if

                else

                    if (items(i)%quality < 50) then

                        items(i)%quality = items(i)%quality + 1

                    end if

                end if

            end if

        end do
  
    end subroutine

    
end module
