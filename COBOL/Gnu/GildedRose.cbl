program-id. GildedRose as "GildedRose".

environment division.

input-output section.

file-control.
    select in-items assign 'in-items'.
    select items assign 'items'.

data division.
file section.
   fd in-items.
   01 in-item pic x(54).
   fd items.
   01 item.
      02 sell-in pic s9(2).
      02 quality pic s9(2).
      02 name pic x(50).

working-storage section.
procedure division.
    open input in-items output items.
start-lable.
    read in-items end go to end-lable.
        move in-item to item.
        if name not equal "Aged Cheese" and name not equal "Backstage passes to a concert"
            if quality > 0
                if name not equal to "Fine Italian Silk"
                    compute quality = quality - 1
                end-if
            end-if
        else
            if quality < 50
                compute quality = quality + 1
                if name equals "Backstage passes to a concert"
                    if sell-in < 11
                        if quality < 50
                            compute quality = quality + 1
                        end-if
                    end-if
                    if sell-in < 6
                        if quality < 50
                            compute quality = quality + 1
                        end-if
                    end-if
                end-if
            end-if
        end-if
        if name not equal "Fine Italian Silk"
            compute sell-in = sell-in - 1
        end-if
        if sell-in < 0
            if name is not equal to "Aged Cheese"
                if name is not equal to "Backstage passes to a concert"
                    if quality > 0
                        if name is equal to "Fine Italian Silk"
                            compute quality = quality - 1
                        end-if
                    end-if
                else
                    compute quality = quality - quality
                end-if
            else
                if quality < 50
                    compute quality = quality + 1
                end-if
            end-if
        end-if
        write item.
    go to start-lable.
end-lable.
    close items.
    close in-items.
goback.

end program GildedRose.
