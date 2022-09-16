program-id. Add as "Add".

environment division.

input-output section.

file-control.
    select in-items assign 'in-items'.

data division.
file section.
    fd in-items.
    01 item.
        02 sell-in pic s9(2).
        02 quality pic s9(2).
        02 name pic x(50).

working-storage section.
    01 accept-item.
        02 sell-in pic s9(2).
        02 quality pic s9(2).
        02 name pic x(50).


procedure division.
    open extend in-items.
    display "name"
    accept name in accept-item.
    display "sell-in"
    accept sell-in in accept-item.
    display "quality"
    accept quality in accept-item.
    move accept-item to item.
    write item.
    close in-items.
goback.

end program Add.
